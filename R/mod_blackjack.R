# ---------------------------------------------------------------------------
# mod_blackjack.R -- Blackjack tables (Classic, European, Switch, Perfect
# Pairs). Game logic lives in engine_blackjack.R; this module handles bets,
# coins, rendering and progression hooks.
# ---------------------------------------------------------------------------

BJ_CHIPS <- c(100, 500, 1000, 5000, 25000, 100000)
BJ_RULES_TEXT <- c(
  classic = "6 decks \u2022 dealer stands on soft 17 \u2022 double on any two cards \u2022 split to 4 hands \u2022 insurance \u2022 Blackjack pays 3:2",
  european = "6 decks \u2022 NO hole card: the dealer draws after you \u2022 a dealer Blackjack takes all bets \u2022 double on 9-11 \u2022 Blackjack pays 3:2",
  switch = "Play two hands and switch their top cards \u2022 dealer hits soft 17 \u2022 dealer 22 pushes \u2022 Blackjack pays 1:1",
  pairs = "Classic rules + Perfect Pairs side bet: Mixed 6:1 \u2022 Coloured 12:1 \u2022 Perfect 25:1")

blackjack_ui <- function(id) {
  ns <- NS(id)
  div(class = "table-game blackjack",
      page_header(tr("Blackjack"), tr("Four tables, real 6-deck shoes, shuffled with R's RNG."), icon = "\U0001F0CF",
                  right = input_switch(ns("hint"), tr("Strategy hint"), value = FALSE)),
      div(class = "variant-pills",
          radioButtons(ns("variant"), NULL, choiceNames = tr(unname(BJ_VARIANTS)),
                       choiceValues = names(BJ_VARIANTS), inline = TRUE)),
      div(class = "rules-line", uiOutput(ns("rules"))),
      div(class = "felt bj-felt",
          uiOutput(ns("table")),
          div(class = "felt-message", uiOutput(ns("message")))),
      div(class = "table-controls",
          div(class = "action-row", uiOutput(ns("actions"))),
          div(class = "bet-row",
              div(class = "bet-chips",
                  span(class = "ctl-label", tr("BET")),
                  lapply(BJ_CHIPS, function(v) chip_button(ns("chip"), v)),
                  tags$button(type = "button", class = "btn-clear", onclick = js_set(ns("clear"), "Math.random()"), tr("CLEAR"))),
              div(class = "bet-info", uiOutput(ns("bet_info")))),
          uiOutput(ns("side_row"))))
}

blackjack_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    st <- reactiveValues(tb = bj_new_table("classic"), bet = 1000, side = 0,
                         seen = 0L, round = 0L)

    in_round <- function() !(isolate(st$tb$phase) %in% c("betting", "settled"))

    observeEvent(input$variant, {
      if (in_round()) {
        updateRadioButtons(session, "variant", selected = isolate(st$tb$variant))
        player$toast(tr("Hand in progress"), tr("Finish this hand before changing tables."), "warning", "\U0001F0CF")
        return()
      }
      if (!identical(input$variant, isolate(st$tb$variant))) {
        st$tb <- bj_new_table(input$variant)
        st$side <- 0
      }
    }, ignoreInit = TRUE)

    max_bet <- reactive(BET_LADDER[max_bet_index(player$rv$level)])
    observeEvent(input$chip, {
      if (in_round()) return()
      st$bet <- min(st$bet + input$chip, max_bet())
    })
    observeEvent(input$clear, { if (!in_round()) st$bet <- 0 })
    observeEvent(input$side_chip, {
      if (in_round()) return()
      st$side <- if (input$side_chip == 0) 0 else min(st$side + input$side_chip, max_bet() / 2)
    })

    settle <- function(tb) {
      if (tb$phase != "settled") return(tb)
      ret <- bj_round_return(tb); stake <- bj_round_stake(tb)
      player$credit(ret)
      results <- vapply(tb$hands, `[[`, "", "result")
      wins <- sum(vapply(tb$hands, function(h) h$payout > h$bet, logical(1)))
      nat <- sum(results == "Blackjack!")
      if (!is.null(tb$side_result) && tb$side_result$pays > 0)
        player$toast(tr(tb$side_result$kind), tr("Perfect Pairs pays %d:1", as.integer(tb$side_result$pays)), "success", "\U0001F46F")
      net <- ret - stake
      if (net > 0 && ret >= 8 * tb$bet) player$celebrate(win_tier(ret, tb$bet), ret, tr("BLACKJACK WIN!"), tr(BJ_VARIANTS[[tb$variant]]))
      player$record("blackjack", tb$variant, NULL, wager = stake, win = ret, bet = tb$bet,
                    metrics = c(bj_wins = wins, bj_naturals = nat))
      tb
    }

    observeEvent(input$deal, {
      if (in_round()) return()
      tb <- st$tb; bet <- st$bet
      if (bet <= 0) { player$toast(tr("Place a bet"), tr("Click the chips to build your bet."), "warning", "\U0001FA99"); return() }
      side <- if (tb$rules$perfect_pairs) st$side else 0
      cost <- bj_deal_cost(tb, bet, side)
      if (!player$debit(cost)) { player$toast(tr("Not enough coins"), tr("Lower your bet."), "warning", "\U0001FA99"); return() }
      tb <- bj_deal(tb, bet, side)
      if (isTRUE(tb$shuffled)) player$toast(tr("New shoe"), tr("The dealer shuffles a fresh 6-deck shoe."), "info", "\U0001F500")
      st$seen <- 0L
      st$round <- st$round + 1L
      st$tb <- settle(tb)
    })

    observeEvent(input$act, {
      tb <- st$tb
      a <- input$act$a
      if (!(a %in% bj_actions(tb))) return()
      cost <- bj_action_cost(tb, a)
      if (cost > 0 && !player$debit(cost)) {
        player$toast(tr("Not enough coins"), tr("You need %s more to %s.", fmt_coins(cost), tr(c(double = "double", split = "split", insure = "take insurance")[[a]])), "warning", "\U0001FA99")
        return()
      }
      tb <- switch(a,
        hit = bj_hit(tb), stand = bj_stand(tb), double = bj_double(tb), split = bj_split(tb),
        switch = bj_switch(tb, TRUE), keep = bj_switch(tb, FALSE),
        insure = bj_insurance(tb, TRUE), no_insure = bj_insurance(tb, FALSE))
      st$tb <- settle(tb)
    })

    output$rules <- renderUI(span(tr(BJ_RULES_TEXT[[st$tb$variant]])))

    # cards already on screen -> only newly dealt cards get the deal animation
    shown <- new.env(parent = emptyenv())
    shown$round <- -1L; shown$dealer <- 0L; shown$hands <- integer(0)

    output$table <- renderUI({
      tb <- st$tb
      if (!identical(shown$round, st$round)) {
        shown$round <- st$round; shown$dealer <- 0L; shown$hands <- integer(0)
      }
      if (!length(tb$hands)) {
        return(div(class = "bj-empty",
                   div(class = "bj-logo", "\u2660 \u2665 BLACKJACK \u2666 \u2663"),
                   div(class = "bj-sub", tr(BJ_VARIANTS[[tb$variant]])),
                   div(class = "shoe-info", tr("Shoe: %d cards \u2022 %d decks", length(tb$shoe), tb$rules$decks))))
      }
      hide <- tb$hole_hidden && tb$phase %in% c("player", "switch", "insurance", "dealt")
      dcards <- tb$dealer
      dealer_total <- if (hide) bj_value(dcards[1])$total else bj_total(dcards)
      prev_d <- shown$dealer; prev_h <- shown$hands
      deal_cls <- function(i, prev) if (i > prev) paste("deal-in", paste0("d", min(i - prev, 6))) else ""
      # when a split happens the hand indices shift; treat all hands as seen up to their old size
      if (length(prev_h) != length(tb$hands)) prev_h <- rep(if (length(prev_h)) min(prev_h) else 0L, length(tb$hands))
      shown$dealer <- length(tb$dealer) - as.integer(hide)
      shown$hands <- vapply(tb$hands, function(h) length(h$cards), integer(1))
      div(class = "bj-layout",
          div(class = "bj-dealer",
              div(class = "hand-label", tr("DEALER"), span(class = "hand-total", dealer_total)),
              div(class = "cards",
                  lapply(seq_along(dcards), function(i) playing_card(dcards[i], hidden = hide && i == 2,
                                                                     class = deal_cls(i, prev_d))),
                  if (tb$rules$enhc && length(dcards) == 1) div(class = "pcard ghost", tr("no hole card")))),
          div(class = "shoe-info", paste0(tr("Shoe: %d cards left", length(tb$shoe)),
                                          if (length(tb$shoe) <= tb$cut) paste0(" \u2022 ", tr("reshuffle next hand")) else "")),
          div(class = "bj-hands",
              lapply(seq_along(tb$hands), function(i) {
                h <- tb$hands[[i]]
                v <- bj_value(h$cards)
                active <- tb$phase == "player" && i == tb$active
                res_cls <- if (grepl("Win|Blackjack!", h$result)) "res-win" else if (grepl("Push", h$result)) "res-push" else if (nzchar(h$result)) "res-lose" else ""
                div(class = paste("bj-hand", if (active) "active", res_cls),
                    div(class = "cards", lapply(seq_along(h$cards), function(k)
                      playing_card(h$cards[k], class = paste(deal_cls(k, prev_h[i]), if (k == 2 && h$switched) "switched")))),
                    div(class = "hand-meta",
                        span(class = "hand-total", if (v$soft && v$total < 21) paste0(v$total - 10, "/", v$total) else v$total),
                        span(class = "hand-bet", paste("\U0001FA99", fmt_coins(h$bet), if (h$doubled) "(x2)" else "")),
                        if (nzchar(h$result)) span(class = "hand-result", tr(h$result))))
              })),
          if (!is.null(tb$side_result)) div(class = paste("side-result", if (tb$side_result$pays > 0) "hit"),
                                     if (tb$side_result$pays > 0) tr("Perfect Pairs: %s! %d:1", tr(tb$side_result$kind), as.integer(tb$side_result$pays)) else tr("Perfect Pairs: no pair")))
    })

    output$message <- renderUI({
      tb <- st$tb
      hint <- if (isTRUE(input$hint) && tb$phase == "player") bj_hint(tb) else ""
      tagList(span(tb$message),
              if (nzchar(hint)) span(class = "hint-pill", tr("Basic strategy: %s", toupper(tr(hint)))))
    })

    output$actions <- renderUI({
      tb <- st$tb
      acts <- bj_actions(tb)
      btn <- function(a, label, cls = "btn-act") {
        tags$button(type = "button", class = paste(cls, if (!(a %in% acts)) "disabled"),
                    disabled = if (!(a %in% acts)) NA else NULL,
                    onclick = js_set(ns("act"), sprintf("{a: '%s', n: Math.random()}", a)), label)
      }
      if (tb$phase %in% c("betting", "settled")) {
        return(tags$button(type = "button", class = "btn-deal", onclick = js_set(ns("deal"), "Math.random()"),
                           if (tb$phase == "settled") tr("DEAL AGAIN") else tr("DEAL")))
      }
      if (tb$phase == "switch") return(tagList(btn("switch", paste("\u21C4", tr("SWITCH TOP CARDS")), "btn-act gold"), btn("keep", tr("KEEP HANDS"))))
      if (tb$phase == "insurance") return(tagList(btn("insure", tr("INSURANCE (%s)", fmt_coins(bj_insurance_cost(tb))), "btn-act gold"),
                                                  btn("no_insure", tr("NO THANKS"))))
      tagList(btn("hit", tr("HIT")), btn("stand", tr("STAND"), "btn-act red"), btn("double", tr("DOUBLE")), btn("split", tr("SPLIT")))
    })

    output$bet_info <- renderUI({
      tb <- st$tb
      n <- if (tb$rules$switch) 2 else 1
      tagList(span(class = "bet-amount", fmt_coins(st$bet)),
              span(class = "bet-sub", if (n == 2) tr("x2 hands = %s", fmt_coins(2 * st$bet)) else tr("max %s", fmt_coins(max_bet()))))
    })

    output$side_row <- renderUI({
      if (!isTRUE(st$tb$rules$perfect_pairs)) return(NULL)
      div(class = "bet-row side",
          div(class = "bet-chips",
              span(class = "ctl-label", tr("PERFECT PAIRS")),
              lapply(c(100, 500, 1000, 5000), function(v) chip_button(ns("side_chip"), v)),
              tags$button(type = "button", class = "btn-clear", onclick = js_set(ns("side_chip"), "0"), tr("CLEAR"))),
          span(class = "bet-amount", fmt_coins(st$side)))
    })
    invisible(st)
  })
}
