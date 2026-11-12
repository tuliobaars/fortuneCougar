# ---------------------------------------------------------------------------
# mod_video_poker.R -- Video Poker (Jacks or Better, Deuces Wild, Bonus Poker,
# Joker Poker). Engine: engine_poker.R.
# ---------------------------------------------------------------------------

VP_DENOMS <- c(100, 500, 1000, 5000, 10000, 20000)
vp_game_names <- function() unname(vapply(VP_GAMES, `[[`, "", "name"))
VP_GOOD <- c("three", "straight", "flush", "fh", "four", "four_aces", "four_low",
             "sf", "five", "wild_royal", "four_deuces", "royal")

video_poker_ui <- function(id) {
  ns <- NS(id)
  div(class = "table-game vpoker",
      page_header(tr("Video Poker"), tr("Deal, hold, draw. Play up to 25 hands at once, then go double or nothing."),
                  icon = "\U0001F3B4"),
      div(class = "variant-pills",
          radioButtons(ns("game"), NULL, choiceNames = tr(vp_game_names()),
                       choiceValues = names(VP_GAMES), inline = TRUE)),
      div(class = "rules-line", uiOutput(ns("blurb"))),
      div(class = "vp-machine",
          uiOutput(ns("paytable")),
          div(class = "vp-screen",
              uiOutput(ns("hands_grid")),
              uiOutput(ns("cards")),
              div(class = "vp-message", uiOutput(ns("message"))),
              uiOutput(ns("double_bar"))),
          div(class = "vp-controls",
              div(class = "ctl-group",
                  span(class = "ctl-label", tr("COIN VALUE")),
                  div(class = "bet-chips", lapply(VP_DENOMS, function(v) chip_button(ns("denom"), v, selected = v == 100, selectable = TRUE)))),
              div(class = "ctl-group",
                  span(class = "ctl-label", tr("HANDS")),
                  uiOutput(ns("hands_sel"))),
              div(class = "ctl-group",
                  span(class = "ctl-label", tr("COINS")),
                  div(class = "vp-coin-btns",
                      actionButton(ns("bet_one"), tr("BET ONE"), class = "btn-act"),
                      actionButton(ns("bet_max"), tr("BET MAX"), class = "btn-act gold"))),
              div(class = "ctl-group",
                  span(class = "ctl-label", tr("TOTAL BET")),
                  div(class = "bet-amount", uiOutput(ns("bet_txt")))),
              div(class = "ctl-group",
                  actionButton(ns("suggest"), tr("AUTO HOLD"), class = "btn-act"),
                  actionButton(ns("deal_draw"), tr("DEAL"), class = "btn-deal")))))
}

# A small face-up card for the multi-hand grid ("" = face down).
vp_mini_card <- function(card = NULL, fresh = FALSE) {
  if (is.null(card)) return(span(class = "mcard back"))
  if (card == JOKER) return(span(class = paste("mcard joker", if (fresh) "fresh"), "JK"))
  span(class = paste("mcard", SUIT_COLOR[card_suit(card)], if (fresh) "fresh"),
       RANK_LABELS[card_rank(card)], SUIT_SYMBOLS[card_suit(card)])
}

video_poker_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    st <- reactiveValues(game = "jacks", coins = 5L, denom = 100, hands = 1L, round = NULL,
                         held = rep(FALSE, 5), n = 0L, bank = 0, dbl = NULL)
    busy <- function() !is.null(isolate(st$round)) && isolate(st$round$phase) == "dealt"
    hand_bet <- reactive(st$coins * st$denom)
    total_bet <- reactive(st$hands * st$coins * st$denom)
    max_bet <- function() BET_LADDER[player$max_bet_index()]
    locked <- function() player$toast(tr("Bet locked"), tr("Max bet per hand at your level is %s", fmt_coins(max_bet())), "warning", "\U0001F512")
    end_double <- function() { st$dbl <- NULL; st$bank <- 0 }

    observeEvent(input$game, {
      if (busy()) {
        updateRadioButtons(session, "game", selected = isolate(st$game))
        player$toast(tr("Hand in progress"), tr("Draw first, then switch games."), "warning", "\U0001F3B4")
        return()
      }
      st$game <- input$game; st$round <- NULL; end_double()
    }, ignoreInit = TRUE)
    observeEvent(input$denom, {
      if (busy()) return()
      ok <- input$denom * isolate(st$coins) <= max_bet()
      if (ok) st$denom <- input$denom else locked()
    })
    observeEvent(input$hands, {
      h <- as.integer(input$hands)
      if (busy() || !(h %in% VP_HAND_CHOICES)) return()
      st$hands <- h
    })
    observeEvent(input$bet_one, { if (!busy()) st$coins <- if (st$coins >= 5L) 1L else st$coins + 1L })
    observeEvent(input$bet_max, { if (!busy()) { st$coins <- 5L; deal() } })

    deal <- function() {
      per_hand <- isolate(hand_bet())
      cost <- isolate(total_bet())
      if (per_hand > max_bet()) { locked(); return() }
      if (!player$debit(cost)) {
        player$toast(tr("Not enough coins"), tr("Lower the coin value or play fewer hands."), "warning", "\U0001FA99")
        return()
      }
      end_double()
      st$round <- vp_new_round(isolate(st$game), isolate(st$coins), isolate(st$denom), isolate(st$hands))
      st$held <- rep(FALSE, 5)
      st$n <- isolate(st$n) + 1L
    }

    draw <- function() {
      r <- vp_draw(isolate(st$round), isolate(st$held))
      st$round <- r
      player$credit(r$payout)
      cost <- vp_round_cost(r)
      per_hand <- r$coins * r$denom
      best <- vp_best_hand(r)
      if (best$payout >= 25 * per_hand)
        player$celebrate(if (best$key %in% c("royal", "four_deuces")) "jackpot" else win_tier(r$payout, per_hand),
                         r$payout, toupper(tr(vp_label(best$key, r$game))), tr(VP_GAMES[[r$game]]$name))
      st$bank <- r$payout
      player$record("poker", r$game, NULL, wager = cost, win = r$payout, bet = cost,
                    metrics = c(vp_hands = r$hands, vp_good = sum(r$results$keys %in% VP_GOOD)))
    }

    observeEvent(input$deal_draw, { if (busy()) draw() else deal() })
    observeEvent(input$hold, {
      if (!busy()) return()
      i <- as.integer(input$hold$i)
      h <- st$held; h[i] <- !h[i]; st$held <- h
    })
    observeEvent(input$suggest, {
      if (!busy()) return()
      st$held <- vp_suggest(st$round$hand, st$game)
    })

    # ---- double or nothing -------------------------------------------------------
    observeEvent(input$double, {
      if (busy() || isolate(st$bank) <= 0 || !vp_can_double(isolate(st$dbl))) return()
      d <- isolate(st$dbl)
      st$dbl <- vp_double_start(isolate(st$bank), if (is.null(d)) 0L else d$doubles)
    })
    observeEvent(input$dpick, {
      d <- isolate(st$dbl)
      i <- as.integer(input$dpick$i)
      if (is.null(d) || d$phase != "pick" || !(i %in% 1:4)) return()
      # the stake moves only now, so leaving the game mid-double costs nothing
      if (!player$debit(d$amount)) {
        player$toast(tr("Not enough coins"), tr("Your winnings were already spent elsewhere."), "warning", "\U0001FA99")
        end_double(); return()
      }
      d <- vp_double_pick(d, i)
      player$credit(d$amount)
      st$dbl <- d
      st$bank <- d$amount
      if (d$outcome == "win" && d$doubles >= 3)
        player$celebrate(win_tier(d$amount, d$stake), d$amount, tr("DOUBLED x%d!", 2^d$doubles), tr("Double or Nothing"))
      player$record("poker", "double_up", NULL, wager = d$stake, win = d$amount, bet = d$stake)
    })
    observeEvent(input$take, {
      if (isolate(st$bank) > 0 && !is.null(isolate(st$dbl)))
        player$toast(tr("Winnings collected"), tr("%s coins banked", fmt_coins(isolate(st$bank))), "success", "\U0001FA99")
      end_double()
      st$bank <- 0
    })

    output$blurb <- renderUI(span(tr(VP_GAMES[[st$game]]$blurb)))
    output$bet_txt <- renderUI({
      formula <- if (st$hands > 1L) sprintf("%d \u00D7 %d \u00D7 %s", st$hands, st$coins, fmt_coins(st$denom))
                 else sprintf("%d \u00D7 %s", st$coins, fmt_coins(st$denom))
      tagList(span(class = "vp-bet-total", fmt_coins(total_bet())), span(class = "vp-bet-formula", formula))
    })
    output$hands_sel <- renderUI({
      div(class = "vp-hands-btns", lapply(VP_HAND_CHOICES, function(h)
        tags$button(type = "button", class = paste("vp-hands-btn", if (h == st$hands) "selected"),
                    title = trn(h, "Play %d hand", "Play %d hands", h),
                    onclick = js_set(ns("hands"), h), h)))
    })

    output$paytable <- renderUI({
      g <- VP_GAMES[[st$game]]
      r <- st$round
      hit <- if (!is.null(r) && r$phase == "done") unique(r$results$keys %||% r$key) else if (!is.null(r)) r$dealt_key else ""
      tags$table(class = "vp-paytable",
                 tags$thead(tags$tr(tags$th(tr("Hand")), lapply(1:5, function(k) tags$th(class = if (k == st$coins) "col-active", k)))),
                 tags$tbody(lapply(seq_len(nrow(g$table)), function(i) {
                   key <- g$table$key[i]
                   tags$tr(class = if (key %in% hit) "row-hit",
                           tags$td(tr(g$table$label[i])),
                           lapply(1:5, function(k) tags$td(class = if (k == st$coins) "col-active",
                                                          fmt_coins(if (key == "royal" && k == 5) 4000 else g$table$pay[i] * k))))
                 })))
    })

    # The extra hands (2..N) of a multi-hand round.
    output$hands_grid <- renderUI({
      r <- st$round
      n <- if (is.null(r)) st$hands else r$hands
      if (n <= 1L || !is.null(st$dbl)) return(NULL)
      held <- if (!is.null(r) && r$phase == "dealt") st$held else if (!is.null(r)) r$held else rep(FALSE, 5)
      div(class = paste0("vp-hands-grid n", n),
          lapply(2:n, function(h) {
            done <- !is.null(r) && r$phase == "done"
            key <- if (done) r$results$keys[h] else ""
            pay <- if (done) r$results$payouts[h] else 0
            div(class = paste("vp-mini-hand", if (pay > 0) "won", if (done && pay == 0) "lost"),
                span(class = "mh-no", h),
                span(class = "mh-cards", lapply(1:5, function(j) {
                  if (is.null(r)) vp_mini_card(NULL)
                  else if (done) vp_mini_card(r$results$cards[h, j], fresh = !held[j])
                  else if (held[j]) vp_mini_card(r$hand[j])
                  else vp_mini_card(NULL)
                })),
                span(class = "mh-res", if (pay > 0) paste(tr(vp_label(key, r$game)), fmt_short(pay)) else ""))
          }))
    })

    output$cards <- renderUI({
      d <- st$dbl
      if (!is.null(d)) {
        return(div(class = "vp-double",
                   div(class = "vp-dbl-dealer", span(class = "ctl-label", tr("DEALER")),
                       playing_card(d$dealer, class = "vp-card deal-in d1")),
                   div(class = "vp-dbl-picks", span(class = "ctl-label", if (d$phase == "pick") tr("PICK A HIGHER CARD") else tr("YOUR CARD")),
                       div(class = "vp-cards",
                           lapply(1:4, function(i) {
                             if (d$phase == "pick")
                               div(class = "vp-dbl-slot", onclick = js_set(ns("dpick"), sprintf("{i: %d, n: Math.random()}", i)),
                                   playing_card(1, hidden = TRUE, class = "clickable"))
                             else playing_card(d$cards[i], class = paste("vp-card", if (i == d$chosen) paste("chosen", d$outcome) else "dim"))
                           })))))
      }
      r <- st$round
      if (is.null(r)) {
        return(div(class = "vp-cards", lapply(1:5, function(i) playing_card(1, hidden = TRUE))))
      }
      fresh <- if (r$phase == "done") which(!r$held) else 1:5
      div(class = "vp-cards",
          lapply(1:5, function(i) {
            held <- if (r$phase == "dealt") st$held[i] else r$held[i]
            playing_card(r$hand[i], held = held,
                         class = paste("vp-card", if (r$phase == "dealt") "clickable",
                                       if (i %in% fresh) paste0("deal-in d", i)),
                         onclick = if (r$phase == "dealt") js_set(ns("hold"), sprintf("{i: %d, n: Math.random()}", i)) else NULL)
          }))
    })

    output$message <- renderUI({
      r <- st$round
      d <- st$dbl
      if (!is.null(d)) {
        if (d$phase == "pick") return(span(tr("Beat the dealer's card to win %s (aces high).", fmt_coins(2 * d$amount))))
        return(switch(d$outcome,
                      win = span(class = "vp-win", tr("Doubled! You now have %s", fmt_coins(d$amount))),
                      push = span(tr("Push - tie with the dealer. You still have %s", fmt_coins(d$amount))),
                      lose = span(class = "vp-lose", tr("Lost the double. Deal again!"))))
      }
      if (is.null(r)) return(span(tr("Choose hands and a coin value, then DEAL (or BET MAX).")))
      if (r$phase == "dealt") {
        lab <- vp_label(r$dealt_key, r$game)
        return(span(if (nzchar(lab)) paste0(tr("Dealt: %s", tr(lab)), " \u2014 ") else "", tr("Click cards to HOLD, then DRAW.")))
      }
      if (r$hands > 1L) {
        won <- sum(r$results$payouts > 0)
        main <- if (r$results$payouts[1] > 0) paste0(tr(vp_label(r$key, r$game)), " \u2022 ") else ""
        if (r$payout > 0) return(span(class = "vp-win", main, trn(won, "%d of %d hands won - total %s", "%d of %d hands won - total %s",
                                                                   won, r$hands, fmt_coins(r$payout))))
        return(span(tr("No winning hands. Deal again!")))
      }
      if (r$payout > 0) span(class = "vp-win", tr("%s! You win %s", tr(vp_label(r$key, r$game)), fmt_coins(r$payout)))
      else span(tr("No win. Deal again!"))
    })

    # Double or nothing / take win buttons
    output$double_bar <- renderUI({
      r <- st$round; d <- st$dbl
      if (is.null(r) || r$phase != "done" || st$bank <= 0) return(NULL)
      if (!is.null(d) && d$phase == "pick") return(NULL)
      if (!vp_can_double(d)) return(div(class = "vp-double-bar", span(class = "ctl-label", tr("Maximum doubles reached")),
                                        actionButton(ns("take"), tr("TAKE WIN"), class = "btn-act")))
      div(class = "vp-double-bar",
          actionButton(ns("double"), if (is.null(d)) tr("DOUBLE OR NOTHING %s", fmt_short(st$bank))
                                     else tr("DOUBLE AGAIN %s", fmt_short(st$bank)), class = "btn-deal gold"),
          actionButton(ns("take"), tr("TAKE WIN"), class = "btn-act"),
          if (!is.null(d)) span(class = "vp-dbl-count", tr("Doubles: %d / %d", d$doubles, VP_MAX_DOUBLES)))
    })

    observe({
      lab <- if (!is.null(st$round) && st$round$phase == "dealt") tr("DRAW") else tr("DEAL")
      updateActionButton(session, "deal_draw", label = lab)
    })
    invisible(st)
  })
}
