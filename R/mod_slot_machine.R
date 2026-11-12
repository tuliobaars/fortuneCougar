# ---------------------------------------------------------------------------
# mod_slot_machine.R -- generic slot machine module (UI + server).
#
# The server computes the full outcome of a spin instantly with the engine
# (slot_play) and ships it to the browser as a list of animation frames
# (`fc_spin` custom message). inst/app/www/casino.js spins the reels, plays cascades /
# respins / nudges frame by frame and then highlights the wins, so the UI
# stays snappy while all maths live in R.
# ---------------------------------------------------------------------------

slot_machine_ui <- function(id, def) {
  ns <- NS(id)
  init <- with_seed(str_seed(def$id) + 99L, spin_grid(def))
  reels <- div(
    class = "reels", id = ns("reels"),
    style = sprintf("--reels:%d; --rows:%d;", def$reels, def$rows),
    lapply(seq_len(def$reels), function(r)
      div(class = "reel", `data-reel` = r,
          lapply(seq_len(def$rows), function(w) sym_cell(def, init[w, r]))))
  )
  div(
    class = paste("slot-machine", def$theme, paste0("reels-", def$reels), paste0("rows-", def$rows),
                  if (def$dual_screen) "dual"),
    id = ns("machine"), `data-symbols` = symbol_map_json(def),
    `data-spin` = ns("spin"), `data-bonus` = ns("bonus_active"),
    div(class = "sm-top",
        div(class = "sm-title",
            span(class = "sm-art", HTML(def$art)),
            div(h3(def$name), p(tr(def$tagline)))),
        div(class = "sm-badges",
            span(class = "badge-vol", tr(def$volatility)),
            if (!is.na(def$rtp %||% NA)) span(class = "badge-rtp", paste0("RTP ", fmt_num(100 * def$rtp), "%")),
            popover(
              tags$button(type = "button", class = "btn-info-round", title = tr("Paytable & rules"), "i"),
              paytable_ui(def), title = tr("%s - paytable", def$name), placement = "bottom"))),
    uiOutput(ns("meter")),
    if (def$dual_screen) div(class = "top-screen", uiOutput(ns("top_screen"))),
    div(class = "reels-wrap",
        div(class = "reels-frame", reels),
        div(class = "reel-note", id = ns("note")),
        div(class = "win-banner", id = ns("banner")),
        if (!def$dual_screen) div(class = "bonus-layer", uiOutput(ns("bonus")))),
    div(class = "sm-controls",
        div(class = "ctl-group bet-group",
            span(class = "ctl-label", tr("TOTAL BET")),
            div(class = "bet-stepper",
                actionButton(ns("bet_down"), "\u2212", class = "btn-step"),
                div(class = "bet-val", textOutput(ns("bet_txt"), inline = TRUE)),
                actionButton(ns("bet_up"), "+", class = "btn-step")),
            actionButton(ns("bet_max"), tr("MAX BET"), class = "btn-maxbet")),
        div(class = "ctl-group win-group",
            span(class = "ctl-label", tr("WIN")),
            div(class = "win-val", id = ns("winval"), "0")),
        div(class = "ctl-group spin-group",
            tags$button(type = "button", class = "btn-auto", id = ns("auto"),
                        `data-spin` = ns("spin"), tr("AUTO")),
            actionButton(ns("spin"), label = span(class = "spin-label", tr("SPIN")), class = "btn-spin"))),
    div(class = "sm-status", uiOutput(ns("status")))
  )
}

# @param def machine definition
# @param player global player state (state_player.R)
# @param linked optional server-wide linked jackpot pool (Bank of Jackpots)
slot_machine_server <- function(id, def, player, linked = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    st <- reactiveValues(bet_i = 4L, gs = slot_init_state(def), bonus = NULL,
                         last = NULL, spins = 0L)
    bet <- reactive(BET_LADDER[st$bet_i])

    # ---- bet controls ----------------------------------------------------
    observeEvent(input$bet_up, {
      mx <- player$max_bet_index()
      if (st$bet_i < mx) st$bet_i <- st$bet_i + 1L
      else if (mx < length(BET_LADDER))
        player$toast(tr("Bet locked"), tr("Reach level %d to bet %s", as.integer(BET_UNLOCK_LEVEL[mx + 1]), fmt_coins(BET_LADDER[mx + 1])), "warning", "\U0001F512")
    })
    observeEvent(input$bet_down, { if (st$bet_i > 1L) st$bet_i <- st$bet_i - 1L })
    observeEvent(input$bet_max, {
      mx <- player$max_bet_index()
      aff <- which(BET_LADDER <= player$coins())
      st$bet_i <- as.integer(max(1L, min(mx, if (length(aff)) max(aff) else 1L)))
    })
    output$bet_txt <- renderText(fmt_coins(bet()))

    # ---- spin --------------------------------------------------------------
    observeEvent(input$spin, {
      if (!is.null(st$bonus)) {
        player$toast(tr("Bonus in progress"), tr("Finish the bonus game first!"), "warning", "\U0001F381")
        session$sendCustomMessage("fc_auto_stop", list(id = ns("auto")))
        return()
      }
      gs <- modifyList(st$gs, player$get_meta(def$id))
      free <- (gs$fs_left %||% 0) > 0
      b <- bet()
      if (!free && !player$can_afford(b)) {
        player$toast(tr("Not enough coins"), tr("Lower your bet or spin the Wheel of Fun for free coins."), "warning", "\U0001FA99")
        session$sendCustomMessage("fc_auto_stop", list(id = ns("auto")))
        return()
      }
      pots <- if (!is.null(linked)) isolate(linked$pots) else NULL
      res <- slot_play(def, b, gs, now = now_secs(), jackpots = pots)
      player$debit(res$cost)
      if (!is.null(linked) && !is.null(res$jp_contrib)) {
        linked$pots <- isolate(linked$pots) + res$jp_contrib[names(JACKPOT_SEEDS)] * res$cost / max(res$bet, 1)
        linked_jackpots_save(isolate(linked$pots))
      }
      win <- if (res$win > 0) player$boost_slot_win(res$win) else 0
      player$credit(win)
      st$gs <- res$gs
      player$set_meta(def$id, res$gs)
      st$spins <- st$spins + 1L
      if (!is.null(res$bonus)) st$bonus <- res$bonus

      g <- res$gs
      session$sendCustomMessage("fc_spin", list(
        machine = ns("machine"),
        frames = lapply(res$frames, function(f) list(
          g = f$grid, h = f$hl, c = f$cls, b = f$badge, k = f$hold,
          n = f$note, w = f$win, m = f$mode)),
        win = win, bet = res$bet, tier = win_tier(win, res$bet),
        balance = player$coins(),
        free = res$free, fs_left = g$fs_left, fs_total = g$fs_total,
        fs_awarded = res$fs_award, fs_ended = res$fs_ended, fs_won = g$fs_won,
        bonus = !is.null(res$bonus),
        stop_auto = !is.null(res$bonus)
      ))
      st$last <- list(win = win, bet = res$bet, parts = res$parts, free = res$free,
                      fs_award = res$fs_award, fs_ended = res$fs_ended, fs_won = g$fs_won)
      player$record("slots", def$id, def$category, wager = res$cost, win = win,
                    bet = res$bet, events = res$events)
    })

    # ---- bonus games --------------------------------------------------------
    observeEvent(input$pick, {
      b <- st$bonus
      req(b, !isTRUE(b$done))
      b <- bonus_pick(b, input$pick)
      if (isTRUE(b$done)) {
        s <- bonus_settle(def, st$gs, b, if (!is.null(linked)) isolate(linked$pots) else NULL)
        st$gs <- s$gs
        player$set_meta(def$id, s$gs)
        b$paid <- s$win
        if (s$win > 0) {
          player$credit(s$win)
          delay <- if (identical(b$type, "wheel")) 4400 else 400
          player$celebrate(if (!is.null(s$jackpot)) "jackpot" else win_tier(s$win, b$bet %||% bet()),
                           s$win, if (!is.null(s$jackpot)) tr("%s JACKPOT!", tr(toupper(s$jackpot))) else b$title,
                           def$name, delay = delay)
          session$sendCustomMessage("fc_win_value", list(id = ns("winval"), value = s$win, delay = delay))
        }
        if (identical(b$type, "match3") && !is.null(linked)) {
          pots <- isolate(linked$pots); pots[[s$jackpot]] <- JACKPOT_SEEDS[[s$jackpot]]
          linked$pots <- pots
          linked$last <- list(tier = s$jackpot, amount = s$win, time = Sys.time())
          linked_jackpots_save(pots)
          if (!is.null(player$id())) player$store$jackpot_won(player$id(), s$jackpot, s$win)
        }
        if (identical(b$type, "volatility")) {
          player$toast(tr("Free spins!"), tr("%d free spins at x%g - press SPIN", b$fs_award, b$fs_mult), "success", "\U0001F42F")
        }
        player$record("slots", def$id, def$category, wager = 0, win = s$win, bet = b$bet %||% bet(),
                      events = c("bonus", if (!is.null(s$jackpot)) "jackpot"))
      }
      st$bonus <- b
    })
    observeEvent(input$bonus_close, { st$bonus <- NULL })

    bonus_view <- reactive({ bonus_panel_ui(st$bonus, ns) })
    if (def$dual_screen) {
      output$top_screen <- renderUI({
        bonus_view() %||% div(class = "top-screen-idle",
                              div(class = "ts-title", paste("\U0001F3AF", tr("SALOON SHOOTING GALLERY"))),
                              div(class = "ts-sub", tr("Land 3+ Sheriff Badges to open the gallery")),
                              div(class = "ts-bottles", lapply(1:12, function(i) span("\U0001F37E"))))
      })
    } else {
      output$bonus <- renderUI(bonus_view())
    }

    # ---- meters & status -------------------------------------------------------
    output$meter <- renderUI({
      gs <- modifyList(st$gs, player$rv$game_meta[[def$id]] %||% list())
      items <- if (!is.null(def$meter)) def$meter(gs, bet(), if (!is.null(linked)) linked$pots else NULL) else list()
      if ((gs$fs_left %||% 0) > 0 || isTRUE(st$last$fs_ended)) {
        items <- c(list(list(label = tr("FREE SPINS"), text = tr("%d / %d  \u2022  x%g  \u2022  won %s",
                                                                  as.integer(gs$fs_left), as.integer(gs$fs_total),
                                                                  gs$fs_mult, fmt_coins(gs$fs_won)),
                             pct = if (gs$fs_total > 0) 100 * (gs$fs_total - gs$fs_left) / gs$fs_total else 100,
                             kind = "freespins")), items)
      }
      boosts <- player$rv$boosts
      if ((boosts$win20 %||% 0) > 0)
        items <- c(items, list(list(label = tr("WIN BOOST"), text = tr("+20%% \u2022 %d spins", as.integer(boosts$win20)), kind = "boost")))
      meter_ui(items)
    })

    output$status <- renderUI({
      l <- st$last
      if (is.null(l)) return(div(class = "status-idle", tr("Good luck! Press SPIN (or hold the Space bar).")))
      div(class = "status-line",
          if (l$win > 0) span(class = "st-win", tr("Won %s", fmt_coins(l$win)))
          else span(class = "st-lose", if (l$free) tr("Free spin - no win") else tr("No win")),
          if (l$fs_award > 0) span(class = "st-fs", tr("+%d FREE SPINS", l$fs_award)),
          if (isTRUE(l$fs_ended)) span(class = "st-fs", tr("Free spins complete: %s", fmt_coins(l$fs_won))),
          if (length(l$parts)) span(class = "st-parts", paste(utils::head(l$parts, 4), collapse = "  \u2022  ")))
    })

    invisible(st)
  })
}

# Render the interactive bonus panel for a bonus object.
bonus_panel_ui <- function(b, ns) {
  if (is.null(b)) return(NULL)
  pick_js <- function(i) js_set(ns("pick"), i)
  tiles <- b$tiles
  body <- switch(b$type,
    wheel = {
      seg <- b$segments
      tagList(
        svg_wheel(ns("bonus_wheel"), tr(seg$label),
                  c("#00a6c8", "#7b2cbf", "#00a6c8", "#ff2e88", "#00a6c8", "#7b2cbf", "#00a6c8", "#ffcc00"),
                  size = 260, text_colors = ifelse(seg$label == "GRAND", "#1a0933", "#ffffff"),
                  spin_to = if (isTRUE(b$done)) b$segment else NULL),
        if (!isTRUE(b$done)) tags$button(class = "btn-bonus-action", onclick = pick_js(1), tr("SPIN THE PEARL WHEEL")))
    },
    volatility = div(class = "vol-options", lapply(seq_len(nrow(tiles)), function(i) {
      tags$button(class = paste("vol-option", if (isTRUE(b$chosen == i)) "chosen"), onclick = pick_js(i),
                  disabled = if (isTRUE(b$done)) NA else NULL,
                  div(class = "vo-tiger", strrep("\U0001F42F", i)),
                  div(class = "vo-label", tiles$label[i]),
                  div(class = "vo-sub", tr(c("Low volatility", "Medium volatility", "High volatility")[i])))
    })),
    {
      cls <- if (b$type == "trail") "grid-3" else if (nrow(tiles) <= 3) "grid-3" else if (nrow(tiles) <= 9) "grid-3x3" else "grid-4x3"
      div(class = paste("bonus-tiles", cls), lapply(seq_len(nrow(tiles)), function(i) {
        t <- tiles[i, ]
        if (isTRUE(t$revealed) || isTRUE(b$done)) {
          div(class = paste("bonus-tile revealed", t$kind, if (!isTRUE(t$revealed)) "unpicked"),
              span(class = "bt-label", if (b$type == "match3") tr(toupper(t$label)) else t$label))
        } else {
          tags$button(class = "bonus-tile", onclick = pick_js(i), span(class = "bt-face", HTML(b$face)))
        }
      }))
    })
  status <- switch(b$type,
    trail = tr("Chamber %d of %d  \u2022  Treasure so far: %s", b$level, b$levels, fmt_coins(b$total)),
    shots = tr("Bullets left: %d  \u2022  Bounty: %s", as.integer(b$picks_left), fmt_coins(b$total)),
    pick_collect = paste0(tr("Collected: %s", fmt_coins(b$total)), if (b$mult > 1) tr(" (x%g at the end)", b$mult) else ""),
    match3 = tr("Match three symbols to win that jackpot"),
    "")
  div(class = paste("bonus-panel", paste0("bonus-", b$type), if (isTRUE(b$done)) "done"),
      div(class = "bonus-title", b$title),
      # (the wheel keeps its subtitle so the result isn't spoiled while it spins)
      div(class = "bonus-sub", if (isTRUE(b$done) && !identical(b$type, "wheel")) paste(utils::tail(b$log, 1), collapse = "") else b$subtitle),
      body,
      div(class = "bonus-status", status),
      if (isTRUE(b$done))
        div(class = "bonus-done",
            if (identical(b$type, "wheel")) div(class = "bonus-sub", paste(utils::tail(b$log, 1), collapse = "")),
            div(class = "bonus-total", if ((b$paid %||% 0) > 0) paste("+", fmt_coins(b$paid)) else if (identical(b$type, "volatility")) tr("Free spins ready!") else tr("Better luck next time!")),
            tags$button(type = "button", class = "btn-collect", onclick = js_set(ns("bonus_close"), "Math.random()"),
                        if (identical(b$type, "volatility")) tr("START FREE SPINS") else tr("COLLECT"))))
}
