# ---------------------------------------------------------------------------
# mod_meta.R -- Meta & Side Games module: Cougar Album (+ Lucky Cards), Wheel of
# Fun, Star Market, Monsterpedia, Playtika Rewards and Daily Dash.
# Data & rules: meta_data.R. Progress lives in the global player state.
# ---------------------------------------------------------------------------

# ---- Cougar Album ---------------------------------------------------------------
album_ui <- function(id) {
  ns <- NS(id)
  div(class = "meta-page album-page",
      uiOutput(ns("header")),
      uiOutput(ns("recent")),
      uiOutput(ns("sets")))
}

album_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$header <- renderUI({
      owned <- player$rv$album
      ap <- album_progress(owned)
      spare <- spare_value(owned)
      page_header(tr("Cougar Album: Like a Pro"),
                  tr("%d / %d cards \u2022 %d / 9 sets complete \u2022 finish the album for %s coins",
                     ap$have, ap$total, length(ap$complete_sets), fmt_coins(ALBUM_COMPLETE_REWARD)),
                  icon = "\U0001F4D6",
                  right = div(class = "album-actions",
                              progress_bar(100 * ap$have / ap$total, sprintf("%d%%", as.integer(round(100 * ap$have / ap$total))), "album-bar"),
                              tags$button(type = "button", class = paste("btn-act gold", if (spare <= 0) "disabled"),
                                          disabled = if (spare <= 0) NA else NULL,
                                          onclick = js_set(ns("trade"), "Math.random()"),
                                          tr("Trade spares \u2192 %s \u2B50", fmt_coins(spare))),
                              if (ap$have == ap$total && !player$rv$album_claimed)
                                tags$button(type = "button", class = "btn-deal", onclick = js_set(ns("claim_album"), "Math.random()"), tr("CLAIM ALBUM PRIZE"))))
    })
    output$recent <- renderUI({
      rp <- player$rv$recent_pulls
      if (!length(rp)) return(div(class = "album-hint", paste("\U0001F0CF", tr(
        "Card packs drop randomly while you play (4% of plays, better packs at higher bets), on level-ups, from Daily Dash chests, Monsterpedia and the Star Market. Golden Lucky Cards trigger the Lucky Cards mini game!"))))
      div(class = "recent-pulls", span(class = "ctl-label", tr("RECENT PULLS")),
          lapply(utils::head(rp, 12), function(id) {
            c0 <- ALBUM_CARDS[ALBUM_CARDS$id == id, ]
            span(class = paste("mini-card", if (c0$lucky) "lucky"), title = tr(c0$name), c0$glyph)
          }))
    })
    output$sets <- renderUI({
      owned <- player$rv$album; claimed <- player$rv$sets_claimed
      div(class = "album-sets", lapply(seq_len(nrow(ALBUM_SETS)), function(s) {
        cards <- ALBUM_CARDS[ALBUM_CARDS$set == s, ]
        cnt <- owned[cards$id]; cnt[is.na(cnt)] <- 0L
        have <- sum(cnt > 0)
        done <- have == 9; got <- s %in% claimed
        div(class = paste("album-set", if (done) "complete", if (got) "claimed"),
            div(class = "set-head",
                span(class = "set-icon", ALBUM_SETS$icon[s]),
                div(class = "set-name", tr(ALBUM_SETS$name[s]), span(class = "set-count", sprintf("%d/9", have))),
                div(class = "set-reward",
                    if (got) span(class = "claimed-tag", paste("\u2714", tr("CLAIMED")))
                    else if (done) tags$button(type = "button", class = "btn-claim", onclick = js_set(ns("claim_set"), s),
                                               paste(tr("CLAIM"), fmt_short(ALBUM_SETS$reward[s])))
                    else span(paste("\U0001FA99", fmt_short(ALBUM_SETS$reward[s]))))),
            div(class = "set-cards", lapply(seq_len(nrow(cards)), function(i) {
              cd <- cards[i, ]; n <- cnt[i]
              div(class = paste("album-card", if (n > 0) "owned" else "missing", if (cd$lucky) "lucky", paste0("s", cd$stars)),
                  title = sprintf("%s (%d\u2605)%s", tr(cd$name), as.integer(cd$stars), if (cd$lucky) paste(" -", tr("Lucky Card")) else ""),
                  div(class = "ac-glyph", if (n > 0) cd$glyph else "?"),
                  div(class = "ac-name", if (n > 0) tr(cd$name) else "???"),
                  div(class = "ac-stars", strrep("\u2605", cd$stars)),
                  if (n > 1) span(class = "ac-count", paste0("x", n)))
            })))
      }))
    })
    observeEvent(input$trade, player$trade_spares())
    observeEvent(input$claim_set, player$claim_set(input$claim_set))
    observeEvent(input$claim_album, player$claim_album())
  })
}

# Lucky Cards mini game - a modal shown whenever a golden card is pulled.
lucky_cards_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(player$rv$lucky, {
      b <- player$rv$lucky
      req(b, !isTRUE(b$done))
      showModal(modalDialog(
        uiOutput(ns("board")), title = NULL, footer = NULL, easyClose = FALSE,
        size = "m"))
    }, ignoreNULL = TRUE)
    output$board <- renderUI({
      b <- player$rv$lucky
      if (is.null(b)) return(NULL)
      div(class = "lucky-board",
          div(class = "lucky-title", paste("\U0001F340", tr("LUCKY CARDS"), "\U0001F340")),
          div(class = "lucky-sub", b$subtitle),
          div(class = "lucky-cards", lapply(seq_len(nrow(b$tiles)), function(i) {
            t <- b$tiles[i, ]
            if (isTRUE(b$done)) div(class = paste("lucky-card flipped", if (identical(b$chosen, i)) "chosen"), span(t$label))
            else tags$button(type = "button", class = "lucky-card", onclick = js_set(ns("pick"), i), span("\U0001F340"))
          })),
          if (isTRUE(b$done)) tags$button(type = "button", class = "btn-collect",
                                          onclick = js_set(ns("collect"), "Math.random()"), tr("COLLECT")))
    })
    observeEvent(input$pick, {
      b <- player$rv$lucky
      req(b, !isTRUE(b$done))
      b <- bonus_pick(b, input$pick)
      player$rv$lucky <- b
    })
    observeEvent(input$collect, {
      b <- player$rv$lucky
      removeModal()
      if (!is.null(b) && isTRUE(b$done)) player$settle_lucky(b)
    })
  })
}

# ---- Wheel of Fun ------------------------------------------------------------------
wheel_ui <- function(id) {
  ns <- NS(id)
  div(class = "meta-page wheel-page",
      page_header(tr("Wheel of Fun"), tr("Free coins every hour, and a daily spin with a multiplier ring."), icon = "\U0001F3A1"),
      div(class = "wheels-row",
          div(class = "wheel-card hourly",
              div(class = "wc-title", tr("HOURLY WHEEL")),
              uiOutput(ns("hourly_wheel")),
              uiOutput(ns("hourly_btn"))),
          div(class = "wheel-card daily",
              div(class = "wc-title", tr("DAILY BONUS WHEEL")),
              uiOutput(ns("daily_wheel")),
              uiOutput(ns("daily_btn")))),
      div(class = "rules-line", uiOutput(ns("vip_note"))))
}

wheel_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    last <- reactiveValues(hourly = NULL, daily = NULL)
    seg_labels <- fmt_short(WHEEL_SEGMENTS$value)
    output$hourly_wheel <- renderUI({
      r <- last$hourly
      svg_wheel(ns("hw"), seg_labels, WHEEL_SEGMENTS$color, size = 300,
                text_colors = ifelse(WHEEL_SEGMENTS$color == "#ffcc00", "#1a0933", "#ffffff"),
                spin_to = r$segment)
    })
    output$daily_wheel <- renderUI({
      r <- last$daily
      tagList(
        svg_wheel(ns("dw"), seg_labels, WHEEL_SEGMENTS$color, size = 300,
                  text_colors = ifelse(WHEEL_SEGMENTS$color == "#ffcc00", "#1a0933", "#ffffff"),
                  spin_to = r$segment),
        div(class = "mult-ring", lapply(seq_len(nrow(WHEEL_MULTS)), function(i)
          span(class = paste("mult-chip", if (!is.null(r) && r$mult == WHEEL_MULTS$mult[i]) "hit"), paste0("x", WHEEL_MULTS$mult[i])))))
    })
    status <- reactive({ invalidateLater(1000); player$wheel_status() })
    output$hourly_btn <- renderUI({
      s <- status()
      if (s$hourly_ready) tags$button(type = "button", class = "btn-deal pulse",
                                      onclick = js_set(ns("spin_hourly"), "Math.random()"), tr("SPIN FREE!"))
      else div(class = "wheel-wait", span(tr("Next spin in")), strong(fmt_duration(s$hourly_wait)),
               if (!is.null(last$hourly)) div(class = "wheel-won", tr("Won %s", fmt_coins(last$hourly$total))))
    })
    output$daily_btn <- renderUI({
      s <- status()
      if (s$daily_ready) tags$button(type = "button", class = "btn-deal pulse gold",
                                     onclick = js_set(ns("spin_daily"), "Math.random()"), tr("SPIN DAILY BONUS!"))
      else div(class = "wheel-wait", span(tr("Daily bonus resets in")), strong(fmt_duration(s$daily_wait)),
               if (!is.null(last$daily)) div(class = "wheel-won", sprintf("%s \u00D7 %d = %s", fmt_coins(last$daily$base), as.integer(last$daily$mult), fmt_coins(last$daily$total))))
    })
    output$vip_note <- renderUI({
      v <- player$vip()
      span(paste(VIP_TIERS$icon[v$index], tr("%s status: +%d%% on every wheel prize. Raise your Playtika Rewards tier for bigger spins.",
                                              tr(v$tier), as.integer(round(100 * VIP_TIERS$wheel_bonus[v$index])))))
    })
    observeEvent(input$spin_hourly, {
      r <- player$spin_wheel(daily = FALSE); req(r)
      last$hourly <- r
      player$celebrate("win", r$total, tr("WHEEL OF FUN!"), if (r$vip_bonus > 0) tr("incl. +%d%% VIP bonus", as.integer(round(100 * r$vip_bonus))) else "", delay = 4300)
      player$record("side", "wheel", NULL, wager = 0, win = r$total, bet = 1)
    })
    observeEvent(input$spin_daily, {
      r <- player$spin_wheel(daily = TRUE); req(r)
      last$daily <- r
      player$celebrate("mega", r$total, tr("DAILY BONUS!"), tr("%s x%d multiplier", fmt_coins(r$base), as.integer(r$mult)), delay = 4300)
      player$award_pack(r$pack, "Daily Bonus Wheel")
      player$record("side", "wheel", NULL, wager = 0, win = r$total, bet = 1)
    })
  })
}

# ---- Star Market -----------------------------------------------------------------------
market_ui <- function(id) {
  ns <- NS(id)
  div(class = "meta-page market-page", uiOutput(ns("header")), uiOutput(ns("items")))
}

market_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$header <- renderUI({
      spare <- spare_value(player$rv$album)
      page_header(tr("Star Market"), tr("Trade duplicate album cards for stars, then spend them on bonuses and exclusive avatars."),
                  icon = "\u2B50",
                  right = div(class = "star-balance",
                              span(class = "sb-stars", paste("\u2B50", fmt_coins(player$rv$stars))),
                              tags$button(type = "button", class = paste("btn-act gold", if (spare <= 0) "disabled"),
                                          disabled = if (spare <= 0) NA else NULL,
                                          onclick = js_set(ns("trade"), "Math.random()"),
                                          tr("Trade spares (+%s)", fmt_coins(spare)))))
    })
    output$items <- renderUI({
      stars <- player$rv$stars; owned <- player$rv$avatars; cur <- player$rv$avatar; boosts <- player$rv$boosts
      section <- function(title, kinds) {
        it <- MARKET_ITEMS[MARKET_ITEMS$kind %in% kinds, ]
        tagList(h4(class = "market-section", tr(title)),
                div(class = "market-grid", lapply(seq_len(nrow(it)), function(i) {
                  x <- it[i, ]
                  have <- x$kind == "avatar" && x$value %in% owned
                  active <- have && identical(cur, x$value)
                  afford <- stars >= x$price
                  div(class = paste("market-item", x$kind, if (have) "owned", if (active) "active"),
                      div(class = "mi-icon", x$icon),
                      div(class = "mi-name", tr(x$name)),
                      div(class = "mi-desc", tr(x$desc)),
                      tags$button(type = "button",
                                  class = paste("btn-buy", if (!have && !afford) "disabled"),
                                  disabled = if ((!have && !afford) || active) NA else NULL,
                                  onclick = js_set(ns("buy"), sprintf("'%s'", x$id)),
                                  if (active) tr("EQUIPPED") else if (have) tr("EQUIP") else paste("\u2605", x$price)))
                })))
      }
      tagList(
        if ((boosts$xp2 %||% 0) > 0 || (boosts$win20 %||% 0) > 0)
          div(class = "boost-strip",
              if ((boosts$xp2 %||% 0) > 0) span(class = "boost-pill", paste("\u26A1", tr("Double XP: %d plays", as.integer(boosts$xp2)))),
              if ((boosts$win20 %||% 0) > 0) span(class = "boost-pill", paste("\U0001F680", tr("Win Booster: %d spins", as.integer(boosts$win20))))),
        section("Bonuses", c("coins", "pack", "boost", "wheel")),
        section("Exclusive Avatars", "avatar"),
        div(class = "avatar-row", span(class = "ctl-label", tr("YOUR AVATARS")),
            lapply(unique(c(DEFAULT_AVATAR, owned)), function(a) tags$button(
              type = "button", class = paste("avatar-pick", if (identical(a, cur)) "active"),
              onclick = js_set(ns("avatar"), sprintf("'%s'", a)), a)))
      )
    })
    observeEvent(input$buy, player$buy(input$buy))
    observeEvent(input$trade, player$trade_spares())
    observeEvent(input$avatar, player$set_avatar(input$avatar))
  })
}

# ---- Monsterpedia -------------------------------------------------------------------------
monsterpedia_ui <- function(id) {
  ns <- NS(id)
  div(class = "meta-page monster-page",
      page_header(tr("Monsterpedia"), tr("A leveling map of monster slots. Capture every monster in a chapter to unlock the next."),
                  icon = "\U0001F47E"),
      uiOutput(ns("map")))
}

monsterpedia_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$map <- renderUI({
      board <- player$monster_board()
      div(class = "monster-map",
          lapply(seq_along(board), function(i) {
            b <- board[[i]]; ch <- b$chapter; cc <- b$challenges
            caught <- sum(cc$captured)
            div(class = paste("chapter", if (!b$unlocked) "locked", if (b$complete) "complete", if (b$claimed) "claimed"),
                style = sprintf("--ch-color:%s", ch$color),
                div(class = "chapter-node", span(ch$icon), span(class = "node-num", i)),
                div(class = "chapter-body",
                    div(class = "chapter-head",
                        div(h4(tr(ch$name)), span(class = "chapter-sub", tr("Play %s \u2022 %d/5 captured", slot_def(ch$game)$name, as.integer(caught)))),
                        div(class = "chapter-actions",
                            if (!b$unlocked) span(class = "lock-tag", paste("\U0001F512", tr("Clear the previous chapter")))
                            else tags$button(type = "button", class = "btn-act",
                                             onclick = js_set(ns("play"), sprintf("'%s'", ch$game)), paste(tr("PLAY"), "\u25B6")),
                            if (b$claimed) span(class = "claimed-tag", paste("\u2714", tr("CLEARED")))
                            else if (b$complete && b$unlocked) tags$button(type = "button", class = "btn-claim",
                                                                           onclick = js_set(ns("claim"), sprintf("'%s'", ch$id)),
                                                                           paste(tr("CLAIM"), fmt_short(ch$reward), "+", ch$avatar))
                            else span(class = "reward-tag", paste("\U0001F3C6", fmt_short(ch$reward), "+", tr("avatar"), ch$avatar)))),
                    div(class = "monsters", lapply(seq_len(nrow(cc)), function(k) {
                      m <- cc[k, ]
                      div(class = paste("monster", if (m$captured) "captured"),
                          div(class = "mon-glyph", if (m$captured || b$unlocked) m$glyph else "?"),
                          div(class = "mon-name", tr(m$monster)),
                          div(class = "mon-task", m$text),
                          progress_bar(100 * m$progress / m$target,
                                       if (m$captured) tr("CAPTURED") else sprintf("%s / %s", fmt_coins(floor(m$progress)), fmt_coins(m$target))))
                    }))))
          }))
    })
    observeEvent(input$play, {
      d <- slot_def(input$play)
      player$go(d$category, d$id)
    })
    observeEvent(input$claim, player$monster_claim(input$claim))
  })
}

# ---- Playtika Rewards -----------------------------------------------------------------------
rewards_ui <- function(id) {
  ns <- NS(id)
  div(class = "meta-page rewards-page",
      page_header(tr("Playtika Rewards"), tr("Earn loyalty points on every coin you wager - across every game in the casino."),
                  icon = "\U0001F451"),
      uiOutput(ns("status")),
      div(class = "rewards-grid",
          div(class = "rw-card", h4(tr("Points by game")), uiOutput(ns("families"))),
          div(class = "rw-card", h4(tr("Status tiers")), uiOutput(ns("tiers")))),
      uiOutput(ns("leaderboard")),
      div(class = "rw-card profile-card", h4(tr("Your profile")), uiOutput(ns("profile"))))
}

# Display name of a leaderboard row (players saved before names existed have none).
lb_name <- function(name, id) {
  if (!is.null(name) && length(name) == 1 && !is.na(name) && nzchar(name)) name
  else tr("Player %s", toupper(substr(id, 1, 4)))
}

rewards_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$status <- renderUI({
      v <- player$vip()
      i <- v$index
      gift_ready <- !identical(player$rv$vip_gift_on, period_keys()$day)
      div(class = "vip-hero", style = sprintf("--tier:%s", VIP_TIERS$color[i]),
          div(class = "vip-badge", VIP_TIERS$icon[i]),
          div(class = "vip-info",
              div(class = "vip-tier", tr(v$tier)),
              div(class = "vip-points", tr("%s loyalty points", fmt_coins(floor(v$points)))),
              progress_bar(v$pct, if (is.na(v$next_at)) tr("Top tier reached!") else
                tr("%s to %s", fmt_coins(ceiling(v$next_at - v$points)), tr(VIP_TIERS$tier[i + 1])), "vip-bar")),
          div(class = "vip-gift",
              if (gift_ready) tags$button(type = "button", class = "btn-deal pulse gold", onclick = js_set(ns("gift"), "Math.random()"),
                                          tr("CLAIM DAILY GIFT %s", fmt_short(VIP_TIERS$daily_gift[i])))
              else span(class = "claimed-tag", tr("Gift claimed \u2022 next in %s", fmt_duration(secs_until("day"))))))
    })
    output$families <- renderUI({
      f <- player$rv$vip_by_family
      mx <- max(1, f)
      div(class = "family-bars", lapply(names(REWARD_FAMILIES), function(k)
        div(class = "family-row",
            span(class = "fam-name", tr(REWARD_FAMILIES[[k]])),
            progress_bar(100 * f[[k]] / mx, fmt_coins(floor(f[[k]])), "fam-bar"))))
    })
    output$tiers <- renderUI({
      cur <- player$vip()$index
      tags$table(class = "tier-table",
                 tags$thead(tags$tr(tags$th(tr("Tier")), tags$th(tr("Points")), tags$th(tr("Wheel bonus")), tags$th(tr("Daily gift")))),
                 tags$tbody(lapply(seq_len(nrow(VIP_TIERS)), function(i) tags$tr(
                   class = if (i == cur) "current" else if (i < cur) "passed",
                   tags$td(paste(VIP_TIERS$icon[i], tr(VIP_TIERS$tier[i]))), tags$td(fmt_coins(VIP_TIERS$min_points[i])),
                   tags$td(sprintf("+%d%%", as.integer(round(100 * VIP_TIERS$wheel_bonus[i])))), tags$td(fmt_short(VIP_TIERS$daily_gift[i]))))))
    })
    output$profile <- renderUI({
      s <- player$rv$stats
      div(class = "profile-grid",
          div(class = "pf-stat", span(tr("Plays")), strong(fmt_coins(s$plays))),
          div(class = "pf-stat", span(tr("Coins wagered")), strong(fmt_short(s$wagered))),
          div(class = "pf-stat", span(tr("Coins won")), strong(fmt_short(s$won))),
          div(class = "pf-stat", span(tr("Biggest win")), strong(fmt_short(s$biggest)),
              if (nzchar(s$biggest_game)) em(game_display_name(s$biggest_game))),
          div(class = "pf-stat", span(tr("Packs opened")), strong(s$packs)),
          div(class = "pf-stat", span(tr("Level")), strong(player$rv$level)),
          div(class = "pf-feed", span(class = "ctl-label", tr("ACTIVITY")),
              if (length(player$rv$feed)) tags$ul(lapply(utils::head(player$rv$feed, 8), tags$li)) else p(tr("Nothing yet - go play!"))),
          div(class = "pf-reset",
              if (player$store$server_side && !is.null(player$id()))
                div(class = "pf-id", tr("Saved to the %s database", player$store$backend), br(), code(substr(player$id(), 1, 8))),
              tags$button(type = "button", class = "btn-act red",
                          onclick = sprintf("if (confirm('%s')) %s", gsub("'", "\\\\'", tr("Reset ALL progress and start over with 1,000,000 coins?")),
                                            js_set(ns("reset"), "Math.random()")),
                          tr("Reset progress"))))
    })
    # Leaderboard (needs server-side storage)
    output$leaderboard <- renderUI({
      if (!player$store$server_side) return(NULL)
      invalidateLater(15000)
      player$rv$coins                      # refresh when our own balance changes
      lb <- player$store$leaderboard(10L)
      if (is.null(lb) || !nrow(lb)) return(NULL)
      me <- player$id()
      div(class = "rw-card leaderboard", h4(paste("\U0001F3C6", tr("Leaderboard"))),
          tags$table(class = "tier-table lb-table",
                     tags$thead(tags$tr(tags$th("#"), tags$th(tr("Player")), tags$th(tr("Level")), tags$th(tr("Coins")))),
                     tags$tbody(lapply(seq_len(nrow(lb)), function(i) {
                       mine <- identical(lb$player_id[i], me)
                       tags$tr(class = if (mine) "current",
                               tags$td(i),
                               tags$td(paste(if (nzchar(lb$avatar[i] %||% "")) lb$avatar[i] else DEFAULT_AVATAR,
                                             lb_name(lb$name[i], lb$player_id[i])),
                                       if (mine) span(class = "you-tag", tr("You"))),
                               tags$td(lb$level[i]), tags$td(fmt_short(lb$coins[i])))
                     }))))
    })
    observeEvent(input$gift, player$claim_vip_gift())
    observeEvent(input$reset, {
      player$reset()
      player$toast(tr("Fresh start"), tr("Progress reset. Good luck!"), "info", "\U0001F504")
    })
  })
}

# ---- Daily Dash --------------------------------------------------------------------------------
dash_ui <- function(id) {
  ns <- NS(id)
  div(class = "meta-page dash-page",
      page_header(tr("Daily Dash"), tr("Daily, weekly and monthly mission passes. Complete missions, claim rewards, open the chest."),
                  icon = "\U0001F3C1"),
      uiOutput(ns("board")))
}

dash_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$board <- renderUI({
      invalidateLater(30000)
      board <- player$dash_board()
      titles <- c(day = tr("TODAY"), week = tr("THIS WEEK"), month = tr("THIS MONTH"))
      div(class = "dash-grid", lapply(board, function(b) {
        m <- b$missions; r <- DASH_REWARD[[b$period]]
        div(class = paste("dash-col", b$period),
            div(class = "dash-head", span(titles[[b$period]]), span(class = "dash-reset", tr("resets in %s", fmt_duration(b$resets_in)))),
            lapply(seq_len(nrow(m)), function(i) {
              div(class = paste("mission", if (m$done[i]) "done", if (m$claimed[i]) "claimed"),
                  div(class = "mission-text", m$text[i]),
                  progress_bar(100 * m$progress[i] / m$target[i],
                               sprintf("%s / %s", fmt_short(floor(m$progress[i])), fmt_short(m$target[i]))),
                  div(class = "mission-foot",
                      span(class = "mission-reward", paste("\U0001FA99", fmt_short(r$mission))),
                      if (m$claimed[i]) span(class = "claimed-tag", "\u2714")
                      else if (m$done[i]) tags$button(type = "button", class = "btn-claim", onclick = js_set(ns("claim"), sprintf("'%s'", m$id[i])), tr("CLAIM"))))
            }),
            div(class = paste("dash-chest", if (b$chest_ready) "ready", if (b$chest_claimed) "claimed"),
                span(class = "chest-icon", if (b$chest_claimed) "\U0001F4ED" else "\U0001F381"),
                div(class = "chest-text", tr(c(day = "Daily chest: %s + %s", week = "Weekly chest: %s + %s", month = "Monthly chest: %s + %s")[[b$period]],
                                             fmt_short(r$chest), tr(PACKS[[r$pack]]$name))),
                if (b$chest_ready) tags$button(type = "button", class = "btn-claim", onclick = js_set(ns("claim"), sprintf("'%s'", b$chest_id)), tr("OPEN"))))
      }))
    })
    observeEvent(input$claim, player$dash_claim(input$claim))
  })
}

# ---- Module wrapper --------------------------------------------------------------------------
# The Meta & Side Games module: returns the nav panels for the navbar menu.
meta_nav_panels <- function(id) {
  ns <- NS(id)
  list(
    nav_panel(tr("Cougar Album"), value = "album", album_ui(ns("album")), icon = span("\U0001F4D6")),
    nav_panel(tr("Wheel of Fun"), value = "wheel", wheel_ui(ns("wheel")), icon = span("\U0001F3A1")),
    nav_panel(tr("Star Market"), value = "market", market_ui(ns("market")), icon = span("\u2B50")),
    nav_panel(tr("Monsterpedia"), value = "monsterpedia", monsterpedia_ui(ns("monsters")), icon = span("\U0001F47E")),
    nav_panel(tr("Playtika Rewards"), value = "rewards", rewards_ui(ns("rewards")), icon = span("\U0001F451")),
    nav_panel(tr("Daily Dash"), value = "dash", dash_ui(ns("dash")), icon = span("\U0001F3C1"))
  )
}

meta_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    album_server("album", player)
    lucky_cards_server("lucky", player)
    wheel_server("wheel", player)
    market_server("market", player)
    monsterpedia_server("monsters", player)
    rewards_server("rewards", player)
    dash_server("dash", player)
  })
}
