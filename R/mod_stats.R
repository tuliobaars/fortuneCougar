# ---------------------------------------------------------------------------
# mod_stats.R -- "My Stats": the player's saved statistics (money, time
# played, drawdown, tendency, category) and classification (kNN archetype +
# k-means community clusters). Opened from the player name in the navbar.
# Calculations: fct_stats.R. Data: the player state's track (saved with the
# profile) and, with server-side storage, the other players' features.
# ---------------------------------------------------------------------------

# "2h 05m", "12m 30s", "45s"
fmt_playtime <- function(secs) {
  secs <- max(0, round(as.numeric(secs)))
  h <- secs %/% 3600; m <- (secs %% 3600) %/% 60; s <- secs %% 60
  if (h > 0) sprintf("%dh %02dm", h, m) else if (m > 0) sprintf("%dm %02ds", m, s) else sprintf("%ds", s)
}

fmt_pct <- function(x, digits = 1) paste0(fmt_num(100 * x, digits), "%")

fmt_when <- function(secs) {
  format(as.POSIXct(secs, origin = "1970-01-01"), if (fc_lang() == "pt_BR") "%d/%m %H:%M" else "%m/%d %H:%M")
}

# Signed coin amount with a colour class.
signed_coins <- function(x, short = TRUE) {
  txt <- if (short) fmt_short(abs(x)) else fmt_coins(abs(x))
  span(class = if (x > 0) "pos" else if (x < 0) "neg" else "flat",
       paste0(if (x > 0) "+" else if (x < 0) "\u2212" else "", txt))
}

kpi <- function(label, value, sub = NULL, icon = NULL, class = "") {
  div(class = paste("kpi", class),
      div(class = "kpi-label", if (!is.null(icon)) span(class = "kpi-icon", icon), label),
      div(class = "kpi-value", value),
      if (!is.null(sub)) div(class = "kpi-sub", sub))
}

stats_ui <- function(id) {
  ns <- NS(id)
  div(class = "meta-page stats-page",
      uiOutput(ns("header")),
      uiOutput(ns("kpis")),
      div(class = "rw-card stats-chart-card",
          div(class = "stats-card-head", h4(tr("Balance history")),
              div(class = "stats-legend",
                  span(class = "lg lg-bal", tr("Balance")), span(class = "lg lg-peak", tr("Peak")),
                  span(class = "lg lg-dd", tr("Drawdown")))),
          uiOutput(ns("chart"))),
      div(class = "stats-grid",
          div(class = "rw-card", h4(paste("\U0001F4C8", tr("Tendency"))), uiOutput(ns("tendency"))),
          div(class = "rw-card", h4(paste("\U0001F3AF", tr("Category"))), uiOutput(ns("category")))),
      div(class = "stats-grid",
          div(class = "rw-card", h4(paste("\U0001F9E0", tr("Player classification"))), uiOutput(ns("classify"))),
          div(class = "rw-card", h4(paste("\U0001F465", tr("Community clusters"))), uiOutput(ns("community")))),
      div(class = "rw-card", h4(paste("\U0001F4C5", tr("Sessions"))), uiOutput(ns("sessions"))))
}

stats_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    store <- player$store

    report <- reactive(stats_report(player$rv$track, player$rv$coins, session = player$session()))

    output$header <- renderUI({
      nm <- player$rv$name
      page_header(tr("My Stats"),
                  if (nzchar(nm)) tr("%s \u2022 saved between sessions", nm) else tr("Choose a name to save your stats"),
                  icon = "\U0001F4CA",
                  right = tags$button(type = "button", class = "btn-act", onclick = js_set(ns("switch"), "Math.random()"),
                                      if (store$server_side) paste("\U0001F465", tr("Switch player"))
                                      else paste("\u270F\uFE0F", tr("Change name"))))
    })
    observeEvent(input$switch, player$ask_name(if (nzchar(player$name())) "switch" else "welcome"))

    output$kpis <- renderUI({
      invalidateLater(30000)                      # keeps the "online" timer fresh
      r <- stats_report(player$rv$track, player$rv$coins, session = player$session())
      s <- player$rv$stats
      net <- s$won - s$wagered
      div(class = "kpi-grid",
          kpi(tr("Balance"), fmt_coins(player$rv$coins), tr("Peak %s", fmt_short(max(r$peak, player$rv$coins))), "\U0001FA99", "gold"),
          kpi(tr("Net result"), signed_coins(net), tr("%s wagered \u2022 %s won", fmt_short(s$wagered), fmt_short(s$won)),
              if (net >= 0) "\U0001F4B0" else "\U0001F4B8"),
          kpi(tr("Return to player"), if (s$wagered > 0) fmt_pct(s$won / s$wagered) else "\u2014",
              trn(s$plays, "%s play", "%s plays", fmt_coins(s$plays)), "\U0001F3B2"),
          kpi(tr("Time played"), fmt_playtime(r$active), tr("%s online", fmt_playtime(r$online)), "\u23F1\uFE0F"),
          kpi(tr("Max drawdown"), span(class = if (r$max_dd > 0) "neg", fmt_short(r$max_dd)),
              tr("%s of the peak", fmt_pct(r$max_dd_pct)), "\U0001F4C9"),
          kpi(tr("Current drawdown"), span(class = if (r$cur_dd > 0) "neg", fmt_short(r$cur_dd)),
              tr("%s below the peak", fmt_pct(r$cur_dd_pct)), "\U0001F9ED"),
          kpi(tr("Biggest win"), fmt_short(s$biggest), if (nzchar(s$biggest_game)) game_display_name(s$biggest_game), "\U0001F3C6"))
    })

    output$chart <- renderUI({
      bal <- player$rv$track$bal
      svg <- stats_balance_svg(bal)
      if (is.null(svg)) return(div(class = "stats-empty", tr("Play a few rounds to see your balance history.")))
      div(class = "stats-chart-wrap", svg,
          div(class = "stats-chart-note", tr("Last %d plays. Shaded area: distance below the running peak (drawdown).", length(bal))))
    })

    output$tendency <- renderUI({
      t <- report()$tendency
      if (t$balance == "unknown") return(div(class = "stats-empty", tr("Not enough plays yet.")))
      arrow <- c(rising = "\u2197\uFE0F", falling = "\u2198\uFE0F", steady = "\u27A1\uFE0F")[[t$balance]]
      bal_txt <- c(rising = tr("Rising"), falling = tr("Falling"), steady = tr("Steady"))[[t$balance]]
      bet_txt <- c(increasing = tr("Increasing"), decreasing = tr("Decreasing"), stable = tr("Stable"),
                   unknown = tr("Not enough data"))[[t$bet]]
      chase_txt <- c(chasing = tr("Raises bets after losses"), cautious = tr("Lowers bets after losses"),
                     even = tr("Same bets after wins and losses"), unknown = tr("Not enough data"))[[t$chase_label]]
      streak <- if (t$streak > 0) trn(t$streak, "%d win in a row", "%d wins in a row", t$streak)
                else if (t$streak < 0) trn(-t$streak, "%d loss in a row", "%d losses in a row", -t$streak) else "\u2014"
      div(class = "tendency",
          div(class = paste("tend-hero", t$balance),
              span(class = "tend-arrow", arrow),
              div(div(class = "tend-label", bal_txt),
                  div(class = "tend-sub", tr("%s%% of balance per 100 plays", fmt_num(t$slope_pct, 1))))),
          tags$table(class = "stats-table",
                     tags$tbody(
                       tags$tr(tags$td(tr("Last %d plays", t$window)), tags$td(signed_coins(t$net))),
                       tags$tr(tags$td(tr("Return in that window")), tags$td(if (is.na(t$rtp)) "\u2014" else fmt_pct(t$rtp))),
                       tags$tr(tags$td(tr("Bet size")), tags$td(bet_txt, if (t$bet %in% c("increasing", "decreasing"))
                         span(class = "muted", sprintf(" (%s%s)", if (t$bet_change > 0) "+" else "", fmt_pct(t$bet_change, 0))))),
                       tags$tr(tags$td(tr("After a loss")), tags$td(chase_txt, if (!is.na(t$chase))
                         span(class = "muted", sprintf(" (\u00D7%s)", fmt_num(t$chase, 2))))),
                       tags$tr(tags$td(tr("Current streak")), tags$td(streak)))))
    })

    output$category <- renderUI({
      c0 <- report()$category
      if (is.na(c0$favourite)) return(div(class = "stats-empty", tr("Not enough plays yet.")))
      style <- c(slots = tr("Slots player"), tables = tr("Table player"), allround = tr("All-rounder"), unknown = "\u2014")[[c0$style]]
      sh <- c0$share
      div(class = "category",
          div(class = "cat-hero",
              div(class = "cat-fav", tr(STAT_CATEGORIES[[c0$favourite]])),
              div(class = "cat-tags",
                  span(class = "cat-tag", style),
                  if (!is.na(c0$stake)) span(class = "cat-tag gold", tr(c0$stake)),
                  span(class = "cat-tag", tr("avg bet %s", fmt_short(c0$avg_bet))))),
          div(class = "family-bars", lapply(names(STAT_CATEGORIES), function(k)
            div(class = "family-row",
                span(class = "fam-name", tr(STAT_CATEGORIES[[k]])),
                progress_bar(100 * sh[[k]], fmt_pct(sh[[k]], 0), "fam-bar")))))
    })

    output$classify <- renderUI({
      r <- report()
      if (is.null(r$class))
        return(div(class = "stats-empty",
                   trn(r$need, "Play %d more round to get classified.", "Play %d more rounds to get classified.", r$need),
                   progress_bar(100 * (STAT_MIN_PLAYS - r$need) / STAT_MIN_PLAYS, NULL, "fam-bar")))
      a <- archetype(r$class$label)
      f <- r$features
      div(class = "classify",
          div(class = "arch-hero",
              span(class = "arch-icon", a$icon),
              div(div(class = "arch-name", tr(a$label)),
                  div(class = "arch-conf", tr("%s confidence \u2022 k-nearest neighbours (k = 9)", fmt_pct(r$class$confidence, 0))),
                  div(class = "arch-desc", tr(a$desc)))),
          tags$table(class = "stats-table feat-table",
                     tags$thead(tags$tr(tags$th(tr("Feature")), tags$th(tr("You")), tags$th(tr("Typical %s", tr(a$label))))),
                     tags$tbody(lapply(names(STAT_FEATURES), function(k) {
                       fmt <- function(v) switch(k, bet = fmt_short(10^v), pace = fmt_num(v, 1),
                                                 risk = fmt_pct(v, 0), table = fmt_pct(v, 0),
                                                 escalation = sprintf("\u00D7%s", fmt_num(exp(v), 2)))
                       tags$tr(tags$td(tr(STAT_FEATURES[[k]])), tags$td(fmt(f[[k]])), tags$td(class = "muted", fmt(a[[k]])))
                     }))),
          div(class = "arch-votes", lapply(ARCHETYPES$id, function(id) {
            v <- r$class$votes[[id]]
            if (v <= 0) return(NULL)
            span(class = paste("vote", if (id == r$class$label) "top"), archetype(id)$icon, " ", tr(archetype(id)$label), " ", as.integer(v))
          })))
    })

    community <- reactive({
      invalidateLater(60000)
      feats <- store$all_features(500L)
      list(feats = feats, cl = community_clusters(feats))
    })

    output$community <- renderUI({
      if (!store$server_side)
        return(div(class = "stats-empty", tr("Community clustering needs the server database (storage = \"sqlite\").")))
      cm <- community()
      n <- if (is.null(cm$feats)) 0L else nrow(cm$feats)
      cl <- cm$cl
      if (is.null(cl))
        return(div(class = "stats-empty",
                   tr("k-means clustering starts when %d players have played %d+ rounds (now: %d).", 8L, STAT_MIN_PLAYS, n)))
      me <- match(player$id() %||% NA_character_, cl$player_id)
      mine <- if (!is.na(me)) cl$cluster[me] else NA
      div(class = "community",
          p(class = "muted", tr("k-means on %d players, %d clusters (%s of the variance between clusters).",
                                n, length(cl$size), fmt_pct(cl$betweenss, 0))),
          tags$table(class = "stats-table",
                     tags$thead(tags$tr(tags$th(tr("Cluster")), tags$th(tr("Players")), tags$th(tr("Avg bet")),
                                        tags$th(tr("Pace")), tags$th(tr("High-risk")), tags$th(tr("Tables")))),
                     tags$tbody(lapply(order(-cl$size), function(i) {
                       a <- archetype(cl$label[i])
                       tags$tr(class = if (identical(i, mine)) "current",
                               tags$td(paste(a$icon, tr(a$label)), if (identical(i, mine)) span(class = "you-tag", tr("You"))),
                               tags$td(cl$size[i]), tags$td(fmt_short(10^cl$centers[i, "bet"])),
                               tags$td(fmt_num(cl$centers[i, "pace"], 1)),
                               tags$td(fmt_pct(max(0, cl$centers[i, "risk"]), 0)),
                               tags$td(fmt_pct(max(0, cl$centers[i, "table"]), 0)))
                     }))))
    })

    output$sessions <- renderUI({
      invalidateLater(30000)
      tk <- player$rv$track
      cur <- player$session()
      online <- tk$online + if (!is.null(cur)) max(0, now_secs() - cur$t0) else 0
      rows <- tk$sessions
      live <- if (!is.null(cur)) list(start = cur$t0, seconds = now_secs() - cur$t0, plays = tk$n - cur$plays,
                                      start_balance = cur$balance, end_balance = player$rv$coins, live = TRUE)
      all <- c(if (!is.null(live)) list(live), rows)
      if (!length(all)) return(div(class = "stats-empty", tr("No sessions yet.")))
      k <- length(all)
      div(class = "sessions",
          p(class = "muted", trn(k, "%s online in %d session.", "%s online in %d sessions.", fmt_playtime(online), k)),
          tags$table(class = "stats-table",
                     tags$thead(tags$tr(tags$th(tr("Started")), tags$th(tr("Duration")), tags$th(tr("Plays")), tags$th(tr("Result")))),
                     tags$tbody(lapply(all, function(s) tags$tr(
                       class = if (isTRUE(s$live)) "current",
                       tags$td(fmt_when(s$start), if (isTRUE(s$live)) span(class = "you-tag", tr("now"))),
                       tags$td(fmt_playtime(s$seconds)), tags$td(fmt_coins(s$plays)),
                       tags$td(signed_coins(s$end_balance - s$start_balance)))))))
    })
  })
}
