# ---------------------------------------------------------------------------
# mod_roulette.R -- European / American / French / Multi-Wheel roulette.
# Engine: engine_roulette.R. The betting layout is generated here with a hit
# zone for every inside bet (straights, splits, streets, corners, six-lines,
# zero trios, first-four / basket); chips are painted client side.
# ---------------------------------------------------------------------------

RL_CHIPS <- c(100, 500, 1000, 5000, 10000, 50000)
RL_RULES_TEXT <- c(
  european = "Single-zero wheel (37 pockets) \u2022 house edge 2.70% \u2022 straight 35:1, split 17:1, street 11:1, corner 8:1, six-line 5:1",
  american = "0 and 00 wheel (38 pockets) \u2022 house edge 5.26% \u2022 extra 0-00 split and the 0-00-1-2-3 basket (6:1)",
  french = "Single zero with LA PARTAGE: even-money bets return half on zero (edge 1.35% on those bets) \u2022 announced bets available",
  multi = "One set of bets plays on up to 6 European wheels spinning together - total stake = bets \u00D7 wheels")

rl_key <- function(type, nums) paste0(type, ":", paste(sort(nums), collapse = "-"))
rl_n <- function(c, r) 3 * c - (r - 1)          # column 1..12, row 1 (top) .. 3

# SVG roulette wheel for a variant.
roulette_wheel_svg <- function(id, variant, size = 300) {
  order <- rl_wheel(variant)
  n <- length(order)
  r <- size / 2; cx <- r; cy <- r; seg <- 360 / n
  fill <- c(red = "#c0142b", black = "#15121c", green = "#0d8a3e")
  pockets <- vapply(seq_len(n), function(i) {
    a0 <- (i - 1) * seg - 90 - seg / 2; a1 <- a0 + seg
    p <- function(rad, a) c(cx + rad * cos(a * pi / 180), cy + rad * sin(a * pi / 180))
    o0 <- p(r - 6, a0); o1 <- p(r - 6, a1); i0 <- p(r * 0.62, a0); i1 <- p(r * 0.62, a1)
    tp <- p(r * 0.83, (a0 + a1) / 2)
    sprintf(paste0('<path d="M%.2f,%.2f L%.2f,%.2f A%.2f,%.2f 0 0,1 %.2f,%.2f L%.2f,%.2f A%.2f,%.2f 0 0,0 %.2f,%.2f Z" fill="%s" stroke="#d4af37" stroke-width="0.8"/>',
                   '<text x="%.2f" y="%.2f" fill="#fff" font-size="%.1f" font-weight="700" text-anchor="middle" dominant-baseline="middle" transform="rotate(%.2f %.2f %.2f)">%s</text>'),
            i0[1], i0[2], o0[1], o0[2], r - 6, r - 6, o1[1], o1[2], i1[1], i1[2], r * 0.62, r * 0.62, i0[1], i0[2],
            fill[[rl_color(order[i])]], tp[1], tp[2], size / 26, (a0 + a1) / 2 + 90, tp[1], tp[2], rl_label(order[i]))
  }, "")
  spokes <- paste(vapply(0:3, function(k) sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#d4af37" stroke-width="3"/>',
                                                  cx, cy, cx + r * 0.45 * cos(k * pi / 2), cy + r * 0.45 * sin(k * pi / 2)), ""), collapse = "")
  div(class = "rl-wheel", id = id, `data-n` = n, style = sprintf("width:%dpx;height:%dpx", size, size),
      HTML(sprintf(paste0('<svg viewBox="0 0 %d %d" width="%d" height="%d"><circle cx="%.1f" cy="%.1f" r="%.1f" fill="#3b2314" stroke="#d4af37" stroke-width="5"/>',
                          '<g class="rl-rotor">%s<circle cx="%.1f" cy="%.1f" r="%.1f" fill="#5a3820" stroke="#d4af37" stroke-width="2"/>%s',
                          '<circle cx="%.1f" cy="%.1f" r="%.1f" fill="#d4af37"/></g></svg>'),
                   size, size, size, size, cx, cy, r - 1, paste(pockets, collapse = ""), cx, cy, r * 0.6, spokes, cx, cy, r * 0.08)),
      div(class = "rl-ball-track", div(class = "rl-ball")),
      div(class = "rl-result-bubble"))
}

# The betting layout with every hit zone.
roulette_table_ui <- function(ns, variant) {
  click <- function(key) js_set(ns("place"), sprintf("{key: '%s', n: Math.random()}", key))
  spot <- function(key, left, top, cls = "rl-spot", label = NULL) {
    div(class = cls, `data-key` = key, onclick = click(key), title = rl_bet_label(key),
        style = sprintf("left:%.3f%%;top:%.3f%%", left, top), label)
  }
  cells <- list(); spots <- list()
  for (c in 1:12) for (r in 1:3) {
    n <- rl_n(c, r)
    cells[[length(cells) + 1]] <- div(
      class = paste("rl-num", rl_color(n)), `data-key` = rl_key("straight", n), onclick = click(rl_key("straight", n)),
      title = paste(tr("Straight up"), n), style = sprintf("grid-column:%d;grid-row:%d", c, r), span(n))
    if (r < 3) spots[[length(spots) + 1]] <- spot(rl_key("split", c(n, rl_n(c, r + 1))), (c - 0.5) / 12 * 100, r / 3 * 100, "rl-spot split-v")
    if (c < 12) spots[[length(spots) + 1]] <- spot(rl_key("split", c(n, rl_n(c + 1, r))), c / 12 * 100, (r - 0.5) / 3 * 100, "rl-spot split-h")
    if (c < 12 && r < 3) spots[[length(spots) + 1]] <- spot(rl_key("corner", c(n, rl_n(c, r + 1), rl_n(c + 1, r), rl_n(c + 1, r + 1))),
                                                           c / 12 * 100, r / 3 * 100, "rl-spot corner")
  }
  for (c in 1:12) {
    spots[[length(spots) + 1]] <- spot(rl_key("street", (3 * c - 2):(3 * c)), (c - 0.5) / 12 * 100, 100, "rl-spot street")
    if (c < 12) spots[[length(spots) + 1]] <- spot(rl_key("sixline", (3 * c - 2):(3 * c + 3)), c / 12 * 100, 100, "rl-spot sixline")
  }
  # bets touching the zero(s)
  if (variant == "american") {
    zs <- list(spot(rl_key("split", c(37, 3)), 0, 100 / 6, "rl-spot zero"),
               spot(rl_key("street", c(37, 2, 3)), 0, 100 / 3, "rl-spot zero"),
               spot(rl_key("split", c(37, 2)), 0, 44, "rl-spot zero"),
               spot(rl_key("street", c(0, 37, 2)), 0, 50, "rl-spot zero trio"),
               spot(rl_key("split", c(0, 2)), 0, 56, "rl-spot zero"),
               spot(rl_key("street", c(0, 1, 2)), 0, 200 / 3, "rl-spot zero"),
               spot(rl_key("split", c(0, 1)), 0, 500 / 6, "rl-spot zero"),
               spot(rl_key("basket", c(0, 37, 1, 2, 3)), 0, 100, "rl-spot zero basket"))
    zero_col <- div(class = "rl-zero double",
                    div(class = "rl-num green", `data-key` = "straight:37", onclick = click("straight:37"), span("00")),
                    div(class = "rl-num green", `data-key` = "straight:0", onclick = click("straight:0"), span("0")),
                    spot(rl_key("split", c(0, 37)), 50, 50, "rl-spot split-zero"))
  } else {
    zs <- list(spot(rl_key("split", c(0, 3)), 0, 100 / 6, "rl-spot zero"),
               spot(rl_key("street", c(0, 2, 3)), 0, 100 / 3, "rl-spot zero"),
               spot(rl_key("split", c(0, 2)), 0, 50, "rl-spot zero"),
               spot(rl_key("street", c(0, 1, 2)), 0, 200 / 3, "rl-spot zero"),
               spot(rl_key("split", c(0, 1)), 0, 500 / 6, "rl-spot zero"),
               spot(rl_key("corner", c(0, 1, 2, 3)), 0, 100, "rl-spot zero basket"))
    zero_col <- div(class = "rl-zero",
                    div(class = "rl-num green", `data-key` = "straight:0", onclick = click("straight:0"), span("0")))
  }
  outside <- function(key, label, cls = "") div(class = paste("rl-out", cls), `data-key` = key, onclick = click(key), span(label))
  div(class = paste("rl-table", variant), id = ns("table"),
      zero_col,
      div(class = "rl-grid", cells, spots, zs),
      div(class = "rl-cols", outside("col3", "2:1"), outside("col2", "2:1"), outside("col1", "2:1")),
      div(class = "rl-dozens", outside("dozen1", tr("1st 12")), outside("dozen2", tr("2nd 12")), outside("dozen3", tr("3rd 12"))),
      div(class = "rl-outside",
          outside("low", "1-18"), outside("even", tr("EVEN")), outside("red", "\u25C6", "red"),
          outside("black", "\u25C6", "black"), outside("odd", tr("ODD")), outside("high", "19-36")))
}

roulette_ui <- function(id) {
  ns <- NS(id)
  div(class = "table-game roulette",
      page_header(tr("Roulette"), tr("Single zero, double zero, La Partage - and up to six wheels at once."), icon = "\U0001F3A1"),
      div(class = "variant-pills",
          radioButtons(ns("variant"), NULL, choiceNames = tr(unname(RL_VARIANTS)), choiceValues = names(RL_VARIANTS), inline = TRUE)),
      div(class = "rules-line", uiOutput(ns("rules"))),
      div(class = "rl-layout",
          div(class = "rl-wheel-area",
              uiOutput(ns("wheels")),
              div(class = "rl-last", id = ns("last"), div(class = "rl-last-empty", tr("Place your bets!"))),
              div(class = "rl-history", id = ns("history"))),
          div(class = "rl-board",
              uiOutput(ns("table")),
              uiOutput(ns("call_bets")),
              div(class = "table-controls",
                  div(class = "bet-row",
                      div(class = "bet-chips",
                          span(class = "ctl-label", tr("CHIP")),
                          lapply(RL_CHIPS, function(v) tags$button(
                            type = "button", class = paste("chip", chip_color(v), if (v == 1000) "selected"),
                            `data-chip` = v, onclick = paste0("FC.selectChip(this);", js_set(ns("chip"), v)), fmt_short(v)))),
                      uiOutput(ns("multi_ctl"))),
                  div(class = "action-row",
                      actionButton(ns("undo"), tr("UNDO"), class = "btn-act"),
                      actionButton(ns("clear"), tr("CLEAR"), class = "btn-act"),
                      actionButton(ns("rebet"), tr("REBET"), class = "btn-act"),
                      actionButton(ns("double"), tr("DOUBLE"), class = "btn-act"),
                      div(class = "bet-info", uiOutput(ns("stake"))),
                      actionButton(ns("spin"), tr("SPIN"), class = "btn-deal"))))))
}

roulette_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    st <- reactiveValues(variant = "european", bets = numeric(0), stack = list(), last_bets = numeric(0),
                         chip = 1000, wheels = 1L, history = integer(0), spinning_until = 0)
    n_wheels <- reactive(if (st$variant == "multi") st$wheels else 1L)
    wheel_variant <- function(v) if (v == "multi") "european" else v

    push_chips <- function() {
      session$sendCustomMessage("fc_chips", list(table = ns("table"), bets = as.list(isolate(st$bets))))
    }

    observeEvent(input$variant, {
      st$variant <- input$variant
      st$bets <- numeric(0); st$stack <- list(); st$history <- integer(0)
    }, ignoreInit = TRUE)
    observeEvent(input$chip, { st$chip <- input$chip })
    observeEvent(input$n_wheels, { st$wheels <- as.integer(input$n_wheels) })

    place <- function(keys_amounts) {
      v <- isolate(st$variant)
      b <- isolate(st$bets)
      stake_now <- sum(b); add <- sum(keys_amounts)
      mx <- BET_LADDER[player$max_bet_index()] * 10
      if (stake_now + add > mx) {
        player$toast(tr("Table limit"), tr("Max total stake at your level is %s", fmt_coins(mx)), "warning", "\U0001F512")
        return(FALSE)
      }
      if ((stake_now + add) * isolate(n_wheels()) > player$coins()) {
        player$toast(tr("Not enough coins"), tr("Your bets exceed your balance."), "warning", "\U0001FA99"); return(FALSE)
      }
      for (k in names(keys_amounts)) {
        if (!rl_valid(k, wheel_variant(v)) && !(k %in% names(OUTSIDE_BETS))) next
        b <- rl_add_bet(b, k, keys_amounts[[k]])
      }
      st$stack <- c(isolate(st$stack), list(isolate(st$bets)))
      st$bets <- b
      push_chips()
      TRUE
    }

    observeEvent(input$place, {
      place(stats::setNames(isolate(st$chip), input$place$key))
    })
    observeEvent(input$call, {
      cb <- RL_CALL_BETS[[input$call$id]]
      place(cb$chips * isolate(st$chip))
    })
    observeEvent(input$undo, {
      s <- st$stack
      if (length(s)) { st$bets <- s[[length(s)]]; st$stack <- s[-length(s)]; push_chips() }
    })
    observeEvent(input$clear, { st$stack <- c(st$stack, list(st$bets)); st$bets <- numeric(0); push_chips() })
    observeEvent(input$rebet, {
      if (length(st$last_bets)) { st$bets <- numeric(0); place(st$last_bets) }
    })
    observeEvent(input$double, { if (length(st$bets)) place(st$bets) })

    observeEvent(input$spin, {
      if (now_secs() < isolate(st$spinning_until)) return()
      bets <- isolate(st$bets)
      if (!length(bets)) { player$toast(tr("No bets"), tr("Click the layout to place chips."), "warning", "\U0001F3A1"); return() }
      v <- isolate(st$variant); nw <- isolate(n_wheels()); wv <- wheel_variant(v)
      stake <- sum(bets) * nw
      if (!player$debit(stake)) { player$toast(tr("Not enough coins"), tr("Remove some chips."), "warning", "\U0001FA99"); return() }
      results <- vapply(seq_len(nw), function(i) rl_spin(wv), numeric(1))
      settles <- lapply(results, function(res) rl_settle(bets, res, v))
      ret <- sum(vapply(settles, `[[`, numeric(1), "total"))
      player$credit(ret)
      order <- rl_wheel(wv)
      dur <- 4200
      st$spinning_until <- now_secs() + 1.2     # no double spins while the wheel turns
      st$history <- utils::head(c(rev(results), isolate(st$history)), 18)
      st$last_bets <- bets
      straight_hits <- sum(vapply(settles, function(s) sum(grepl("^straight:", s$lines$bet) & s$lines$returned > 0), numeric(1)))

      result_html <- as.character(tagList(
        div(class = "rl-last-nums", lapply(results, function(x)
          span(class = paste("rl-ball-num", rl_color(x)), rl_label(x)))),
        div(class = paste("rl-net", if (ret - stake > 0) "up" else if (ret - stake < 0) "down"),
            if (ret > 0) tr("Returned %s", fmt_coins(ret)) else tr("No winning bets"),
            span(paste0(" (", tr("net %s", paste0(if (ret - stake >= 0) "+" else "-", fmt_coins(abs(ret - stake)))), ")"))),
        div(class = "rl-win-lines", lapply(seq_along(settles), function(w) {
          l <- settles[[w]]$lines; l <- l[l$returned > 0, , drop = FALSE]
          lapply(seq_len(nrow(l)), function(i) div(sprintf("%s%s: %s", if (nw > 1) paste0(tr("W%d", w), " \u2022 ") else "",
                                                           rl_bet_label(l$bet[i]), l$note[i]),
                                                   span(fmt_coins(l$returned[i]))))
        }))))
      history_html <- as.character(tagList(lapply(isolate(st$history), function(x)
        span(class = paste("rl-hist", rl_color(x)), rl_label(x)))))

      session$sendCustomMessage("fc_roulette", list(
        wheels = lapply(seq_len(nw), function(i) list(id = ns(paste0("wheel", i)),
                                                      index = match(results[i], order) - 1L,
                                                      n = length(order), label = rl_label(results[i]),
                                                      color = rl_color(results[i]))),
        duration = dur, last = ns("last"), history = ns("history"),
        result_html = result_html, history_html = history_html))
      if (ret >= 20 * sum(bets) && ret > 0) player$celebrate(win_tier(ret, stake), ret, tr("ROULETTE WIN!"), tr(RL_VARIANTS[[v]]), delay = dur)
      player$record("roulette", v, NULL, wager = stake, win = ret, bet = stake,
                    metrics = c(rl_spins = nw, rl_straight = straight_hits))
    })

    output$rules <- renderUI(span(tr(RL_RULES_TEXT[[st$variant]])))

    output$wheels <- renderUI({
      nw <- n_wheels(); wv <- wheel_variant(st$variant)
      size <- if (nw == 1) 320 else if (nw <= 2) 220 else if (nw <= 4) 160 else 130
      div(class = paste("rl-wheels", paste0("n", nw)),
          lapply(seq_len(nw), function(i) roulette_wheel_svg(ns(paste0("wheel", i)), wv, size)))
    })
    output$table <- renderUI({
      ui <- roulette_table_ui(ns, wheel_variant(st$variant))
      session$onFlushed(push_chips, once = TRUE)
      ui
    })
    output$call_bets <- renderUI({
      if (st$variant != "french") return(NULL)
      div(class = "call-bets",
          span(class = "ctl-label", tr("ANNOUNCED BETS")),
          lapply(names(RL_CALL_BETS), function(k) {
            cb <- RL_CALL_BETS[[k]]
            tags$button(type = "button", class = "btn-call", onclick = js_set(ns("call"), sprintf("{id: '%s', n: Math.random()}", k)),
                        cb$label, span(tr("%d chips", as.integer(sum(cb$chips)))))
          }))
    })
    output$multi_ctl <- renderUI({
      if (st$variant != "multi") return(NULL)
      div(class = "multi-ctl", span(class = "ctl-label", tr("WHEELS")),
          radioButtons(ns("n_wheels"), NULL, choices = 1:6, selected = isolate(st$wheels), inline = TRUE))
    })
    output$stake <- renderUI({
      s <- sum(st$bets); nw <- n_wheels()
      tagList(span(class = "bet-amount", fmt_coins(s * nw)),
              span(class = "bet-sub", if (nw > 1) tr("%s \u00D7 %d wheels", fmt_coins(s), nw) else trn(length(st$bets), "%d bet", "%d bets", length(st$bets))))
    })
    invisible(st)
  })
}
