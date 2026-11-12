# ---------------------------------------------------------------------------
# ui_components.R -- reusable htmltools builders shared by the modules.
# ---------------------------------------------------------------------------

# A reel cell (symbol glyph + optional corner badge).
sym_cell <- function(def, id, badge = "") {
  s <- def$symbols[[id]]
  div(class = paste("cell", s$cls), `data-sym` = id,
      div(class = "sym", HTML(s$glyph)),
      span(class = "cell-badge", badge))
}

# JSON symbol map used by the front-end animator: id -> [glyph, classes].
symbol_map_json <- function(def) {
  jsonlite::toJSON(lapply(def$symbols, function(s) c(s$glyph, s$cls)), auto_unbox = FALSE)
}

# Paytable as shown in the machine's info popover: pays in x total bet.
paytable_ui <- function(def) {
  unit <- if (def$ways) 1 / def$ways_unit else 1 / nrow(def$lines)
  rows <- lapply(def$symbols, function(s) {
    p <- def$P[s$id, ] * unit * def$scale
    shown <- which(p > 0)
    if (!length(shown)) return(NULL)
    tags$tr(
      tags$td(class = paste("pt-sym", s$cls), HTML(s$glyph)),
      tags$td(tr(s$name)),
      tags$td(class = "pt-pays", HTML(paste(sprintf("<span>%d&times;</span> %s", shown,
                                                   vapply(p[shown], function(v) format(signif(v, 3), big.mark = fc_big_mark(), decimal.mark = fc_dec_mark()), "")),
                                           collapse = " &middot; "))))
  })
  special <- Filter(function(s) grepl("scatter|collect|mystery", s$cls) || identical(s$id, def$wild), def$symbols)
  tagList(
    p(class = "pt-desc", tr(def$description)),
    tags$table(class = "paytable", tags$tbody(rows)),
    p(class = "pt-note",
      if (def$ways) tr("%s ways; pays shown as multiples of total bet per way-combination.",
                       fmt_coins(def$rows^def$reels))
      else tr("%d paylines, left to right; pays shown as multiples of total bet.", nrow(def$lines)),
      if (!is.null(def$wild)) paste0(" ", tr("%s substitutes for all paying symbols.", tr(def$symbols[[def$wild]]$name))),
      if (!is.na(def$rtp %||% NA)) paste0(" ", tr("RTP %s%% (\u00B1%s%%, Monte-Carlo calibrated), hit rate %s%%.",
                                                  fmt_num(100 * def$rtp), fmt_num(100 * (def$rtp_se %||% 0)), fmt_num(100 * def$hit, 0))))
  )
}

# Feature meter bar(s) from a def$meter() result.
meter_ui <- function(items) {
  if (!length(items)) return(NULL)
  div(class = "meters", lapply(items, function(m) {
    if (!is.null(m$pct)) {
      div(class = paste("meter", m$kind),
          div(class = "meter-label", m$label),
          div(class = "meter-bar", div(class = "meter-fill", style = sprintf("width:%.1f%%", clamp(m$pct, 0, 100)))),
          div(class = "meter-text", m$text))
    } else {
      div(class = paste("jp-pill", m$kind), span(class = "jp-label", m$label), span(class = "jp-value", m$text))
    }
  }))
}

# A playing card face.
playing_card <- function(card, hidden = FALSE, class = "", held = FALSE, onclick = NULL) {
  if (hidden) return(div(class = paste("pcard back", class), div(class = "pcard-back-inner")))
  if (card == JOKER) {
    return(div(class = paste("pcard joker", class, if (held) "held"), onclick = onclick,
               div(class = "pc-corner", "JK"), div(class = "pc-center", "\U0001F0CF"),
               if (held) div(class = "held-tag", tr("HELD"))))
  }
  r <- RANK_LABELS[card_rank(card)]; s <- SUIT_SYMBOLS[card_suit(card)]
  col <- SUIT_COLOR[card_suit(card)]
  div(class = paste("pcard", col, class, if (held) "held"), onclick = onclick,
      div(class = "pc-corner tl", div(r), div(s)),
      div(class = "pc-center", s),
      div(class = "pc-corner br", div(r), div(s)),
      if (held) div(class = "held-tag", tr("HELD")))
}

# Casino chip (visual) for table games.
chip_color <- function(v) {
  if (v >= 100000) "chip-black" else if (v >= 25000) "chip-purple" else if (v >= 5000) "chip-orange"
  else if (v >= 1000) "chip-blue" else if (v >= 500) "chip-green" else "chip-red"
}

# @param selectable TRUE = the chip stays highlighted once chosen (coin values)
chip_button <- function(input_id, value, selected = FALSE, selectable = FALSE) {
  tags$button(type = "button", class = paste("chip", chip_color(value), if (selected) "selected"),
              onclick = paste0(if (selectable) "FC.selectChip(this);" else "",
                               sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", input_id, value)),
              fmt_short(value))
}

# Lobby tile for a slot machine.
game_tile <- function(def, onclick) {
  vol <- def$volatility
  div(class = paste("game-tile", def$theme), onclick = onclick, role = "button", tabindex = "0",
      div(class = "tile-art", HTML(def$art)),
      div(class = "tile-body",
          div(class = "tile-name", def$name),
          div(class = "tile-tag", tr(def$tagline)),
          div(class = "tile-chips",
              span(class = "chip-mini vol", tr(vol)),
              lapply(utils::head(def$features, 2), function(f) span(class = "chip-mini", tr(f))))),
      div(class = "tile-play", tr("PLAY")))
}

# Section header used on meta pages.
page_header <- function(title, subtitle = NULL, icon = NULL, right = NULL) {
  div(class = "page-header",
      div(class = "ph-left",
          if (!is.null(icon)) span(class = "ph-icon", icon),
          div(h2(title), if (!is.null(subtitle)) p(subtitle))),
      if (!is.null(right)) div(class = "ph-right", right))
}

progress_bar <- function(pct, label = NULL, class = "") {
  div(class = paste("fc-progress", class),
      div(class = "fc-progress-fill", style = sprintf("width:%.1f%%", clamp(pct, 0, 100))),
      if (!is.null(label)) span(class = "fc-progress-label", label))
}

# JS snippet: send an event-priority value to a Shiny input.
js_set <- function(input_id, value_js) {
  sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", input_id, value_js)
}

# SVG prize wheel (Wheel of Fun / Pearl Wheel / roulette uses its own).
# @param spin_to optional 1-based segment index: the browser animates the
#   wheel to land on it as soon as the element appears (see casino.js).
svg_wheel <- function(id, labels, colors, size = 320, text_colors = NULL, class = "",
                      spin_to = NULL, duration = 4200) {
  n <- length(labels)
  r <- size / 2; cx <- r; cy <- r
  seg <- 360 / n
  text_colors <- text_colors %||% rep("#fff", n)
  paths <- vapply(seq_len(n), function(i) {
    a0 <- (i - 1) * seg - 90 - seg / 2; a1 <- a0 + seg
    x0 <- cx + (r - 4) * cos(a0 * pi / 180); y0 <- cy + (r - 4) * sin(a0 * pi / 180)
    x1 <- cx + (r - 4) * cos(a1 * pi / 180); y1 <- cy + (r - 4) * sin(a1 * pi / 180)
    am <- (a0 + a1) / 2
    tx <- cx + (r * 0.66) * cos(am * pi / 180); ty <- cy + (r * 0.66) * sin(am * pi / 180)
    sprintf(paste0('<path d="M%.1f,%.1f L%.2f,%.2f A%.1f,%.1f 0 0,1 %.2f,%.2f Z" fill="%s" stroke="rgba(255,255,255,.35)" stroke-width="2"/>',
                   '<text x="%.1f" y="%.1f" fill="%s" font-size="%d" font-weight="800" text-anchor="middle" dominant-baseline="middle" transform="rotate(%.1f %.1f %.1f)">%s</text>'),
            cx, cy, x0, y0, r - 4, r - 4, x1, y1, colors[(i - 1) %% length(colors) + 1],
            tx, ty, text_colors[i], if (n > 10) 13 else 16, am + 90, tx, ty, htmltools::htmlEscape(labels[i]))
  }, "")
  div(class = paste("prize-wheel", class, if (!is.null(spin_to)) "spinning"),
      `data-spin-to` = spin_to, `data-n` = n, `data-duration` = duration,
      div(class = "wheel-pointer"),
      HTML(sprintf('<svg id="%s" class="wheel-svg" viewBox="0 0 %d %d" width="%d" height="%d"><g class="wheel-rotor">%s<circle cx="%.1f" cy="%.1f" r="%.1f" fill="#1a0933" stroke="#ffcc00" stroke-width="4"/></g></svg>',
                   id, size, size, size, size, paste(paths, collapse = ""), cx, cy, r * 0.18)),
      div(class = "wheel-hub", "\u2605"))
}

# Display name for any game id (slot id or table variant), translated.
game_display_name <- function(id) {
  d <- slot_registry()[[id]]
  if (!is.null(d)) return(d$name)
  nm <- c(BJ_VARIANTS, vapply(VP_GAMES, `[[`, "", "name"), RL_VARIANTS, wheel = "Wheel of Fun",
          double_up = "Double or Nothing")[id]
  if (is.na(nm)) id else tr(unname(nm))
}
