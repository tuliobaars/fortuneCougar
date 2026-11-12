# ---------------------------------------------------------------------------
# mod_hud.R -- navbar heads-up display (avatar, level/XP, coins, stars, VIP)
# plus the client-side overlays (toasts, celebrations). Values are pushed with
# a single `fc_hud` message; casino.js animates the coin counter.
# ---------------------------------------------------------------------------

hud_ui <- function(id) {
  ns <- NS(id)
  div(class = "hud",
      tags$button(type = "button", class = "hud-name", id = "hud-name", title = tr("My Stats"),
                  onclick = js_set(ns("stats"), "Math.random()"),
                  span(class = "hud-name-icon", "\U0001F4CA"), span(id = "hud-name-txt", tr("Guest"))),
      tags$button(type = "button", class = "hud-avatar", id = "hud-avatar", title = tr("Profile & rewards"),
                  onclick = js_set(ns("profile"), "Math.random()"), DEFAULT_AVATAR),
      div(class = "hud-level", title = tr("Level & XP"),
          span(class = "hud-lv", tr("LV"), " ", span(id = "hud-level", "1")),
          div(class = "hud-xp", div(class = "hud-xp-fill", id = "hud-xp"))),
      div(class = "hud-coins", title = tr("Coins"),
          span(class = "hud-coin-icon", "\U0001FA99"), span(id = "hud-coins", fmt_coins(START_COINS))),
      div(class = "hud-stars", title = tr("Stars (Star Market)"), "\u2B50", span(id = "hud-stars", "25")),
      div(class = "hud-vip", id = "hud-vip", title = tr("Playtika Rewards status"), paste("\U0001F949", tr("Bronze"))),
      tags$button(type = "button", class = "hud-turbo", id = "hud-turbo", title = tr("Turbo animations (T)"),
                  onclick = "FC.toggleTurbo()", "\u26A1"),
      # language switch: shows the *other* language
      if (fc_lang() == "pt_BR")
        tags$button(type = "button", class = "hud-lang", title = "Switch to English", onclick = "FC.setLang('en')", "\U0001F1FA\U0001F1F8 EN")
      else
        tags$button(type = "button", class = "hud-lang", title = "Mudar para portugu\u00eas", onclick = "FC.setLang('pt_BR')", "\U0001F1E7\U0001F1F7 PT"))
}

hud_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    observe({
      rv <- player$rv
      v <- player$vip()
      lvl <- rv$level
      session$sendCustomMessage("fc_hud", list(
        coins = rv$coins, level = lvl, xp_pct = round(100 * rv$xp / xp_needed(lvl), 1),
        stars = floor(rv$stars), vip = paste(VIP_TIERS$icon[v$index], tr(v$tier)),
        vip_color = VIP_TIERS$color[v$index], avatar = rv$avatar,
        name = if (nzchar(rv$name)) rv$name else tr("Guest")))
    })
    observeEvent(input$profile, player$go("rewards"))
    observeEvent(input$stats, player$go("stats"))
  })
}
