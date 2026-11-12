# ---------------------------------------------------------------------------
# mod_lobby.R -- the casino lobby: hero banner, live linked jackpots, quick
# side-game shortcuts and the full machine catalogue.
# ---------------------------------------------------------------------------

lobby_ui <- function(id) {
  ns <- NS(id)
  go <- function(tab, game = NULL) js_set(ns("go"), sprintf("{tab: '%s', game: %s, n: Math.random()}", tab,
                                                            if (is.null(game)) "null" else sprintf("'%s'", game)))
  cat_card <- function(tab, title, sub, icon, cls) {
    div(class = paste("cat-card", cls), onclick = go(tab), role = "button",
        div(class = "cat-icon", icon), div(class = "cat-title", tr(title)), div(class = "cat-sub", sub))
  }
  div(class = "lobby",
      div(class = "hero",
          div(class = "hero-text",
              div(class = "hero-kicker", tr("WELCOME TO")),
              h1(class = "hero-title", HTML("fortune<span>Cougar</span>")),
              p(class = "hero-sub", tr("22 feature-packed slots, 12 table games, a card album, monsters to capture and a daily wheel - all in R.")),
              div(class = "hero-ctas",
                  tags$button(type = "button", class = "btn-deal pulse", onclick = go("wheel"), paste("\U0001F3A1", tr("SPIN THE WHEEL"))),
                  tags$button(type = "button", class = "btn-act gold", onclick = go("dash"), paste("\U0001F3C1", tr("DAILY DASH"))),
                  tags$button(type = "button", class = "btn-act", onclick = go("album"), paste("\U0001F4D6", tr("COUGAR ALBUM"))))),
          div(class = "hero-art", span("\U0001F3B0"), span("\U0001F48E"), span("\U0001F451"), span("\U0001F3B2"))),
      uiOutput(ns("jackpots")),
      div(class = "cat-row",
          cat_card("classic", "Classic & Vegas", tr("%d machines", 6L), "\U0001F3B0", "c-classic"),
          cat_card("myth", "Mythological", tr("%d machines", 9L), "\U0001F3FA", "c-myth"),
          cat_card("animal", "Animal 3D", tr("%d machines", 7L), "\U0001F42F", "c-animal"),
          cat_card("blackjack", "Blackjack", tr("%d tables", 4L), "\U0001F0CF", "c-bj"),
          cat_card("poker", "Video Poker", tr("%d machines", 4L), "\U0001F3B4", "c-vp"),
          cat_card("roulette", "Roulette", tr("%d wheels", 4L), "\U0001F3A1", "c-rl")),
      lapply(names(SLOT_CATEGORIES), function(cat) tagList(
        h3(class = "lobby-section", tr(SLOT_CATEGORIES[[cat]])),
        div(class = "tile-grid compact",
            lapply(slots_in(cat), function(d) game_tile(d, go(d$category, d$id)))))))
}

lobby_server <- function(id, player, linked = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go, player$go(input$go$tab, input$go$game))
    output$jackpots <- renderUI({
      pots <- if (!is.null(linked)) linked$pots else JACKPOT_SEEDS
      ref <- 10000
      last <- if (!is.null(linked)) linked$last else NULL
      div(class = "jackpot-banner", onclick = js_set(session$ns("go"), "{tab: 'classic', game: 'bankjackpots', n: Math.random()}"),
          div(class = "jb-title", paste("\U0001F3E6", tr("BANK OF JACKPOTS \u2022 LINKED PROGRESSIVE")), span(tr("(values at 10K bet, shared by all players)"))),
          div(class = "jb-pots", lapply(names(pots), function(t)
            div(class = paste("jb-pot", t), span(class = "jb-tier", tr(toupper(t))), span(class = "jb-val", fmt_coins(pots[[t]] * ref))))),
          if (!is.null(last)) div(class = "jb-last", tr("Last hit: %s for %s", tr(toupper(last$tier)), fmt_coins(last$amount))))
    })
  })
}
