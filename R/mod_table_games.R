# ---------------------------------------------------------------------------
# mod_table_games.R -- Table Games module (Blackjack, Video Poker, Roulette).
# Returns nav panels for the "Table Games" navbar menu and wires the three
# table servers to the shared player state.
# ---------------------------------------------------------------------------

table_games_nav_panels <- function(id) {
  ns <- NS(id)
  list(
    nav_panel(tr("Blackjack"), value = "blackjack", blackjack_ui(ns("blackjack")), icon = span("\U0001F0CF")),
    nav_panel(tr("Video Poker"), value = "poker", video_poker_ui(ns("poker")), icon = span("\U0001F3B4")),
    nav_panel(tr("Roulette"), value = "roulette", roulette_ui(ns("roulette")), icon = span("\U0001F3A1"))
  )
}

table_games_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    blackjack_server("blackjack", player)
    video_poker_server("poker", player)
    roulette_server("roulette", player)
  })
}
