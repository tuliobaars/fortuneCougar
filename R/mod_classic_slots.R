# ---------------------------------------------------------------------------
# mod_classic_slots.R -- Classic & Vegas Slots module.
# Machines are defined in games_classic.R; the lobby + machine switching lives
# in mod_slot_category.R.
# ---------------------------------------------------------------------------

classic_slots_ui <- function(id) {
  slot_category_ui(id, "classic", "Classic & Vegas Slots",
                   "Steppers, neon strips, vaults and linked progressives - pure Vegas.", icon = "\U0001F3B0")
}

classic_slots_server <- function(id, player, linked = NULL) {
  slot_category_server(id, "classic", tab = "classic", player = player, linked = linked)
}
