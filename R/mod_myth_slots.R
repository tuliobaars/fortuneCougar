# ---------------------------------------------------------------------------
# mod_myth_slots.R -- Mythological & Adventure module.
# Machines are defined in games_myth.R; the lobby + machine switching lives
# in mod_slot_category.R.
# ---------------------------------------------------------------------------

myth_slots_ui <- function(id) {
  slot_category_ui(id, "myth", "Mythological & Adventure",
                   "Pharaohs, gorgons, pirates, giants and phoenixes - epic features on every reel.", icon = "\U0001F3FA")
}

myth_slots_server <- function(id, player, linked = NULL) {
  slot_category_server(id, "myth", tab = "myth", player = player, linked = linked)
}
