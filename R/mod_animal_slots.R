# ---------------------------------------------------------------------------
# mod_animal_slots.R -- Animal & Fantasy 3D module.
# Machines are defined in games_animal.R; the lobby + machine switching lives
# in mod_slot_category.R.
# ---------------------------------------------------------------------------

animal_slots_ui <- function(id) {
  slot_category_ui(id, "animal", "Animal & Fantasy 3D",
                   "Tigers, kitties, candy kings and creatures of the night.", icon = "\U0001F42F")
}

animal_slots_server <- function(id, player, linked = NULL) {
  slot_category_server(id, "animal", tab = "animal", player = player, linked = linked)
}
