# ---------------------------------------------------------------------------
# mod_slot_category.R -- a slot category page: a lobby of machine tiles plus
# every machine of the category (one slot_machine module instance each),
# switched with a hidden navset so machine state survives navigation.
# Used by mod_classic_slots.R, mod_myth_slots.R and mod_animal_slots.R.
# ---------------------------------------------------------------------------

slot_category_ui <- function(id, category, title, subtitle, icon) {
  ns <- NS(id)
  games <- slots_in(category)
  lobby <- nav_panel_hidden(
    "lobby",
    page_header(tr(title), tr(subtitle), icon = icon,
                right = span(class = "ph-count", tr("%d machines", length(games)))),
    div(class = "tile-grid",
        lapply(games, function(d) game_tile(d, js_set(ns("open"), sprintf("'%s'", d$id)))))
  )
  machines <- lapply(games, function(d) nav_panel_hidden(
    d$id,
    div(class = "machine-nav",
        tags$button(type = "button", class = "btn-back", onclick = js_set(ns("open"), "'lobby'"),
                    "\u2190 ", tr(title)),
        div(class = "machine-switch",
            lapply(games, function(o) tags$button(
              type = "button", class = paste("ms-pill", if (o$id == d$id) "active"),
              title = o$name, onclick = js_set(ns("open"), sprintf("'%s'", o$id)), HTML(o$art))))),
    slot_machine_ui(ns(d$id), d)
  ))
  do.call(navset_hidden, c(list(lobby), unname(machines), list(id = ns("view"))))
}

slot_category_server <- function(id, category, tab, player, linked = NULL) {
  moduleServer(id, function(input, output, session) {
    games <- slots_in(category)
    machines <- lapply(games, function(d)
      slot_machine_server(d$id, d, player, linked = if (d$id == "bankjackpots") linked else NULL))
    names(machines) <- vapply(games, `[[`, "", "id")

    observeEvent(input$open, {
      nav_select("view", input$open)
    })
    # deep links from the lobby / other modules: player$go(tab, game)
    observeEvent(player$rv$nav, {
      n <- player$rv$nav
      if (identical(n$tab, tab) && !is.null(n$game) && n$game %in% names(machines)) {
        nav_select("view", n$game)
      }
    })
    invisible(machines)
  })
}
