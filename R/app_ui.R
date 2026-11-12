# ---------------------------------------------------------------------------
# app_ui.R -- top-level page: bslib navbar, dark casino theme, all modules.
# The page is built in the language requested with ?lang= (or the default).
# ---------------------------------------------------------------------------

fc_theme <- function() {
  bs_theme(
    version = 5,
    bg = "#0b0418", fg = "#f3eaff",
    primary = "#ff2e88", secondary = "#7b2cbf", success = "#1fd18b",
    info = "#00c2ff", warning = "#ffcc00", danger = "#ff4d6d",
    base_font = font_collection("Poppins", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    heading_font = font_collection("Bungee", "Poppins", "Segoe UI", "sans-serif"),
    "border-radius" = "0.9rem"
  )
}

# Language requested by the browser (?lang=...), else the default.
fc_request_lang <- function(request) {
  q <- if (is.null(request)) "" else request$QUERY_STRING %||% ""
  fc_normalize_lang(if (nzchar(q)) shiny::parseQueryString(q)$lang else NULL) %||% fc_default_lang()
}

# The app UI (a function of `request`, as accepted by shinyApp()).
fc_app_ui <- function(request) {
  lang <- fc_request_lang(request)
  shiny::addResourcePath("fc-assets", fc_assets_dir())
  with_lang(lang, page_navbar(
    id = "main_nav",
    title = tags$span(class = "brand", tags$span(class = "brand-logo", "\U0001F406"),
                      tags$span(class = "brand-text", HTML("fortune<b>Cougar</b>"))),
    window_title = tr("fortuneCougar \u2022 Shiny Casino"),
    lang = if (lang == "pt_BR") "pt-BR" else "en",
    theme = fc_theme(),
    fillable = FALSE,
    header = tagList(
      tags$head(
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
        tags$link(rel = "stylesheet",
                  href = "https://fonts.googleapis.com/css2?family=Bungee&family=Poppins:wght@400;600;800&display=swap"),
        tags$link(rel = "stylesheet", href = "fc-assets/casino.css"),
        tags$script(HTML(sprintf("window.FC_LANG = '%s'; window.FC_I18N = %s;", lang,
                                 jsonlite::toJSON(fc_js_i18n(lang), auto_unbox = TRUE)))),
        tags$script(src = "fc-assets/casino.js")
      ),
      div(id = "fc-toasts"),
      div(id = "fc-celebrate")
    ),
    nav_panel(tr("Lobby"), value = "lobby", lobby_ui("lobby"), icon = tags$span("\U0001F3E0")),
    nav_panel(tr("Classic & Vegas"), value = "classic", classic_slots_ui("classic"), icon = tags$span("\U0001F3B0")),
    nav_panel(tr("Mythological"), value = "myth", myth_slots_ui("myth"), icon = tags$span("\U0001F3FA")),
    nav_panel(tr("Animal 3D"), value = "animal", animal_slots_ui("animal"), icon = tags$span("\U0001F42F")),
    do.call(nav_menu, c(list(title = tr("Table Games"), value = "tables", icon = tags$span("\U0001F0CF")),
                        table_games_nav_panels("tables"))),
    do.call(nav_menu, c(list(title = tr("Meta & Side Games"), value = "meta", icon = tags$span("\u2B50")),
                        meta_nav_panels("meta"),
                        list(nav_panel(tr("My Stats"), value = "stats", stats_ui("stats"), icon = tags$span("\U0001F4CA"))))),
    nav_spacer(),
    nav_item(hud_ui("hud"))
  ))
}
