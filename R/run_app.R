#' Run the fortuneCougar casino app
#'
#' Launches the Shiny app bundled with the package.
#'
#' @param lang Default language: `"en"` (English) or `"pt_BR"` (Brazilian
#'   Portuguese). Players can switch language with the EN/PT button in the
#'   header; a URL parameter `?lang=pt_BR` also works.
#' @param storage Where player progress is saved: `"sqlite"` (default when
#'   RSQLite is installed), `"browser"` (the browser's localStorage) or
#'   `"memory"` (lost when the app stops).
#' @param db_path Path of the SQLite database file. Defaults to a file in
#'   `tools::R_user_dir("fortuneCougar", "data")`.
#' @param ... Arguments passed on to [shiny::shinyApp()], e.g. `options`.
#' @return A `shiny.appobj`; printing it (or calling it at the console)
#'   starts the app.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#'   run_app(lang = "pt_BR", db_path = tempfile(fileext = ".sqlite"))
#' }
run_app <- function(lang = getOption("fortuneCougar.lang", "en"),
                    storage = getOption("fortuneCougar.storage", NULL),
                    db_path = getOption("fortuneCougar.db_path", NULL), ...) {
  opts <- list(fortuneCougar.lang = fc_normalize_lang(lang) %||% "en")
  if (!is.null(storage)) opts$fortuneCougar.storage <- match.arg(storage, c("sqlite", "browser", "memory"))
  if (!is.null(db_path)) opts$fortuneCougar.db_path <- db_path
  shiny::shinyApp(
    ui = fc_app_ui, server = fc_app_server,
    onStart = function() {
      old <- options(opts)
      shiny::onStop(function() {
        fc_db_close_all()
        linked_jackpots_reset()
        options(old)
      })
    },
    ...)
}

# Where casino.css / casino.js live: inst/app/www in the source tree (when run
# with shiny::runApp() from the project folder) or app/www in the installed
# package.
fc_assets_dir <- function() {
  src <- file.path("inst", "app", "www")
  if (dir.exists(src)) return(normalizePath(src))
  system.file("app", "www", package = "fortuneCougar")
}
