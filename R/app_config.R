# ---------------------------------------------------------------------------
# app_config.R -- run-time options.
#
#   options(
#     fortuneCougar.lang    = "en" | "pt_BR",        # default language
#     fortuneCougar.storage = "sqlite" | "browser",  # where progress is saved
#     fortuneCougar.db_path = "path/to/fortuneCougar.sqlite"
#   )
#
# run_app(lang =, storage =, db_path =) sets the same options.
# ---------------------------------------------------------------------------

fc_storage_backend <- function() {
  b <- getOption("fortuneCougar.storage", NULL)
  if (is.null(b)) b <- if (requireNamespace("RSQLite", quietly = TRUE)) "sqlite" else "browser"
  match.arg(b, c("sqlite", "browser", "memory"))
}

fc_db_path <- function() {
  getOption("fortuneCougar.db_path",
            file.path(tools::R_user_dir("fortuneCougar", which = "data"), "fortuneCougar.sqlite"))
}
