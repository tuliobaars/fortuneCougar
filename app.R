# ===========================================================================
#  fortuneCougar - Shiny casino
#
#  From this folder:        shiny::runApp()
#  As an installed package: fortuneCougar::run_app()
#
#  This folder is an R package; app.R loads the package sources (with
#  pkgload when available, so imports behave exactly like the installed
#  package) and returns run_app(). Shiny's automatic sourcing of R/ is
#  switched off by R/_disable_autoload.R.
# ===========================================================================

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
} else {
  library(shiny)
  library(bslib)
  for (f in sort(list.files("R", pattern = "^[A-Za-z0-9].*\\.[Rr]$", full.names = TRUE))) {
    source(f, local = TRUE)
  }
}

run_app()
