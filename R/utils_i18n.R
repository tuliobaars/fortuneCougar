# ---------------------------------------------------------------------------
# utils_i18n.R -- translations (English / Brazilian Portuguese).
#
# Strings are written in English and wrapped in tr(); the pt_BR dictionary
# (utils_i18n_pt_BR.R) maps each English string to its translation, falling
# back to English. The active language is resolved as:
#   1. a language forced while the UI is being built (with_lang()),
#   2. the current Shiny session (session$userData$fc_lang),
#   3. getOption("fortuneCougar.lang", "en").
# Engine code therefore stays language-neutral outside a session (tests,
# the RTP simulator) and follows the player's language inside the app.
# ---------------------------------------------------------------------------

FC_LANGS <- c(en = "English", pt_BR = "Portugu\u00eas (Brasil)")

.fc_i18n <- new.env(parent = emptyenv())

# Normalise a language tag: "pt", "pt-BR", "pt_br" -> "pt_BR"; "en-US" -> "en".
fc_normalize_lang <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(x[1])) return(NULL)
  x <- tolower(gsub("-", "_", x[1]))
  if (startsWith(x, "pt")) return("pt_BR")
  if (startsWith(x, "en")) return("en")
  NULL
}

fc_default_lang <- function() fc_normalize_lang(getOption("fortuneCougar.lang", "en")) %||% "en"

# Active language (see header).
fc_lang <- function() {
  if (!is.null(.fc_i18n$forced)) return(.fc_i18n$forced)
  s <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (!is.null(s)) {
    l <- tryCatch(s$userData$fc_lang, error = function(e) NULL)
    if (!is.null(l)) return(l)
  }
  fc_default_lang()
}

# Evaluate `expr` with a fixed language (used while building the UI).
with_lang <- function(lang, expr) {
  old <- .fc_i18n$forced
  .fc_i18n$forced <- fc_normalize_lang(lang) %||% "en"
  on.exit(.fc_i18n$forced <- old)
  expr
}

.fc_dict <- function(lang) {
  if (lang != "pt_BR") return(NULL)
  if (is.null(.fc_i18n$pt_BR)) .fc_i18n$pt_BR <- list2env(as.list(I18N_PT_BR), hash = TRUE)
  .fc_i18n$pt_BR
}

# Translate an English string; extra arguments are passed to sprintf().
tr <- function(text, ...) {
  out <- text
  d <- .fc_dict(fc_lang())
  if (!is.null(d) && length(text) == 1 && !is.na(text)) {
    v <- d[[text]]
    if (!is.null(v)) out <- v
  } else if (!is.null(d) && length(text) > 1) {
    out <- vapply(text, function(t) d[[t]] %||% t, "", USE.NAMES = FALSE)
  }
  if (...length()) sprintf(out, ...) else out
}

# Locale separators for numbers.
fc_big_mark <- function() if (fc_lang() == "pt_BR") "." else ","
fc_dec_mark <- function() if (fc_lang() == "pt_BR") "," else "."

# Strings used by the browser (casino.js), sent once with the page.
FC_JS_STRINGS <- c(
  "WIN!", "BIG WIN!", "MEGA WIN!", "EPIC WIN!", "JACKPOT!", "LEVEL UP!", "COMPLETE!", "NEW STATUS!",
  "tap to continue", "FREE SPINS!", "FREE SPINS WON", "WIN", "BONUS!", "FREE", "left", "SPIN",
  "AUTO", "STOP", "Turbo ON", "Turbo OFF", "Animations at hyper speed.", "Full animations.")

fc_js_i18n <- function(lang = fc_lang()) {
  with_lang(lang, stats::setNames(as.list(tr(FC_JS_STRINGS)), FC_JS_STRINGS))
}

# Singular / plural: trn(n, "%d spin", "%d spins", n)
trn <- function(n, singular, plural, ...) tr(if (isTRUE(n == 1)) singular else plural, ...)
