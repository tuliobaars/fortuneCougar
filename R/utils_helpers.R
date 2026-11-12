# ---------------------------------------------------------------------------
# utils.R -- small, dependency-free helpers shared by every engine and module.
# Everything here is plain base R so the game maths can be unit-tested and
# simulated without a running Shiny session.
# ---------------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Format a coin amount: 1234567 -> "1,234,567"
# (thousands separator follows the active language: 1,000 / 1.000)
fmt_coins <- function(x) {
  format(round(as.numeric(x)), big.mark = fc_big_mark(), decimal.mark = fc_dec_mark(),
         scientific = FALSE, trim = TRUE)
}

# Compact coin format for tight spaces: 2500000 -> "2.5M"
fmt_short <- function(x) {
  x <- as.numeric(x)
  dm <- fc_dec_mark()
  vapply(x, function(v) {
    a <- abs(v)
    num <- function(y) format(y, nsmall = 0, decimal.mark = dm)
    if (a >= 1e9) return(paste0(num(round(v / 1e9, 2)), "B"))
    if (a >= 1e6) return(paste0(num(round(v / 1e6, 2)), "M"))
    if (a >= 1e4) return(paste0(num(round(v / 1e3, 1)), "K"))
    fmt_coins(v)
  }, character(1))
}

# Evaluate `expr` with a fixed RNG seed, then restore the caller's RNG stream.
# Used to build deterministic reel strips / daily missions without disturbing
# the live random number stream that drives the games.
with_seed <- function(seed, expr) {
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_seed) old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (has_seed) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  })
  set.seed(seed)
  expr
}

# Stable integer hash of a string (for seeding per-day content).
str_seed <- function(s) {
  v <- utf8ToInt(paste(s, collapse = "|"))
  h <- 7
  for (x in v) h <- (h * 31 + x) %% 2147483647
  as.integer(h)
}

# Weighted draw of `n` items (with replacement).
wdraw <- function(items, weights, n = 1) {
  items[sample.int(length(items), n, replace = TRUE, prob = weights)]
}

# Fisher-Yates via sample(): returns a uniformly shuffled copy.
shuffle <- function(x) x[sample.int(length(x))]

clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

now_secs <- function() as.numeric(Sys.time())

# Period keys used by Daily Dash / Wheel cooldowns.
period_keys <- function(time = Sys.time()) {
  d <- as.Date(time)
  list(
    day   = format(d, "%Y-%m-%d"),
    week  = format(d, "%G-W%V"),
    month = format(d, "%Y-%m")
  )
}

# Seconds until the next local midnight / next week / next month boundary.
secs_until <- function(period = c("day", "week", "month"), time = Sys.time()) {
  period <- match.arg(period)
  d <- as.Date(format(time, "%Y-%m-%d"))
  nxt <- switch(period,
    day   = d + 1,
    week  = d + (8 - as.integer(format(d, "%u"))),
    month = {
      y <- as.integer(format(d, "%Y")); m <- as.integer(format(d, "%m"))
      if (m == 12) as.Date(sprintf("%d-01-01", y + 1)) else as.Date(sprintf("%d-%02d-01", y, m + 1))
    })
  max(0, as.numeric(difftime(as.POSIXct(format(nxt)), time, units = "secs")))
}

fmt_duration <- function(secs) {
  secs <- max(0, round(secs))
  h <- secs %/% 3600; m <- (secs %% 3600) %/% 60; s <- secs %% 60
  if (h >= 24) return(sprintf("%dd %02dh", h %/% 24, h %% 24))
  if (h > 0) sprintf("%dh %02dm", h, m) else sprintf("%02d:%02d", m, s)
}

# Win-size tier used for celebrations (multiple of total bet).
win_tier <- function(win, bet) {
  if (is.null(bet) || bet <= 0 || win <= 0) return("none")
  m <- win / bet
  if (m >= 50) "epic" else if (m >= 20) "mega" else if (m >= 8) "big" else "win"
}

# Fixed-decimal number with the active language's decimal mark: 95.5 / 95,5
fmt_num <- function(x, digits = 1) {
  formatC(as.numeric(x), format = "f", digits = digits, decimal.mark = fc_dec_mark(), big.mark = fc_big_mark())
}
