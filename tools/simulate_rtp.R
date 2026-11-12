# ---------------------------------------------------------------------------
# tools/simulate_rtp.R -- Monte-Carlo return-to-player simulator.
#
# From a terminal (in the project folder):
#   Rscript tools/simulate_rtp.R                  # all machines, 100k spins each
#   Rscript tools/simulate_rtp.R vegasstrip 1e6   # one machine, 1M spins
#   Rscript tools/simulate_rtp.R giant,gummy 2e5  # several machines
#
# From R / RStudio:
#   source("tools/simulate_rtp.R")
#   simulate_slot("vegasstrip", n = 1e5)          # one machine (id or definition)
#   simulate_rtp(n = 2e4)                         # every machine, one row each
#   simulate_rtp(c("giant", "gummy"), n = 1e5)
#   names(slot_registry())                        # the machine ids
#
# Plays every machine through exactly the same code path as the Shiny module
# (slot_play() + bonus_autoplay() + bonus_settle()), including free spins,
# retriggers, persistent meters and the linked progressive pools.
# ---------------------------------------------------------------------------

if (!exists("slot_registry", mode = "function")) {
  if (dir.exists("R") && file.exists("DESCRIPTION")) {
    for (f in sort(list.files("R", pattern = "^[A-Za-z0-9].*\\.[Rr]$", full.names = TRUE))) source(f)
  } else if (requireNamespace("fortuneCougar", quietly = TRUE)) {
    attach(asNamespace("fortuneCougar"), name = "fortuneCougar:internals", warn.conflicts = FALSE)
  } else {
    stop("Run this from the fortuneCougar project folder (or install the package).")
  }
}

# Simulate one machine.
# @param def a machine id (e.g. "vegasstrip") or a definition from slot_def()
simulate_slot <- function(def, n = 1e5, bet = 1000, spin_gap = 5, batches = 50) {
  if (missing(def) || is.null(def))
    stop("simulate_slot() needs a machine, e.g. simulate_slot(\"vegasstrip\"). ",
         "Use simulate_rtp() for every machine. Ids: ", paste(names(slot_registry()), collapse = ", "), call. = FALSE)
  if (is.character(def)) {
    id <- def
    def <- slot_def(id)
    if (is.null(def)) stop("Unknown machine '", id, "'. Ids: ", paste(names(slot_registry()), collapse = ", "), call. = FALSE)
  }
  batches <- max(1L, min(as.integer(batches), as.integer(n)))
  gs <- slot_init_state(def)
  pots <- JACKPOT_SEEDS
  paid <- numeric(batches); won <- numeric(batches); unsc <- 0
  hits <- 0; spins <- 0; features <- 0; maxw <- 0
  per <- ceiling(n / batches)
  for (k in seq_len(batches)) {
    for (i in seq_len(per)) {
      spins <- spins + 1
      r <- slot_play(def, bet, gs, now = spins * spin_gap, jackpots = pots)
      gs <- r$gs
      w <- r$win
      unsc <- unsc + (r$unscaled %||% 0)
      if (!is.null(r$jp_contrib)) pots <- pots + r$jp_contrib[names(pots)]
      if (!is.null(r$bonus)) {
        b <- bonus_autoplay(r$bonus)
        s <- bonus_settle(def, gs, b, pots)
        w <- w + s$win; gs <- s$gs; unsc <- unsc + (s$unscaled %||% 0)
        if (identical(b$type, "match3")) pots[[s$jackpot]] <- JACKPOT_SEEDS[[s$jackpot]]
      }
      if (length(r$events)) features <- features + 1
      if (w > 0) hits <- hits + 1
      maxw <- max(maxw, w)
      paid[k] <- paid[k] + r$cost
      won[k] <- won[k] + w
    }
  }
  rtp <- sum(won) / sum(paid)
  ok <- paid > 0                                   # batches of pure free spins have no stake
  se <- if (sum(ok) > 1) sd(won[ok] / paid[ok]) / sqrt(sum(ok)) else NA_real_
  jp_part <- unsc / sum(paid)
  data.frame(id = def$id, spins = spins, rtp = round(rtp, 4), se = round(se, 4),
             jackpot_part = round(jp_part, 4), scale = def$scale,
             suggested_scale = round(def$scale * (0.955 - jp_part) / (rtp - jp_part), 4),
             hit_rate = round(hits / spins, 3), feature_rate = round(features / spins, 4),
             max_win_x = round(maxw / bet, 1), stringsAsFactors = FALSE)
}

# Simulate several machines (all by default); one row per machine.
simulate_rtp <- function(ids = NULL, n = 1e5, ...) {
  ids <- ids %||% names(slot_registry())
  do.call(rbind, lapply(ids, function(id) {
    message("simulating ", id, " (", format(n, big.mark = ",", scientific = FALSE), " spins)")
    simulate_slot(id, n = n, ...)
  }))
}

# Command-line entry point (Rscript tools/simulate_rtp.R [ids] [n]).
if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  ids <- if (length(args) >= 1 && args[1] != "all") strsplit(args[1], ",")[[1]] else NULL
  n <- if (length(args) >= 2) as.numeric(args[2]) else 1e5
  set.seed(as.integer(Sys.time()) %% 100000)
  print(simulate_rtp(ids, n = n), row.names = FALSE)
}
