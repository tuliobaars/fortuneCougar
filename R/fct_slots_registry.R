# ---------------------------------------------------------------------------
# slot_registry.R -- the catalogue of all 22 machines + RTP calibration.
#
# `scale` multiplies every pay of a machine (line/ways/scatter pays and fixed
# feature prizes; linked/fixed jackpots are excluded). RTP is linear in
# `scale`, so each value was solved as 0.955 / (return per unit of scale),
# pooling 180k-340k simulated spins per machine from tools/simulate_rtp.R
# (inverse-variance weighted, batch standard errors inflated 1.5x). `rtp` is the resulting expected return,
# `rtp_se` its standard error, `hit` the hit rate and `feat` the share of
# spins with a feature event (100k-spin run).
# Re-run `Rscript tools/simulate_rtp.R all 1e6` after changing any paytable.
# ---------------------------------------------------------------------------

SLOT_CALIBRATION <- read.csv(text = "
id,scale,rtp,rtp_se,hit,feat
classic777,0.4988,0.955,0.0162,0.242,0.0
bankjackpots,1.472,0.955,0.015,0.402,0.003
vegasstrip,0.9888,0.955,0.0159,0.451,0.02
classicruby,0.7648,0.955,0.0076,0.482,0.1966
cityqueens,1.8013,0.955,0.0291,0.32,0.01
rapidfire,2.4755,0.955,0.0077,0.435,0.0
purrymid,0.384,0.955,0.0128,0.385,0.3546
stonegold,0.5983,0.955,0.0082,0.337,0.3374
blackbeard,0.2373,0.955,0.0032,0.428,0.4284
giant,0.7134,0.955,0.0062,0.365,0.1432
cinderella,0.9816,0.955,0.0138,0.341,0.1141
phoenix,1.9286,0.955,0.0652,0.275,0.0047
dragon,1.8052,0.955,0.0176,0.378,0.1096
mermaid,1.2319,0.955,0.0334,0.341,0.0097
wildwest,1.0703,0.955,0.0171,0.296,0.0244
tigers,1.8902,0.955,0.0218,0.365,0.0266
kittygems,1.8367,0.955,0.0107,0.541,0.0
gummy,0.2425,0.955,0.0122,0.7,0.6999
frankenstein,0.9245,0.955,0.0113,0.412,0.325
vixens,1.7934,0.955,0.0107,0.452,0.5996
jaguar,3.8281,0.955,0.012,0.34,0.5852
vampire,2.5147,0.955,0.0109,0.315,0.0247
", stringsAsFactors = FALSE)

.slot_cache <- new.env(parent = emptyenv())

# All slot definitions, built once per R process and cached.
slot_registry <- function() {
  if (is.null(.slot_cache$all)) {
    all <- c(classic_games(), myth_games(), animal_games())
    names(all) <- vapply(all, `[[`, "", "id")
    for (id in names(all)) {
      cal <- SLOT_CALIBRATION[SLOT_CALIBRATION$id == id, ]
      if (nrow(cal)) {
        all[[id]]$scale <- cal$scale
        all[[id]]$rtp <- cal$rtp; all[[id]]$rtp_se <- cal$rtp_se
        all[[id]]$hit <- cal$hit; all[[id]]$feat <- cal$feat
      }
    }
    .slot_cache$all <- all
  }
  .slot_cache$all
}

slot_def <- function(id) slot_registry()[[id]]

# Unnamed on purpose: the result is spliced into bslib navsets / tag lists,
# which reject (or misinterpret) named elements.
slots_in <- function(category) unname(Filter(function(d) d$category == category, slot_registry()))

SLOT_CATEGORIES <- c(classic = "Classic & Vegas", myth = "Mythological & Adventure",
                     animal = "Animal & Fantasy 3D")
