# Monte-Carlo sanity check of the calibrated return on low-variance machines.
# (Full calibration: Rscript tools/simulate_rtp.R all 1e6)
test_that("calibrated RTP is close to 95.5% on low-variance machines", {
  skip_on_cran()
  set.seed(2024)
  for (id in c("blackbeard", "giant")) {
    d <- slot_def(id)
    gs <- slot_init_state(d); paid <- 0; won <- 0
    for (i in 1:15000) {
      r <- slot_play(d, 100, gs); gs <- r$gs
      paid <- paid + r$cost; won <- won + r$win
    }
    expect_gt(won / paid, 0.90, label = paste(id, "RTP"))
    expect_lt(won / paid, 1.01, label = paste(id, "RTP"))
  }
})
