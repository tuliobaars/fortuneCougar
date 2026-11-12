test_that("payline evaluator matches the naive reference on every payline machine", {
  set.seed(42)
  for (d in Filter(function(d) !d$ways, slot_registry())) {
    for (i in 1:150) {
      g <- spin_grid(d)
      if (!is.null(d$wild) && runif(1) < 0.5) g[sample(length(g), 3)] <- d$wild
      mult <- if (runif(1) < 0.5) matrix(sample(c(1, 2, 3, 5), length(g), TRUE), nrow(g)) else NULL
      if (!is.null(mult)) mult[g != (d$wild %||% "")] <- 1
      expect_equal(eval_lines(d, g, 10, mult)$win, ref_eval_lines(d, g, 10, mult),
                   info = paste(d$id, "grid", i))
    }
  }
})

test_that("classic 777 multiplier wilds, ANY groups and pure-wild lines", {
  d <- slot_def("classic777")
  g <- matrix(c("r7", "cash", "r7"), 3, 3, byrow = TRUE)
  g[1, ] <- c("lemon", "bell", "cherry"); g[3, ] <- "blank"
  m <- matrix(1, 3, 3); m[2, 2] <- 3
  expect_equal(eval_lines(d, g, 1, m)$win, 100 * 3)
  g2 <- matrix("blank", 3, 3); g2[2, ] <- c("r7", "b7", "r7")
  expect_equal(eval_lines(d, g2, 1)$win, 25)                      # ANY 7
  g3 <- matrix("blank", 3, 3); g3[2, ] <- c("bar1", "bar3", "cash")
  expect_equal(eval_lines(d, g3, 1)$win, 5)                       # ANY BAR with a wild
  g4 <- matrix("blank", 3, 3); g4[2, ] <- "cash"
  m4 <- matrix(1, 3, 3); m4[2, ] <- 5
  expect_equal(eval_lines(d, g4, 1, m4)$win, 300)                 # wilds pay unmultiplied
})

test_that("ways evaluation counts ways across adjacent reels", {
  d <- slot_def("phoenix")
  g <- matrix("A", 3, 5); g[, 4:5] <- "K"
  expect_equal(eval_ways(d, g, 1)$win, d$P["A", 3] * 27)
  g[1, 2] <- "phoenix"
  expect_equal(eval_ways(d, g, 1)$win, d$P["A", 3] * 27)           # the wild is one of the 3 ways
  g <- matrix("Q", 3, 5); g[2, 1] <- "A"; g[, 2] <- "phoenix"; g[1, 3] <- "A"
  expect_equal(eval_ways(d, g, 1)$win, d$P["A", 3] * 3 + d$P["Q", 5] * (2 * 3 * 2 * 3 * 3))
})

test_that("scatters pay anywhere as multiples of the total bet", {
  d <- slot_def("vegasstrip")
  g <- matrix("A", 3, 5); g[1, c(1, 3, 5)] <- "star"
  expect_equal(eval_scatter(d, g, 100)$win, 500)
})

test_that("reel strips are deterministic, valid and uniformly sampled", {
  for (d in slot_registry()) expect_true(all(unlist(d$strips) %in% d$ids), info = d$id)
  expect_false(any(slot_def("purrymid")$strips[[1]] == "cat"))    # wild only on reels 2-4
  expect_identical(build_strips(slot_def("vegasstrip")), slot_def("vegasstrip")$strips)
  d <- slot_def("classicruby")
  n5 <- length(d$strips[[5]])
  w <- window_from_stops(d, c(1L, 5L, 9L, 2L, n5))
  expect_identical(w[, 5], d$strips[[5]][c(n5, 1, 2)])            # wraps around
  set.seed(1)
  s <- replicate(20000, spin_stops(d)[1])
  expect_gt(suppressWarnings(chisq.test(table(factor(s, levels = seq_along(d$strips[[1]]))))$p.value), 0.001)
  expect_false(anyDuplicated(apply(PAYLINES_5x3, 1, paste, collapse = "")) > 0)
})

test_that("cascades remove winners, drop survivors and refill from the top", {
  d <- slot_def("blackbeard")
  set.seed(2)
  g <- spin_grid(d); hl <- matrix(FALSE, 3, 5); hl[2:3, 2] <- TRUE
  cs <- cascade_step(d, g, hl)
  expect_identical(cs$grid[3, 2], g[1, 2])
  expect_true(all(cs$new[1:2, 2]))
  expect_false(any(cs$new[, -2]))
})
