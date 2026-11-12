test_that("wheels and colours", {
  expect_length(RL_ORDER_EU, 37)
  expect_setequal(RL_ORDER_EU, 0:36)
  expect_length(RL_ORDER_US, 38)
  expect_setequal(RL_ORDER_US, 0:37)
  expect_length(RL_RED, 18)
})

test_that("payout ratios and valid bets", {
  expect_equal(vapply(c("straight:17", "split:17-20", "street:1-2-3", "corner:1-2-4-5",
                        "sixline:1-2-3-4-5-6", "dozen2", "red"), rl_ratio, 0),
               c(35, 17, 11, 8, 5, 2, 1), ignore_attr = TRUE)
  expect_equal(rl_ratio("basket:0-1-2-3-37"), 6)
  expect_false(rl_valid("basket:0-1-2-3-37", "european"))
  expect_false(rl_valid("straight:37", "european"))
  expect_true(rl_valid("straight:37", "american"))
})

test_that("settlement, La Partage and call bets", {
  expect_equal(rl_settle(c(red = 100, `straight:0` = 10), 0, "french")$total, 410)
  expect_equal(rl_settle(c(red = 100), 0, "european")$total, 0)
  expect_equal(rl_settle(c(red = 100, odd = 100, `split:17-20` = 10), 17, "american")$total, 380)
  vo <- RL_CALL_BETS$voisins$chips
  expect_length(unique(unlist(lapply(names(vo), rl_numbers))), 17)
  expect_equal(sum(vo), 9)
})

test_that("exact house edge of every bet type", {
  ev <- function(key, variant) mean(vapply(rl_wheel(variant), function(r)
    rl_settle(stats::setNames(1, key), r, variant)$total, 0))
  for (k in c("straight:7", "split:7-8", "street:7-8-9", "corner:7-8-10-11", "sixline:7-8-9-10-11-12", "dozen1", "col2", "red"))
    expect_equal(ev(k, "european"), 36 / 37, info = k)
  expect_equal(ev("red", "american"), 36 / 38)
  expect_equal(ev("basket:0-1-2-3-37", "american"), 35 / 38)
  expect_equal(ev("even", "french"), 1 - 1 / 74)
})

test_that("spins are uniform over the wheel", {
  set.seed(1)
  sp <- vapply(1:37000, function(i) rl_spin("european"), 0)
  expect_gt(suppressWarnings(chisq.test(table(factor(sp, levels = 0:36)))$p.value), 0.001)
})
