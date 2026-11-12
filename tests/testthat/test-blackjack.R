test_that("hand values handle soft aces", {
  expect_identical(bj_value(c(C(1), C(13))), list(total = 21L, soft = TRUE))
  expect_equal(bj_total(c(C(1), C(1), C(9))), 21)
  expect_identical(bj_value(c(C(1), C(6), C(10))), list(total = 17L, soft = FALSE))
  expect_equal(bj_total(c(C(13), C(12), C(5))), 25)
})

test_that("a 6-deck shoe with a 75% cut card", {
  tb <- bj_new_table("classic")
  expect_length(tb$shoe, 312)
  expect_true(all(tabulate(tb$shoe, 52) == 6))
  expect_equal(tb$cut, 78)
})

test_that("classic rules: deal order, pushes, naturals, soft 17, insurance, double, split", {
  tb <- bj_deal(rig_shoe(bj_new_table("classic"), c(C(10), C(9), C(7), C(8))), 100)
  expect_identical(tb$hands[[1]]$cards, c(C(10), C(7)))
  expect_identical(tb$dealer, c(C(9), C(8)))
  expect_setequal(bj_actions(tb), c("hit", "stand", "double"))
  tb <- bj_stand(tb)
  expect_equal(tb$hands[[1]]$result, "Push")
  expect_equal(bj_round_return(tb), 100)

  tb <- bj_deal(rig_shoe(bj_new_table("classic"), c(C(1), C(9), C(13), C(7))), 100)
  expect_equal(tb$phase, "settled")
  expect_equal(bj_round_return(tb), 250)                       # 3:2

  tb <- bj_deal(rig_shoe(bj_new_table("classic"), c(C(10), C(1), C(8), C(6), C(5))), 100)
  tb <- bj_stand(bj_insurance(tb, FALSE))
  expect_equal(length(tb$dealer), 2)                           # stands on soft 17
  expect_equal(bj_total(tb$dealer), 17)

  tb <- bj_deal(rig_shoe(bj_new_table("classic"), c(C(10), C(1), C(9), C(13))), 100)
  expect_equal(tb$phase, "insurance")
  tb <- bj_insurance(tb, TRUE)
  expect_equal(c(bj_round_return(tb), bj_round_stake(tb)), c(150, 150))

  tb <- bj_double(bj_deal(rig_shoe(bj_new_table("classic"), c(C(6), C(10), C(5), C(7), C(10), C(10))), 100))
  expect_equal(tb$hands[[1]]$bet, 200)
  expect_equal(bj_round_return(tb), 400)

  tb <- bj_deal(rig_shoe(bj_new_table("classic"), c(C(1), C(10), C(1, 2), C(7), C(13), C(12))), 100)
  expect_true("split" %in% bj_actions(tb))
  tb <- bj_split(tb)
  expect_length(tb$hands, 2)
  expect_equal(vapply(tb$hands, `[[`, "", "result"), c("Win", "Win"))   # split aces are not blackjacks
  expect_equal(bj_round_return(tb), 400)
})

test_that("European: no hole card, dealer blackjack takes doubled bets, double on 9-11", {
  tb <- bj_deal(rig_shoe(bj_new_table("european"), c(C(6), C(1), C(4), C(5), C(13))), 100)
  expect_length(tb$dealer, 1)
  expect_true("double" %in% bj_actions(tb))
  tb <- bj_double(tb)
  expect_equal(c(bj_round_return(tb), bj_round_stake(tb)), c(0, 200))
  tb <- bj_deal(rig_shoe(bj_new_table("european"), c(C(10), C(9), C(8), C(7))), 100)
  expect_false("double" %in% bj_actions(tb))
})

test_that("Switch: swap top cards, blackjack pays 1:1, dealer 22 pushes", {
  tb <- bj_deal(rig_shoe(bj_new_table("switch"), c(C(10), C(5), C(9), C(6), C(1), C(7), C(6, 2))), 100)
  expect_equal(tb$phase, "switch")
  tb <- bj_switch(tb, TRUE)
  expect_equal(c(bj_total(tb$hands[[1]]$cards), bj_total(tb$hands[[2]]$cards)), c(21, 11))
  tb <- bj_stand(tb)
  expect_equal(bj_total(tb$dealer), 22)
  expect_equal(c(tb$hands[[1]]$payout, tb$hands[[2]]$payout), c(200, 100))
})

test_that("Perfect Pairs side bet", {
  expect_equal(perfect_pairs_result(C(8, 1), C(8, 1))$pays, 25)
  expect_equal(perfect_pairs_result(C(8, 2), C(8, 3))$pays, 12)
  expect_equal(perfect_pairs_result(C(8, 1), C(8, 2))$pays, 6)
  tb <- bj_deal(rig_shoe(bj_new_table("pairs"), c(C(8, 2), C(10), C(8, 3), C(9))), 100, side = 50)
  expect_equal(tb$side_result$payout, 650)
  expect_equal(bj_round_stake(tb), 150)
})

test_that("basic strategy over many rounds returns a realistic amount", {
  set.seed(99)
  net <- 0; stake <- 0
  for (v in names(BJ_VARIANTS)) {
    tb <- bj_new_table(v)
    for (i in 1:600) {
      tb <- bj_deal(tb, 100, side = if (v == "pairs") 10 else 0)
      while (tb$phase != "settled") {
        a <- bj_actions(tb)
        act <- if (tb$phase == "player") bj_hint(tb) else a[2]
        tb <- switch(act, hit = bj_hit(tb), stand = bj_stand(tb), double = bj_double(tb), split = bj_split(tb),
                     switch = bj_switch(tb, TRUE), keep = bj_switch(tb, FALSE),
                     insure = bj_insurance(tb, TRUE), no_insure = bj_insurance(tb, FALSE))
      }
      net <- net + bj_round_return(tb) - bj_round_stake(tb); stake <- stake + bj_round_stake(tb)
    }
  }
  expect_gt(1 + net / stake, 0.92)
  expect_lt(1 + net / stake, 1.05)
})
