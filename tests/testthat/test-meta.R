test_that("the Cougar Album has 9 sets of 9 cards and one Lucky Card per set", {
  expect_equal(nrow(ALBUM_CARDS), 81)
  expect_true(all(table(ALBUM_CARDS$set) == 9))
  expect_false(anyDuplicated(ALBUM_CARDS$id) > 0)
  expect_true(all(tapply(ALBUM_CARDS$lucky, ALBUM_CARDS$set, sum) == 1))
})

test_that("packs guarantee their minimum rarity", {
  set.seed(1)
  for (tier in names(PACKS)) for (i in 1:100) {
    p <- open_pack(tier)
    expect_length(p$ids, PACKS[[tier]]$cards)
    expect_gte(max(ALBUM_CARDS$stars[match(p$ids, ALBUM_CARDS$id)]), PACKS[[tier]]$min_star)
  }
  p <- open_pack("diamond", c(c011 = 1L))
  expect_false(any(p$new[p$ids == "c011"]))
})

test_that("album progress and spare values", {
  owned <- stats::setNames(rep(1L, 9), ALBUM_CARDS$id[1:9])
  expect_identical(album_progress(owned)$complete_sets, 1L)
  expect_equal(spare_value(c(c019 = 3L)), 24)
  lc <- bonus_pick(make_lucky_cards(10), 2)
  expect_setequal(lc$tiles$kind, c("coins", "stars", "pack"))
  expect_true(lc$done && all(lc$tiles$revealed))
})

test_that("levels, bet unlocks and VIP tiers", {
  expect_true(all(diff(xp_needed(1:50)) > 0))
  expect_equal(c(max_bet_index(1), max_bet_index(6), max_bet_index(15)), c(6, 8, 10))
  expect_equal(vapply(c(0, 2500, 499999, 1e9), vip_index, 0), c(1, 2, 5, 6))
})

test_that("the daily wheel pays base x multiplier x VIP bonus on average", {
  set.seed(2)
  tot <- vapply(1:10000, function(i) spin_fun_wheel(daily = TRUE, vip_bonus = 0.25)$total, 0)
  eb <- sum(WHEEL_SEGMENTS$value * WHEEL_SEGMENTS$weight) / sum(WHEEL_SEGMENTS$weight)
  em <- sum(WHEEL_MULTS$mult * WHEEL_MULTS$weight) / sum(WHEEL_MULTS$weight)
  expect_lt(abs(mean(tot) / (eb * em * 1.25) - 1), 0.1)
})

test_that("Daily Dash missions are deterministic per period and leave the RNG alone", {
  expect_identical(dash_missions("day", "2026-01-01"), dash_missions("day", "2026-01-01"))
  wk <- dash_missions("week", "2026-W01")
  expect_equal(wk$target, ceiling(MISSION_CATALOG$daily[match(wk$metric, MISSION_CATALOG$metric)] * 5))
  set.seed(5); old <- .Random.seed
  invisible(dash_missions("month", "2026-03"))
  expect_identical(.Random.seed, old)
  pk <- period_keys(as.POSIXct("2026-03-04 10:00:00"))
  expect_equal(c(pk$day, pk$month), c("2026-03-04", "2026-03"))
})

test_that("Monsterpedia chapters are tied to monster slots", {
  expect_equal(nrow(MONSTER_CHALLENGES), 20)
  expect_true(all(vapply(MONSTER_CHAPTERS, function(ch) isTRUE(slot_def(ch$game)$monster), TRUE)))
  expect_true(all(nzchar(mapply(monster_challenge_text, MONSTER_CHALLENGES$metric, MONSTER_CHALLENGES$target))))
})
