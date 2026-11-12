test_that("every machine plays 150 spins with consistent bookkeeping", {
  set.seed(3)
  for (d in slot_registry()) {
    gs <- slot_init_state(d)
    for (i in 1:150) {
      r <- slot_play(d, 1000, gs, now = i, jackpots = JACKPOT_SEEDS)
      expect_true(r$win >= 0 && r$cost %in% c(0, 1000), info = d$id)
      expect_true(length(r$frames) >= 1, info = d$id)
      gs <- r$gs
      if (!is.null(r$bonus)) {
        b <- bonus_autoplay(r$bonus)
        expect_true(b$done, info = d$id)
        gs <- bonus_settle(d, gs, b, JACKPOT_SEEDS)$gs
      }
    }
    expect_equal(gs$spins, 150L, info = d$id)
  }
})

test_that("Vile Vixens: sticky wilds stay for three spins", {
  set.seed(7)
  d <- slot_def("vixens"); gs <- slot_init_state(d); seen <- FALSE
  for (i in 1:300) {
    prev <- gs$sticky
    r <- slot_play(d, 100, gs); gs <- r$gs
    if (is.matrix(prev) && any(prev > 0)) {
      seen <- TRUE
      expect_true(all(r$frames[[1]]$grid[prev > 0] == "vixen"))
    }
  }
  expect_true(seen)
  expect_lte(max(gs$sticky), 3)
})

test_that("Cinderella: midnight converts every pumpkin and mouse", {
  set.seed(8)
  d <- slot_def("cinderella"); gs <- slot_init_state(d); n <- 0
  for (i in 1:200) {
    r <- slot_play(d, 100, gs); gs <- r$gs
    if ("midnight" %in% r$events) {
      n <- n + 1
      expect_false(any(r$frames[[2]]$grid %in% c("pumpkin", "mouse")))
    }
  }
  expect_gt(n, 5)
})

test_that("Giant's Treasure: beanstalks grow to the top of their reel", {
  set.seed(3)
  d <- slot_def("giant")
  for (i in 1:300) {
    r <- slot_play(d, 100)
    if (length(r$frames) == 2) {
      f0 <- r$frames[[1]]$grid; f1 <- r$frames[[2]]$grid
      for (c in which(colSums(f0 == "bean") > 0)) {
        low <- max(which(f0[, c] == "bean"))
        expect_true(all(f1[seq_len(low), c] == "bean"))
      }
    }
  }
})

test_that("Dark Jaguar: all mystery symbols reveal the same symbol", {
  set.seed(4)
  d <- slot_def("jaguar"); n <- 0
  for (i in 1:300) {
    r <- slot_play(d, 100)
    if ("mystery" %in% r$events) {
      n <- n + 1
      expect_length(unique(r$frames[[2]]$grid[r$frames[[1]]$grid == "mystery"]), 1)
    }
  }
  expect_gt(n, 10)
})

test_that("Stone Gold: locked stone symbols never change during respins", {
  set.seed(5)
  d <- slot_def("stonegold")
  for (i in 1:300) {
    r <- slot_play(d, 100)
    if (length(r$frames) > 1) for (k in 2:length(r$frames)) {
      h <- r$frames[[k]]$hold
      expect_identical(r$frames[[k]]$grid[h], r$frames[[k - 1]]$grid[h])
    }
  }
})

test_that("Mighty Phoenix: Hold & Spin resets to 3 respins and ends after 3 blanks", {
  set.seed(6)
  d <- slot_def("phoenix"); n <- 0
  for (i in 1:3000) {
    r <- slot_play(d, 100)
    if ("hold_spin" %in% r$events) {
      n <- n + 1
      rs <- Filter(function(f) f$mode == "respin", r$frames)
      fresh <- vapply(rs, function(f) any(f$cls == "new-held"), logical(1))
      held <- vapply(rs, function(f) sum(f$hold), 0)
      expect_true(all(diff(held) >= 0))
      expect_true(length(rs) - max(c(0, which(fresh))) == 3 || all(rs[[length(rs)]]$hold))
    }
  }
  expect_gt(n, 5)
})

test_that("free spins cost nothing and use the locked bet", {
  d <- slot_def("vegasstrip")
  gs <- start_free_spins(slot_init_state(d), 3, 500, 2)
  r <- slot_play(d, 999999, gs)
  expect_equal(r$cost, 0)
  expect_equal(r$bet, 500)
  expect_equal(r$gs$fs_left, 2L)
})

test_that("bonus games resolve correctly", {
  set.seed(9)
  b <- bonus_autoplay(make_vault_bonus(100))
  expect_true(b$done)
  expect_identical(b$jackpot, b$planned)
  expect_equal(bonus_settle(slot_def("bankjackpots"), list(), b, JACKPOT_SEEDS)$win,
               floor(JACKPOT_SEEDS[[b$jackpot]] * 100))
  expect_true(bonus_autoplay(make_queens_bonus(10))$done)
  b <- bonus_pick(make_tiger_choice(100), 3)
  gs <- bonus_settle(slot_def("tigers"), slot_init_state(slot_def("tigers")), b)$gs
  expect_equal(c(gs$fs_left, gs$fs_mult, gs$fs_bet), c(6, 5, 100))
  b <- bonus_pick(make_pearl_wheel(100), 1)
  expect_true(b$done && b$total > 0 && b$jackpot %in% MERMAID_WHEEL$label)
})
