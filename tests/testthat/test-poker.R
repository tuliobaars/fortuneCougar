H <- function(...) vapply(list(...), function(x) make_card(x[1], x[2]), integer(1))

test_that("natural hands are ranked correctly", {
  expect_equal(vp_evaluate(H(c(1, 1), c(13, 1), c(12, 1), c(11, 1), c(10, 1)), "jacks"), "royal")
  expect_equal(vp_evaluate(H(c(1, 1), c(2, 2), c(3, 1), c(4, 3), c(5, 1)), "jacks"), "straight")
  expect_equal(vp_evaluate(H(c(12, 1), c(13, 2), c(1, 1), c(2, 3), c(3, 1)), "jacks"), "")
  expect_equal(vp_evaluate(H(c(11, 1), c(11, 2), c(3, 1), c(7, 3), c(9, 1)), "jacks"), "jacks")
  expect_equal(vp_evaluate(H(c(10, 1), c(10, 2), c(3, 1), c(7, 3), c(9, 1)), "jacks"), "")
  expect_equal(vp_evaluate(H(c(1, 1), c(1, 2), c(1, 3), c(1, 4), c(9, 1)), "bonus"), "four_aces")
  expect_equal(vp_evaluate(H(c(3, 1), c(3, 2), c(3, 3), c(3, 4), c(9, 1)), "bonus"), "four_low")
  expect_equal(vp_evaluate(H(c(13, 1), c(13, 2), c(13, 3), c(13, 4), c(9, 1)), "bonus"), "four")
})

test_that("wild cards (deuces and the joker)", {
  expect_equal(vp_evaluate(H(c(2, 1), c(2, 2), c(2, 3), c(2, 4), c(9, 1)), "deuces"), "four_deuces")
  expect_equal(vp_evaluate(H(c(2, 1), c(13, 2), c(12, 2), c(11, 2), c(10, 2)), "deuces"), "wild_royal")
  expect_equal(vp_evaluate(H(c(7, 1), c(7, 2), c(7, 3), c(2, 4), c(2, 1)), "deuces"), "five")
  expect_equal(vp_evaluate(H(c(7, 1), c(7, 2), c(9, 3), c(2, 4), c(4, 1)), "deuces"), "three")
  expect_equal(vp_evaluate(H(c(13, 1), c(13, 2), c(9, 3), c(5, 4), c(4, 1)), "deuces"), "")
  expect_equal(vp_evaluate(c(H(c(13, 1), c(9, 2), c(7, 3), c(4, 4)), JOKER), "joker"), "kings")
  expect_equal(vp_evaluate(H(c(12, 1), c(12, 2), c(7, 3), c(4, 4), c(9, 1)), "joker"), "")
  expect_equal(vp_evaluate(c(H(c(8, 1), c(8, 2), c(8, 3), c(8, 4)), JOKER), "joker"), "five")
  expect_equal(vp_evaluate(c(H(c(8, 1), c(8, 2), c(5, 3), c(5, 4)), JOKER), "joker"), "fh")
  expect_equal(vp_payout("royal", "jacks", 5, 1), 4000)
  expect_equal(vp_payout("royal", "jacks", 4, 1), 1000)
})

test_that("the wild evaluator equals the best natural substitution", {
  best <- function(cards, game) {
    g <- VP_GAMES[[game]]
    wild <- if (g$wild == "deuces") card_rank(cards) == 2L else cards == JOKER
    nat <- cards[!wild]; w <- sum(wild)
    subs <- as.matrix(expand.grid(rep(list(1:52), w)))
    out <- 0
    for (i in seq_len(nrow(subs))) {
      h <- c(nat, subs[i, ])
      r <- card_rank(h); s <- card_suit(h); cnt <- sort(tabulate(r, 13), decreasing = TRUE)
      fl <- length(unique(s)) == 1; st <- straight_possible(r) && length(unique(r)) == 5
      key <- if (cnt[1] == 5) "five" else if (fl && st && all(r %in% c(1, 10:13))) "wild_royal"
        else if (fl && st) "sf" else if (cnt[1] == 4) "four" else if (cnt[1] == 3 && cnt[2] == 2) "fh"
        else if (fl) "flush" else if (st) "straight" else if (cnt[1] == 3) "three"
        else if (cnt[1] == 2 && cnt[2] == 2) "two_pair"
        else if (cnt[1] == 2 && game == "joker" && any(tabulate(r, 13)[c(1, 13)] >= 2)) "kings" else ""
      if (!(key %in% g$table$key)) key <- ""
      out <- max(out, vp_payout(key, game, 1, 1))
    }
    out
  }
  set.seed(3)
  for (i in 1:60) {
    game <- if (i %% 2) "joker" else "deuces"
    hand <- new_shoe(1, if (game == "joker") 1 else 0)[1:5]
    hand[1] <- if (game == "joker") JOKER else make_card(2, 1)
    if (anyDuplicated(hand)) next
    if (sum(if (game == "deuces") card_rank(hand) == 2 else hand == JOKER) > 2) next
    expect_equal(vp_payout(vp_evaluate(hand, game), game, 1, 1), best(hand, game), info = paste(card_label(hand), collapse = " "))
  }
})

test_that("dealt-hand frequencies match the exact combinatorics", {
  set.seed(8)
  n <- 30000
  keys <- vapply(seq_len(n), function(i) vp_evaluate(sample.int(52, 5), "jacks"), "")
  fr <- table(factor(keys, levels = c("jacks", "two_pair", "three", "straight", "flush", "fh")))
  exact <- c(337920, 123552, 54912, 10200, 5108, 3744) / 2598960
  z <- (as.numeric(fr) / n - exact) / sqrt(exact * (1 - exact) / n)
  expect_true(all(abs(z) < 4.5))
})

test_that("deal / hold / draw keeps held cards", {
  set.seed(1)
  r <- vp_new_round("jacks", 5, 10)
  held <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
  r2 <- vp_draw(r, held)
  expect_identical(r2$hand[held], r$hand[held])
  expect_equal(r2$phase, "done")
  expect_equal(r2$payout, vp_payout(r2$key, "jacks", 5, 10))
})

test_that("multi-hand rounds share the held cards and draw from independent decks", {
  set.seed(3)
  r <- vp_new_round("jacks", 5, 100, hands = 25)
  expect_equal(vp_round_cost(r), 25 * 5 * 100)
  held <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
  r2 <- vp_draw(r, held)
  cards <- r2$results$cards
  expect_equal(dim(cards), c(25L, 5L))
  expect_true(all(cards[, held] == matrix(r$hand[held], 25, 2, byrow = TRUE)))      # held everywhere
  expect_true(all(!cards[, !held] %in% r$hand))                                      # replacements never dealt cards
  expect_true(all(apply(cards, 1, function(h) !anyDuplicated(h))))                   # each hand is a valid hand
  expect_identical(cards[1, ], r2$hand)                                              # hand 1 = the main hand
  expect_gt(nrow(unique(cards)), 1)                                                  # hands differ
  expect_equal(r2$results$keys, apply(cards, 1, vp_evaluate, game = "jacks"))
  expect_equal(r2$payout, sum(vapply(r2$results$keys, vp_payout, 0, game = "jacks", coins = 5, denom = 100)))
  expect_error(vp_new_round("jacks", 5, 100, hands = 26))
  expect_error(vp_new_round("jacks", 5, 100, hands = 0))
})

test_that("multi-hand draws are unbiased: every hand sees the same odds", {
  # hold a low pair; the chance of improving to trips or better must be the
  # same for hand 1 (round deck) and the extra hands (fresh shuffles)
  set.seed(4)
  improve <- matrix(0, 400, 10)
  for (i in 1:400) {
    r <- vp_new_round("jacks", 1, 1, hands = 10)
    r$hand <- c(C(5, 1), C(5, 2), C(9, 3), C(12, 4), C(2, 1))
    r$deck <- setdiff(1:52, r$hand)[sample.int(47)]
    d <- vp_draw(r, c(TRUE, TRUE, FALSE, FALSE, FALSE))
    improve[i, ] <- d$results$keys %in% c("three", "fh", "four")
  }
  p_exact <- 0.1273   # P(a pair of 5s improves to trips / full house / quads drawing 3 of 47)
  expect_lt(abs(mean(improve[, 1]) - p_exact), 0.06)
  expect_lt(abs(mean(improve[, -1]) - p_exact), 0.025)
})

test_that("double or nothing: aces high, ties push, fair odds", {
  d <- vp_double_start(1000)
  d$dealer <- C(10, 1); d$cards <- c(C(1, 2), C(10, 3), C(4, 4), C(13, 1))
  expect_equal(vp_double_pick(d, 1)$amount, 2000)                # ace beats ten
  expect_equal(vp_double_pick(d, 1)$outcome, "win")
  expect_equal(vp_double_pick(d, 2)$outcome, "push")
  expect_equal(vp_double_pick(d, 2)$amount, 1000)
  expect_equal(vp_double_pick(d, 2)$doubles, 0L)                  # a push does not count
  expect_equal(vp_double_pick(d, 3)$amount, 0)
  expect_true(vp_can_double(vp_double_pick(d, 1)))
  expect_false(vp_can_double(vp_double_pick(d, 3)))              # nothing left to double
  expect_error(vp_double_pick(vp_double_pick(d, 1), 2))          # one pick per double
  x <- vp_double_pick(d, 1); x$doubles <- VP_MAX_DOUBLES
  expect_false(vp_can_double(x))
  set.seed(5)
  res <- vapply(1:4000, function(i) vp_double_pick(vp_double_start(1), sample(1:4, 1))$amount, 0)
  expect_lt(abs(mean(res) - 1), 0.06)                             # fair: EV = stake
  expect_lt(abs(mean(res == 1) - 3 / 51), 0.02)                   # push: same rank (3 of 51 cards)
})
