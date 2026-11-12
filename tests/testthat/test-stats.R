# Player statistics: track, drawdown, tendency, category, kNN and k-means.

# A track built from a vector of wagers / wins (one play every `gap` seconds).
make_track <- function(wager, win, cat = "classic", gap = 5, start = 1e6, risky = FALSE) {
  tk <- track_new(0)
  bal <- start
  cat <- rep_len(cat, length(wager)); risky <- rep_len(risky, length(wager))
  for (i in seq_along(wager)) {
    bal <- bal - wager[i] + win[i]
    tk <- track_add(tk, i * gap, bal, wager[i], win[i], cat[i], risky[i])
  }
  tk
}

test_that("the track keeps a bounded ring buffer and lifetime aggregates", {
  n <- STAT_TRACK_CAP + 40
  tk <- make_track(rep(100, n), rep(c(0, 150), length.out = n), cat = c("classic", "poker"))
  expect_equal(tk$n, n)
  expect_length(tk$bal, STAT_TRACK_CAP)
  expect_length(tk$cat, STAT_TRACK_CAP)
  expect_equal(sum(tk$wager_cat), 100 * n)
  expect_equal(tk$plays_cat[["classic"]], n / 2)
  expect_equal(tk$active, STAT_PLAY_SECS + 5 * (n - 1))        # first play after "idle", then 5 s gaps
})

test_that("idle gaps do not count as play time", {
  tk <- track_new(0)
  tk <- track_add(tk, 100, 1e6, 100, 0, "classic")
  tk <- track_add(tk, 110, 1e6, 100, 0, "classic")
  tk <- track_add(tk, 5000, 1e6, 100, 0, "classic")              # came back after 80 minutes
  expect_equal(tk$active, STAT_PLAY_SECS + 10 + STAT_PLAY_SECS)
})

test_that("maximum drawdown is the largest peak-to-trough fall", {
  # balance: 1000 -> 1500 (peak) -> 900 (trough) -> 1400 -> 1200
  wager <- c(0, 600, 0, 200); win <- c(500, 0, 500, 0)
  tk <- make_track(wager, win, start = 1000)
  expect_equal(tk$bal, c(1500, 900, 1400, 1200))
  expect_equal(tk$peak, 1500)
  expect_equal(tk$max_dd, 600)
  expect_equal(tk$max_dd_pct, 600 / 1500)
  ds <- drawdown_series(c(10, 12, 8, 15, 9))
  expect_equal(ds$peak, c(10, 12, 12, 15, 15))
  expect_equal(ds$dd, c(0, 0, 4, 0, 6))
  expect_equal(max(ds$dd), 6)
})

test_that("the starting balance counts as a peak (losing from the first play)", {
  tk <- make_track(rep(1000, 10), rep(0, 10), start = 1e6)
  expect_equal(tk$peak, 1e6)
  expect_equal(tk$max_dd, 10000)
})

test_that("tendency detects rising / falling balances, bet trends and loss chasing", {
  up <- make_track(rep(100, 60), rep(300, 60), start = 1e5)
  down <- make_track(rep(1000, 60), rep(0, 60), start = 1e5)
  expect_equal(stats_tendency(up)$balance, "rising")
  expect_equal(stats_tendency(down)$balance, "falling")
  expect_equal(stats_tendency(down)$streak, -60L)
  expect_equal(stats_tendency(make_track(rep(100, 5), rep(0, 5)))$balance, "unknown")
  # bets grow 100 -> 400
  bets <- make_track(c(rep(100, 50), rep(400, 50)), rep(0, 100))
  expect_equal(stats_tendency(bets)$bet, "increasing")
  expect_equal(stats_tendency(bets)$bet_change, 3)
  # loss chaser: doubles the bet after every loss, back to 100 after a win
  w <- numeric(0); x <- numeric(0); b <- 100
  set.seed(9)
  for (i in 1:200) { won <- runif(1) < 0.45; w <- c(w, b); x <- c(x, if (won) 2 * b else 0); b <- if (won) 100 else min(b * 2, 1600) }
  ch <- make_track(w, x)
  expect_gt(chase_ratio(ch$wager, ch$net), 2)
  expect_equal(stats_tendency(ch)$chase_label, "chasing")
  flat <- make_track(rep(100, 200), rep(c(0, 200), 100))
  expect_equal(chase_ratio(flat$wager, flat$net), 1)
  expect_true(is.na(chase_ratio(c(100, 100), c(-100, 0))))
})

test_that("category: favourite by wagered coins, stake class and style", {
  tk <- make_track(c(rep(100, 30), rep(5000, 10)), rep(0, 40), cat = c(rep("classic", 30), rep("blackjack", 10)))
  c0 <- stats_category(tk)
  expect_equal(c0$favourite, "blackjack")                          # 50,000 vs 3,000 coins
  expect_equal(sum(c0$share), 1)
  expect_equal(c0$style, "tables")
  expect_equal(stake_class(100), "Micro stakes")
  expect_equal(stake_class(2500), "Mid stakes")
  expect_equal(stake_class(1e6), "VIP stakes")
  expect_equal(stats_category(track_new())$style, "unknown")
  expect_true(is.na(stats_category(track_new())$favourite))
})

test_that("features need enough plays and stay in range", {
  expect_null(stats_features(make_track(rep(100, STAT_MIN_PLAYS - 1), rep(0, STAT_MIN_PLAYS - 1))))
  f <- stats_features(make_track(rep(1000, 60), rep(c(0, 2000), 30), cat = c("classic", "roulette"), gap = 6, risky = c(TRUE, FALSE)))
  expect_named(f, names(STAT_FEATURES))
  expect_equal(f[["bet"]], 3)
  expect_equal(f[["pace"]], 10)                                    # one play every 6 s
  expect_equal(f[["risk"]], 0.5)
  expect_equal(f[["table"]], 0.5)
  expect_equal(f[["escalation"]], 0)
})

test_that("kNN recovers the archetype of held-out synthetic players", {
  ref <- archetype_reference()
  expect_equal(nrow(ref), 60 * nrow(ARCHETYPES))
  expect_identical(archetype_reference(), ref)                     # deterministic & cached
  # leave-one-out on a sample of the reference set
  set.seed(1)
  idx <- sample(nrow(ref), 100)
  hits <- vapply(idx, function(i) knn_classify(unlist(ref[i, names(ARCHETYPE_SD)]), ref[-i, ])$label == ref$label[i], logical(1))
  expect_gt(mean(hits), 0.9)
  # archetype centres classify as themselves, with high confidence
  for (i in seq_len(nrow(ARCHETYPES))) {
    k <- knn_classify(unlist(ARCHETYPES[i, names(ARCHETYPE_SD)]))
    expect_equal(k$label, ARCHETYPES$id[i])
    expect_gte(k$confidence, 0.75)
    expect_equal(sum(k$votes), 9)
  }
})

test_that("the reference set does not disturb the caller's random stream", {
  .stats_cache$ref <- NULL
  set.seed(42); a <- runif(3)
  set.seed(42); archetype_reference(); b <- runif(3)
  expect_equal(a, b)
})

test_that("realistic play patterns are classified sensibly", {
  set.seed(2)
  grinder <- make_track(rep(1000, 200), rbinom(200, 1, .3) * 3000, gap = 4)
  expect_equal(knn_classify(stats_features(grinder))$label, "grinder")
  shark <- make_track(rep(2500, 60), rbinom(60, 1, .48) * 5000, cat = c("blackjack", "poker", "roulette"), gap = 15)
  expect_equal(knn_classify(stats_features(shark))$label, "shark")
  roller <- make_track(rep(50000, 60), rbinom(60, 1, .3) * 150000, gap = 10)
  expect_equal(knn_classify(stats_features(roller))$label, "roller")
  casual <- make_track(rep(250, 40), rbinom(40, 1, .3) * 750, gap = 15)
  expect_equal(knn_classify(stats_features(casual))$label, "casual")
})

test_that("k-means finds the community clusters", {
  ref <- archetype_reference()
  set.seed(3)
  pick <- ref[ref$label %in% c("casual", "roller", "shark"), ]
  pick <- pick[sample(nrow(pick), 60), ]
  feats <- data.frame(player_id = sprintf("p%02d", 1:60), pick[names(ARCHETYPE_SD)])
  cl <- community_clusters(feats, k = 3)
  expect_length(cl$cluster, 60)
  expect_equal(sum(cl$size), 60)
  expect_setequal(cl$label, c("casual", "roller", "shark"))
  # clusters agree with the true groups (purity)
  purity <- sum(vapply(split(pick$label, cl$cluster), function(x) max(table(x)), 0)) / 60
  expect_gt(purity, 0.9)
  expect_null(community_clusters(feats[1:5, ]))                   # too few players
  expect_null(community_clusters(NULL))
  same <- feats[rep(1, 10), ]
  expect_null(community_clusters(same))                            # no structure to find
})

test_that("a track survives the JSON round trip of a saved profile", {
  tk <- make_track(rep(100, 30), rep(c(0, 250), 15), cat = c("myth", "roulette"), risky = TRUE)
  tk <- track_add_session(tk, 1.7e9, 600, 30, 1e6, 1e6 + 750)
  json <- jsonlite::toJSON(track_snapshot(tk), auto_unbox = TRUE, digits = NA)
  back <- track_restore(jsonlite::fromJSON(json, simplifyVector = FALSE))
  for (k in c("t", "bal", "wager", "net")) expect_equal(back[[k]], tk[[k]])
  expect_equal(back$cat, tk$cat)
  expect_equal(back$wager_cat, tk$wager_cat)
  expect_equal(back$max_dd, tk$max_dd)
  expect_equal(back$online, 600)
  expect_equal(back$sessions[[1]]$plays, 30)
  expect_equal(stats_features(back), stats_features(tk))
  # garbage in -> a fresh track
  bad <- track_restore(list(bal = list(1, 2), t = list(1)))
  expect_length(bad$bal, 0)
  expect_equal(track_restore("nope")$n, 0)
})

test_that("the balance chart is an inline SVG with the drawdown marked", {
  expect_null(stats_balance_svg(1000))
  svg <- as.character(stats_balance_svg(c(1000, 1500, 900, 1400)))
  expect_match(svg, "^<svg")
  expect_match(svg, "sc-dd", fixed = TRUE)
  expect_match(svg, "max drawdown -600", fixed = TRUE)
  expect_false(grepl("sc-ddmark", as.character(stats_balance_svg(c(1, 2, 3))), fixed = TRUE))
})

test_that("the report combines everything", {
  tk <- make_track(rep(1000, 40), rep(c(0, 1500), 20))
  r <- stats_report(tk, coins = 5e5, session = list(t0 = now_secs() - 60))
  expect_equal(r$plays, 40)
  expect_equal(r$wagered, 40000)
  expect_gte(r$online, 60)
  expect_equal(r$cur_dd, tk$peak - 5e5)
  expect_false(is.null(r$class))
  expect_equal(r$need, 0)
  expect_equal(stats_report(track_new(), 1e6)$need, STAT_MIN_PLAYS)
})
