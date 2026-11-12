test_that("coins move only through debit / credit", {
  p <- new_test_player()
  expect_equal(shiny::isolate(p$rv$coins), START_COINS)
  expect_true(p$debit(1000))
  expect_false(p$debit(1e12))
  p$credit(500)
  expect_equal(shiny::isolate(p$rv$coins), START_COINS - 500)
})

test_that("snapshot / restore round-trips through JSON", {
  p <- new_test_player()
  shiny::isolate({
    p$rv$coins <- 1234567; p$rv$album <- c(c011 = 2L, c095 = 1L); p$rv$stars <- 77; p$rv$level <- 4L
    p$rv$game_meta <- list(mermaid = list(pearls = 42, pearl_n = 3, pearl_sum = 3000))
  })
  json <- jsonlite::toJSON(shiny::isolate(p$snapshot()), auto_unbox = TRUE, null = "null", digits = NA)
  q <- new_test_player()
  expect_true(q$restore(jsonlite::fromJSON(json, simplifyVector = FALSE)))
  shiny::isolate({
    expect_equal(q$rv$coins, 1234567)
    expect_equal(q$rv$level, 4L)
    expect_identical(q$rv$album, c(c011 = 2L, c095 = 1L))
    expect_equal(q$rv$game_meta$mermaid$pearls, 42)
  })
  expect_false(q$restore(list(v = 99)))
  expect_false(q$restore("nope"))
})

test_that("record() drives XP, loyalty points, missions, Monsterpedia and the play log", {
  store <- fc_storage("memory")
  p <- new_test_player(store)
  p$set_id("player-test-0001")
  p$record("poker", "jacks", wager = 10.5e6, win = 0, bet = 1e5)
  expect_equal(shiny::isolate(p$vip())$tier, "Gold")
  expect_gt(shiny::isolate(p$rv$level), 1L)
  for (i in 1:25) p$record("slots", "stonegold", "myth", wager = 1000, win = 0, bet = 1000)
  p$record("slots", "stonegold", "myth", wager = 1000, win = 16000, bet = 1000, events = "stone_respin")
  mb <- shiny::isolate(p$monster_board())
  expect_true(mb[[1]]$challenges$captured[1])          # 25 spins
  expect_true(mb[[1]]$challenges$captured[4])          # a single 15x win
  expect_length(store$env$plays, 27)
  expect_equal(store$player_totals("player-test-0001")$plays, 27)
})

test_that("album claims, market purchases, wheel and VIP gift", {
  p <- new_test_player()
  shiny::isolate(p$rv$album <- stats::setNames(rep(2L, 81), ALBUM_CARDS$id))
  c0 <- shiny::isolate(p$rv$coins)
  expect_true(p$claim_set(1)); expect_false(p$claim_set(1))
  expect_equal(shiny::isolate(p$rv$coins) - c0, ALBUM_SETS$reward[1])
  expect_equal(p$trade_spares(), sum(DUPLICATE_STARS[ALBUM_CARDS$stars]))
  shiny::isolate(p$rv$stars <- 5000)
  for (id in MARKET_ITEMS$id) expect_true(p$buy(id), info = id)
  expect_equal(shiny::isolate(p$rv$boosts$win20), 25L)
  expect_equal(p$boost_slot_win(1000), 1200)
  shiny::isolate(p$rv$wheel_hourly_at <- 0)
  expect_false(is.null(p$spin_wheel(daily = FALSE)))
  expect_null(p$spin_wheel(daily = FALSE))              # cooldown
  shiny::isolate(p$rv$vip_points <- 15000)
  expect_true(p$claim_vip_gift()); expect_false(p$claim_vip_gift())
})

test_that("a new Lucky Card never overwrites an unplayed one", {
  p <- new_test_player()
  first <- make_lucky_cards(1)
  shiny::isolate(p$rv$lucky <- first)
  set.seed(1)
  for (i in 1:20) p$award_pack("diamond")
  expect_identical(shiny::isolate(p$rv$lucky), first)
})

test_that("every play feeds the statistics track (balance, category, risk)", {
  store <- fc_storage("memory")
  p <- new_test_player(store)
  p$set_id("player-track-0001")
  p$debit(1000); p$record("slots", "classic777", "classic", wager = 1000, win = 0)     # High volatility
  p$debit(500); p$credit(1500); p$record("blackjack", "classic", wager = 500, win = 1500)
  p$debit(2000); p$credit(4000); p$record("poker", "double_up", wager = 2000, win = 4000)
  p$record("side", "wheel", wager = 0, win = 0)
  tk <- shiny::isolate(p$rv$track)
  expect_equal(tk$n, 4)
  expect_equal(tk$bal, START_COINS + c(-1000, 0, 2000, 2000))
  expect_equal(tk$net, c(-1000, 1000, 2000, 0))
  expect_equal(names(STAT_CATEGORIES)[tk$cat], c("classic", "blackjack", "poker", "side"))
  expect_equal(tk$risky, 3000)                                     # high-volatility slot + double-up
  expect_equal(tk$max_dd, 1000)
  last <- store$env$plays[[3]]
  expect_equal(last$balance, START_COINS + 2000)
  expect_equal(last$category, "poker")
})

test_that("name, track and sessions are saved with the profile", {
  p <- new_test_player()
  p$set_name("Zoe")
  p$session_begin(now = 1000)
  for (i in 1:3) p$record("roulette", "european", wager = 100, win = 0)
  expect_true(p$session_end(now = 1600))
  json <- jsonlite::toJSON(shiny::isolate(p$snapshot()), auto_unbox = TRUE, null = "null", digits = NA)
  q <- new_test_player()
  expect_true(q$restore(jsonlite::fromJSON(json, simplifyVector = FALSE)))
  expect_equal(q$name(), "Zoe")
  tk <- shiny::isolate(q$rv$track)
  expect_equal(tk$n, 3)
  expect_equal(tk$online, 600)
  expect_equal(tk$sessions[[1]]$plays, 3)
  # an invalid name in a saved profile is dropped
  bad <- jsonlite::fromJSON(json, simplifyVector = FALSE); bad$name <- "<b>x</b>"
  expect_true(q$restore(bad))
  expect_equal(q$name(), "")
  # reset keeps the player's name, clears the statistics
  p$reset()
  expect_equal(p$name(), "Zoe")
  expect_equal(shiny::isolate(p$rv$track$n), 0)
})

test_that("short empty sessions are not logged; profiles need a name to be saved", {
  store <- fc_storage("memory")
  p <- new_test_player(store)
  p$set_id("player-empty-0001")
  p$session_begin(now = 0)
  expect_null(p$session_end(now = 10))
  expect_length(store$env$sessions, 0)
  expect_false(p$save())                                          # unnamed
  p$set_name("Yan")
  expect_true(p$save())
  expect_equal(store$load_player("player-empty-0001")$name, "Yan")
})
