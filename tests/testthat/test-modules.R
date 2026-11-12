# Module servers, exercised with shiny::testServer().

test_that("slot machine: a spin debits the bet, credits the win and records the play", {
  player <- new_test_player()
  shiny::testServer(slot_machine_server, args = list(def = slot_def("vegasstrip"), player = player), {
    c0 <- shiny::isolate(player$rv$coins)
    session$setInputs(spin = 1)
    expect_false(is.null(st$last))
    expect_equal(shiny::isolate(player$rv$coins), c0 - st$last$bet + st$last$win)
    expect_equal(shiny::isolate(player$rv$stats$spins), 1)
    expect_match(as.character(output$status$html), "Won|No win")
    expect_equal(output$bet_txt, "1,000")
    session$setInputs(bet_up = 1)
    expect_equal(output$bet_txt, "2,500")
  })
})

test_that("slot machine: bet is locked above the level limit", {
  player <- new_test_player()
  shiny::testServer(slot_machine_server, args = list(def = slot_def("classic777"), player = player), {
    for (i in 1:12) session$setInputs(bet_up = i)
    expect_equal(BET_LADDER[st$bet_i], BET_LADDER[max_bet_index(1L)])
  })
})

test_that("slot machine: a forced bonus blocks spins until it is collected", {
  player <- new_test_player()
  def <- slot_def("cityqueens")
  play <- def$play
  def$play <- function(ctx) { for (k in 1:20000) { r <- play(ctx); if (!is.null(r$bonus)) return(r) }; r }
  shiny::testServer(slot_machine_server, args = list(def = def, player = player), {
    session$setInputs(spin = 1)
    expect_false(is.null(st$bonus))
    n <- shiny::isolate(player$rv$stats$spins)
    session$setInputs(spin = 2)                                    # refused
    expect_equal(shiny::isolate(player$rv$stats$spins), n)
    expect_match(as.character(output$bonus$html), "bonus-panel")
    i <- 0
    while (!isTRUE(st$bonus$done) && i < 12) {
      i <- i + 1
      if (!st$bonus$tiles$revealed[i]) session$setInputs(pick = i)
    }
    expect_true(st$bonus$done)
    session$setInputs(bonus_close = 1)
    expect_null(st$bonus)
  })
})

test_that("slot machine: texts follow the session language", {
  player <- new_test_player()
  shiny::testServer(slot_machine_server, args = list(def = slot_def("giant"), player = player), {
    session$userData$fc_lang <- "pt_BR"
    expect_match(as.character(output$status$html), "Boa sorte")
    expect_equal(output$bet_txt, "1.000")
  })
})

test_that("blackjack: a full round settles the exact stake and return", {
  set.seed(11)
  player <- new_test_player()
  shiny::testServer(blackjack_server, args = list(player = player), {
    session$setInputs(chip = 500)
    c0 <- shiny::isolate(player$rv$coins)
    session$setInputs(deal = 1)
    k <- 0
    while (st$tb$phase != "settled" && k < 20) {
      k <- k + 1
      a <- bj_actions(st$tb)
      session$setInputs(act = list(a = if ("stand" %in% a) "stand" else a[2], n = k))
    }
    expect_equal(st$tb$phase, "settled")
    expect_equal(shiny::isolate(player$rv$coins) - c0, bj_round_return(st$tb) - bj_round_stake(st$tb))
    expect_match(as.character(output$actions$html), "DEAL AGAIN")
  })
})

test_that("video poker: deal, hold and draw", {
  set.seed(12)
  player <- new_test_player()
  shiny::testServer(video_poker_server, args = list(player = player), {
    c0 <- shiny::isolate(player$rv$coins)
    session$setInputs(deal_draw = 1)
    expect_equal(st$round$phase, "dealt")
    first <- st$round$hand
    session$setInputs(hold = list(i = 2, n = 1))
    expect_true(st$held[2])
    session$setInputs(deal_draw = 2)
    expect_equal(st$round$phase, "done")
    expect_identical(st$round$hand[2], first[2])
    expect_equal(shiny::isolate(player$rv$coins) - c0, st$round$payout - 5 * 100)
  })
})

test_that("video poker: 10 hands at once, then double or nothing", {
  set.seed(14)
  player <- new_test_player()
  shiny::testServer(video_poker_server, args = list(player = player), {
    session$setInputs(hands = 10)
    expect_equal(st$hands, 10L)
    expect_match(as.character(output$bet_txt$html), "5,000", fixed = TRUE)
    won <- FALSE
    for (k in 1:40) {
      c0 <- shiny::isolate(player$rv$coins)
      session$setInputs(deal_draw = 2 * k - 1)
      session$setInputs(suggest = k)
      session$setInputs(deal_draw = 2 * k)
      expect_equal(shiny::isolate(player$rv$coins) - c0, st$round$payout - 10 * 5 * 100)
      if (st$round$payout > 0) { won <- TRUE; break }
    }
    expect_true(won)
    expect_match(as.character(output$hands_grid$html), "vp-mini-hand")
    expect_match(as.character(output$message$html), "hands won")
    expect_match(as.character(output$double_bar$html), "DOUBLE OR NOTHING")
    bank <- st$bank
    session$setInputs(double = 1)
    expect_equal(st$dbl$phase, "pick")
    expect_match(as.character(output$cards$html), "PICK A HIGHER CARD")
    c0 <- shiny::isolate(player$rv$coins)
    session$setInputs(dpick = list(i = 3, n = 1))
    d <- st$dbl
    expect_equal(shiny::isolate(player$rv$coins) - c0, d$amount - bank)
    expect_equal(st$bank, d$amount)
    session$setInputs(take = 1)
    expect_null(st$dbl)
    session$setInputs(hands = 7)                                       # not an option
    expect_equal(st$hands, 10L)
  })
})

test_that("my stats: every panel renders for a player with history", {
  set.seed(15)
  player <- new_test_player()
  player$set_name("Statler")
  player$session_begin()
  for (i in 1:40) {
    w <- sample(c(1000, 2500), 1); win <- if (runif(1) < .3) 3 * w else 0
    player$debit(w); player$credit(win)
    player$record(if (i %% 2) "slots" else "roulette", if (i %% 2) "classic777" else "european",
                  if (i %% 2) "classic" else NULL, wager = w, win = win)
  }
  shiny::testServer(stats_server, args = list(player = player), {
    expect_match(as.character(output$header$html), "Statler")
    expect_match(as.character(output$kpis$html), "Max drawdown")
    expect_match(as.character(output$chart$html), "<svg")
    expect_match(as.character(output$tendency$html), "tend-hero")
    expect_match(as.character(output$category$html), "Classic &amp; Vegas slots|Roulette")
    expect_match(as.character(output$classify$html), "k-nearest neighbours")
    expect_match(as.character(output$community$html), "k-means")
    expect_match(as.character(output$sessions$html), "now")
  })
})

test_that("roulette: bets, spin and settlement", {
  set.seed(13)
  player <- new_test_player()
  shiny::testServer(roulette_server, args = list(player = player), {
    session$setInputs(chip = 1000)
    session$setInputs(place = list(key = "red", n = 1))
    session$setInputs(place = list(key = "straight:17", n = 2))
    expect_equal(sum(st$bets), 2000)
    c0 <- shiny::isolate(player$rv$coins)
    bets <- st$bets
    session$setInputs(spin = 1)
    res <- st$history[1]
    expect_equal(shiny::isolate(player$rv$coins) - c0, rl_settle(bets, res, "european")$total - 2000)
    session$setInputs(clear = 1)
    expect_length(st$bets, 0)
    session$setInputs(rebet = 1)
    expect_equal(sum(st$bets), 2000)
  })
})

test_that("meta: Daily Dash claims pay once", {
  player <- new_test_player()
  b <- shiny::isolate(player$dash_board())
  prog <- list()
  for (x in b) for (i in seq_len(nrow(x$missions))) prog[[x$missions$id[i]]] <- x$missions$target[i]
  shiny::isolate(player$rv$dash <- prog)
  shiny::testServer(dash_server, args = list(player = player), {
    c0 <- shiny::isolate(player$rv$coins)
    id <- b[[1]]$missions$id[1]
    session$setInputs(claim = id)
    expect_equal(shiny::isolate(player$rv$coins) - c0, DASH_REWARD$day$mission)
    expect_match(as.character(output$board$html), "mission")
  })
})
