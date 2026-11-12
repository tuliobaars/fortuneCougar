contract <- function(store) {
  id <- "abcdef0123456789"
  expect_null(store$load_player(id))
  snap <- list(v = 1L, coins = 4242, level = 3L, avatar = "\U0001F981", album = list(c011 = 2))
  expect_true(isTRUE(store$save_player(id, snap, "pt_BR")))
  got <- store$load_player(id)
  expect_equal(got$coins, 4242)
  expect_equal(got$album$c011, 2)
  snap$coins <- 5000
  store$save_player(id, snap, "pt_BR")                   # upsert
  expect_equal(store$load_player(id)$coins, 5000)
  store$save_player("zzzz00001111", list(v = 1L, coins = 9e6, level = 9L, avatar = ""), "en")
  lb <- store$leaderboard(10L)
  expect_equal(lb$player_id[1], "zzzz00001111")
  expect_equal(nrow(lb), 2)
  store$log_play(id, "slots", "vegasstrip", 1000, 2500)
  store$log_play(id, "blackjack", "classic", 500, 0)
  tot <- store$player_totals(id)
  expect_equal(c(tot$plays, tot$wagered, tot$won), c(2, 1500, 2500))
  expect_null(store$load_jackpots())
  pots <- JACKPOT_SEEDS + c(1, 2, 3, 4)
  store$save_jackpots(pots)
  expect_equal(store$load_jackpots(), pots)
  expect_true(isTRUE(store$jackpot_won(id, "mini", 20000)))

  # names: unique, case-insensitive lookup; a taken name never breaks a save
  expect_null(store$find_player_by_name("Lucky Cat"))
  snap$name <- "Lucky Cat"
  expect_true(isTRUE(store$save_player(id, snap, "en", features = c(bet = 3, pace = 5, risk = .2, table = .1, escalation = 0))))
  expect_equal(store$find_player_by_name("lucky cat"), id)
  expect_equal(store$find_player_by_name("  LUCKY   CAT "), id)
  thief <- list(v = 1L, coins = 1, level = 1L, avatar = "", name = "LUCKY CAT")
  expect_true(isTRUE(store$save_player("thief0000000001", thief, "en")))
  expect_equal(store$find_player_by_name("Lucky Cat"), id)          # still the first owner
  expect_equal(store$load_player("thief0000000001")$coins, 1)        # ...but the profile was saved
  lb <- store$leaderboard(10L)
  expect_equal(lb$name[lb$player_id == id], "Lucky Cat")
  # features for the community clustering (kept when a save has none)
  store$save_player(id, snap, "en")
  f <- store$all_features(10L)
  expect_equal(f$player_id, id)
  expect_equal(f$pace, 5)
  # play log with balance / category, sessions
  expect_true(isTRUE(store$log_play(id, "poker", "jacks", 500, 0, balance = 9999, category = "poker")))
  expect_equal(store$player_totals(id)$plays, 3)
  expect_true(isTRUE(store$log_session(id, 1.7e9, 1.7e9 + 600, 600, 1e6, 1.1e6, 42)))
  ss <- store$player_sessions(id, 5L)
  expect_equal(nrow(ss), 1)
  expect_equal(ss$plays, 42)
  expect_equal(ss$end_balance, 1.1e6)
}

test_that("the memory backend honours the storage contract", {
  contract(fc_storage("memory"))
})

test_that("the SQLite backend honours the storage contract and persists to disk", {
  skip_if_not_installed("RSQLite")
  path <- tempfile(fileext = ".sqlite")
  on.exit({ fc_db_close_all(); unlink(path) }, add = TRUE)
  store <- fc_storage("sqlite", path)
  contract(store)
  fc_db_close_all()                                       # "restart the app"
  again <- fc_storage("sqlite", path)
  expect_equal(again$load_player("abcdef0123456789")$coins, 5000)
  expect_equal(again$load_jackpots(), JACKPOT_SEEDS + c(1, 2, 3, 4))
  con <- fc_db_shared(path)
  expect_equal(DBI::dbGetQuery(con, "PRAGMA user_version")[[1]], FC_SCHEMA_VERSION)
  expect_setequal(DBI::dbListTables(con), c("players", "jackpots", "plays", "jackpot_wins", "sessions", "sqlite_sequence"))
})

test_that("a version-1 database is migrated in place", {
  skip_if_not_installed("RSQLite")
  path <- tempfile(fileext = ".sqlite")
  on.exit({ fc_db_close_all(); unlink(path) }, add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE players (player_id TEXT PRIMARY KEY, state TEXT NOT NULL, coins REAL NOT NULL DEFAULT 0,
                       level INTEGER NOT NULL DEFAULT 1, avatar TEXT, lang TEXT, created_at TEXT NOT NULL, updated_at TEXT NOT NULL)")
  DBI::dbExecute(con, "CREATE TABLE plays (id INTEGER PRIMARY KEY AUTOINCREMENT, player_id TEXT NOT NULL, ts TEXT NOT NULL,
                       family TEXT NOT NULL, game TEXT NOT NULL, wager REAL NOT NULL, win REAL NOT NULL)")
  DBI::dbExecute(con, "INSERT INTO players VALUES ('oldplayer0001', '{\"v\":1,\"coins\":777}', 777, 2, '', 'en', 'x', 'x')")
  DBI::dbExecute(con, "INSERT INTO plays (player_id, ts, family, game, wager, win) VALUES ('oldplayer0001', 'x', 'slots', 'giant', 100, 0)")
  DBI::dbExecute(con, "PRAGMA user_version = 1")
  DBI::dbDisconnect(con)

  store <- fc_storage("sqlite", path)
  expect_equal(store$load_player("oldplayer0001")$coins, 777)        # data kept
  con <- fc_db_shared(path)
  expect_equal(DBI::dbGetQuery(con, "PRAGMA user_version")[[1]], FC_SCHEMA_VERSION)
  expect_true(all(c("name", "features") %in% DBI::dbGetQuery(con, "PRAGMA table_info(players)")$name))
  expect_true(all(c("balance", "category") %in% DBI::dbGetQuery(con, "PRAGMA table_info(plays)")$name))
  expect_true("sessions" %in% DBI::dbListTables(con))
  expect_equal(store$player_totals("oldplayer0001")$plays, 1)
  expect_true(isTRUE(store$save_player("oldplayer0001", list(v = 1L, coins = 800, level = 2L, name = "Old Timer"), "en")))
  expect_equal(store$find_player_by_name("old timer"), "oldplayer0001")
  fc_db_close_all()
  expect_equal(fc_storage("sqlite", path)$find_player_by_name("Old Timer"), "oldplayer0001")   # idempotent re-open
})

test_that("storage errors become warnings, not crashes", {
  skip_if_not_installed("RSQLite")
  not_a_dir <- tempfile(); writeLines("x", not_a_dir)      # a file where a folder should be
  on.exit(unlink(not_a_dir), add = TRUE)
  store <- fc_storage("sqlite", file.path(not_a_dir, "db", "fortuneCougar.sqlite"))
  expect_warning(res <- store$load_player("abcdef0123456789"), "storage")
  expect_null(res)
})

test_that("the browser backend stores nothing server side", {
  b <- fc_storage("browser")
  expect_false(b$server_side)
  expect_null(b$load_player("abcdef0123456789"))
})

test_that("player names are cleaned and validated", {
  expect_equal(fc_clean_name("  Lucky   Cougar "), "Lucky Cougar")
  expect_equal(fc_clean_name(NULL), "")
  expect_true(fc_valid_name("Jo"))
  expect_true(fc_valid_name("Jos\u00e9 Ma\u00f1a_2.0"))
  expect_false(fc_valid_name("J"))
  expect_false(fc_valid_name(strrep("a", 21)))
  expect_false(fc_valid_name("_underscore"))
  expect_false(fc_valid_name("<script>"))
  expect_false(fc_valid_name("robert'); DROP"))
  expect_false(fc_valid_name(NA_character_))
})

test_that("player ids are validated", {
  expect_true(fc_valid_player_id("3f2a9c1e-7b1d-4e0a-9f00-1234567890ab"))
  expect_false(fc_valid_player_id("x"))
  expect_false(fc_valid_player_id("robert'); DROP TABLE players;--"))
  expect_true(fc_valid_player_id(fc_new_player_id()))
})

test_that("linked jackpot pots are restored from storage", {
  linked_jackpots_reset()
  on.exit(linked_jackpots_reset(), add = TRUE)
  store <- fc_storage("memory")
  store$save_jackpots(JACKPOT_SEEDS * 2)
  pool <- linked_jackpots(store)
  expect_equal(shiny::isolate(pool$pots), JACKPOT_SEEDS * 2)
  linked_jackpots_save(JACKPOT_SEEDS * 3)
  expect_equal(store$load_jackpots(), JACKPOT_SEEDS * 3)
})
