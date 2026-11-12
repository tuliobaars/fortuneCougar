test_that("run_app() returns a Shiny app object", {
  app <- run_app()
  expect_s3_class(app, "shiny.appobj")
})

# Full page HTML including <head> (as.character() alone drops head content).
page_html <- function(ui) {
  r <- htmltools::renderTags(ui)
  paste(as.character(r$head), as.character(r$html))
}

test_that("the UI builds in English and in Brazilian Portuguese", {
  en <- page_html(fc_app_ui(list(QUERY_STRING = "")))
  pt <- page_html(fc_app_ui(list(QUERY_STRING = "?lang=pt_BR")))
  expect_match(en, "Table Games", fixed = TRUE)
  expect_match(en, "window.FC_LANG = 'en'", fixed = TRUE)
  expect_match(pt, "Jogos de Mesa", fixed = TRUE)
  expect_match(pt, "window.FC_LANG = 'pt_BR'", fixed = TRUE)
  expect_false(grepl("Table Games", pt, fixed = TRUE))
})

test_that("run_app(lang = 'pt_BR') sets the default language", {
  withr::local_options(fortuneCougar.lang = "pt_BR")
  expect_match(page_html(fc_app_ui(NULL)), "Mesa", fixed = TRUE)
})

test_that("the app server loads and saves the player through storage", {
  withr::local_options(fortuneCougar.storage = "memory")
  linked_jackpots_reset()
  on.exit(linked_jackpots_reset(), add = TRUE)
  shiny::testServer(fc_app_server, {
    expect_equal(session$userData$fc_lang, "en")
    session$setInputs(fc_hello = list(id = "player-abcdef-123", saved = ""))
    expect_true(shiny::isolate(player$rv$restored))
    expect_equal(player$id(), "player-abcdef-123")
    session$setInputs(`profile-name` = "Tester", `profile-ok` = 1)
    expect_equal(store$load_player("player-abcdef-123")$coins, START_COINS)   # saved once named
    player$credit(777)
    session$flushReact()
    expect_equal(store$load_player("player-abcdef-123")$coins, START_COINS + 777)
  })
})

test_that("an invalid player id is replaced by a fresh one", {
  withr::local_options(fortuneCougar.storage = "memory")
  linked_jackpots_reset()
  on.exit(linked_jackpots_reset(), add = TRUE)
  shiny::testServer(fc_app_server, {
    session$setInputs(fc_hello = list(id = "bad id!", saved = ""))
    expect_true(fc_valid_player_id(player$id()))
  })
})

test_that("a new player must choose a valid name before anything is saved", {
  withr::local_options(fortuneCougar.storage = "memory")
  linked_jackpots_reset()
  on.exit(linked_jackpots_reset(), add = TRUE)
  shiny::testServer(fc_app_server, {
    session$setInputs(fc_hello = list(id = "player-newcomer-01", saved = ""))
    expect_equal(shiny::isolate(player$rv$ask_name$mode), "welcome")
    expect_null(store$load_player("player-newcomer-01"))              # unnamed: not saved
    session$setInputs(`profile-name` = "x", `profile-ok` = 1)          # too short
    expect_equal(player$name(), "")
    expect_null(store$load_player("player-newcomer-01"))
    session$setInputs(`profile-name` = "  Alice   Cooper ", `profile-ok` = 2)
    expect_equal(player$name(), "Alice Cooper")
    expect_equal(store$load_player("player-newcomer-01")$name, "Alice Cooper")
    expect_equal(store$find_player_by_name("alice cooper"), "player-newcomer-01")
    expect_false(is.null(player$session()))                            # the play session started
  })
})

test_that("a returning player continues the saved profile from any browser", {
  skip_if_not_installed("RSQLite")
  path <- tempfile(fileext = ".sqlite")
  withr::local_options(fortuneCougar.storage = "sqlite", fortuneCougar.db_path = path)
  linked_jackpots_reset()
  on.exit({ linked_jackpots_reset(); fc_db_close_all(); unlink(path) }, add = TRUE)
  shiny::testServer(fc_app_server, {
    session$setInputs(fc_hello = list(id = "browser-one-000001", saved = ""))
    session$setInputs(`profile-name` = "Carla", `profile-ok` = 1)
    for (i in 1:3) { player$debit(1000); player$record("slots", "vegasstrip", "classic", wager = 1000, win = 0) }
    session$flushReact()
  })                                                                   # session ends -> logged & saved
  s1 <- fc_storage("sqlite", path)
  expect_equal(s1$load_player("browser-one-000001")$coins, START_COINS - 3000)
  expect_equal(nrow(s1$player_sessions("browser-one-000001")), 1)
  expect_equal(s1$player_sessions("browser-one-000001")$plays, 3)
  shiny::testServer(fc_app_server, {
    session$setInputs(fc_hello = list(id = "browser-two-000002", saved = ""))   # another browser
    expect_equal(shiny::isolate(player$rv$ask_name$mode), "welcome")
    session$setInputs(`profile-name` = "CARLA", `profile-ok` = 1)
    expect_equal(player$id(), "browser-one-000001")
    expect_equal(player$name(), "Carla")
    expect_equal(player$coins(), START_COINS - 3000)
    tk <- shiny::isolate(player$rv$track)
    expect_equal(tk$n, 3)
    expect_length(tk$sessions, 1)
    expect_equal(tk$wager_cat[["classic"]], 3000)
  })
  expect_null(s1$load_player("browser-two-000002"))                    # no orphan profile
})

test_that("switching player saves the current one and starts a fresh profile", {
  withr::local_options(fortuneCougar.storage = "memory")
  linked_jackpots_reset()
  on.exit(linked_jackpots_reset(), add = TRUE)
  shiny::testServer(fc_app_server, {
    session$setInputs(fc_hello = list(id = "player-switcher-1", saved = ""))
    session$setInputs(`profile-name` = "Dora", `profile-ok` = 1)
    player$credit(5000)
    session$setInputs(`stats-switch` = 1)
    expect_equal(shiny::isolate(player$rv$ask_name$mode), "switch")
    session$setInputs(`profile-name` = "Eve", `profile-ok` = 2)
    expect_equal(player$name(), "Eve")
    expect_false(identical(player$id(), "player-switcher-1"))
    expect_equal(player$coins(), START_COINS)
    expect_equal(store$load_player("player-switcher-1")$coins, START_COINS + 5000)
    # ...and back again
    player$ask_name("switch")
    session$setInputs(`profile-name` = "dora", `profile-ok` = 3)
    expect_equal(player$id(), "player-switcher-1")
    expect_equal(player$coins(), START_COINS + 5000)
  })
})
