# ---------------------------------------------------------------------------
# fct_storage.R -- persistent data.
#
# Backends (see app_config.R):
#   "sqlite"  : an SQLite database (DBI + RSQLite). Player profiles, the
#               server-wide linked jackpot pots and a play log survive app
#               restarts. The browser only keeps an anonymous player id.
#   "memory"  : the same interface kept in R memory (tests, demos).
#   "browser" : no server storage; the profile is saved in localStorage.
#
# Every backend returns the same list of functions, so the rest of the app
# never talks to the database directly. Database errors are converted into
# warnings and never crash a session.
# ---------------------------------------------------------------------------

FC_SCHEMA_VERSION <- 2L

# Tables (created if missing). Version 2 added players.name/features,
# plays.balance/category and the sessions table; fc_db_migrate() upgrades
# version-1 databases in place.
FC_SCHEMA <- c(
  "CREATE TABLE IF NOT EXISTS players (
     player_id  TEXT PRIMARY KEY,
     state      TEXT NOT NULL,
     coins      REAL NOT NULL DEFAULT 0,
     level      INTEGER NOT NULL DEFAULT 1,
     avatar     TEXT,
     lang       TEXT,
     created_at TEXT NOT NULL,
     updated_at TEXT NOT NULL,
     name       TEXT,
     features   TEXT)",
  "CREATE TABLE IF NOT EXISTS jackpots (
     tier       TEXT PRIMARY KEY,
     value      REAL NOT NULL,
     updated_at TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS plays (
     id        INTEGER PRIMARY KEY AUTOINCREMENT,
     player_id TEXT NOT NULL,
     ts        TEXT NOT NULL,
     family    TEXT NOT NULL,
     game      TEXT NOT NULL,
     wager     REAL NOT NULL,
     win       REAL NOT NULL,
     balance   REAL,
     category  TEXT)",
  "CREATE TABLE IF NOT EXISTS jackpot_wins (
     id        INTEGER PRIMARY KEY AUTOINCREMENT,
     player_id TEXT NOT NULL,
     ts        TEXT NOT NULL,
     tier      TEXT NOT NULL,
     amount    REAL NOT NULL)",
  "CREATE TABLE IF NOT EXISTS sessions (
     id            INTEGER PRIMARY KEY AUTOINCREMENT,
     player_id     TEXT NOT NULL,
     started_at    TEXT NOT NULL,
     ended_at      TEXT NOT NULL,
     seconds       REAL NOT NULL,
     start_balance REAL,
     end_balance   REAL,
     plays         INTEGER NOT NULL DEFAULT 0)"
)

# Columns added after version 1: table -> c(column = type).
FC_SCHEMA_COLUMNS <- list(
  players = c(name = "TEXT", features = "TEXT"),
  plays = c(balance = "REAL", category = "TEXT")
)

# Indexes (created after the migration, as they use the new columns).
FC_SCHEMA_INDEXES <- c(
  "CREATE INDEX IF NOT EXISTS idx_plays_player ON plays (player_id, ts)",
  "CREATE UNIQUE INDEX IF NOT EXISTS idx_players_name ON players (name COLLATE NOCASE)",
  "CREATE INDEX IF NOT EXISTS idx_sessions_player ON sessions (player_id, started_at)"
)

FC_SQL <- list(
  load_player = "SELECT state FROM players WHERE player_id = ?",
  save_player = paste(
    "INSERT INTO players (player_id, state, coins, level, avatar, lang, created_at, updated_at, name, features)",
    "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    "ON CONFLICT(player_id) DO UPDATE SET state = excluded.state, coins = excluded.coins,",
    "level = excluded.level, avatar = excluded.avatar, lang = excluded.lang, updated_at = excluded.updated_at,",
    "name = excluded.name, features = COALESCE(excluded.features, players.features)"),
  find_player_by_name = "SELECT player_id FROM players WHERE name = ? COLLATE NOCASE LIMIT 1",
  log_play = "INSERT INTO plays (player_id, ts, family, game, wager, win, balance, category) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
  log_session = paste("INSERT INTO sessions (player_id, started_at, ended_at, seconds, start_balance, end_balance, plays)",
                      "VALUES (?, ?, ?, ?, ?, ?, ?)"),
  player_sessions = paste("SELECT started_at, ended_at, seconds, start_balance, end_balance, plays FROM sessions",
                          "WHERE player_id = ? ORDER BY id DESC LIMIT ?"),
  all_features = paste("SELECT player_id, name, features FROM players WHERE features IS NOT NULL",
                       "ORDER BY updated_at DESC LIMIT ?"),
  load_jackpots = "SELECT tier, value FROM jackpots",
  save_jackpot = paste(
    "INSERT INTO jackpots (tier, value, updated_at) VALUES (?, ?, ?)",
    "ON CONFLICT(tier) DO UPDATE SET value = excluded.value, updated_at = excluded.updated_at"),
  jackpot_won = "INSERT INTO jackpot_wins (player_id, ts, tier, amount) VALUES (?, ?, ?, ?)",
  leaderboard = "SELECT player_id, name, coins, level, avatar FROM players ORDER BY coins DESC LIMIT ?",
  player_totals = "SELECT COUNT(*) AS plays, COALESCE(SUM(wager), 0) AS wagered, COALESCE(SUM(win), 0) AS won FROM plays WHERE player_id = ?"
)

fc_now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
fc_iso <- function(secs) format(as.POSIXct(secs, origin = "1970-01-01"), "%Y-%m-%dT%H:%M:%S%z")

# ---- player names ------------------------------------------------------------------

# Tidy a typed name: trim and collapse inner whitespace.
fc_clean_name <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) return("")
  gsub("\\s+", " ", trimws(x))
}
# A valid player name: 2-20 letters (any alphabet), digits, spaces, dots,
# dashes or underscores, starting with a letter or digit.
fc_valid_name <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nchar(x) >= 2 && nchar(x) <= 20 &&
    grepl("^[\\p{L}\\p{N}][\\p{L}\\p{N} ._-]*$", x, perl = TRUE)
}

# ---- SQLite primitives -------------------------------------------------------------

# Open (and create / migrate) a database.
fc_db_connect <- function(path = fc_db_path()) {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE))
    stop("The 'sqlite' storage backend needs the DBI and RSQLite packages.", call. = FALSE)
  if (!identical(path, ":memory:")) dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbGetQuery(con, "PRAGMA busy_timeout = 5000")
  if (!identical(path, ":memory:")) DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL")
  fc_db_init(con)
  con
}

fc_db_init <- function(con) {
  for (sql in FC_SCHEMA) DBI::dbExecute(con, sql)
  fc_db_migrate(con)
  for (sql in FC_SCHEMA_INDEXES) DBI::dbExecute(con, sql)
  DBI::dbExecute(con, sprintf("PRAGMA user_version = %d", FC_SCHEMA_VERSION))
  invisible(con)
}

# Add the columns introduced after schema version 1 to an older database.
fc_db_migrate <- function(con) {
  for (tbl in names(FC_SCHEMA_COLUMNS)) {
    have <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", tbl))$name
    add <- FC_SCHEMA_COLUMNS[[tbl]]
    for (col in setdiff(names(add), have))
      DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", tbl, col, add[[col]]))
  }
  invisible(con)
}

fc_db_load_player <- function(con, player_id) {
  r <- DBI::dbGetQuery(con, FC_SQL$load_player, params = list(player_id))
  if (!nrow(r)) return(NULL)
  jsonlite::fromJSON(r$state[1], simplifyVector = FALSE)
}

# @param features optional named numeric behaviour features (fct_stats.R)
fc_db_save_player <- function(con, player_id, snapshot, lang = "en", features = NULL) {
  json <- as.character(jsonlite::toJSON(snapshot, auto_unbox = TRUE, null = "null", digits = NA))
  name <- fc_clean_name(snapshot$name %||% "")
  feat <- if (length(features)) as.character(jsonlite::toJSON(as.list(features), auto_unbox = TRUE, digits = 6)) else NA_character_
  now <- fc_now_iso()
  params <- function(nm) list(
    player_id, json, as.numeric(snapshot$coins %||% 0), as.integer(snapshot$level %||% 1L),
    as.character(snapshot$avatar %||% ""), as.character(lang), now, now, nm, feat)
  # A name taken by another player (race between two sessions) must not lose
  # the save: keep the profile, without the name.
  tryCatch(DBI::dbExecute(con, FC_SQL$save_player, params = params(if (nzchar(name)) name else NA_character_)),
           error = function(e) {
             if (!grepl("UNIQUE", conditionMessage(e), ignore.case = TRUE)) stop(e)
             DBI::dbExecute(con, FC_SQL$save_player, params = params(NA_character_))
           })
  invisible(TRUE)
}

fc_db_find_player_by_name <- function(con, name) {
  r <- DBI::dbGetQuery(con, FC_SQL$find_player_by_name, params = list(fc_clean_name(name)))
  if (!nrow(r)) NULL else r$player_id[1]
}

fc_db_log_play <- function(con, player_id, family, game, wager, win, balance = NA, category = NA) {
  DBI::dbExecute(con, FC_SQL$log_play, params = list(
    player_id, fc_now_iso(), family, game, as.numeric(wager), as.numeric(win),
    as.numeric(balance %||% NA), as.character(category %||% NA)))
  invisible(TRUE)
}

fc_db_log_session <- function(con, player_id, started, ended, seconds, start_balance, end_balance, plays) {
  DBI::dbExecute(con, FC_SQL$log_session, params = list(
    player_id, fc_iso(started), fc_iso(ended), as.numeric(seconds), as.numeric(start_balance),
    as.numeric(end_balance), as.integer(plays)))
  invisible(TRUE)
}

fc_db_player_sessions <- function(con, player_id, n = 10L) {
  DBI::dbGetQuery(con, FC_SQL$player_sessions, params = list(player_id, as.integer(n)))
}

# Behaviour features of the most recently active players (community clustering).
fc_db_all_features <- function(con, n = 500L) {
  r <- DBI::dbGetQuery(con, FC_SQL$all_features, params = list(as.integer(n)))
  fc_features_frame(r$player_id, r$name, lapply(r$features, function(j)
    tryCatch(unlist(jsonlite::fromJSON(j)), error = function(e) NULL)))
}

# data.frame(player_id, name, <features>) from parallel lists.
fc_features_frame <- function(ids, names, feats) {
  f <- names(ARCHETYPE_SD)
  ok <- vapply(feats, function(x) !is.null(x) && all(f %in% names(x)), logical(1))
  if (!any(ok)) return(data.frame(player_id = character(0), name = character(0)))
  m <- do.call(rbind, lapply(feats[ok], function(x) as.numeric(x[f])))
  colnames(m) <- f
  data.frame(player_id = as.character(ids[ok]), name = as.character(names[ok]), m, stringsAsFactors = FALSE)
}

fc_db_load_jackpots <- function(con) {
  r <- DBI::dbGetQuery(con, FC_SQL$load_jackpots)
  if (!nrow(r)) return(NULL)
  pots <- JACKPOT_SEEDS
  hit <- intersect(r$tier, names(pots))
  pots[hit] <- r$value[match(hit, r$tier)]
  pots
}

fc_db_save_jackpots <- function(con, pots) {
  DBI::dbExecute(con, FC_SQL$save_jackpot, params = list(
    names(pots), as.numeric(pots), rep(fc_now_iso(), length(pots))))
  invisible(TRUE)
}

fc_db_jackpot_won <- function(con, player_id, tier, amount) {
  DBI::dbExecute(con, FC_SQL$jackpot_won, params = list(player_id, fc_now_iso(), tier, as.numeric(amount)))
  invisible(TRUE)
}

fc_db_leaderboard <- function(con, n = 10L) {
  DBI::dbGetQuery(con, FC_SQL$leaderboard, params = list(as.integer(n)))
}

fc_db_player_totals <- function(con, player_id) {
  DBI::dbGetQuery(con, FC_SQL$player_totals, params = list(player_id))
}

# ---- shared connections ---------------------------------------------------------------
.fc_db <- new.env(parent = emptyenv())

# One connection per database file per R process (Shiny is single-threaded),
# re-opened transparently if it was closed.
fc_db_shared <- function(path = fc_db_path()) {
  con <- .fc_db$cons[[path]]
  if (is.null(con) || !DBI::dbIsValid(con)) {
    con <- fc_db_connect(path)
    if (is.null(.fc_db$cons)) .fc_db$cons <- list()
    .fc_db$cons[[path]] <- con
  }
  con
}

fc_db_close_all <- function() {
  for (con in .fc_db$cons) try(DBI::dbDisconnect(con), silent = TRUE)
  .fc_db$cons <- list()
  invisible(TRUE)
}

# ---- storage interface -------------------------------------------------------------------

# Run a storage operation; on failure warn and return `default`.
fc_safely <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) {
    warning("fortuneCougar storage: ", conditionMessage(e), call. = FALSE)
    default
  })
}

# Create a storage backend. Every backend exposes the same functions:
#   load_player(id), save_player(id, snapshot, lang, features),
#   find_player_by_name(name) -> player id or NULL,
#   log_play(id, family, game, wager, win, balance, category),
#   log_session(id, started, ended, seconds, start_balance, end_balance, plays),
#   player_sessions(id, n), all_features(n),
#   load_jackpots(), save_jackpots(pots), jackpot_won(id, tier, amount),
#   leaderboard(n), player_totals(id)
fc_storage <- function(backend = fc_storage_backend(), path = fc_db_path()) {
  backend <- match.arg(backend, c("sqlite", "memory", "browser"))
  if (backend == "sqlite") {
    con <- function() fc_db_shared(path)
    return(list(
      backend = "sqlite", path = path, server_side = TRUE,
      load_player = function(id) fc_safely(fc_db_load_player(con(), id)),
      save_player = function(id, snapshot, lang = "en", features = NULL)
        fc_safely(fc_db_save_player(con(), id, snapshot, lang, features), FALSE),
      find_player_by_name = function(name) fc_safely(fc_db_find_player_by_name(con(), name)),
      log_play = function(id, family, game, wager, win, balance = NA, category = NA)
        fc_safely(fc_db_log_play(con(), id, family, game, wager, win, balance, category), FALSE),
      log_session = function(id, started, ended, seconds, start_balance, end_balance, plays)
        fc_safely(fc_db_log_session(con(), id, started, ended, seconds, start_balance, end_balance, plays), FALSE),
      player_sessions = function(id, n = 10L) fc_safely(fc_db_player_sessions(con(), id, n)),
      all_features = function(n = 500L) fc_safely(fc_db_all_features(con(), n)),
      load_jackpots = function() fc_safely(fc_db_load_jackpots(con())),
      save_jackpots = function(pots) fc_safely(fc_db_save_jackpots(con(), pots), FALSE),
      jackpot_won = function(id, tier, amount) fc_safely(fc_db_jackpot_won(con(), id, tier, amount), FALSE),
      leaderboard = function(n = 10L) fc_safely(fc_db_leaderboard(con(), n)),
      player_totals = function(id) fc_safely(fc_db_player_totals(con(), id))
    ))
  }
  if (backend == "memory") {
    m <- new.env(parent = emptyenv())
    m$players <- list(); m$meta <- list(); m$plays <- list(); m$jackpots <- NULL; m$wins <- list()
    m$sessions <- list()
    return(list(
      backend = "memory", path = NULL, server_side = TRUE, env = m,
      load_player = function(id) m$players[[id]],
      save_player = function(id, snapshot, lang = "en", features = NULL) {
        name <- fc_clean_name(snapshot$name %||% "")
        if (nzchar(name)) {
          other <- Filter(function(k) !identical(k, id) && identical(tolower(m$meta[[k]]$name %||% ""), tolower(name)), names(m$meta))
          if (length(other)) name <- ""                     # taken: keep the profile, without the name
        }
        m$players[[id]] <- snapshot
        m$meta[[id]] <- list(coins = snapshot$coins %||% 0, level = snapshot$level %||% 1L,
                             avatar = snapshot$avatar %||% "", lang = lang, name = name,
                             features = if (length(features)) features else m$meta[[id]]$features)
        TRUE
      },
      find_player_by_name = function(name) {
        name <- tolower(fc_clean_name(name))
        hit <- Filter(function(k) identical(tolower(m$meta[[k]]$name %||% ""), name), names(m$meta))
        if (nzchar(name) && length(hit)) hit[1] else NULL
      },
      log_play = function(id, family, game, wager, win, balance = NA, category = NA) {
        m$plays[[length(m$plays) + 1]] <- list(player_id = id, family = family, game = game, wager = wager, win = win,
                                               balance = balance, category = category)
        TRUE
      },
      log_session = function(id, started, ended, seconds, start_balance, end_balance, plays) {
        m$sessions[[length(m$sessions) + 1]] <- list(player_id = id, started_at = fc_iso(started), ended_at = fc_iso(ended),
                                                     seconds = seconds, start_balance = start_balance,
                                                     end_balance = end_balance, plays = plays)
        TRUE
      },
      player_sessions = function(id, n = 10L) {
        s <- rev(Filter(function(x) identical(x$player_id, id), m$sessions))
        if (!length(s)) return(data.frame(started_at = character(0), seconds = numeric(0)))
        utils::head(do.call(rbind, lapply(s, function(x) as.data.frame(x[-1], stringsAsFactors = FALSE))), n)
      },
      all_features = function(n = 500L) {
        ids <- utils::head(names(m$meta), n)
        fc_features_frame(ids, vapply(ids, function(k) m$meta[[k]]$name %||% "", ""),
                          lapply(ids, function(k) m$meta[[k]]$features))
      },
      load_jackpots = function() m$jackpots,
      save_jackpots = function(pots) { m$jackpots <- pots; TRUE },
      jackpot_won = function(id, tier, amount) { m$wins[[length(m$wins) + 1]] <- list(id, tier, amount); TRUE },
      leaderboard = function(n = 10L) {
        if (!length(m$meta)) return(data.frame(player_id = character(0), name = character(0), coins = numeric(0),
                                               level = integer(0), avatar = character(0)))
        d <- data.frame(player_id = names(m$meta),
                        name = vapply(m$meta, function(x) { v <- x$name %||% ""; if (nzchar(v)) v else NA_character_ }, ""),
                        coins = vapply(m$meta, function(x) as.numeric(x$coins), 0),
                        level = vapply(m$meta, function(x) as.integer(x$level), 0L),
                        avatar = vapply(m$meta, function(x) as.character(x$avatar), ""), stringsAsFactors = FALSE)
        utils::head(d[order(-d$coins), , drop = FALSE], n)
      },
      player_totals = function(id) {
        p <- Filter(function(x) identical(x$player_id, id), m$plays)
        data.frame(plays = length(p), wagered = sum(vapply(p, `[[`, 0, "wager")), won = sum(vapply(p, `[[`, 0, "win")))
      }
    ))
  }
  nothing <- function(...) NULL
  list(backend = "browser", path = NULL, server_side = FALSE,
       load_player = nothing, save_player = nothing, find_player_by_name = nothing, log_play = nothing,
       log_session = nothing, player_sessions = nothing, all_features = nothing, load_jackpots = nothing,
       save_jackpots = nothing, jackpot_won = nothing, leaderboard = nothing, player_totals = nothing)
}

# A valid player id: 8-64 letters, digits or dashes (generated by the browser).
fc_valid_player_id <- function(id) {
  is.character(id) && length(id) == 1 && !is.na(id) && grepl("^[A-Za-z0-9-]{8,64}$", id)
}

fc_new_player_id <- function() {
  paste(sample(c(0:9, letters[1:6]), 32, replace = TRUE), collapse = "")
}
