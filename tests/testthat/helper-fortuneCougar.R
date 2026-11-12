# Shared helpers for the fortuneCougar test suite.

# A card: C(1) = Ace of spades, C(12, 2) = Queen of hearts.
C <- function(rank, suit = 1) make_card(rank, suit)

# Put known cards on top of a blackjack shoe (and disable the cut card).
rig_shoe <- function(tb, cards) {
  tb$shoe <- c(cards, tb$shoe)
  tb$cut <- 0L
  tb
}

# A player state outside of any Shiny session, with in-memory storage.
new_test_player <- function(store = fc_storage("memory")) {
  player_state(session = NULL, store = store)
}

# Naive, obviously-correct reference evaluator for payline machines.
ref_eval_lines <- function(def, grid, line_bet, mult = NULL) {
  total <- 0
  for (i in seq_len(nrow(def$lines))) {
    cells <- grid[cbind(def$lines[i, ], seq_len(def$reels))]
    mvals <- if (is.null(mult)) rep(1, def$reels) else mult[cbind(def$lines[i, ], seq_len(def$reels))]
    w <- def$wild %||% "__none__"
    nonw <- cells[cells != w]
    target <- if (length(nonw)) nonw[1] else w
    run <- 0; for (x in cells) if (x == target || x == w) run <- run + 1 else break
    wrun <- 0; for (x in cells) if (x == w) wrun <- wrun + 1 else break
    ps <- def$P[target, run]
    pw <- if (!is.null(def$wild) && wrun > 0) def$P[w, wrun] else 0
    pay <- if (pw > ps || target == w) pw else ps * prod(mvals[seq_len(run)])
    if (pay == 0 && !is.null(def$groups)) for (g in def$groups)
      if (all(cells %in% c(g$ids, w)) && any(cells %in% g$ids)) { pay <- g$pay * prod(mvals); break }
    total <- total + pay * line_bet
  }
  total
}

# Keys that must exist in the pt_BR dictionary: literals passed to tr()/trn()
# in the package sources (when the sources are available) plus every data
# value translated at display time.
i18n_literal_keys <- function(dir) {
  op <- options(keep.source = TRUE, keep.parse.data = TRUE)   # getParseData() needs both
  on.exit(options(op))
  keys <- character(0)
  for (f in list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE)) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% c("tr", "trn"), ]
    for (k in seq_len(nrow(calls))) {
      if (any(pd$token[pd$parent == calls$parent[k]] == "'$'")) next   # tags$tr()
      expr_id <- pd$parent[pd$id == calls$parent[k]]
      kids <- pd[pd$parent == expr_id, ]
      args <- kids[kids$token == "expr", ]
      args <- args[order(args$line1, args$col1), ][-1, , drop = FALSE]
      take <- if (calls$text[k] == "tr") 1 else 2:3
      for (a in take) if (a <= nrow(args)) {
        ids <- args$id[a]; desc <- ids
        repeat { nxt <- pd$id[pd$parent %in% ids]; if (!length(nxt)) break; desc <- c(desc, nxt); ids <- nxt }
        s <- pd$text[pd$id %in% desc & pd$token == "STR_CONST"]
        keys <- c(keys, vapply(s, function(x) eval(parse(text = x)), ""))
      }
    }
  }
  unique(keys)
}

i18n_data_keys <- function() {
  reg <- slot_registry()
  slots <- unlist(lapply(reg, function(d) c(d$tagline, d$description, d$features, d$volatility,
                                           vapply(d$symbols, `[[`, "", "name"))))
  unique(c(
    slots, SLOT_CATEGORIES, "ANY 7", "ANY BAR",
    "Classic & Vegas", "Mythological", "Animal 3D", "Blackjack", "Video Poker", "Roulette",
    ALBUM_SETS$name, ALBUM_CARDS$name, vapply(PACKS, `[[`, "", "name"),
    MARKET_ITEMS$name, MARKET_ITEMS$desc, VIP_TIERS$tier, REWARD_FAMILIES, MISSION_CATALOG$text,
    vapply(MONSTER_CHAPTERS, `[[`, "", "name"), MONSTER_CHALLENGES$monster, unlist(MONSTER_EVENT_TEXT),
    BJ_VARIANTS, BJ_RULES_TEXT,
    c("Bust", "Push", "Dealer Blackjack", "Blackjack!", "Push (dealer 22)", "Win - dealer busts", "Win", "Lose"),
    c("hit", "stand", "double", "split", "take insurance", "Perfect Pair", "Coloured Pair", "Mixed Pair"),
    vp_game_names(), vapply(VP_GAMES, `[[`, "", "blurb"), unlist(lapply(VP_GAMES, function(g) g$table$label)),
    RL_VARIANTS, RL_RULES_TEXT, RL_OUTSIDE_NAMES, RL_TYPE_NAMES,
    TIGER_OPTIONS$label, unname(RAPID_NAMES), "MINI", "MINOR", "MAJOR", "GRAND", MERMAID_WHEEL$label,
    c("lucky drop", "Star Market", "Lucky Cards", "Daily Dash", "Monsterpedia", "Daily Bonus Wheel"),
    c("Bonuses", "Exclusive Avatars"), FC_JS_STRINGS,
    "Double or Nothing", STAT_CATEGORIES, STAKE_CLASSES$label, ARCHETYPES$label, ARCHETYPES$desc, STAT_FEATURES))
}

# Location of the package sources, if the tests run from a source checkout.
source_r_dir <- function() {
  d <- testthat::test_path("..", "..", "R")
  if (dir.exists(d) && length(list.files(d, pattern = "^app_ui\\.R$"))) d else NULL
}
