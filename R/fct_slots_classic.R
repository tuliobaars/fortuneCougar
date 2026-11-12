# ---------------------------------------------------------------------------
# games_classic.R -- Classic & Vegas slot definitions.
# Each machine is a `make_slot()` definition plus a `play(ctx)` function that
# composes the engine helpers into the game's signature mechanic.
# `scale` values were calibrated with tools/simulate_rtp.R (see `rtp`).
# ---------------------------------------------------------------------------

royal <- function(id, pays, weight, reels = NULL) {
  sym(id, id, id, pays = pays, weight = weight, cls = "royal", reels = reels)
}
p3 <- function(a, b = NULL, c = NULL) {
  v <- c(`3` = a)
  if (!is.null(b)) v <- c(v, `4` = b)
  if (!is.null(c)) v <- c(v, `5` = c)
  v
}

BAR_HTML <- function(n) paste(rep("<b class='bar'>BAR</b>", n), collapse = "")

# ---- 1. Classic 777 Cash ------------------------------------------------------
# 3-reel stepper, 5 lines, blanks for high volatility, multiplier cash wilds
# (x2 / x3 / x5, multiplied together) and ANY-7 / ANY-BAR mixed pays.
game_classic777 <- function() {
  make_slot(
    id = "classic777", name = "Classic 777 Cash", category = "classic",
    tagline = "Old-school 3-reel stepper with multiplier Cash Wilds",
    description = paste(
      "A traditional 3-reel stepper with blanks and 5 paylines. Every Cash Wild lands",
      "with a random x2, x3 or x5 multiplier - two wilds on a line multiply together",
      "(up to x25). Mixed sevens pay ANY 7 and mixed bars pay ANY BAR."),
    reels = 3, rows = 3, volatility = "High", theme = "theme-classic",
    art = "7\uFE0F\u20E3",
    symbols = list(
      sym("cash", "\U0001F4B0", "Cash Wild", pays = c(`3` = 300), weight = 3, cls = "wild"),
      sym("r7", "7", "Red 7", pays = c(`3` = 100), weight = 4, cls = "seven seven-red"),
      sym("b7", "7", "Blue 7", pays = c(`3` = 50), weight = 5, cls = "seven seven-blue"),
      sym("bar3", BAR_HTML(3), "Triple BAR", pays = c(`3` = 30), weight = 5, cls = "bars"),
      sym("bar2", BAR_HTML(2), "Double BAR", pays = c(`3` = 20), weight = 6, cls = "bars"),
      sym("bar1", BAR_HTML(1), "BAR", pays = c(`3` = 10), weight = 7, cls = "bars"),
      sym("bell", "\U0001F514", "Bell", pays = c(`3` = 12), weight = 6),
      sym("cherry", "\U0001F352", "Cherry", pays = c(`3` = 8), weight = 7),
      sym("lemon", "\U0001F34B", "Lemon", pays = c(`3` = 5), weight = 8),
      sym("blank", "", "Blank", weight = 12, cls = "blank")
    ),
    wild = "cash",
    groups = list(list(ids = c("r7", "b7"), pay = 25, name = "ANY 7"),
                  list(ids = c("bar1", "bar2", "bar3"), pay = 5, name = "ANY BAR")),
    features = c("Multiplier Wilds x2/x3/x5", "ANY 7 / ANY BAR", "High volatility"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      mult <- matrix(1, def$rows, def$reels)
      badge <- matrix("", def$rows, def$reels)
      w <- grid == "cash"
      if (any(w)) {
        m <- wdraw(c(2, 3, 5), c(0.6, 0.3, 0.1), sum(w))
        mult[w] <- m
        badge[w] <- paste0("x", m)
      }
      ev <- eval_grid(def, grid, ctx$bet, mult = mult, fs_mult = ctx$fs_mult)
      slot_result(ctx, ev$win, list(frame(def, grid, ev$hl, badge = badge, win = ev$win)),
                  parts = ev$parts)
    }
  )
}

# ---- 2. Bank of Jackpots ---------------------------------------------------------
# 5x3, 25 lines. Vault symbols (3+) open the Vault: match 3 to win one of four
# LINKED progressive jackpots shared by every player connected to the server.
JACKPOT_SEEDS <- c(mini = 20, minor = 50, major = 250, grand = 2000)   # x bet
JACKPOT_RATE  <- c(mini = 0.004, minor = 0.002, major = 0.0008, grand = 0.0004) # x bet / spin
JACKPOT_ODDS  <- c(mini = 0.62, minor = 0.27, major = 0.09, grand = 0.02)

make_vault_bonus <- function(bet) {
  tier <- wdraw(names(JACKPOT_ODDS), JACKPOT_ODDS)
  others <- setdiff(names(JACKPOT_ODDS), tier)
  # 3 of the winning tier + at most 2 of each other tier => only `tier` can hit 3
  labels <- shuffle(c(rep(tier, 3), rep(others, each = 2)))
  new_bonus("match3", tr("The Vault"), tr("Pick coins - match 3 to win a LINKED jackpot!"),
            bonus_tiles("jackpot", 0, labels), face = "\U0001FA99", bet = bet,
            planned = tier)
}

game_bankjackpots <- function() {
  make_slot(
    id = "bankjackpots", name = "Bank of Jackpots", category = "classic",
    tagline = "High-roller vault with a 4-tier LINKED progressive",
    description = paste(
      "5 reels, 25 lines of pure Vegas high-roller action. Three or more Vault",
      "symbols anywhere crack the safe: pick coins until you match three to win the",
      "Mini, Minor, Major or GRAND. The four progressive pots are linked - every bet",
      "from every player connected to this casino grows them."),
    volatility = "High", theme = "theme-bank", art = "\U0001F3E6",
    symbols = list(
      sym("bwild", "\U0001F3E6", "Bank Wild", pays = p3(50, 200, 1000), weight = 3, cls = "wild", reels = 2:5),
      sym("diamond", "\U0001F48E", "Diamond", pays = p3(40, 150, 750), weight = 4),
      sym("bag", "\U0001F4B0", "Money Bag", pays = p3(30, 100, 400), weight = 5),
      sym("cash", "\U0001F4B5", "Cash Stack", pays = p3(20, 75, 250), weight = 6),
      sym("key", "\U0001F511", "Golden Key", pays = p3(15, 50, 200), weight = 7),
      royal("A", p3(10, 30, 100), 9), royal("K", p3(8, 25, 80), 10),
      royal("Q", p3(5, 20, 60), 11), royal("J", p3(5, 15, 50), 12),
      sym("vault", "\U0001F510", "Vault", weight = c(2, 1, 2, 1, 2), cls = "scatter")
    ),
    wild = "bwild", scatter = "vault", scatter_pays = c(0, 0, 2, 5, 20),
    features = c("4-tier linked progressive", "Vault match-3 bonus"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      bonus <- if (ev$scatters >= 3) make_vault_bonus(ctx$bet) else NULL
      slot_result(ctx, ev$win, list(frame(def, grid, ev$hl, win = ev$win)),
                  bonus = bonus, parts = ev$parts,
                  events = if (!is.null(bonus)) "bonus" else character(0),
                  jp_contrib = if (ctx$free) NULL else JACKPOT_RATE)
    },
    meter = function(gs, bet, jackpots = NULL) {
      jp <- jackpots %||% JACKPOT_SEEDS
      lapply(names(JACKPOT_SEEDS), function(t)
        list(label = tr(toupper(t)), text = fmt_coins(jp[[t]] * bet), kind = paste0("jp-", t)))
    }
  )
}

# ---- 3. Vegas Strip --------------------------------------------------------------
# Neon scatter pays (2+ anywhere), 3+ scatters = 10 free spins at x2, retriggerable.
game_vegasstrip <- function() {
  make_slot(
    id = "vegasstrip", name = "Vegas Strip", category = "classic",
    tagline = "Neon scatter pays & retriggering free spins",
    description = paste(
      "Neon Stars pay anywhere from just two on screen (up to 100x bet for five).",
      "Three or more award 10 FREE SPINS where every win is doubled - and Neon Stars",
      "keep paying and retrigger another 10 spins during the feature."),
    volatility = "Medium", theme = "theme-vegas", art = "\U0001F31F",
    symbols = list(
      sym("slot", "\U0001F3B0", "Slot Wild", pays = p3(40, 150, 500), weight = 3, cls = "wild"),
      sym("girl", "\U0001F483", "Showgirl", pays = p3(30, 100, 400), weight = 4),
      sym("dice", "\U0001F3B2", "Dice", pays = p3(20, 60, 200), weight = 6),
      sym("drink", "\U0001F378", "Cocktail", pays = p3(15, 50, 150), weight = 7),
      sym("money", "\U0001F4B5", "Bankroll", pays = p3(10, 40, 100), weight = 8),
      royal("A", p3(8, 25, 80), 10), royal("K", p3(6, 20, 60), 11),
      royal("Q", p3(5, 15, 50), 12), royal("J", p3(5, 12, 40), 13),
      sym("star", "\U0001F31F", "Neon Star", weight = 3, cls = "scatter")
    ),
    wild = "slot", scatter = "star", scatter_pays = c(0, 1, 5, 20, 100),
    n_lines = 20, fs_mult_default = 2,
    features = c("Scatter pays from 2", "10 free spins x2", "Retriggers"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      fs <- if (ev$scatters >= 3) 10L else 0L
      slot_result(ctx, ev$win, list(frame(def, grid, ev$hl, win = ev$win)),
                  fs_award = fs, fs_mult = 2, parts = ev$parts,
                  events = if (fs > 0) "free_spins" else character(0))
    }
  )
}

# ---- 4. Classic Ruby -------------------------------------------------------------
# Retro 5x3, 10 lines. Rubies are stacked (3-high) WILDS. After a spin the
# machine may NUDGE one reel one step up or down to complete a payline.
ruby_best_nudge <- function(def, stops, bet, fs_mult) {
  base <- eval_grid(def, window_from_stops(def, stops), bet, fs_mult = fs_mult)$win
  best <- list(gain = 0)
  for (r in seq_len(def$reels)) for (d in c(-1L, 1L)) {
    s2 <- stops; n <- length(def$strips[[r]])
    s2[r] <- ((stops[r] - 1L + d) %% n) + 1L
    g2 <- window_from_stops(def, s2)
    w2 <- eval_grid(def, g2, bet, fs_mult = fs_mult)$win
    if (w2 - base > best$gain) best <- list(gain = w2 - base, reel = r, dir = d, stops = s2)
  }
  best
}

game_classicruby <- function() {
  make_slot(
    id = "classicruby", name = "Classic Ruby", category = "classic",
    tagline = "Retro stacked Ruby wilds with a payline NUDGE",
    description = paste(
      "A retro 5-reel, 10-line cabinet. Rubies are WILD and arrive in stacks of three.",
      "When a line is one step short, the machine can NUDGE a reel up or down to",
      "slide a stack (or the missing symbol) into place."),
    volatility = "Medium", theme = "theme-ruby", art = "\U0001F48E",
    symbols = list(
      sym("ruby", "\U0001F48E", "Ruby Wild", pays = p3(25, 100, 500), weight = 4, cls = "wild ruby", stack = 3),
      sym("r7", "7", "Lucky 7", pays = p3(20, 80, 300), weight = 4, cls = "seven seven-red"),
      sym("bar", BAR_HTML(2), "Double BAR", pays = p3(15, 50, 150), weight = 6, cls = "bars"),
      sym("bell", "\U0001F514", "Bell", pays = p3(10, 30, 100), weight = 8),
      sym("plum", "\U0001F347", "Grapes", pays = p3(8, 20, 60), weight = 10),
      sym("cherry", "\U0001F352", "Cherry", pays = p3(5, 15, 40), weight = 12),
      sym("lemon", "\U0001F34B", "Lemon", pays = p3(4, 10, 30), weight = 13)
    ),
    wild = "ruby", n_lines = 10,
    features = c("Stacked Ruby wilds", "Payline nudge"),
    play = function(ctx) {
      def <- ctx$def
      stops <- spin_stops(def)
      grid <- window_from_stops(def, stops)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      frames <- list(frame(def, grid, if (runif(1) < 0.5) NULL else ev$hl, win = ev$win))
      events <- character(0)
      nudge <- ruby_best_nudge(def, stops, ctx$bet, ctx$fs_mult)
      if (nudge$gain > 0 && runif(1) < 0.3) {
        grid <- window_from_stops(def, nudge$stops)
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        cls <- matrix("", def$rows, def$reels); cls[, nudge$reel] <- "nudged"
        frames <- list(frame(def, window_from_stops(def, stops), win = 0),
                       frame(def, grid, ev$hl, cls = cls, win = ev$win, mode = "swap",
                             note = tr("NUDGE %s reel %d!", if (nudge$dir > 0) "\u25B2" else "\u25BC", nudge$reel)))
        events <- "nudge"
      } else {
        frames[[1]]$hl <- ev$hl
      }
      slot_result(ctx, ev$win, frames, parts = ev$parts, events = events)
    }
  )
}

# ---- 5. City of Queens -------------------------------------------------------------
# 3 crown scatters (reels 1, 3, 5) open a pick-and-win: pick jewels until COLLECT.
make_queens_bonus <- function(bet) {
  vals <- sample(c(1, 2, 2, 3, 3, 4, 5, 5, 8, 10, 15, 25), 9)
  kinds <- c(rep("coin", 9), "mult", "collect", "collect")
  values <- c(vals * bet, 2, 0, 0)
  labels <- c(fmt_coins(vals * bet), "x2", tr("COLLECT"), tr("COLLECT"))
  o <- sample.int(12)
  new_bonus("pick_collect", tr("Queen's Jewel Vault"),
            tr("Pick jewels to reveal prizes - keep going until you find COLLECT"),
            bonus_tiles(kinds[o], values[o], labels[o]), face = "\U0001F48D", bet = bet)
}

game_cityqueens <- function() {
  make_slot(
    id = "cityqueens", name = "City of Queens", category = "classic",
    tagline = "Luxury reels with a pick-and-win jewel bonus",
    description = paste(
      "Crown scatters land on reels 1, 3 and 5. Land all three to enter the Queen's",
      "Jewel Vault: pick jewels to reveal coin prizes and a x2 multiplier until you",
      "uncover COLLECT."),
    volatility = "Medium", theme = "theme-queens", art = "\U0001F478",
    symbols = list(
      sym("city", "\U0001F3D9\uFE0F", "City Wild", pays = p3(50, 200, 750), weight = 3, cls = "wild", reels = 2:4),
      sym("queen", "\U0001F478", "Queen", pays = p3(40, 150, 500), weight = 4),
      sym("ring", "\U0001F48D", "Ring", pays = p3(25, 80, 250), weight = 5),
      sym("heels", "\U0001F460", "Heels", pays = p3(20, 60, 200), weight = 6),
      sym("bubbly", "\U0001F942", "Champagne", pays = p3(15, 50, 150), weight = 7),
      sym("lips", "\U0001F484", "Lipstick", pays = p3(10, 40, 120), weight = 8),
      royal("A", p3(8, 25, 80), 10), royal("K", p3(6, 20, 60), 11),
      royal("Q", p3(5, 15, 50), 12), royal("J", p3(5, 12, 40), 12),
      royal("10", p3(4, 10, 30), 13),
      sym("crown", "\U0001F451", "Crown", weight = 4, cls = "scatter", reels = c(1, 3, 5))
    ),
    wild = "city", scatter = "crown",
    features = c("Pick-and-win bonus", "Crown scatters on reels 1-3-5"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      bonus <- if (ev$scatters >= 3) make_queens_bonus(ctx$bet * def$scale) else NULL
      slot_result(ctx, ev$win, list(frame(def, grid, ev$hl, win = ev$win)), bonus = bonus,
                  parts = ev$parts, events = if (!is.null(bonus)) "bonus" else character(0))
    }
  )
}

# ---- 6. Rapid Fire Jackpot ----------------------------------------------------------
# Spin fast! Each spin within 3s of the last heats the engine (0-10). Heat adds
# extra Rapid Fire bolts to the strips; 6+ bolts anywhere win a fixed jackpot.
RAPID_JP <- c(`6` = 15, `7` = 40, `8` = 150, `9` = 1000)
RAPID_NAMES <- c(`6` = "MINI", `7` = "MINOR", `8` = "MAJOR", `9` = "GRAND")

rapid_heat <- function(gs, now) {
  last <- gs$last_spin %||% 0
  gap <- now - last
  heat <- gs$heat %||% 0
  if (last > 0 && gap <= 3) heat <- min(10, heat + 1) else heat <- max(0, heat - floor(gap / 3))
  heat
}

game_rapidfire <- function() {
  make_slot(
    id = "rapidfire", name = "Rapid Fire Jackpot", category = "classic",
    tagline = "Fast sequential spins heat up the jackpot odds",
    description = paste(
      "Spin within 3 seconds of your last spin to heat the engine (up to 10). Every",
      "heat level loads extra Rapid Fire bolts onto the reels. Land 6+ bolts anywhere",
      "for a jackpot: 6 = Mini (15x), 7 = Minor (40x), 8 = Major (150x), 9+ = GRAND (1000x).",
      "Let the engine idle and it cools down. Turbo + Auto are your friends."),
    volatility = "High", theme = "theme-rapid", art = "\u26A1",
    symbols = list(
      sym("fire", "\U0001F525", "Fire Wild", pays = p3(40, 150, 500), weight = 3, cls = "wild"),
      sym("car", "\U0001F3CE\uFE0F", "Race Car", pays = p3(30, 100, 300), weight = 4),
      sym("rocket", "\U0001F680", "Rocket", pays = p3(20, 60, 200), weight = 5),
      sym("watch", "\u23F1\uFE0F", "Stopwatch", pays = p3(15, 40, 150), weight = 6),
      sym("target", "\U0001F3AF", "Target", pays = p3(10, 30, 100), weight = 7),
      royal("A", p3(8, 20, 80), 9), royal("K", p3(6, 15, 60), 10),
      royal("Q", p3(5, 12, 50), 11), royal("J", p3(4, 10, 40), 12),
      sym("bolt", "\u26A1", "Rapid Fire", weight = 4, cls = "scatter")
    ),
    wild = "fire", scatter = "bolt",
    init_state = list(heat = 0, last_spin = 0),
    features = c("Heat meter", "Fixed jackpots for 6-9+ bolts", "Turbo friendly"),
    play = function(ctx) {
      def <- ctx$def
      gs <- ctx$gs
      heat <- rapid_heat(gs, ctx$now)
      gs$heat <- heat; gs$last_spin <- ctx$now
      grid <- spin_grid(def)
      # Heat: each non-bolt cell converts to a bolt with probability heat * 0.4%
      if (heat > 0) {
        conv <- matrix(runif(length(grid)) < heat * 0.004, nrow(grid))
        grid[conv] <- "bolt"
      }
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      nb <- sum(grid == "bolt")
      win <- ev$win; parts <- ev$parts; events <- character(0)
      hl <- ev$hl
      if (nb >= 6) {
        k <- as.character(min(nb, 9))
        jp <- floor(RAPID_JP[[k]] * ctx$bet)
        win <- win + jp
        parts <- c(parts, tr("%d bolts: %s JACKPOT %s", nb, tr(RAPID_NAMES[[k]]), fmt_coins(jp)))
        events <- c("jackpot", "bonus")
        hl <- hl | grid == "bolt"
      }
      res <- slot_result(ctx, win, list(frame(def, grid, hl, win = win,
                                       note = if (nb >= 6) tr("%s JACKPOT!", tr(RAPID_NAMES[[as.character(min(nb, 9))]])) else "")),
                  gs = gs, parts = parts, events = events)
      res$unscaled <- win - ev$win          # fixed jackpots are not pay-scaled
      res
    },
    meter = function(gs, bet, jackpots = NULL) {
      h <- gs$heat %||% 0
      list(list(label = tr("HEAT"), text = sprintf("%d / 10", as.integer(h)), pct = h * 10, kind = "heat"),
           list(label = tr("GRAND"), text = fmt_coins(1000 * bet), kind = "jp-grand"),
           list(label = tr("MAJOR"), text = fmt_coins(150 * bet), kind = "jp-major"))
    }
  )
}

classic_games <- function() {
  list(game_classic777(), game_bankjackpots(), game_vegasstrip(),
       game_classicruby(), game_cityqueens(), game_rapidfire())
}
