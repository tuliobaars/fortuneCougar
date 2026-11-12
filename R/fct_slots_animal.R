# ---------------------------------------------------------------------------
# games_animal.R -- Animal & Fantasy 3D slot definitions.
# ---------------------------------------------------------------------------

# ---- 16. 3 Tigers ----------------------------------------------------------------
# 3+ paw scatters: the player CHOOSES the volatility of the free spins.
TIGER_OPTIONS <- data.frame(
  label = c("Steady: 15 spins x2", "Balanced: 10 spins x3", "Wild: 6 spins x5"),
  spins = c(15L, 10L, 6L), mult = c(2, 3, 5), stringsAsFactors = FALSE)

make_tiger_choice <- function(bet) {
  b <- new_bonus("volatility", tr("Choose Your Tiger"),
                 tr("Pick the volatility of your free spins - same value, different ride."),
                 bonus_tiles("fs", TIGER_OPTIONS$spins, tr(TIGER_OPTIONS$label)),
                 face = "\U0001F42F", bet = bet)
  b$options_mult <- TIGER_OPTIONS$mult
  b
}

game_tigers <- function() {
  make_slot(
    id = "tigers", name = "3 Tigers", category = "animal",
    tagline = "Choose your volatility in the free spins",
    description = paste(
      "Three or more Paw scatters let you choose your tiger: 15 free spins at x2,",
      "10 free spins at x3 or 6 free spins at x5. Each option has the same expected",
      "value - you pick the variance. Three paws during free spins add 5 more spins."),
    volatility = "High", theme = "theme-tiger", art = "\U0001F42F",
    symbols = list(
      sym("tiger", "\U0001F405", "Tiger Wild", pays = p3(40, 150, 500), weight = 3, cls = "wild", reels = 2:5),
      sym("face", "\U0001F42F", "Tiger King", pays = p3(30, 100, 400), weight = 4),
      sym("cub", "\U0001F408", "Cub", pays = p3(20, 60, 200), weight = 5),
      sym("bamboo", "\U0001F38B", "Bamboo", pays = p3(15, 50, 150), weight = 6),
      sym("lantern", "\U0001F3EE", "Lantern", pays = p3(12, 40, 120), weight = 7),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12), royal("J", p3(4, 10, 30), 13),
      royal("10", p3(3, 8, 25), 14),
      sym("paw", "\U0001F43E", "Paw Scatter", weight = 3.5, cls = "scatter")
    ),
    wild = "tiger", scatter = "paw", scatter_pays = c(0, 0, 2, 10, 50), n_lines = 25,
    features = c("Choose volatility", "Up to x5 free spins", "Retriggers +5"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      bonus <- NULL; fs <- 0L; events <- character(0)
      if (ev$scatters >= 3) {
        if (ctx$free) { fs <- 5L; events <- "retrigger" }
        else { bonus <- make_tiger_choice(ctx$bet); events <- c("free_spins", "bonus") }
      }
      slot_result(ctx, ev$win, list(frame(def, grid, ev$hl, win = ev$win)),
                  fs_award = fs, bonus = bonus, parts = ev$parts, events = events)
    }
  )
}

# ---- 17. Kitty Gems --------------------------------------------------------------
# Frequent small wins (low symbols pay from 2) + multiplying diamond wilds.
game_kittygems <- function() {
  make_slot(
    id = "kittygems", name = "Kitty Gems", category = "animal",
    tagline = "Multiplying diamond wilds & frequent purr-fect wins",
    description = paste(
      "30 lines with kitty symbols that pay from just two in a row for frequent",
      "small wins. Diamond Wilds carry x2 or x3 multipliers - several on one line",
      "multiply together."),
    volatility = "Low", theme = "theme-kitty", art = "\U0001F63B",
    symbols = list(
      sym("gem", "\U0001F48E", "Diamond Wild", pays = p3(25, 100, 400), weight = 2.5, cls = "wild", reels = 2:5),
      sym("love", "\U0001F63B", "Heart-Eyes Kitty", pays = c(`2` = 2, p3(15, 50, 200)), weight = 4),
      sym("grin", "\U0001F638", "Grinning Kitty", pays = c(`2` = 1, p3(10, 30, 120)), weight = 5),
      sym("smile", "\U0001F63A", "Happy Kitty", pays = c(`2` = 1, p3(8, 25, 80)), weight = 6),
      sym("yarn", "\U0001F9F6", "Yarn", pays = p3(5, 15, 50), weight = 9),
      sym("fish", "\U0001F41F", "Fish", pays = p3(4, 12, 40), weight = 10),
      sym("bow", "\U0001F380", "Bow", pays = p3(3, 10, 30), weight = 11),
      sym("milk", "\U0001F95B", "Milk", pays = p3(3, 8, 25), weight = 12)
    ),
    wild = "gem", n_lines = 30,
    features = c("x2/x3 multiplying wilds", "Pays from 2 kitties", "Low volatility"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      mult <- matrix(1, def$rows, def$reels); badge <- no_badge(def)
      w <- grid == "gem"
      if (any(w)) { m <- wdraw(c(2, 3), c(0.7, 0.3), sum(w)); mult[w] <- m; badge[w] <- paste0("x", m) }
      ev <- eval_grid(def, grid, ctx$bet, mult = mult, fs_mult = ctx$fs_mult)
      slot_result(ctx, ev$win, list(frame(def, grid, ev$hl, badge = badge, win = ev$win)),
                  parts = ev$parts)
    }
  )
}

# ---- 18. Gummy King 2 --------------------------------------------------------------
# 5x4, 1024 ways with cascades; the multiplier climbs on consecutive wins.
GUMMY_MULTS <- c(1, 2, 3, 5, 8, 12, 20)

game_gummy <- function() {
  make_slot(
    id = "gummy", name = "Gummy King 2", category = "animal",
    tagline = "Cascading candy with a climbing win multiplier",
    description = paste(
      "1024 ways on a 5x4 candy grid. Winning candies are eaten and new ones drop in.",
      "Each consecutive cascade climbs the multiplier: x1, x2, x3, x5, x8, x12, x20."),
    volatility = "Medium-High", theme = "theme-candy", art = "\U0001F36D", rows = 4,
    symbols = list(
      sym("king", "\U0001F451", "Gummy King Wild", weight = 2.5, cls = "wild", reels = 2:4),
      sym("bear", "\U0001F9F8", "Gummy Bear", pays = p3(20, 50, 150), weight = 4),
      sym("lolly", "\U0001F36D", "Lollipop", pays = p3(15, 40, 100), weight = 5),
      sym("cupcake", "\U0001F9C1", "Cupcake", pays = p3(10, 25, 75), weight = 6),
      sym("donut", "\U0001F369", "Donut", pays = p3(8, 20, 50), weight = 7),
      sym("choc", "\U0001F36B", "Chocolate", pays = p3(5, 15, 40), weight = 8),
      sym("candy", "\U0001F36C", "Candy", pays = p3(4, 10, 30), weight = 9),
      sym("cookie", "\U0001F36A", "Cookie", pays = p3(3, 8, 25), weight = 10)
    ),
    wild = "king", ways = TRUE, ways_unit = 100,
    features = c("1024 ways", "Cascades", "Progressive multiplier up to x20"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      step <- 1L; total <- 0; parts <- character(0)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      frames <- list(frame(def, grid, ev$hl, win = ev$win, note = ""))
      while (ev$win > 0 && step <= 15) {
        total <- total + ev$win
        parts <- c(parts, sprintf("%s (x%g)", ev$parts, GUMMY_MULTS[min(step, length(GUMMY_MULTS))]))
        cs <- cascade_step(def, grid, ev$hl)
        grid <- cs$grid
        step <- step + 1L
        m <- GUMMY_MULTS[min(step, length(GUMMY_MULTS))]
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult, extra_mult = m)
        cls <- no_cls(def); cls[cs$new] <- "dropped"
        frames[[length(frames) + 1]] <- frame(def, grid, ev$hl, cls = cls, win = total + ev$win,
                                              mode = "cascade", note = tr("Cascade x%g", m))
      }
      slot_result(ctx, total, frames, parts = parts,
                  events = if (step > 2) c("cascade", "feature") else if (step > 1) "cascade" else character(0))
    }
  )
}

# ---- 19. Frankenstein Rising ----------------------------------------------------------
# Reviving wilds (45% chance to return in the same spot next spin) + a lab
# mini-game (3+ flasks) that also feeds the Monsterpedia.
game_frankenstein <- function() {
  make_slot(
    id = "frankenstein", name = "Frankenstein Rising", category = "animal",
    tagline = "Reviving wilds & a laboratory mini-game",
    description = paste(
      "It's alive! Every Monster Wild has a 45% chance to REVIVE in the same position",
      "on the next spin. Three or more Flasks open the Laboratory: choose one of three",
      "potions for 5x, 10x or 25x bet. Lab visits count towards Monsterpedia."),
    volatility = "Medium", theme = "theme-franken", art = "\U0001F9DF", monster = TRUE,
    symbols = list(
      sym("monster", "\U0001F9DF", "Monster Wild", pays = p3(40, 150, 500), weight = 3, cls = "wild", reels = 2:5),
      sym("brain", "\U0001F9E0", "Brain", pays = p3(30, 100, 400), weight = 4),
      sym("bolt", "\u26A1", "Lightning", pays = p3(20, 60, 200), weight = 5),
      sym("candle", "\U0001F56F\uFE0F", "Candelabra", pays = p3(15, 50, 150), weight = 6),
      sym("bat", "\U0001F987", "Bat", pays = p3(12, 40, 120), weight = 7),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12), royal("J", p3(4, 10, 30), 13),
      sym("flask", "\U0001F9EA", "Lab Flask", weight = 3.5, cls = "scatter")
    ),
    wild = "monster", scatter = "flask", scatter_pays = c(0, 0, 2, 8, 30), n_lines = 20,
    init_state = list(revive = NULL),
    features = c("Reviving wilds", "Lab mini-game", "Monsterpedia: The Laboratory"),
    play = function(ctx) {
      def <- ctx$def; gs <- ctx$gs
      grid <- spin_grid(def)
      frames <- list(); events <- character(0)
      rv <- gs$revive
      revived <- matrix(FALSE, def$rows, def$reels)
      if (is.matrix(rv) && any(rv)) {
        revived <- rv & matrix(runif(length(rv)) < 0.45, def$rows) & grid != "flask"
      }
      if (any(revived)) {
        frames[[1]] <- frame(def, grid)
        grid[revived] <- "monster"
        cls <- no_cls(def); cls[revived] <- "revived"
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        frames[[2]] <- frame(def, grid, ev$hl, cls = cls, win = ev$win, mode = "swap",
                             note = trn(sum(revived), "IT'S ALIVE! %d wild revived", "IT'S ALIVE! %d wilds revived", sum(revived)))
        events <- "revive"
      } else {
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        frames[[1]] <- frame(def, grid, ev$hl, win = ev$win)
      }
      gs$revive <- grid == "monster"
      bonus <- NULL
      if (ev$scatters >= 3) {
        b <- ctx$bet * def$scale
        v <- sample(c(5, 10, 25)) * b
        bonus <- new_bonus("choose", tr("The Laboratory"), tr("Choose a potion to zap the monster!"),
                           bonus_tiles("coin", v, fmt_coins(v)), face = "\u2697\uFE0F", bet = b)
        events <- c(events, "bonus", "lab")
      }
      slot_result(ctx, ev$win, frames, gs = gs, bonus = bonus, parts = ev$parts, events = events)
    }
  )
}

# ---- 20. Vile Vixens ---------------------------------------------------------------
# Sticky wilds that remain on the board for 3 more spins.
STICKY_SPINS <- 3L

game_vixens <- function() {
  make_slot(
    id = "vixens", name = "Vile Vixens", category = "animal",
    tagline = "Sticky wilds that haunt the reels for 3 spins",
    description = paste(
      "Every Vixen Wild that lands becomes STICKY and stays in place for the next 3",
      "spins (the counter shows how many remain). Sticky wilds pile up - three or more",
      "at once is a Monsterpedia challenge."),
    volatility = "Medium", theme = "theme-vixen", art = "\U0001F608", monster = TRUE,
    symbols = list(
      sym("vixen", "\U0001F608", "Vixen Wild", pays = p3(40, 150, 500), weight = 2, cls = "wild", reels = 2:5),
      sym("vamp", "\U0001F9DB\u200D\u2640\uFE0F", "Vampira", pays = p3(30, 100, 400), weight = 4),
      sym("witch", "\U0001F9D9\u200D\u2640\uFE0F", "Witch", pays = p3(20, 60, 200), weight = 5),
      sym("zombie", "\U0001F9DF\u200D\u2640\uFE0F", "Zombie Queen", pays = p3(15, 50, 150), weight = 6),
      sym("spider", "\U0001F577\uFE0F", "Spider", pays = p3(12, 40, 120), weight = 7),
      sym("moon", "\U0001F319", "Moon", pays = p3(10, 30, 100), weight = 8),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12), royal("J", p3(4, 10, 30), 13)
    ),
    wild = "vixen", n_lines = 20, init_state = list(sticky = NULL),
    features = c("Sticky wilds (3 spins)", "Monsterpedia: Vixen's Crypt"),
    play = function(ctx) {
      def <- ctx$def; gs <- ctx$gs
      st <- gs$sticky
      if (!is.matrix(st)) st <- matrix(0L, def$rows, def$reels)
      grid <- spin_grid(def)
      held <- st > 0
      grid[held] <- "vixen"
      cls <- no_cls(def); cls[held] <- "sticky"
      badge <- no_badge(def); badge[held] <- as.character(st[held])
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      # tick down existing stickies, new wilds get a full counter
      st[held] <- st[held] - 1L
      fresh <- grid == "vixen" & !held
      st[fresh] <- STICKY_SPINS
      gs$sticky <- st
      nst <- sum(grid == "vixen")
      slot_result(ctx, ev$win,
                  list(frame(def, grid, ev$hl, cls = cls, badge = badge, win = ev$win, hold = held,
                             note = if (any(held)) trn(sum(held), "%d sticky wild", "%d sticky wilds", sum(held)) else "")),
                  gs = gs, parts = ev$parts,
                  events = c(if (nst >= 3) "sticky3", if (any(held)) "feature"))
    },
    meter = function(gs, bet, jackpots = NULL) {
      st <- gs$sticky
      n <- if (is.matrix(st)) sum(st > 0) else 0
      list(list(label = tr("STICKY WILDS"), text = tr("%d active", n), pct = min(100, n * 20), kind = "sticky"))
    }
  )
}

# ---- 21. Dark Jaguar ---------------------------------------------------------------
# Stacked mystery symbols reveal into one identical random symbol.
game_jaguar <- function() {
  make_slot(
    id = "jaguar", name = "Dark Jaguar", category = "animal",
    tagline = "Stacked mystery symbols reveal a matching symbol",
    description = paste(
      "Mystery symbols land in tall stacks. After the reels stop, every mystery symbol",
      "on screen reveals into the SAME randomly chosen symbol - sometimes the Jaguar,",
      "sometimes the Moon Wild."),
    volatility = "Medium-High", theme = "theme-jungle", art = "\U0001F406",
    symbols = list(
      sym("moonw", "\U0001F315", "Moon Wild", pays = p3(40, 150, 500), weight = 2.5, cls = "wild", reels = 2:5),
      sym("jaguar", "\U0001F406", "Dark Jaguar", pays = p3(30, 100, 400), weight = 4),
      sym("owl", "\U0001F989", "Owl", pays = p3(20, 60, 200), weight = 5),
      sym("frog", "\U0001F438", "Tree Frog", pays = p3(15, 50, 150), weight = 6),
      sym("flower", "\U0001F33A", "Hibiscus", pays = p3(12, 40, 120), weight = 7),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12), royal("J", p3(4, 10, 30), 13),
      royal("10", p3(3, 8, 25), 14),
      sym("mystery", "\u2753", "Mystery", weight = 9, cls = "mystery", stack = 3)
    ),
    wild = "moonw", n_lines = 25,
    features = c("Stacked mystery symbols", "Identical reveal"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      mys <- grid == "mystery"
      if (any(mys)) {
        pool <- setdiff(def$ids, "mystery")
        w <- c(moonw = 0.5, jaguar = 1, owl = 1.5, frog = 2, flower = 2.5,
               A = 3, K = 3, Q = 3.5, J = 3.5, `10` = 4)[pool]
        s <- wdraw(pool, w)
        f1 <- frame(def, grid, note = tr("Mystery stacks..."))
        grid[mys] <- s
        cls <- no_cls(def); cls[mys] <- "reveal"
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        frames <- list(f1, frame(def, grid, ev$hl, cls = cls, win = ev$win, mode = "swap",
                                 note = tr("Mystery reveals %s!", tr(def$symbols[[s]]$name))))
        events <- "mystery"
      } else {
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        frames <- list(frame(def, grid, ev$hl, win = ev$win)); events <- character(0)
      }
      slot_result(ctx, ev$win, frames, parts = ev$parts, events = events)
    }
  )
}

# ---- 22. Vampire's Kiss ------------------------------------------------------------
# 3+ Blood Moons: 3 Blood Moon respins in which every bat wild TRANSFERS to a
# new random position and one extra bat joins each respin.
game_vampire <- function() {
  make_slot(
    id = "vampire", name = "Vampire's Kiss", category = "animal",
    tagline = "Blood Moon respins with transferring wilds",
    description = paste(
      "Three or more Blood Moons rise for 3 Blood Moon respins. All Bat Wilds on the",
      "reels are kept but TRANSFER (fly) to new random positions each respin, and one",
      "extra bat joins the flock every respin. Every respin pays."),
    volatility = "Medium-High", theme = "theme-vampire", art = "\U0001F9DB", monster = TRUE,
    symbols = list(
      sym("batw", "\U0001F987", "Bat Wild", pays = p3(40, 150, 500), weight = 3, cls = "wild", reels = 2:5),
      sym("count", "\U0001F9DB", "The Count", pays = p3(30, 100, 400), weight = 4),
      sym("kiss", "\U0001F48B", "Kiss", pays = p3(20, 60, 200), weight = 5),
      sym("rose", "\U0001F940", "Wilted Rose", pays = p3(15, 50, 150), weight = 6),
      sym("wine", "\U0001F377", "Blood Wine", pays = p3(12, 40, 120), weight = 7),
      sym("coffin", "\u26B0\uFE0F", "Coffin", pays = p3(10, 30, 100), weight = 8),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12), royal("J", p3(4, 10, 30), 13),
      sym("bloodmoon", "\U0001F315", "Blood Moon", weight = 3.5, cls = "scatter blood-moon")
    ),
    wild = "batw", scatter = "bloodmoon", scatter_pays = c(0, 0, 2, 8, 30), n_lines = 20,
    features = c("Blood Moon respins", "Transferring wilds", "Monsterpedia: Count's Castle"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      frames <- list(frame(def, grid, ev$hl, win = ev$win))
      total <- ev$win; parts <- ev$parts; events <- character(0)
      if (ev$scatters >= 3) {
        frames[[1]]$note <- tr("The BLOOD MOON rises!")
        frames[[1]]$hl <- grid == "bloodmoon" | ev$hl
        nb <- sum(grid == "batw")
        for (r in 1:3) {
          nb <- min(nb + 1L, 10L)
          g <- matrix("", def$rows, def$reels)
          for (c in seq_len(def$reels)) g[, c] <- draw_cells(def, c, def$rows, exclude = c("batw", "bloodmoon"))
          pos <- sample(which(col(g) > 1), nb)          # bats fly to reels 2-5
          g[pos] <- "batw"
          cls <- no_cls(def); cls[pos] <- "transfer"
          e2 <- eval_grid(def, g, ctx$bet, fs_mult = ctx$fs_mult)
          total <- total + e2$win
          parts <- c(parts, tr("Blood Moon respin %d: %s", r, fmt_coins(e2$win)))
          frames[[length(frames) + 1]] <- frame(def, g, e2$hl, cls = cls, win = total, mode = "respin",
                                                note = tr("Blood Moon respin %d/3 - %d bats", r, nb))
        }
        events <- c("blood_moon", "bonus")
      }
      slot_result(ctx, total, frames, parts = parts, events = events)
    }
  )
}

animal_games <- function() {
  list(game_tigers(), game_kittygems(), game_gummy(), game_frankenstein(),
       game_vixens(), game_jaguar(), game_vampire())
}
