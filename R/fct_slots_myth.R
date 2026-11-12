# ---------------------------------------------------------------------------
# games_myth.R -- Mythological & Adventure slot definitions.
# ---------------------------------------------------------------------------

no_cls <- function(def) matrix("", def$rows, def$reels)
no_badge <- function(def) matrix("", def$rows, def$reels)

# Average bet over a collection period (meters pay on average bet so that
# collecting at low stakes and cashing in at high stakes is not exploitable).
avg_bet <- function(gs, key, bet) {
  n <- gs[[paste0(key, "_n")]] %||% 0
  s <- gs[[paste0(key, "_sum")]] %||% 0
  if (n <= 0) bet else s / n
}
track_bet <- function(gs, key, bet) {
  gs[[paste0(key, "_n")]] <- (gs[[paste0(key, "_n")]] %||% 0) + 1
  gs[[paste0(key, "_sum")]] <- (gs[[paste0(key, "_sum")]] %||% 0) + bet
  gs
}
reset_bet <- function(gs, key) {
  gs[[paste0(key, "_n")]] <- 0; gs[[paste0(key, "_sum")]] <- 0; gs
}

# ---- 7. Purrymid Prince ---------------------------------------------------------
# Expanding cat-pharaoh wilds (reels 2-4) + Tomb Raid trail bonus (3+ scarabs).
tomb_level <- function(level, bet) {
  k <- if (level == 1) 3 else 2
  vals <- sample(c(2, 3, 4, 5, 6), k, replace = TRUE) * level * bet
  kinds <- c(rep("coin", k), if (level > 1) "trap")
  values <- c(vals, if (level > 1) 0)
  labels <- c(fmt_coins(vals), if (level > 1) paste("\U0001F9DF", tr("MUMMY")))
  o <- sample.int(3)
  bonus_tiles(kinds[o], values[o], labels[o])
}

game_purrymid <- function() {
  make_slot(
    id = "purrymid", name = "Purrymid Prince", category = "myth",
    tagline = "Expanding cat-pharaoh wilds & a tomb-raiding trail",
    description = paste(
      "The Cat Pharaoh is WILD on reels 2, 3 and 4 and expands to cover the whole reel.",
      "Three or more Scarabs start the Tomb Raid: choose one of three doors in each of",
      "four chambers. Treasure doors pay out and lead deeper; the mummy's door ends the",
      "raid. Survive all four for the Pharaoh's treasure (40x bet)."),
    volatility = "Medium", theme = "theme-egypt", art = "\U0001F63C",
    symbols = list(
      sym("cat", "\U0001F63C", "Cat Pharaoh Wild", pays = p3(50, 200, 750), weight = 3, cls = "wild", reels = 2:4),
      sym("eye", "\U0001F441\uFE0F", "Eye of Ra", pays = p3(30, 100, 400), weight = 4),
      sym("ankh", "\u2625", "Ankh", pays = p3(25, 75, 250), weight = 5, cls = "gold-glyph"),
      sym("urn", "\U0001F3FA", "Canopic Urn", pays = p3(20, 50, 200), weight = 6),
      sym("pyramid", "\U0001F53A", "Pyramid", pays = p3(15, 40, 150), weight = 7),
      royal("A", p3(8, 25, 80), 10), royal("K", p3(6, 20, 60), 11),
      royal("Q", p3(5, 15, 50), 12), royal("J", p3(4, 12, 40), 13),
      sym("scarab", "\U0001FAB2", "Scarab", weight = 3.5, cls = "scatter")
    ),
    wild = "cat", scatter = "scarab", scatter_pays = c(0, 0, 2, 10, 50), n_lines = 20,
    features = c("Expanding wilds", "Tomb Raid trail bonus"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      frames <- list()
      wr <- which(colSums(grid == "cat") > 0)
      if (length(wr)) {
        frames[[1]] <- frame(def, grid)
        cls <- no_cls(def); cls[, wr] <- "expanded"
        grid[, wr] <- "cat"
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        frames[[2]] <- frame(def, grid, ev$hl, cls = cls, win = ev$win, mode = "swap",
                             note = tr("The Pharaoh expands!"))
      } else {
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        frames[[1]] <- frame(def, grid, ev$hl, win = ev$win)
      }
      bonus <- NULL
      if (ev$scatters >= 3) {
        b <- ctx$bet * def$scale
        bonus <- new_bonus("trail", tr("Tomb Raid"), tr("Choose a door in each chamber - avoid the mummy!"),
                           tomb_level(1, b), face = "\U0001F6AA", bet = b,
                           levels = 4L, treasure = 40 * b, make_level = tomb_level)
      }
      slot_result(ctx, ev$win, frames, bonus = bonus, parts = ev$parts,
                  events = c(if (length(wr)) "expanding_wild", if (!is.null(bonus)) "bonus"))
    }
  )
}

# ---- 8. Stone Gold ---------------------------------------------------------------
# Medusa's gaze: every winning symbol turns to stone and locks; the rest respin
# until no new stone appears. The final board pays.
game_stonegold <- function() {
  make_slot(
    id = "stonegold", name = "Stone Gold", category = "myth",
    tagline = "Medusa turns winners to stone for locking respins",
    description = paste(
      "Any win triggers Medusa's gaze: winning symbols turn to stone and lock in place",
      "while every other position respins. Each respin that adds new winning positions",
      "petrifies them too and grants another respin. The final petrified board pays."),
    volatility = "Medium", theme = "theme-stone", art = "\U0001F40D", monster = TRUE,
    symbols = list(
      sym("medusa", "\U0001F40D", "Medusa Wild", pays = p3(40, 150, 600), weight = 3, cls = "wild", reels = 2:5),
      sym("statue", "\U0001F5FF", "Statue", pays = p3(30, 100, 400), weight = 4),
      sym("shield", "\U0001F6E1\uFE0F", "Shield", pays = p3(20, 60, 250), weight = 5),
      sym("sword", "\U0001F5E1\uFE0F", "Sword", pays = p3(15, 50, 200), weight = 6),
      sym("amphora", "\U0001F3FA", "Amphora", pays = p3(12, 40, 150), weight = 7),
      sym("coin", "\U0001FA99", "Gold Coin", pays = p3(10, 30, 100), weight = 8),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12), royal("J", p3(4, 10, 30), 13)
    ),
    wild = "medusa", n_lines = 20,
    features = c("Stone lock respins", "Monsterpedia: Medusa's Lair"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      frames <- list(frame(def, grid, ev$hl, win = ev$win))
      respins <- 0L
      if (ev$win > 0) {
        locked <- ev$hl
        repeat {
          if (all(locked) || respins >= 8) break
          for (c in seq_len(def$reels)) {
            free_rows <- which(!locked[, c])
            grid[free_rows, c] <- draw_cells(def, c, length(free_rows))
          }
          respins <- respins + 1L
          ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
          new_locked <- locked | ev$hl
          cls <- no_cls(def); cls[locked] <- "stone"
          frames[[length(frames) + 1]] <- frame(def, grid, ev$hl, cls = cls, hold = locked,
                                                win = ev$win, mode = "respin",
                                                note = tr("Medusa respin %d", respins))
          if (sum(new_locked) == sum(locked)) break
          locked <- new_locked
        }
        cls <- no_cls(def); cls[locked | ev$hl] <- "stone"
        frames[[length(frames)]]$cls <- cls
      }
      slot_result(ctx, ev$win, frames, parts = ev$parts,
                  events = if (respins > 0) c("stone_respin", "feature") else character(0))
    }
  )
}

# ---- 9. Blackbeard's Riches ------------------------------------------------------------
# Cascading reels + treasure-map progression bonus.
MAP_STEPS <- 15L
MAP_PRIZES <- c(`5` = 5, `10` = 15, `15` = 60)     # x average bet

game_blackbeard <- function() {
  make_slot(
    id = "blackbeard", name = "Blackbeard's Riches", category = "myth",
    tagline = "Cascading reels & a treasure-map voyage",
    description = paste(
      "Winning symbols explode and new ones tumble down - cascades continue as long",
      "as you keep winning. Every Treasure Map that lands (including on cascades) sails",
      "Blackbeard's ship one island further along a 15-island voyage: islands 5 and 10",
      "pay 5x and 15x your average bet, and the treasure island pays 60x."),
    volatility = "Medium", theme = "theme-pirate", art = "\U0001F3F4\u200D\u2620\uFE0F",
    symbols = list(
      sym("flag", "\U0001F3F4\u200D\u2620\uFE0F", "Jolly Roger Wild", pays = p3(40, 150, 500), weight = 3, cls = "wild", reels = 2:5),
      sym("parrot", "\U0001F99C", "Parrot", pays = p3(30, 100, 300), weight = 4),
      sym("skull", "\U0001F480", "Skull", pays = p3(20, 60, 200), weight = 5),
      sym("anchor", "\u2693", "Anchor", pays = p3(15, 50, 150), weight = 6),
      sym("sword", "\U0001F5E1\uFE0F", "Cutlass", pays = p3(12, 40, 120), weight = 7),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12), royal("J", p3(4, 10, 30), 13),
      sym("map", "\U0001F5FA\uFE0F", "Treasure Map", weight = 3, cls = "collect")
    ),
    n_lines = 20, wild = "flag",
    init_state = list(map = 0L),
    features = c("Cascading reels", "15-island map progression"),
    play = function(ctx) {
      def <- ctx$def; gs <- ctx$gs
      gs <- track_bet(gs, "map", ctx$bet)
      grid <- spin_grid(def)
      maps <- sum(grid == "map")
      total <- 0; parts <- character(0); frames <- list(); casc <- 0L
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      frames[[1]] <- frame(def, grid, ev$hl, win = ev$win)
      while (ev$win > 0 && casc < 12) {
        total <- total + ev$win; parts <- c(parts, ev$parts)
        cs <- cascade_step(def, grid, ev$hl)
        grid <- cs$grid
        maps <- maps + sum(grid == "map" & cs$new)
        casc <- casc + 1L
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        cls <- no_cls(def); cls[cs$new] <- "dropped"
        frames[[length(frames) + 1]] <- frame(def, grid, ev$hl, cls = cls, win = ev$win,
                                              mode = "cascade", note = tr("Cascade %d", casc))
      }
      events <- if (casc > 0) "cascade" else character(0)
      if (maps > 0) {
        old <- gs$map; new <- old + maps
        ab <- avg_bet(gs, "map", ctx$bet)
        for (step in (old + 1):min(new, MAP_STEPS)) {
          key <- as.character(step)
          if (!is.na(MAP_PRIZES[key])) {
            p <- prize(def, MAP_PRIZES[[key]], ab)
            total <- total + p
            parts <- c(parts, tr("Island %d reached: +%s", step, fmt_coins(p)))
            events <- c(events, "bonus")
          }
        }
        if (new >= MAP_STEPS) { new <- new - MAP_STEPS; gs <- reset_bet(gs, "map"); events <- c(events, "map_complete") }
        gs$map <- as.integer(new)
      }
      frames[[length(frames)]]$note <- paste(c(frames[[length(frames)]]$note,
        if (maps > 0) trn(maps, "+%d map", "+%d maps", maps)), collapse = "  ")
      slot_result(ctx, total, frames, gs = gs, parts = parts, events = events)
    },
    meter = function(gs, bet, jackpots = NULL) {
      m <- gs$map %||% 0
      list(list(label = tr("VOYAGE"), text = tr("Island %d / %d", m, MAP_STEPS), pct = 100 * m / MAP_STEPS,
                kind = "map", steps = MAP_STEPS, at = m, marks = as.integer(names(MAP_PRIZES))))
    }
  )
}

# ---- 10. Giant's Treasure ------------------------------------------------------------
# Beanstalk wilds grow vertically to the top of their reel; fully grown
# beanstalks let Jack climb. Five climbs = the Giant's Treasure.
GIANT_CLIMB <- 5L
GIANT_PRIZE <- 30

game_giant <- function() {
  make_slot(
    id = "giant", name = "Giant's Treasure", category = "myth",
    tagline = "Growing beanstalk wilds & a climb to the Giant's gold",
    description = paste(
      "Beanstalk wilds (reels 2-5) grow upwards from where they land all the way to",
      "the top of the reel. A beanstalk that sprouts from the bottom row fills the",
      "entire reel and lets Jack climb one step. Reach the clouds (5 climbs) to steal",
      "the Giant's Treasure: 30x your average bet."),
    volatility = "Medium", theme = "theme-giant", art = "\U0001F331",
    symbols = list(
      sym("bean", "\U0001F331", "Beanstalk Wild", pays = p3(30, 100, 400), weight = 3, cls = "wild", reels = 2:5),
      sym("goose", "\U0001FABF", "Golden Goose", pays = p3(30, 100, 400), weight = 4),
      sym("egg", "\U0001F95A", "Golden Egg", pays = p3(20, 60, 250), weight = 5),
      sym("harp", "\U0001FA89", "Singing Harp", pays = p3(15, 50, 200), weight = 6),
      sym("castle", "\U0001F3F0", "Cloud Castle", pays = p3(12, 40, 150), weight = 7),
      sym("cow", "\U0001F404", "Milky Cow", pays = p3(10, 30, 100), weight = 8),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12), royal("J", p3(4, 10, 30), 13)
    ),
    wild = "bean", n_lines = 20, init_state = list(climb = 0L),
    features = c("Vertically growing wilds", "Climb meter", "Giant's Treasure prize"),
    play = function(ctx) {
      def <- ctx$def; gs <- ctx$gs
      gs <- track_bet(gs, "climb", ctx$bet)
      grid <- spin_grid(def)
      frames <- list(); events <- character(0); total <- 0; parts <- character(0)
      b <- which(grid == "bean", arr.ind = TRUE)
      if (nrow(b)) {
        frames[[1]] <- frame(def, grid, note = tr("A beanstalk sprouts..."))
        cls <- no_cls(def); climbs <- 0L
        for (c in unique(b[, "col"])) {
          lowest <- max(b[b[, "col"] == c, "row"])
          grid[seq_len(lowest), c] <- "bean"
          cls[seq_len(lowest), c] <- "grow"
          if (lowest == def$rows) climbs <- climbs + 1L
        }
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        total <- ev$win; parts <- ev$parts
        if (climbs > 0) {
          gs$climb <- gs$climb + climbs
          events <- c(events, "climb")
          if (gs$climb >= GIANT_CLIMB) {
            p <- prize(def, GIANT_PRIZE, avg_bet(gs, "climb", ctx$bet))
            total <- total + p
            parts <- c(parts, tr("GIANT'S TREASURE: +%s", fmt_coins(p)))
            gs$climb <- gs$climb - GIANT_CLIMB
            gs <- reset_bet(gs, "climb")
            events <- c(events, "bonus")
          }
        }
        frames[[2]] <- frame(def, grid, ev$hl, cls = cls, win = total, mode = "swap",
                             note = if (climbs > 0) tr("Jack climbs %d!", climbs) else tr("The beanstalk grows!"))
      } else {
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        total <- ev$win; parts <- ev$parts
        frames[[1]] <- frame(def, grid, ev$hl, win = total)
      }
      slot_result(ctx, total, frames, gs = gs, parts = parts, events = events)
    },
    meter = function(gs, bet, jackpots = NULL) {
      c0 <- gs$climb %||% 0
      list(list(label = tr("BEANSTALK"), text = tr("Climb %d / %d", c0, GIANT_CLIMB),
                pct = 100 * c0 / GIANT_CLIMB, kind = "climb"))
    }
  )
}

# ---- 11. Cinderella ------------------------------------------------------------------
# Midnight countdown: the clock ticks down every spin (faster with clock
# symbols). On the stroke of midnight, pumpkins & mice become carriage wilds.
CINDER_CLOCK <- 12L

game_cinderella <- function() {
  make_slot(
    id = "cinderella", name = "Cinderella", category = "myth",
    tagline = "Midnight countdown turns pumpkins into carriage wilds",
    description = paste(
      "The palace clock counts down from 12 with every spin; each Clock symbol that lands",
      "knocks off an extra hour. At the stroke of MIDNIGHT every Pumpkin and Mouse on the",
      "reels transforms into a Carriage WILD - then the clock resets."),
    volatility = "Low-Medium", theme = "theme-fairy", art = "\U0001F460",
    symbols = list(
      sym("carriage", "\U0001F3A0", "Carriage Wild", pays = p3(30, 100, 400), weight = 1.5, cls = "wild carriage", reels = 2:4),
      sym("slipper", "\U0001F460", "Glass Slipper", pays = p3(30, 100, 400), weight = 4),
      sym("princess", "\U0001F478", "Cinderella", pays = p3(20, 60, 250), weight = 5),
      sym("fairy", "\U0001F9DA", "Fairy Godmother", pays = p3(15, 50, 200), weight = 6),
      sym("castle", "\U0001F3F0", "Palace", pays = p3(12, 40, 150), weight = 7),
      sym("pumpkin", "\U0001F383", "Pumpkin", pays = p3(5, 15, 50), weight = 9),
      sym("mouse", "\U0001F42D", "Mouse", pays = p3(5, 15, 50), weight = 9),
      royal("A", p3(5, 15, 50), 9), royal("K", p3(4, 12, 40), 10),
      royal("Q", p3(4, 10, 30), 11),
      sym("clock", "\U0001F55B", "Clock", weight = 2.5, cls = "collect")
    ),
    wild = "carriage", n_lines = 20, init_state = list(clock = CINDER_CLOCK),
    features = c("Midnight countdown", "Pumpkin & mouse -> Carriage wilds"),
    play = function(ctx) {
      def <- ctx$def; gs <- ctx$gs
      grid <- spin_grid(def)
      clocks <- sum(grid == "clock")
      gs$clock <- gs$clock - 1L - clocks
      events <- character(0)
      if (gs$clock <= 0) {
        tf <- grid == "pumpkin" | grid == "mouse"
        f1 <- frame(def, grid, note = paste("\U0001F55B", tr("The clock strikes MIDNIGHT!")))
        grid[tf] <- "carriage"
        cls <- no_cls(def); cls[tf] <- "transform"
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        frames <- list(f1, frame(def, grid, ev$hl, cls = cls, win = ev$win, mode = "swap",
                                 note = trn(sum(tf), "%d carriage wild!", "%d carriage wilds!", sum(tf))))
        gs$clock <- CINDER_CLOCK
        events <- c("midnight", "feature")
      } else {
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        frames <- list(frame(def, grid, ev$hl, win = ev$win,
                             note = if (clocks > 0) tr("Tick tock... -%d hours", clocks + 1L) else ""))
      }
      slot_result(ctx, ev$win, frames, gs = gs, parts = ev$parts, events = events)
    },
    meter = function(gs, bet, jackpots = NULL) {
      c0 <- max(0, gs$clock %||% CINDER_CLOCK)
      list(list(label = tr("MIDNIGHT IN"), text = trn(c0, "%d spin", "%d spins", c0),
                pct = 100 * (CINDER_CLOCK - c0) / CINDER_CLOCK, kind = "clock"))
    }
  )
}

# ---- 12. Mighty Phoenix ------------------------------------------------------------------
# 243 ways + Hold & Spin: 6+ fireballs lock with values, 3 respins that reset
# on every new fireball. Fill all 15 for the GRAND.
PHOENIX_VALUES <- c(1, 2, 3, 5, 8, 10, 15, 25)
PHOENIX_VW     <- c(30, 25, 18, 12, 6, 4, 2.5, 1.5)
PHOENIX_JP     <- c(MINI = 10, MINOR = 25, MAJOR = 100, GRAND = 1000)

phoenix_value <- function(n) {
  lab <- character(n); val <- numeric(n)
  for (i in seq_len(n)) {
    u <- runif(1)
    if (u < 0.012) { lab[i] <- "MAJOR"; val[i] <- PHOENIX_JP[["MAJOR"]] }
    else if (u < 0.04) { lab[i] <- "MINOR"; val[i] <- PHOENIX_JP[["MINOR"]] }
    else if (u < 0.10) { lab[i] <- "MINI"; val[i] <- PHOENIX_JP[["MINI"]] }
    else { v <- wdraw(PHOENIX_VALUES, PHOENIX_VW); lab[i] <- paste0(v, "x"); val[i] <- v }
  }
  list(label = lab, value = val)
}

hold_and_spin <- function(def, grid, bet, p_new = 0.075) {
  held <- grid == "fireball"
  vals <- matrix(0, def$rows, def$reels); labs <- matrix("", def$rows, def$reels)
  v <- phoenix_value(sum(held)); vals[held] <- v$value; labs[held] <- v$label
  frames <- list(); respins <- 3L; n <- 0L
  while (respins > 0 && !all(held)) {
    n <- n + 1L
    g <- grid
    hit <- !held & matrix(runif(length(held)) < p_new, def$rows)
    cls <- matrix("", def$rows, def$reels); cls[held] <- "held"
    g[!held] <- "blank"
    if (any(hit)) {
      nv <- phoenix_value(sum(hit)); vals[hit] <- nv$value; labs[hit] <- nv$label
      g[hit] <- "fireball"; held <- held | hit; respins <- 3L
      cls[hit] <- "new-held"
    } else respins <- respins - 1L
    grid <- g
    frames[[n]] <- frame(def, grid, cls = cls, badge = labs, hold = held, mode = "respin",
                         note = tr("Respins left: %d", respins))
  }
  total_x <- sum(vals)
  grand <- all(held)
  if (grand) total_x <- total_x + PHOENIX_JP[["GRAND"]]
  list(frames = frames, win = floor(total_x * bet * def$scale), grand = grand,
       count = sum(held), badge = labs)
}

game_phoenix <- function() {
  make_slot(
    id = "phoenix", name = "Mighty Phoenix", category = "myth",
    tagline = "243 ways with a Hold & Spin jackpot feature",
    description = paste(
      "243 ways to win - matching symbols on adjacent reels from the left pay anywhere",
      "on the reel. Six or more Fireballs ignite HOLD & SPIN: fireballs lock with coin",
      "values or Mini/Minor/Major jackpots and you get 3 respins, reset by every new",
      "fireball. Fill all 15 positions for the GRAND (1000x)."),
    volatility = "High", theme = "theme-phoenix", art = "\U0001F525", rows = 3,
    symbols = list(
      sym("phoenix", "\U0001F985", "Phoenix Wild", weight = 3, cls = "wild phoenix", reels = 2:4),
      sym("lantern", "\U0001F3EE", "Lantern", pays = p3(30, 80, 200), weight = 4),
      sym("envelope", "\U0001F9E7", "Red Envelope", pays = p3(25, 60, 150), weight = 5),
      sym("fan", "\U0001FAAD", "Fan", pays = p3(20, 50, 120), weight = 6),
      sym("bamboo", "\U0001F38B", "Tanabata", pays = p3(15, 40, 100), weight = 7),
      royal("A", p3(10, 20, 50), 9), royal("K", p3(8, 15, 40), 10),
      royal("Q", p3(6, 12, 30), 11), royal("J", p3(5, 10, 25), 12),
      royal("10", p3(5, 10, 20), 12),
      sym("fireball", "\U0001F525", "Fireball", weight = 12, cls = "scatter fireball"),
      sym("blank", "", "Blank", weight = 0, cls = "blank")
    ),
    wild = "phoenix", scatter = "fireball", ways = TRUE, ways_unit = 50,
    features = c("243 ways", "Hold & Spin", "Fixed Mini/Minor/Major/Grand"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      frames <- list(frame(def, grid, ev$hl, win = ev$win))
      total <- ev$win; parts <- ev$parts; events <- character(0)
      nf <- sum(grid == "fireball")
      if (nf >= 6) {
        frames[[1]]$note <- tr("%d fireballs - HOLD & SPIN!", nf)
        frames[[1]]$hl <- grid == "fireball"
        hs <- hold_and_spin(def, grid, ctx$bet)
        frames <- c(frames, hs$frames)
        total <- total + hs$win
        parts <- c(parts, if (hs$grand) tr("Hold & Spin: %d fireballs + GRAND = %s", hs$count, fmt_coins(hs$win))
                          else tr("Hold & Spin: %d fireballs = %s", hs$count, fmt_coins(hs$win)))
        n <- length(frames)
        frames[[n]]$win <- total
        frames[[n]]$note <- if (hs$grand) tr("GRAND JACKPOT!") else tr("Hold & Spin pays %s", fmt_coins(hs$win))
        events <- c("bonus", "hold_spin", if (hs$grand || any(hs$badge %in% c("MINI", "MINOR", "MAJOR"))) "jackpot")
      }
      slot_result(ctx, total, frames, parts = parts, events = events)
    },
    meter = function(gs, bet, jackpots = NULL) {
      lapply(names(PHOENIX_JP), function(t)
        list(label = tr(t), text = fmt_coins(PHOENIX_JP[[t]] * bet), kind = paste0("jp-", tolower(t))))
    }
  )
}

# ---- 13. Dragon's Fury ----------------------------------------------------------------
# High volatility: the dragon randomly breathes fire over 1-3 whole reels,
# turning them completely WILD.
game_dragon <- function() {
  make_slot(
    id = "dragon", name = "Dragon's Fury", category = "myth",
    tagline = "Random full-reel dragon wilds, high volatility",
    description = paste(
      "On any spin the dragon may awaken (about 1 in 9 spins) and breathe fire across",
      "1 to 3 of reels 2-5, turning each of them into a fully WILD reel.",
      "Long dry spells, explosive wins."),
    volatility = "Very High", theme = "theme-dragon", art = "\U0001F409",
    symbols = list(
      sym("dragon", "\U0001F409", "Dragon Wild", pays = p3(50, 250, 1000), weight = 2, cls = "wild", reels = 2:5),
      sym("head", "\U0001F432", "Dragon Head", pays = p3(40, 150, 600), weight = 3),
      sym("pagoda", "\U0001F3EF", "Pagoda", pays = p3(25, 80, 300), weight = 4),
      sym("mahjong", "\U0001F004", "Mahjong", pays = p3(20, 60, 200), weight = 5),
      sym("koi", "\U0001F38F", "Koi", pays = p3(15, 40, 150), weight = 6),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12), royal("J", p3(4, 10, 30), 13),
      royal("10", p3(3, 8, 25), 14)
    ),
    wild = "dragon", n_lines = 25,
    features = c("Random full-reel dragon wilds", "Very high volatility"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      if (runif(1) < 1 / 9) {
        k <- wdraw(1:3, c(0.62, 0.3, 0.08))
        reels <- sort(sample(2:5, k))
        f1 <- frame(def, grid, note = tr("The dragon awakens..."))
        grid[, reels] <- "dragon"
        cls <- no_cls(def); cls[, reels] <- "dragon-reel"
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        frames <- list(f1, frame(def, grid, ev$hl, cls = cls, win = ev$win, mode = "swap",
                                 note = trn(k, "DRAGON'S FURY! %d wild reel", "DRAGON'S FURY! %d wild reels", k)))
        events <- c("feature", "dragon_fury")
      } else {
        ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
        frames <- list(frame(def, grid, ev$hl, win = ev$win)); events <- character(0)
      }
      slot_result(ctx, ev$win, frames, parts = ev$parts, events = events)
    }
  )
}

# ---- 14. Mermaid's Pearl -----------------------------------------------------------------
# Collect 100 pearls (oysters carry 1-3) to earn the Grand Jackpot Spin.
MERMAID_TARGET <- 100L
MERMAID_WHEEL <- data.frame(
  label = c("MINI", "MINOR", "MINI", "MAJOR", "MINI", "MINOR", "MINI", "GRAND"),
  value = c(25, 50, 25, 150, 25, 50, 25, 1000),
  weight = c(0.2, 0.15, 0.2, 0.07, 0.2, 0.15, 0.2, 0.0125),
  stringsAsFactors = FALSE)

make_pearl_wheel <- function(bet) {
  new_bonus("wheel", tr("Grand Jackpot Spin"), tr("100 pearls collected - spin the Pearl Wheel!"),
            bonus_tiles("spin", 0, "SPIN"), face = "\U0001F9AA", bet = bet,
            segments = MERMAID_WHEEL)
}

game_mermaid <- function() {
  make_slot(
    id = "mermaid", name = "Mermaid's Pearl", category = "myth",
    tagline = "Collect 100 pearls for the Grand Jackpot spin",
    description = paste(
      "Every Oyster that lands holds 1-3 pearls that drop into your collection.",
      "Collect 100 pearls to earn the Grand Jackpot Spin: a Pearl Wheel that always",
      "awards a jackpot - Mini 25x, Minor 50x, Major 150x or GRAND 1000x your average bet."),
    volatility = "Medium", theme = "theme-ocean", art = "\U0001F9DC\u200D\u2640\uFE0F",
    symbols = list(
      sym("trident", "\U0001F531", "Trident Wild", pays = p3(40, 150, 500), weight = 3, cls = "wild", reels = 2:5),
      sym("mermaid", "\U0001F9DC\u200D\u2640\uFE0F", "Mermaid", pays = p3(30, 100, 400), weight = 4),
      sym("shell", "\U0001F41A", "Conch", pays = p3(20, 60, 200), weight = 5),
      sym("octopus", "\U0001F419", "Octopus", pays = p3(15, 50, 150), weight = 6),
      sym("fish", "\U0001F420", "Tropical Fish", pays = p3(12, 40, 120), weight = 7),
      sym("crab", "\U0001F980", "Crab", pays = p3(10, 30, 100), weight = 8),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12),
      sym("oyster", "\U0001F9AA", "Pearl Oyster", weight = 3, cls = "collect")
    ),
    wild = "trident", n_lines = 20, init_state = list(pearls = 0L),
    features = c("Pearl collection meter", "Grand Jackpot wheel"),
    play = function(ctx) {
      def <- ctx$def; gs <- ctx$gs
      gs <- track_bet(gs, "pearl", ctx$bet)
      grid <- spin_grid(def)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      oy <- grid == "oyster"
      badge <- no_badge(def); bonus <- NULL; events <- character(0); note <- ""
      if (any(oy)) {
        p <- sample(1:3, sum(oy), replace = TRUE, prob = c(0.6, 0.3, 0.1))
        badge[oy] <- paste0("+", p)
        gs$pearls <- gs$pearls + sum(p)
        note <- trn(sum(p), "+%d pearl", "+%d pearls", sum(p))
        if (gs$pearls >= MERMAID_TARGET) {
          bonus <- make_pearl_wheel(avg_bet(gs, "pearl", ctx$bet) * def$scale)
          gs$pearls <- gs$pearls - MERMAID_TARGET
          gs <- reset_bet(gs, "pearl")
          events <- c("bonus", "jackpot")
        }
      }
      slot_result(ctx, ev$win, list(frame(def, grid, ev$hl | oy, badge = badge, win = ev$win, note = note)),
                  gs = gs, bonus = bonus, parts = ev$parts, events = events)
    },
    meter = function(gs, bet, jackpots = NULL) {
      p <- gs$pearls %||% 0
      list(list(label = tr("PEARLS"), text = sprintf("%d / %d", p, MERMAID_TARGET),
                pct = 100 * p / MERMAID_TARGET, kind = "pearls"))
    }
  )
}

# ---- 15. Wild West Bounty ----------------------------------------------------------------
# Dual-screen: 3+ sheriff badges open the saloon shooting gallery up top.
make_gallery_bonus <- function(bet, shots) {
  vals <- sample(c(2, 3, 3, 4, 5, 5, 6, 8, 10, 12, 15, 25), 12) * bet
  new_bonus("shots", tr("Saloon Shooting Gallery"),
            tr("%d bullets - shoot the targets for instant coin prizes!", shots),
            bonus_tiles("coin", vals, fmt_coins(vals)), face = "\U0001F37E", bet = bet,
            picks_left = as.integer(shots))
}

game_wildwest <- function() {
  make_slot(
    id = "wildwest", name = "Wild West Bounty", category = "myth",
    tagline = "Dual-screen saloon shooting gallery",
    description = paste(
      "Three or more Sheriff Badges anywhere open the shooting gallery on the top",
      "screen: you get 3 bullets (+1 for every extra badge) to shoot bottles hiding",
      "instant coin prizes of up to 25x bet."),
    volatility = "Medium", theme = "theme-west", art = "\U0001F920", dual_screen = TRUE,
    symbols = list(
      sym("wanted", "\U0001F4DC", "Wanted Wild", pays = p3(40, 150, 500), weight = 3, cls = "wild", reels = 2:5),
      sym("cowboy", "\U0001F920", "Outlaw", pays = p3(30, 100, 400), weight = 4),
      sym("horse", "\U0001F40E", "Stallion", pays = p3(20, 60, 200), weight = 5),
      sym("bag", "\U0001F4B0", "Bounty", pays = p3(15, 50, 150), weight = 6),
      sym("cactus", "\U0001F335", "Cactus", pays = p3(12, 40, 120), weight = 7),
      sym("whiskey", "\U0001F943", "Whiskey", pays = p3(10, 30, 100), weight = 8),
      royal("A", p3(6, 20, 60), 10), royal("K", p3(5, 15, 50), 11),
      royal("Q", p3(4, 12, 40), 12), royal("J", p3(4, 10, 30), 13),
      sym("badge", "\u2B50", "Sheriff Badge", weight = 3.5, cls = "scatter")
    ),
    wild = "wanted", scatter = "badge", scatter_pays = c(0, 0, 2, 5, 20), n_lines = 20,
    features = c("Dual-screen gallery", "Instant coin prizes"),
    play = function(ctx) {
      def <- ctx$def
      grid <- spin_grid(def)
      ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
      bonus <- if (ev$scatters >= 3) make_gallery_bonus(ctx$bet * def$scale, 3 + ev$scatters - 3) else NULL
      slot_result(ctx, ev$win, list(frame(def, grid, ev$hl, win = ev$win)), bonus = bonus,
                  parts = ev$parts, events = if (!is.null(bonus)) "bonus" else character(0))
    }
  )
}

myth_games <- function() {
  list(game_purrymid(), game_stonegold(), game_blackbeard(), game_giant(),
       game_cinderella(), game_phoenix(), game_dragon(), game_mermaid(), game_wildwest())
}
