# ---------------------------------------------------------------------------
# engine_slots.R -- generic, UI-free slot machine mathematics.
#
#  * Reel strips    : every machine owns physical reel strips built once from
#                     symbol weights (deterministically, seeded by game id).
#                     A spin picks a uniformly random stop on each strip, so
#                     stacked symbols, nudges and near-misses behave like a
#                     real stepper / video reel.
#  * Evaluation     : left-to-right paylines (with wild substitution and
#                     multiplier wilds), 243/1024 "ways", scatter pays and
#                     "any-of" mixed groups (e.g. ANY BAR).
#  * Helpers        : cascades, frames for the animation layer, free-spin
#                     bookkeeping and interactive bonus games (pick / trail /
#                     shooting gallery / choice / match-3 jackpot).
#
# Each game in games_*.R supplies a `play(ctx)` function composed from these
# helpers. `slot_play()` wraps it with free-spin accounting so that the Shiny
# module and the offline RTP simulator (tools/simulate_rtp.R) share one code
# path.
# ---------------------------------------------------------------------------

# ---- Symbols & machine definition ------------------------------------------

# Define a reel symbol.
# @param pays named numeric: names are symbol counts ("3","4","5"), values are
#   multiples of the line bet (payline games) or of the ways unit.
# @param weight scalar or per-reel vector of strip weights. On the physical
#   reel strips a weight is the number of copies (rounded, min 1 block); the
#   same weights drive independent draws for cascades and respins.
# @param stack number of consecutive copies per strip block (stacked symbols).
sym <- function(id, glyph, name = id, pays = NULL, weight = 10, cls = "",
                stack = 1L, reels = NULL) {
  list(id = id, glyph = glyph, name = name, pays = pays, weight = weight,
       cls = cls, stack = as.integer(stack), reels = reels)
}

PAYLINES_3x3 <- rbind(
  c(2, 2, 2), c(1, 1, 1), c(3, 3, 3), c(1, 2, 3), c(3, 2, 1)
)

PAYLINES_5x3 <- rbind(
  c(2, 2, 2, 2, 2), c(1, 1, 1, 1, 1), c(3, 3, 3, 3, 3), c(1, 2, 3, 2, 1),
  c(3, 2, 1, 2, 3), c(1, 1, 2, 1, 1), c(3, 3, 2, 3, 3), c(2, 3, 3, 3, 2),
  c(2, 1, 1, 1, 2), c(2, 1, 2, 1, 2), c(2, 3, 2, 3, 2), c(1, 2, 1, 2, 1),
  c(3, 2, 3, 2, 3), c(2, 2, 1, 2, 2), c(2, 2, 3, 2, 2), c(1, 2, 2, 2, 1),
  c(3, 2, 2, 2, 3), c(1, 3, 1, 3, 1), c(3, 1, 3, 1, 3), c(1, 3, 3, 3, 1),
  c(3, 1, 1, 1, 3), c(2, 1, 3, 1, 2), c(2, 3, 1, 3, 2), c(1, 1, 3, 1, 1),
  c(3, 3, 1, 3, 3), c(1, 3, 2, 3, 1), c(3, 1, 2, 1, 3), c(2, 1, 2, 3, 2),
  c(2, 3, 2, 1, 2), c(1, 2, 3, 3, 3)
)

# Build a complete machine definition.
make_slot <- function(id, name, category, tagline, description,
                      reels = 5L, rows = 3L, symbols,
                      wild = NULL, scatter = NULL,
                      n_lines = NULL, ways = FALSE, ways_unit = NULL,
                      scatter_pays = NULL, groups = NULL,
                      play = NULL, init_state = list(), meter = NULL,
                      theme = "", art = NULL, features = character(),
                      volatility = "Medium", scale = 1, rtp = NA_real_,
                      monster = FALSE, dual_screen = FALSE, fs_mult_default = 1) {
  ids <- vapply(symbols, `[[`, "", "id")
  stopifnot(!anyDuplicated(ids))
  names(symbols) <- ids

  # Strip weights: symbols x reels
  W <- matrix(0, length(ids), reels, dimnames = list(ids, NULL))
  for (s in symbols) {
    w <- rep_len(s$weight, reels)
    if (!is.null(s$reels)) w[-s$reels] <- 0
    W[s$id, ] <- w
  }

  # Pay table: symbols x count (1..reels), multiples of line bet / ways unit
  P <- matrix(0, length(ids), reels, dimnames = list(ids, as.character(seq_len(reels))))
  for (s in symbols) if (length(s$pays)) {
    cnt <- as.integer(names(s$pays))
    P[s$id, cnt] <- as.numeric(s$pays)
  }

  lines <- NULL
  if (!ways) {
    base <- if (reels == 3) PAYLINES_3x3 else PAYLINES_5x3
    n_lines <- n_lines %||% nrow(base)
    lines <- base[seq_len(n_lines), seq_len(reels), drop = FALSE]
  }

  def <- list(
    id = id, name = name, category = category, tagline = tagline,
    description = description, reels = as.integer(reels), rows = as.integer(rows),
    symbols = symbols, ids = ids, W = W, P = P,
    wild = wild, scatter = scatter, lines = lines, ways = ways,
    ways_unit = ways_unit %||% (if (ways) 50 else NA_real_),
    scatter_pays = scatter_pays, groups = groups,
    play = play %||% play_standard, init_state = init_state, meter = meter,
    theme = theme, art = art %||% symbols[[1]]$glyph, features = features,
    volatility = volatility, scale = scale, rtp = rtp, monster = monster,
    dual_screen = dual_screen, fs_mult_default = fs_mult_default
  )
  def$strips <- build_strips(def)
  def <- precompute_indices(def)
  def
}

# Pre-computed linear indices so spins & evaluation are fully vectorised.
precompute_indices <- function(def) {
  R <- def$reels; rows <- def$rows
  # reel strips wrapped so a window never needs a modulo
  ext <- lapply(def$strips, function(s) c(s, s[seq_len(rows - 1L)]))
  def$strip_len <- vapply(def$strips, length, integer(1))
  def$strip_all <- unlist(ext, use.names = FALSE)
  def$strip_off <- c(0L, cumsum(vapply(ext, length, integer(1))))[seq_len(R)]
  def$win_idx <- rep(def$strip_off, each = rows) + rep(0:(rows - 1L), R)
  if (!is.null(def$lines)) {
    nl <- nrow(def$lines)
    def$line_idx <- matrix(as.vector(def$lines) + rep((seq_len(R) - 1L) * rows, each = nl), nl, R)
  }
  def
}

# Physical reel strips, built deterministically from the weights so every
# session plays the very same machine.
build_strips <- function(def) {
  with_seed(str_seed(def$id), {
    lapply(seq_len(def$reels), function(r) {
      blocks <- list()
      for (s in def$symbols) {
        w <- def$W[s$id, r]
        if (w <= 0) next
        st <- max(1L, s$stack)
        nb <- max(1L, round(w / st))
        blocks <- c(blocks, rep(list(rep(s$id, st)), nb))
      }
      unlist(blocks[sample.int(length(blocks))], use.names = FALSE)
    })
  })
}

# ---- Spinning ------------------------------------------------------------------

# One uniformly random stop per reel (1-based strip positions).
spin_stops <- function(def) {
  as.integer(runif(def$reels) * def$strip_len) + 1L
}

# The visible rows x reels window for a set of stops.
window_from_stops <- function(def, stops) {
  matrix(def$strip_all[def$win_idx + rep(stops, each = def$rows)], def$rows, def$reels)
}

spin_grid <- function(def) window_from_stops(def, spin_stops(def))

# Independent weighted draws for one reel (used for cascades / respins).
draw_cells <- function(def, reel, n, exclude = NULL) {
  w <- def$W[, reel]
  if (!is.null(exclude)) w[names(w) %in% exclude] <- 0
  if (n <= 0) return(character(0))
  sample(def$ids, n, replace = TRUE, prob = w)
}

# ---- Evaluation ------------------------------------------------------------------

empty_hl <- function(def) matrix(FALSE, def$rows, def$reels)

# Left-to-right payline evaluation (vectorised over lines).
# @param mult optional numeric matrix of per-cell multipliers (multiplier wilds)
# @return list(win, hl, parts)
eval_lines <- function(def, grid, line_bet, mult = NULL) {
  LI <- def$line_idx; R <- def$reels; nl <- nrow(LI)
  S <- matrix(grid[LI], nl, R)
  wild <- def$wild %||% "__none__"
  isw <- S == wild

  # first non-wild symbol on each line (all-wild lines keep the wild)
  fc <- max.col(cbind(!isw, TRUE), ties.method = "first")
  fc[fc > R] <- 1L
  first <- S[cbind(seq_len(nl), fc)]

  m <- S == first | isw
  run  <- max.col(cbind(!m, TRUE), ties.method = "first") - 1L
  wrun <- max.col(cbind(!isw, TRUE), ties.method = "first") - 1L

  fi <- match(first, def$ids)
  pay_sym <- def$P[cbind(fi, pmax(run, 1L))]
  pay_wild <- numeric(nl)
  if (!is.null(def$wild)) {
    k <- wrun > 0
    if (any(k)) pay_wild[k] <- def$P[match(wild, def$ids), wrun[k]]
  }
  use_wild <- pay_wild > pay_sym | first == wild   # pure-wild lines pay as wilds
  pay <- pay_sym; pay[use_wild] <- pay_wild[use_wild]
  cnt <- run; cnt[use_wild] <- wrun[use_wild]
  symv <- first; symv[use_wild] <- wild

  # "ANY" groups (e.g. any mixed BARs) for full-length lines that paid nothing
  if (!is.null(def$groups)) for (g in def$groups) {
    inside <- matrix(S %in% c(g$ids, wild), nl, R)
    full <- rowSums(inside) == R & pay == 0 & rowSums(matrix(S %in% g$ids, nl, R)) > 0
    if (any(full)) {
      pay[full] <- g$pay; cnt[full] <- R; symv[full] <- g$name
    }
  }

  # multiplier wilds: product of multipliers inside the paying run
  lm <- rep(1, nl)
  if (!is.null(mult)) {
    M <- matrix(mult[LI], nl, R)
    M[col(M) > cnt] <- 1
    lm <- round(exp(rowSums(log(M))), 6)
    lm[use_wild] <- 1                  # multipliers apply when wilds substitute
  }

  pay <- pay * lm * line_bet
  hl <- empty_hl(def)
  win_lines <- which(pay > 0)
  parts <- character(0)
  if (length(win_lines)) {
    Lw <- LI[win_lines, , drop = FALSE]
    hl[Lw[col(Lw) <= cnt[win_lines]]] <- TRUE
    nm <- vapply(symv[win_lines], function(x)
      if (x %in% def$ids) def$symbols[[x]]$name else x, "")
    parts <- paste0(tr("Line %d: %d x %s", win_lines, cnt[win_lines], tr(nm)),
                    ifelse(lm[win_lines] > 1, sprintf(" (x%g)", lm[win_lines]), ""))
  }
  list(win = sum(pay), hl = hl, parts = parts, line_pays = pay)
}

# "Ways" evaluation (243 on 5x3, 1024 on 5x4): adjacent reels from the left.
eval_ways <- function(def, grid, unit) {
  wild <- def$wild %||% "__none__"
  hl <- empty_hl(def); win <- 0; parts <- character(0)
  isw <- grid == wild
  for (s in def$ids) {
    if (s == wild || identical(s, def$scatter) || max(def$P[s, ]) <= 0) next
    m <- grid == s | isw
    cnt <- colSums(m)
    run <- if (all(cnt > 0)) def$reels else which(cnt == 0)[1] - 1L
    if (run < 1 || def$P[s, run] <= 0) next
    # pure-wild columns alone don't create a symbol win
    if (!any(grid[, seq_len(run)] == s)) next
    ways <- prod(cnt[seq_len(run)])
    p <- def$P[s, run] * ways * unit
    win <- win + p
    hl[, seq_len(run)] <- hl[, seq_len(run)] | m[, seq_len(run)]
    parts <- c(parts, if (ways > 1) tr("%d x %s, %d ways", run, tr(def$symbols[[s]]$name), ways)
                      else tr("%d x %s, 1 way", run, tr(def$symbols[[s]]$name)))
  }
  list(win = win, hl = hl, parts = parts)
}

# Scatter pays (anywhere on screen, multiples of total bet).
eval_scatter <- function(def, grid, bet) {
  if (is.null(def$scatter)) return(list(win = 0, n = 0L, hl = empty_hl(def), parts = character(0)))
  m <- grid == def$scatter
  n <- sum(m)
  sp <- def$scatter_pays
  pay <- if (!is.null(sp) && n >= 1) sp[min(n, length(sp))] else 0
  pay <- (pay %||% 0) * bet
  list(win = pay, n = n, hl = if (pay > 0) m else empty_hl(def),
       parts = if (pay > 0) tr("%d x %s scatter", n, tr(def$symbols[[def$scatter]]$name)) else character(0))
}

# Full evaluation of a grid (lines or ways + scatter), scaled & multiplied.
eval_grid <- function(def, grid, bet, mult = NULL, fs_mult = 1, extra_mult = 1) {
  if (def$ways) {
    base <- eval_ways(def, grid, bet / def$ways_unit)
  } else {
    base <- eval_lines(def, grid, bet / nrow(def$lines), mult = mult)
  }
  sc <- eval_scatter(def, grid, bet)
  k <- def$scale * fs_mult * extra_mult
  list(win = floor((base$win + sc$win) * k),
       line_win = base$win * k, scatter_win = sc$win * k,
       hl = base$hl | sc$hl, parts = c(base$parts, sc$parts),
       scatters = sc$n)
}

# Fixed feature prize expressed as a multiple of the total bet (scaled).
prize <- function(def, x, bet) floor(x * bet * def$scale)

# ---- Cascades ------------------------------------------------------------------

# Remove highlighted cells, drop survivors and refill from the top.
# @return list(grid, new) where `new` marks freshly dropped cells
cascade_step <- function(def, grid, hl) {
  newm <- matrix(FALSE, def$rows, def$reels)
  for (c in seq_len(def$reels)) {
    keep <- grid[!hl[, c], c]
    n_new <- def$rows - length(keep)
    if (n_new > 0) {
      grid[, c] <- c(draw_cells(def, c, n_new), keep)
      newm[seq_len(n_new), c] <- TRUE
    }
  }
  list(grid = grid, new = newm)
}

# ---- Frames (animation payloads) ---------------------------------------------------

# One animation frame for the front end.
# @param cls optional character matrix of extra CSS classes per cell
# @param badge optional character matrix of small corner badges ("x3", "2")
# @param mode "spin" (full reel spin) or "swap" (in-place update, e.g. respin)
frame <- function(def, grid, hl = NULL, cls = NULL, badge = NULL, note = "",
                  win = 0, mode = "spin", hold = NULL) {
  list(grid = grid,
       hl = hl %||% empty_hl(def),
       cls = cls %||% matrix("", def$rows, def$reels),
       badge = badge %||% matrix("", def$rows, def$reels),
       hold = hold %||% matrix(FALSE, def$rows, def$reels),
       note = note, win = win, mode = mode)
}

# ---- Standard play & free-spin framework ----------------------------------------------

# Result constructor with defaults so every play() returns the same shape.
slot_result <- function(ctx, win = 0, frames = list(), gs = ctx$gs, fs_award = 0L,
                        fs_mult = NULL, bonus = NULL, parts = character(0),
                        events = character(0), jp_contrib = NULL, note = "") {
  list(win = win, frames = frames, gs = gs, fs_award = as.integer(fs_award),
       fs_mult = fs_mult, bonus = bonus, parts = parts, events = events,
       jp_contrib = jp_contrib, note = note, unscaled = 0)
}

# Free spins awarded for `n` scatters according to a lookup (index = count).
scatter_fs <- function(n, table = c(0, 0, 10, 15, 25)) {
  if (n < 1) return(0L)
  as.integer(table[min(n, length(table))])
}

# Default play: spin, evaluate, award free spins on 3+ scatters.
play_standard <- function(ctx) {
  def <- ctx$def
  grid <- spin_grid(def)
  ev <- eval_grid(def, grid, ctx$bet, fs_mult = ctx$fs_mult)
  fs <- scatter_fs(ev$scatters)
  slot_result(ctx, win = ev$win,
              frames = list(frame(def, grid, ev$hl, win = ev$win)),
              fs_award = fs, parts = ev$parts,
              events = if (fs > 0) "free_spins" else character(0))
}

slot_init_state <- function(def) {
  c(list(fs_left = 0L, fs_total = 0L, fs_bet = 0, fs_mult = 1, fs_won = 0,
         fs_played = 0L, spins = 0L, total_bet = 0, total_win = 0),
    def$init_state)
}

# Start (or extend) a free-spin round.
start_free_spins <- function(gs, n, bet, mult = 1) {
  if ((gs$fs_left %||% 0) > 0) {
    gs$fs_left <- gs$fs_left + n; gs$fs_total <- gs$fs_total + n
  } else {
    gs$fs_left <- as.integer(n); gs$fs_total <- as.integer(n)
    gs$fs_bet <- bet; gs$fs_mult <- mult; gs$fs_won <- 0; gs$fs_played <- 0L
  }
  gs
}

# Play one spin of a machine including free-spin accounting.
# @param gs per-game persistent state (meters, sticky wilds, free spins...)
# @param jackpots current linked progressive pools (bet multiples), if any
# @return result list with `cost` (coins to debit) and `win` (coins to credit)
slot_play <- function(def, bet, gs = NULL, now = now_secs(), jackpots = NULL) {
  gs <- gs %||% slot_init_state(def)
  free <- (gs$fs_left %||% 0L) > 0L
  use_bet <- if (free) gs$fs_bet else bet
  ctx <- list(def = def, bet = use_bet, free = free,
              fs_mult = if (free) gs$fs_mult else 1,
              gs = gs, now = now, jackpots = jackpots)
  res <- def$play(ctx)
  g <- res$gs
  g$spins <- (g$spins %||% 0L) + 1L
  if (!free) g$total_bet <- (g$total_bet %||% 0) + bet
  g$total_win <- (g$total_win %||% 0) + res$win
  if (free) {
    g$fs_left <- g$fs_left - 1L
    g$fs_played <- g$fs_played + 1L
    g$fs_won <- g$fs_won + res$win
  }
  if (res$fs_award > 0) {
    if (free) res$events <- c(res$events, "retrigger")
    g <- start_free_spins(g, res$fs_award, use_bet, res$fs_mult %||% def$fs_mult_default)
  }
  res$gs <- g
  res$cost <- if (free) 0 else bet
  res$bet <- use_bet
  res$free <- free
  res$fs_ended <- free && g$fs_left == 0L && res$fs_award == 0L
  res
}

# ---- Interactive bonus games ------------------------------------------------------------
# A bonus is a plain list; bonus_pick() advances it. The Shiny module renders
# `tiles` and forwards clicks; the RTP simulator picks at random.

new_bonus <- function(type, title, subtitle, tiles, face = "\u2753", bet = 0,
                      picks_left = NA_integer_, ...) {
  c(list(type = type, title = title, subtitle = subtitle, face = face,
         tiles = tiles, picks_left = picks_left, total = 0, mult = 1,
         done = FALSE, bet = bet, log = character(0), level = 1L,
         fs_award = 0L, fs_mult = 1, jackpot = NULL), list(...))
}

# tiles: data.frame(kind, value, label, revealed)
bonus_tiles <- function(kind, value, label) {
  data.frame(kind = kind, value = value, label = label, revealed = FALSE,
             stringsAsFactors = FALSE)
}

# Advance a bonus by picking tile `i`.
bonus_pick <- function(b, i) {
  if (isTRUE(b$done)) return(b)
  i <- as.integer(i)
  if (is.na(i) || i < 1 || i > nrow(b$tiles) || b$tiles$revealed[i]) return(b)
  t <- b$tiles[i, ]
  b$tiles$revealed[i] <- TRUE

  if (b$type == "pick_collect") {            # City of Queens
    if (t$kind == "collect") {
      b$done <- TRUE
      b$log <- c(b$log, tr("COLLECT!"))
    } else if (t$kind == "mult") {
      b$mult <- b$mult * t$value
      b$log <- c(b$log, tr("Multiplier x%g", t$value))
    } else {
      b$total <- b$total + t$value
      b$log <- c(b$log, sprintf("+%s", fmt_coins(t$value)))
    }
    if (all(b$tiles$revealed[b$tiles$kind != "collect"])) b$done <- TRUE
    if (b$done) b$total <- floor(b$total * b$mult)

  } else if (b$type == "trail") {             # Purrymid Prince tomb raid
    if (t$kind == "trap") {
      b$done <- TRUE
      b$log <- c(b$log, tr("Chamber %d: the mummy awakens!", b$level))
    } else {
      b$total <- b$total + t$value
      b$log <- c(b$log, tr("Chamber %d: +%s", b$level, fmt_coins(t$value)))
      if (b$level >= b$levels) {
        b$total <- b$total + b$treasure
        b$log <- c(b$log, tr("Pharaoh's treasure +%s", fmt_coins(b$treasure)))
        b$done <- TRUE
      } else {
        b$level <- b$level + 1L
        b$prev_tiles <- b$tiles
        b$tiles <- b$make_level(b$level, b$bet)
      }
    }

  } else if (b$type == "shots") {            # Wild West shooting gallery
    b$total <- b$total + t$value
    b$picks_left <- b$picks_left - 1L
    b$log <- c(b$log, tr("Bang! +%s", fmt_coins(t$value)))
    if (b$picks_left <= 0 || all(b$tiles$revealed)) b$done <- TRUE

  } else if (b$type == "choose") {           # Frankenstein lab / Lucky cards
    b$total <- b$total + t$value
    b$tiles$revealed <- TRUE
    b$chosen <- i
    b$log <- c(b$log, if (t$kind == "coin") tr("You won %s!", fmt_coins(t$value)) else t$label)
    b$done <- TRUE

  } else if (b$type == "volatility") {       # 3 Tigers free-spin choice
    b$tiles$revealed <- TRUE
    b$chosen <- i
    b$fs_award <- as.integer(t$value)
    b$fs_mult <- b$options_mult[i]
    b$log <- c(b$log, t$label)
    b$done <- TRUE

  } else if (b$type == "wheel") {            # Mermaid's Pearl jackpot wheel
    seg <- b$segments
    k <- sample.int(nrow(seg), 1L, prob = seg$weight)
    b$segment <- k
    b$jackpot <- seg$label[k]
    b$total <- floor(seg$value[k] * b$bet)
    b$log <- c(b$log, tr("%s JACKPOT: %s", tr(seg$label[k]), fmt_coins(b$total)))
    b$done <- TRUE

  } else if (b$type == "match3") {           # Bank of Jackpots vault
    shown <- b$tiles$label[b$tiles$revealed]
    tab <- table(shown)
    if (any(tab >= 3)) {
      b$jackpot <- names(tab)[tab >= 3][1]
      b$done <- TRUE
      b$log <- c(b$log, tr("%s JACKPOT!", tr(toupper(b$jackpot))))
    }
  }
  b
}

# Play a bonus to completion with random picks (simulation / auto-play).
bonus_autoplay <- function(b) {
  guard <- 0
  while (!isTRUE(b$done) && guard < 100) {
    open <- which(!b$tiles$revealed)
    if (!length(open)) break
    b <- bonus_pick(b, open[sample.int(length(open), 1)])
    guard <- guard + 1
  }
  b
}

# Settle a finished bonus. Shared by the Shiny module and the RTP simulator.
# @param jackpots current linked progressive pools (bet multiples)
# @return list(win, gs, jackpot, unscaled) -- `jackpot` names a tier that was
#   won; `unscaled` is the part of `win` not subject to the machine's pay scale
bonus_settle <- function(def, gs, b, jackpots = NULL) {
  win <- b$total %||% 0
  jp <- NULL; unscaled <- 0
  if (identical(b$type, "match3")) {
    jp <- b$jackpot %||% b$planned
    pots <- jackpots %||% JACKPOT_SEEDS
    win <- floor(pots[[jp]] * b$bet)
    unscaled <- win
  } else if (identical(b$type, "volatility")) {
    gs <- start_free_spins(gs, b$fs_award, b$bet, b$fs_mult)
  } else if (identical(b$type, "wheel")) {
    jp <- b$jackpot
  }
  list(win = win, gs = gs, jackpot = jp, unscaled = unscaled)
}
