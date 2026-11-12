# ---------------------------------------------------------------------------
# fct_stats.R -- player statistics and player classification.
#
# Every finished play is appended to a compact "track" kept in the player
# state (so it is saved with the profile, in every storage backend):
#   * a ring buffer of the last STAT_TRACK_CAP plays (time, balance after the
#     play, wager, net result, category);
#   * lifetime aggregates: active play time, balance peak, maximum drawdown,
#     wager and plays per category, wager on high-risk games, sessions.
# From the track we derive
#   * drawdown  -- peak-to-trough fall of the balance (absolute and %),
#   * tendency  -- balance trend, bet trend, loss chasing, streaks,
#   * category  -- favourite game family, stake class and play style,
#   * classification -- a k-nearest-neighbours classifier (own implementation,
#     standardised features) trained on a synthetic set of player archetypes,
#     and a k-means clustering (stats::kmeans) of the community of players
#     stored in the database.
# Everything here is pure R (no Shiny), so it is unit-tested directly.
# ---------------------------------------------------------------------------

# Categories used by the statistics (slot categories + table/side families).
STAT_CATEGORIES <- c(classic = "Classic & Vegas slots", myth = "Mythological slots",
                     animal = "Animal 3D slots", blackjack = "Blackjack", poker = "Video Poker",
                     roulette = "Roulette", side = "Side games")
STAT_TABLE_CATS <- c("blackjack", "poker", "roulette")
STAT_RISKY_VOL <- c("High", "Medium-High", "Very High")   # "high-risk" slot volatilities
STAT_TRACK_CAP <- 500L      # plays kept in the ring buffer
STAT_IDLE_SECS <- 120       # a gap longer than this is idle time, not play time
STAT_PLAY_SECS <- 5         # time credited for a play that follows an idle gap
STAT_MIN_PLAYS <- 20L       # plays needed before the player is classified
STAT_MAX_SESSIONS <- 20L    # recent sessions kept in the profile

# ---- the track ---------------------------------------------------------------------

stat_zero <- function() stats::setNames(rep(0, length(STAT_CATEGORIES)), names(STAT_CATEGORIES))

track_new <- function(t0 = now_secs()) {
  list(t0 = t0,                                        # time origin (epoch seconds)
       t = numeric(0), bal = numeric(0), wager = numeric(0), net = numeric(0), cat = integer(0),
       n = 0, active = 0, last_t = 0, online = 0,
       peak = 0, max_dd = 0, max_dd_pct = 0,
       wager_cat = stat_zero(), plays_cat = stat_zero(), risky = 0,
       sessions = list())
}

# Category of a play (slot category for slots, else the family).
stat_category <- function(family, category = NULL) {
  cat <- if (identical(family, "slots") && !is.null(category)) category else family
  if (cat %in% names(STAT_CATEGORIES)) cat else "side"
}

# Is a play "high-risk" (high-volatility slot or a double-or-nothing gamble)?
stat_is_risky <- function(family, game) {
  if (identical(game, "double_up")) return(TRUE)
  if (!identical(family, "slots")) return(FALSE)
  d <- slot_registry()[[game]]
  !is.null(d) && isTRUE(d$volatility %in% STAT_RISKY_VOL)
}

# Append one play to the track.
# @param balance coin balance right after the play
track_add <- function(tk, t, balance, wager, win, cat, risky = FALSE) {
  net <- win - wager
  before <- balance - net
  # active play time: gaps up to STAT_IDLE_SECS count, longer gaps are idle
  gap <- if (tk$last_t > 0) t - tk$last_t else Inf
  tk$active <- tk$active + if (is.finite(gap) && gap >= 0 && gap <= STAT_IDLE_SECS) gap else STAT_PLAY_SECS
  tk$last_t <- t
  # peak & drawdown of the balance
  tk$peak <- max(tk$peak, before, balance)
  dd <- tk$peak - balance
  tk$max_dd <- max(tk$max_dd, dd)
  if (tk$peak > 0) tk$max_dd_pct <- max(tk$max_dd_pct, dd / tk$peak)
  # aggregates
  tk$n <- tk$n + 1
  tk$wager_cat[[cat]] <- tk$wager_cat[[cat]] + wager
  tk$plays_cat[[cat]] <- tk$plays_cat[[cat]] + 1
  if (risky) tk$risky <- tk$risky + wager
  # ring buffer (times stored relative to t0, whole seconds)
  keep <- function(v, x) utils::tail(c(v, x), STAT_TRACK_CAP)
  tk$t <- keep(tk$t, round(t - tk$t0))
  tk$bal <- keep(tk$bal, round(balance))
  tk$wager <- keep(tk$wager, round(wager))
  tk$net <- keep(tk$net, round(net))
  tk$cat <- keep(tk$cat, match(cat, names(STAT_CATEGORIES)))
  tk
}

# Rebuild a track from a saved profile (JSON lists); invalid data -> new track.
track_restore <- function(x) {
  tk <- track_new()
  if (!is.list(x)) return(tk)
  num <- function(v, d = 0) { v <- suppressWarnings(as.numeric(unlist(v))); if (length(v) != 1 || is.na(v)) d else v }
  vec <- function(v) { v <- suppressWarnings(as.numeric(unlist(v))); v[is.na(v)] <- 0; v }
  tk$t0 <- num(x$t0, tk$t0)
  cols <- list(t = vec(x$t), bal = vec(x$bal), wager = vec(x$wager), net = vec(x$net), cat = vec(x$cat))
  len <- unique(lengths(cols))
  if (length(len) == 1) {
    n <- min(len, STAT_TRACK_CAP)
    for (k in names(cols)) tk[[k]] <- utils::tail(cols[[k]], n)
    tk$cat <- as.integer(pmin(pmax(tk$cat, 1), length(STAT_CATEGORIES)))
  }
  for (k in c("n", "active", "last_t", "online", "peak", "max_dd", "max_dd_pct", "risky")) tk[[k]] <- max(0, num(x[[k]]))
  tk$n <- max(tk$n, length(tk$bal))
  for (k in c("wager_cat", "plays_cat")) {
    v <- unlist(x[[k]])
    if (length(v)) {
      hit <- intersect(names(v), names(STAT_CATEGORIES))
      tk[[k]][hit] <- pmax(0, suppressWarnings(as.numeric(v[hit])))
      tk[[k]][is.na(tk[[k]])] <- 0
    }
  }
  ss <- Filter(is.list, x$sessions %||% list())
  tk$sessions <- utils::head(lapply(ss, function(s) list(
    start = num(s$start), seconds = num(s$seconds), plays = num(s$plays),
    start_balance = num(s$start_balance), end_balance = num(s$end_balance))), STAT_MAX_SESSIONS)
  tk
}

# Serialisable form of a track (for the saved profile).
track_snapshot <- function(tk) {
  tk$wager_cat <- as.list(tk$wager_cat)
  tk$plays_cat <- as.list(tk$plays_cat)
  tk
}

# Add a finished session (newest first).
track_add_session <- function(tk, start, seconds, plays, start_balance, end_balance) {
  s <- list(start = start, seconds = round(seconds), plays = plays,
            start_balance = round(start_balance), end_balance = round(end_balance))
  tk$sessions <- utils::head(c(list(s), tk$sessions), STAT_MAX_SESSIONS)
  tk$online <- tk$online + round(seconds)
  tk
}

# ---- drawdown ---------------------------------------------------------------------

# Running peak and drawdown of a balance series.
drawdown_series <- function(bal) {
  if (!length(bal)) return(data.frame(bal = numeric(0), peak = numeric(0), dd = numeric(0), dd_pct = numeric(0)))
  peak <- cummax(bal)
  dd <- peak - bal
  data.frame(bal = bal, peak = peak, dd = dd, dd_pct = ifelse(peak > 0, dd / peak, 0))
}

# ---- tendency -------------------------------------------------------------------------

# Ordinary least squares slope of y over 1..n.
ols_slope <- function(y) {
  n <- length(y)
  if (n < 2) return(0)
  x <- seq_len(n)
  sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
}

# Tendency of the recent play.
# @return list(balance = "rising"/"falling"/"steady", slope_pct (balance % per
#   100 plays), bet = "increasing"/"decreasing"/"stable", bet_change,
#   chase (mean bet after a loss / after a win), chase_label, streak, rtp, window)
stats_tendency <- function(tk, window = 100L) {
  n <- length(tk$bal)
  idx <- utils::tail(seq_len(n), window)
  bal <- tk$bal[idx]
  slope <- ols_slope(bal)
  lvl <- max(1, mean(bal))
  slope_pct <- if (length(bal) >= 2) 100 * slope * 100 / lvl else 0
  balance <- if (length(bal) < 10) "unknown" else if (slope_pct > 2) "rising" else if (slope_pct < -2) "falling" else "steady"

  w <- tk$wager; paid <- which(w > 0)
  recent <- utils::tail(paid, 50); prev <- utils::tail(setdiff(paid, recent), 50)
  bet_change <- if (length(recent) >= 10 && length(prev) >= 10) mean(w[recent]) / mean(w[prev]) - 1 else 0
  bet <- if (length(recent) < 10 || length(prev) < 10) "unknown" else if (bet_change > 0.15) "increasing"
         else if (bet_change < -0.15) "decreasing" else "stable"

  chase <- chase_ratio(w, tk$net)
  chase_label <- if (is.na(chase)) "unknown" else if (chase > 1.15) "chasing" else if (chase < 0.87) "cautious" else "even"

  # current streak of winning (net > 0) or losing (net < 0) paid plays
  res <- sign(tk$net[paid])
  streak <- 0L
  if (length(res)) {
    last <- res[length(res)]
    k <- length(res)
    while (k >= 1 && res[k] == last) k <- k - 1L
    streak <- as.integer((length(res) - k) * last)
  }
  wr <- sum(w[idx]); rtp <- if (wr > 0) sum(w[idx] + tk$net[idx]) / wr else NA_real_
  list(balance = balance, slope_pct = slope_pct, bet = bet, bet_change = bet_change,
       chase = chase, chase_label = chase_label, streak = streak, rtp = rtp, window = length(idx),
       net = sum(tk$net[idx]))
}

# Mean bet after a losing play divided by the mean bet after a winning play.
chase_ratio <- function(wager, net, min_n = 5L) {
  n <- length(wager)
  if (n < 2) return(NA_real_)
  prev <- net[-n]; nxt <- wager[-1]
  ok <- nxt > 0 & wager[-n] > 0
  al <- nxt[ok & prev < 0]; aw <- nxt[ok & prev > 0]
  if (length(al) < min_n || length(aw) < min_n) return(NA_real_)
  mean(al) / mean(aw)
}

# ---- category ---------------------------------------------------------------------------

STAKE_CLASSES <- data.frame(
  min = c(0, 500, 2500, 10000, 50000),
  label = c("Micro stakes", "Low stakes", "Mid stakes", "High stakes", "VIP stakes"),
  stringsAsFactors = FALSE)

stake_class <- function(avg_bet) STAKE_CLASSES$label[max(which(STAKE_CLASSES$min <= max(0, avg_bet)))]

# Favourite category (largest share of wagered coins), stake class and style.
stats_category <- function(tk) {
  wc <- tk$wager_cat
  tot <- sum(wc)
  share <- if (tot > 0) wc / tot else wc
  fav <- if (tot > 0) names(which.max(wc)) else if (sum(tk$plays_cat) > 0) names(which.max(tk$plays_cat)) else NA_character_
  table_share <- sum(share[STAT_TABLE_CATS])
  style <- if (tot <= 0) "unknown" else if (table_share >= 0.65) "tables" else if (table_share <= 0.2) "slots" else "allround"
  paid <- tk$wager[tk$wager > 0]
  avg <- if (length(paid)) mean(paid) else 0
  list(favourite = fav, share = share, style = style, avg_bet = avg,
       stake = if (length(paid)) stake_class(avg) else NA_character_)
}

# ---- features & classification ------------------------------------------------------------

STAT_FEATURES <- c(bet = "Average bet", pace = "Plays per minute", risk = "High-risk share",
                   table = "Table-game share", escalation = "Bet escalation after losses")

# Numeric behaviour features, or NULL with fewer than STAT_MIN_PLAYS plays.
stats_features <- function(tk) {
  n <- length(tk$bal)
  if (n < STAT_MIN_PLAYS) return(NULL)
  w <- tk$wager
  paid <- w[w > 0]
  bet <- log10(max(1, if (length(paid)) mean(paid) else 1))
  g <- diff(tk$t)
  g <- g[g >= 0]
  active_min <- sum(pmin(g, STAT_IDLE_SECS)) / 60
  pace <- if (active_min > 0) min(40, length(g) / active_min) else 40
  tot <- sum(tk$wager_cat)
  risk <- if (tot > 0) min(1, tk$risky / tot) else 0
  table <- if (tot > 0) sum(tk$wager_cat[STAT_TABLE_CATS]) / tot else 0
  ch <- chase_ratio(w, tk$net)
  esc <- if (is.na(ch)) 0 else max(-1, min(1, log(ch)))
  c(bet = bet, pace = pace, risk = risk, table = table, escalation = esc)
}

# Player archetypes: centre of each feature (see STAT_FEATURES) and the spread
# used to generate the synthetic training set.
ARCHETYPES <- data.frame(
  id = c("casual", "grinder", "thrill", "roller", "shark"),
  label = c("Casual Spinner", "Grinder", "Thrill Seeker", "High Roller", "Card Shark"),
  icon = c("\U0001F388", "\u2699\uFE0F", "\U0001F3A2", "\U0001F48E", "\U0001F988"),
  desc = c("Small bets at a relaxed pace - playing for the fun of it.",
           "Fast, steady volume at modest stakes. The machines know you well.",
           "Chases the big hits: high-volatility machines and double-or-nothing gambles.",
           "Big stakes, measured pace. The VIP room is yours.",
           "Prefers the tables - skill and strategy over spinning reels."),
  bet = c(2.6, 3.0, 3.5, 4.6, 3.5),
  pace = c(4, 14, 9, 6, 4),
  risk = c(0.3, 0.25, 0.75, 0.4, 0.1),
  table = c(0.1, 0.1, 0.1, 0.3, 0.8),
  escalation = c(0, 0, 0.25, 0.05, -0.05),
  stringsAsFactors = FALSE)
ARCHETYPE_SD <- c(bet = 0.35, pace = 3, risk = 0.15, table = 0.12, escalation = 0.1)

.stats_cache <- new.env(parent = emptyenv())

# Synthetic labelled reference set: `per` players around each archetype.
# Deterministic and RNG-free (so it never touches the session's random stream):
# every feature uses the normal quantiles qnorm((1:per - 0.5) / per), visited in
# a different order per feature (a stratified, Latin-hypercube style sample),
# then clipped to the valid range of the feature. Cached.
archetype_reference <- function(per = 60L) {
  key <- as.character(per)
  if (!is.null(.stats_cache$ref[[key]])) return(.stats_cache$ref[[key]])
  f <- names(ARCHETYPE_SD)
  z <- stats::qnorm((seq_len(per) - 0.5) / per)
  steps <- c(7L, 11L, 13L, 17L, 19L, 23L, 29L, 31L)          # coprime with typical `per`
  steps <- steps[per %% steps != 0]
  perm <- function(k, i) ((seq_len(per) - 1L) * steps[k] + 3L * i + k) %% per + 1L
  ref <- do.call(rbind, lapply(seq_len(nrow(ARCHETYPES)), function(i) {
    m <- vapply(seq_along(f), function(k) ARCHETYPES[[f[k]]][i] + ARCHETYPE_SD[[f[k]]] * z[perm(k, i)], numeric(per))
    colnames(m) <- f
    data.frame(m, label = ARCHETYPES$id[i], stringsAsFactors = FALSE)
  }))
  ref$bet <- pmin(5.5, pmax(1.5, ref$bet)); ref$pace <- pmin(40, pmax(0.5, ref$pace))
  ref$risk <- pmin(1, pmax(0, ref$risk)); ref$table <- pmin(1, pmax(0, ref$table))
  ref$escalation <- pmin(1, pmax(-1, ref$escalation))
  if (is.null(.stats_cache$ref)) .stats_cache$ref <- list()
  .stats_cache$ref[[key]] <- ref
  ref
}

# Standardisation (centre/scale) learned from the reference set.
feature_scaler <- function(ref = archetype_reference()) {
  f <- names(ARCHETYPE_SD)
  list(center = colMeans(ref[f]), scale = apply(ref[f], 2, stats::sd))
}
scale_features <- function(x, sc = feature_scaler()) {
  if (is.null(dim(x))) x <- rbind(x)                   # a single named vector
  x <- as.matrix(x)[, names(sc$center), drop = FALSE]
  sweep(sweep(x, 2, sc$center), 2, sc$scale, "/")
}

# k-nearest-neighbours classification (Euclidean distance on standardised
# features, majority vote; ties broken by the summed inverse distance).
# @param x named numeric feature vector (or a matrix of rows)
# @return list(label, confidence (vote share), votes (named), neighbours)
knn_classify <- function(x, ref = archetype_reference(), k = 9L) {
  sc <- feature_scaler(ref)
  f <- names(ARCHETYPE_SD)
  X <- scale_features(ref[f], sc)
  q <- scale_features(x[f], sc)
  d <- sqrt(rowSums(sweep(X, 2, q[1, ])^2))
  nn <- order(d)[seq_len(min(k, nrow(X)))]
  labs <- ref$label[nn]
  votes <- table(factor(labs, levels = ARCHETYPES$id))
  weight <- tapply(1 / (d[nn] + 1e-9), factor(labs, levels = ARCHETYPES$id), sum)
  weight[is.na(weight)] <- 0
  best <- names(votes)[order(-as.numeric(votes), -as.numeric(weight))][1]
  list(label = best, confidence = as.numeric(votes[best]) / length(nn),
       votes = stats::setNames(as.numeric(votes), names(votes)), distance = mean(d[nn]))
}

# Archetype row for an id.
archetype <- function(id) ARCHETYPES[ARCHETYPES$id == id, , drop = FALSE]

# Deterministic k-means initialisation ("farthest point"): start from the
# player nearest the overall centre, then repeatedly add the player farthest
# from the centres chosen so far. Same data -> same clusters, no RNG needed.
farthest_point_centers <- function(X, k) {
  d2 <- function(p) rowSums(sweep(X, 2, p)^2)
  idx <- which.min(d2(colMeans(X)))
  dmin <- d2(X[idx, ])
  while (length(idx) < k) {
    nxt <- which.max(dmin)
    idx <- c(idx, nxt)
    dmin <- pmin(dmin, d2(X[nxt, ]))
  }
  X[idx, , drop = FALSE]
}

# k-means clustering of a community of players.
# @param feats data.frame with the feature columns (+ player_id)
# @param k clusters (default: between 2 and 4, about one per 4 players)
# @return NULL if there are fewer than `min_players`; else list(cluster (per
#   row), centers (original units), size, label (nearest archetype per cluster))
community_clusters <- function(feats, k = NULL, min_players = 8L) {
  f <- names(ARCHETYPE_SD)
  if (is.null(feats) || nrow(feats) < min_players) return(NULL)
  feats <- feats[stats::complete.cases(feats[f]), , drop = FALSE]
  if (nrow(feats) < min_players) return(NULL)
  sc <- feature_scaler()
  X <- scale_features(feats[f], sc)
  k <- k %||% max(2L, min(4L, nrow(X) %/% 4L))
  k <- min(k, nrow(unique(round(X, 6))))
  if (k < 2) return(NULL)
  km <- stats::kmeans(X, centers = farthest_point_centers(X, k), iter.max = 50)
  centers <- sweep(sweep(km$centers, 2, sc$scale, "*"), 2, sc$center, "+")
  labels <- apply(centers, 1, function(cn) knn_classify(stats::setNames(cn, f))$label)
  list(cluster = unname(km$cluster), centers = centers, size = as.integer(km$size),
       label = unname(labels), player_id = feats$player_id %||% NULL,
       betweenss = km$betweenss / km$totss)
}

# ---- the full report ------------------------------------------------------------------------------

# Everything the stats page shows, from a track and the current balance.
# @param session list(t0 = start time) of the current session, or NULL
stats_report <- function(tk, coins, now = now_secs(), session = NULL) {
  f <- stats_features(tk)
  cls <- if (is.null(f)) NULL else knn_classify(f)
  wagered <- sum(tk$wager_cat)
  cur_dd <- max(0, tk$peak - coins)
  list(
    plays = tk$n, wagered = wagered,
    active = tk$active,
    online = tk$online + if (!is.null(session)) max(0, now - session$t0) else 0,
    peak = tk$peak, max_dd = tk$max_dd, max_dd_pct = tk$max_dd_pct,
    cur_dd = cur_dd, cur_dd_pct = if (tk$peak > 0) cur_dd / tk$peak else 0,
    tendency = stats_tendency(tk), category = stats_category(tk),
    features = f, class = cls, need = max(0L, STAT_MIN_PLAYS - length(tk$bal)))
}

# ---- SVG balance chart --------------------------------------------------------------------------

# Balance line with running peak and the drawdown area shaded (inline SVG).
stats_balance_svg <- function(bal, width = 1000, height = 280, pad = 40) {
  if (length(bal) < 2) return(NULL)
  ds <- drawdown_series(bal)
  n <- length(bal)
  lo <- min(bal); hi <- max(ds$peak)
  if (hi - lo < 1) { hi <- hi + 1; lo <- lo - 1 }
  X <- function(i) pad + (i - 1) / (n - 1) * (width - 2 * pad)
  Y <- function(v) height - pad - (v - lo) / (hi - lo) * (height - 2 * pad)
  xy <- function(i, v) sprintf("%.1f,%.1f", X(i), Y(v))
  pts <- function(v) paste(xy(seq_len(n), v), collapse = " ")
  esc <- htmltools::htmlEscape
  up <- bal[n] >= bal[1]
  grid_v <- pretty(c(lo, hi), 4)
  grid_v <- grid_v[grid_v >= lo & grid_v <= hi]
  grid <- paste(sprintf('<line class="sc-grid" x1="%d" x2="%d" y1="%.1f" y2="%.1f"/><text class="sc-axis" x="2" y="%.1f">%s</text>',
                        pad, width - pad, Y(grid_v), Y(grid_v), Y(grid_v) + 4, esc(fmt_short(grid_v))), collapse = "")
  fill <- paste(c(xy(seq_len(n), bal), sprintf("%.1f,%d", X(n), height - pad), sprintf("%.1f,%d", X(1), height - pad)), collapse = " ")
  dd_area <- paste(c(xy(seq_len(n), ds$peak), rev(xy(seq_len(n), ds$bal))), collapse = " ")
  worst <- which.max(ds$dd)
  # label the worst drawdown beside the middle of its marker (left of it near the right edge)
  right <- X(worst) > width * 0.62
  mark <- if (ds$dd[worst] > 0) sprintf(
    '<line class="sc-ddmark" x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f"/><text class="sc-ddtext" x="%.1f" y="%.1f" text-anchor="%s">%s</text>',
    X(worst), X(worst), Y(ds$peak[worst]), Y(ds$bal[worst]), X(worst) + if (right) -8 else 8,
    (Y(ds$peak[worst]) + Y(ds$bal[worst])) / 2 + 4, if (right) "end" else "start",
    esc(tr("max drawdown -%s", fmt_short(ds$dd[worst])))) else ""
  col <- if (up) "#1fd18b" else "#ff4d6d"
  HTML(paste0(
    sprintf('<svg class="stats-chart" viewBox="0 0 %d %d" role="img" aria-label="%s">', width, height, esc(tr("Balance history"))),
    sprintf('<defs><linearGradient id="fcBalFill" x1="0" y1="0" x2="0" y2="1"><stop offset="0%%" stop-color="%s" stop-opacity=".35"/><stop offset="100%%" stop-color="%s" stop-opacity="0"/></linearGradient></defs>', col, col),
    grid,
    sprintf('<polygon class="sc-fill" points="%s"/>', fill),
    sprintf('<polygon class="sc-dd" points="%s"/>', dd_area),
    sprintf('<polyline class="sc-peak" points="%s"/>', pts(ds$peak)),
    sprintf('<polyline class="sc-line %s" points="%s"/>', if (up) "up" else "down", pts(bal)),
    mark,
    sprintf('<circle class="sc-dot %s" cx="%.1f" cy="%.1f" r="4"/>', if (up) "up" else "down", X(n), Y(bal[n])),
    sprintf('<text class="sc-axis" x="%d" y="%d">%s</text>', pad, height - 8, esc(tr("%d plays ago", n - 1L))),
    sprintf('<text class="sc-axis" x="%d" y="%d" text-anchor="end">%s</text>', width - pad, height - 8, esc(tr("now"))),
    '</svg>'))
}
