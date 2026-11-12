# ---------------------------------------------------------------------------
# engine_poker.R -- video poker: deal / hold / draw and hand evaluation for
# Jacks or Better (9/6), Deuces Wild (full pay), Bonus Poker (8/5) and
# Joker Poker (Kings or Better, 53-card deck).
# Paytables are "per coin"; a 5-coin bet upgrades the Royal Flush to 800/coin.
# ---------------------------------------------------------------------------

vp_paytable <- function(...) {
  x <- list(...)
  data.frame(key = names(x), label = vapply(x, `[[`, "", 1),
             pay = vapply(x, function(v) as.numeric(v[[2]]), 0),
             stringsAsFactors = FALSE)
}

VP_GAMES <- list(
  jacks = list(
    name = "Jacks or Better", wild = "none", deck_jokers = 0L,
    blurb = "The classic: any pair of Jacks or better pays. 9/6 full-pay table.",
    table = vp_paytable(royal = list("Royal Flush", 250), sf = list("Straight Flush", 50),
                        four = list("Four of a Kind", 25), fh = list("Full House", 9),
                        flush = list("Flush", 6), straight = list("Straight", 4),
                        three = list("Three of a Kind", 3), two_pair = list("Two Pair", 2),
                        jacks = list("Jacks or Better", 1))),
  deuces = list(
    name = "Deuces Wild", wild = "deuces", deck_jokers = 0L,
    blurb = "Every 2 is wild! Minimum paying hand is Three of a Kind.",
    table = vp_paytable(royal = list("Natural Royal Flush", 250), four_deuces = list("Four Deuces", 200),
                        wild_royal = list("Wild Royal Flush", 25), five = list("Five of a Kind", 15),
                        sf = list("Straight Flush", 9), four = list("Four of a Kind", 5),
                        fh = list("Full House", 3), flush = list("Flush", 2),
                        straight = list("Straight", 2), three = list("Three of a Kind", 1))),
  bonus = list(
    name = "Bonus Poker", wild = "none", deck_jokers = 0L,
    blurb = "Jacks or Better with bonus pays for Four Aces (80) and Four 2s-4s (40).",
    table = vp_paytable(royal = list("Royal Flush", 250), sf = list("Straight Flush", 50),
                        four_aces = list("Four Aces", 80), four_low = list("Four 2s, 3s or 4s", 40),
                        four = list("Four 5s thru Ks", 25), fh = list("Full House", 8),
                        flush = list("Flush", 5), straight = list("Straight", 4),
                        three = list("Three of a Kind", 3), two_pair = list("Two Pair", 2),
                        jacks = list("Jacks or Better", 1))),
  joker = list(
    name = "Joker Poker", wild = "joker", deck_jokers = 1L,
    blurb = "53-card deck with one wild Joker. Kings or Better to qualify.",
    table = vp_paytable(royal = list("Natural Royal Flush", 250), five = list("Five of a Kind", 200),
                        wild_royal = list("Wild Royal Flush", 100), sf = list("Straight Flush", 50),
                        four = list("Four of a Kind", 20), fh = list("Full House", 7),
                        flush = list("Flush", 5), straight = list("Straight", 3),
                        three = list("Three of a Kind", 2), two_pair = list("Two Pair", 1),
                        kings = list("Kings or Better", 1)))
)

# Is a set of distinct natural ranks completable to a straight with wilds?
# (Ace counts high or low.) `r` are ranks 1..13.
straight_possible <- function(r) {
  if (!length(r)) return(TRUE)
  if (anyDuplicated(r)) return(FALSE)
  hi <- ifelse(r == 1L, 14L, r)
  (max(hi) - min(hi) <= 4L) || (max(r) - min(r) <= 4L)
}

# Evaluate a 5-card hand for the given game.
# @return the paytable key of the hand ("" = no win)
vp_evaluate <- function(cards, game) {
  g <- if (is.character(game)) VP_GAMES[[game]] else game
  wmask <- switch(g$wild,
                  deuces = cards != JOKER & card_rank(cards) == 2L,
                  joker = cards == JOKER,
                  rep(FALSE, length(cards)))
  w <- sum(wmask)
  nat <- cards[!wmask]
  r <- card_rank(nat); s <- card_suit(nat)
  cnt <- if (length(r)) sort(tabulate(r, 13L), decreasing = TRUE) else c(0L, 0L)
  c1 <- cnt[1]; c2 <- cnt[2]
  flush <- length(unique(s)) <= 1L
  straight <- straight_possible(r)
  royal_ranks <- all(r %in% c(1L, 10L, 11L, 12L, 13L))
  royal <- flush && straight && royal_ranks && !anyDuplicated(r)
  has <- function(k) k %in% g$table$key

  if (g$wild == "deuces" && w == 4L) return("four_deuces")
  if (royal && w == 0L) return("royal")
  if (has("five") && w > 0L && c1 + w >= 5L) {
    # Deuces: five of a kind outranks a wild royal in pay only in Joker Poker
    if (g$wild == "joker") return("five")
  }
  if (royal && w > 0L && has("wild_royal")) return("wild_royal")
  if (has("five") && c1 + w >= 5L) return("five")
  if (flush && straight) return("sf")
  if (c1 + w >= 4L) {
    if (g$wild == "none" && has("four_aces")) {
      qr <- which(tabulate(r, 13L) == 4L)
      if (qr == 1L) return("four_aces")
      if (qr %in% 2:4) return("four_low")
    }
    return("four")
  }
  if ((c1 == 3L && c2 == 2L && w == 0L) || (w == 1L && c1 == 2L && c2 == 2L)) return("fh")
  if (flush) return("flush")
  if (straight) return("straight")
  if (c1 + w >= 3L) return("three")
  if (has("three") && !has("two_pair")) return("")     # Deuces: 3oak minimum
  if (w == 0L && c1 == 2L && c2 == 2L) return("two_pair")
  # High pair
  if (has("jacks")) {
    pr <- which(tabulate(r, 13L) >= 2L)
    if (w == 0L && length(pr) && any(pr == 1L | pr >= 11L)) return("jacks")
  }
  if (has("kings")) {
    pr <- which(tabulate(r, 13L) >= 2L)
    if (w == 0L && length(pr) && any(pr == 1L | pr == 13L)) return("kings")
    if (w == 1L && any(r == 1L | r == 13L)) return("kings")
  }
  ""
}

# Payout in coins for `coins` bet (1-5) at coin value `denom`.
vp_payout <- function(key, game, coins, denom) {
  if (!nzchar(key)) return(0)
  g <- if (is.character(game)) VP_GAMES[[game]] else game
  per <- g$table$pay[g$table$key == key]
  if (key == "royal" && coins >= 5) per <- 800
  per * coins * denom
}

# ---- multi-play rounds ------------------------------------------------------
# Up to VP_MAX_HANDS hands are played at once ("Triple / Ten / Fifty Play"
# style): all hands share the dealt cards and the held cards; every hand then
# draws its replacements from its OWN independent shuffle of the 47 (48 with
# the Joker) cards that were not dealt. Hand 1 draws from the round's deck in
# order, so a single-hand round is ordinary video poker.
VP_MAX_HANDS <- 25L
VP_HAND_CHOICES <- c(1L, 3L, 5L, 10L, 25L)

# @param coins coins per hand (1-5); @param denom coin value; @param hands 1..25
# The round costs hands * coins * denom.
vp_new_round <- function(game, coins, denom, hands = 1L) {
  g <- VP_GAMES[[game]]
  hands <- as.integer(hands)
  stopifnot(length(hands) == 1, hands >= 1L, hands <= VP_MAX_HANDS)
  deck <- new_shoe(1L, g$deck_jokers)
  list(game = game, coins = coins, denom = denom, hands = hands, deck = deck[-(1:5)],
       hand = deck[1:5], held = rep(FALSE, 5), phase = "dealt",
       dealt_key = vp_evaluate(deck[1:5], game), key = "", payout = 0,
       results = NULL)
}

vp_round_cost <- function(st) st$hands * st$coins * st$denom

vp_draw <- function(st, held) {
  stopifnot(st$phase == "dealt")
  held <- as.logical(held)
  k <- sum(!held)
  n <- st$hands %||% 1L
  cards <- matrix(st$hand, nrow = n, ncol = 5, byrow = TRUE)
  if (k > 0) {
    cards[1, !held] <- st$deck[seq_len(k)]
    for (h in seq_len(n)[-1]) cards[h, !held] <- st$deck[sample.int(length(st$deck), k)]
    st$deck <- st$deck[-seq_len(k)]
  }
  keys <- vapply(seq_len(n), function(h) vp_evaluate(cards[h, ], st$game), "")
  pays <- vapply(keys, vp_payout, 0, game = st$game, coins = st$coins, denom = st$denom, USE.NAMES = FALSE)
  st$held <- held
  st$hand <- cards[1, ]
  st$key <- keys[1]
  st$results <- list(cards = cards, keys = keys, payouts = pays)
  st$payout <- sum(pays)
  st$phase <- "done"
  st
}

# Best hand of a finished round (highest payout; ties -> first hand).
vp_best_hand <- function(st) {
  if (is.null(st$results)) return(list(i = 1L, key = st$key, payout = st$payout))
  i <- which.max(st$results$payouts)
  list(i = i, key = st$results$keys[i], payout = st$results$payouts[i])
}

# ---- Double or nothing -----------------------------------------------------------
# Classic "double up": the dealer's card is shown face up next to four face-down
# cards from a fresh 52-card deck. Pick one: higher than the dealer doubles the
# amount, equal is a push (keep it and pick again), lower loses it.
# Aces are high; suits do not matter. Fair game: expected value = stake.
VP_MAX_DOUBLES <- 5L

vp_double_rank <- function(card) { r <- card_rank(card); ifelse(r == 1L, 14L, r) }

vp_double_start <- function(amount, doubles = 0L) {
  deck <- new_shoe(1L, 0L)
  list(amount = amount, dealer = deck[1], cards = deck[2:5], chosen = NA_integer_,
       outcome = "", phase = "pick", doubles = as.integer(doubles))
}

vp_double_pick <- function(d, i) {
  stopifnot(d$phase == "pick", i %in% 1:4)
  mine <- vp_double_rank(d$cards[i]); dealer <- vp_double_rank(d$dealer)
  d$chosen <- as.integer(i)
  d$stake <- d$amount
  d$outcome <- if (mine > dealer) "win" else if (mine == dealer) "push" else "lose"
  d$amount <- switch(d$outcome, win = 2 * d$amount, push = d$amount, lose = 0)
  if (d$outcome != "push") d$doubles <- d$doubles + 1L
  d$phase <- "done"
  d
}

# Can the player double (again)?
vp_can_double <- function(d) {
  is.null(d) || (d$phase == "done" && d$amount > 0 && d$doubles < VP_MAX_DOUBLES)
}

vp_label <- function(key, game) {
  if (!nzchar(key)) return("")
  t <- VP_GAMES[[game]]$table
  t$label[t$key == key]
}

# Simple hold suggestion (standard "hold what already pays / the best draw").
vp_suggest <- function(hand, game) {
  g <- VP_GAMES[[game]]
  key <- vp_evaluate(hand, game)
  held <- rep(FALSE, 5)
  wild <- switch(g$wild, deuces = card_rank(hand) == 2L & hand != JOKER, joker = hand == JOKER, rep(FALSE, 5))
  if (key %in% c("royal", "four_deuces", "wild_royal", "five", "sf", "straight", "flush", "fh")) return(rep(TRUE, 5))
  r <- card_rank(hand); s <- card_suit(hand)
  held[wild] <- TRUE
  tr <- tabulate(r[!wild], 13L)
  paired <- which(tr >= 2L)
  if (length(paired)) { held <- held | (r %in% paired & !wild); return(held) }
  # 4 to a flush
  for (su in 1:4) if (sum(s == su & !wild) + sum(wild) >= 4) return(held | (s == su & !wild))
  # keep high cards (J+ / K+)
  hi <- if (game == "joker") c(1L, 13L) else c(1L, 11L, 12L, 13L)
  if (g$wild == "none" || game == "joker") held <- held | (r %in% hi & !wild)
  held
}
