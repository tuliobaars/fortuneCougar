# ---------------------------------------------------------------------------
# engine_blackjack.R -- pure blackjack state machine (4 rule sets).
#
#   Classic        : 6 decks, dealer stands on soft 17, double any two cards,
#                    double after split, split to 4 hands, dealer peeks,
#                    insurance, blackjack pays 3:2.
#   European       : no hole card (dealer draws the 2nd card after the player),
#                    no peek -> a dealer blackjack takes every bet (incl.
#                    doubles & splits), double on hard 9-11 only.
#   Blackjack Switch: two hands, the top (second) cards may be switched;
#                    dealer hits soft 17, blackjack pays 1:1, dealer 22
#                    pushes every live non-blackjack hand.
#   Perfect Pairs  : Classic rules + Perfect Pairs side bet
#                    (mixed 6:1, coloured 12:1, perfect 25:1).
#
# The table is a plain list; every action returns an updated table. Coin
# movement is reported through `cost` helpers and per-hand `payout` (total
# returned including stake) so the Shiny module can debit/credit the player.
# ---------------------------------------------------------------------------

BJ_VARIANTS <- c(classic = "Classic Blackjack", european = "European Blackjack",
                 switch = "Blackjack Switch", pairs = "Perfect Pairs")

bj_rules <- function(variant) {
  base <- list(decks = 6L, penetration = 0.75, h17 = FALSE, peek = TRUE,
               enhc = FALSE, bj_pays = 1.5, double = "any", das = TRUE,
               max_hands = 4L, insurance = TRUE, switch = FALSE,
               dealer22push = FALSE, perfect_pairs = FALSE)
  switch(variant,
    classic = base,
    european = modifyList(base, list(peek = FALSE, enhc = TRUE, double = "9-11",
                                     insurance = FALSE)),
    switch = modifyList(base, list(h17 = TRUE, bj_pays = 1, switch = TRUE,
                                   dealer22push = TRUE, insurance = FALSE)),
    pairs = modifyList(base, list(perfect_pairs = TRUE)),
    stop("unknown blackjack variant: ", variant))
}

# Hand value: list(total, soft)
bj_value <- function(cards) {
  r <- card_rank(cards)
  v <- sum(pmin(r, 10L))
  soft <- any(r == 1L) && v + 10L <= 21L
  list(total = if (soft) v + 10L else v, soft = soft)
}

bj_total <- function(cards) bj_value(cards)$total

bj_new_table <- function(variant = "classic") {
  rules <- bj_rules(variant)
  tb <- list(variant = variant, rules = rules, shoe = integer(0), cut = 0L,
             phase = "betting", hands = list(), dealer = integer(0),
             hole_hidden = TRUE, active = 0L, bet = 0, side_bet = 0,
             side_result = NULL, insurance = 0, insurance_payout = 0,
             message = tr("Place your bet and press DEAL."), shuffled = FALSE,
             hands_played = 0L)
  bj_shuffle(tb)
}

bj_shuffle <- function(tb) {
  tb$shoe <- new_shoe(tb$rules$decks)
  tb$cut <- as.integer(length(tb$shoe) * (1 - tb$rules$penetration))
  tb$shuffled <- TRUE
  tb
}

bj_draw <- function(tb) {
  if (!length(tb$shoe)) tb <- bj_shuffle(tb)
  card <- tb$shoe[1]
  tb$shoe <- tb$shoe[-1]
  list(card = card, tb = tb)
}

new_hand <- function(cards, bet, split = FALSE, split_aces = FALSE) {
  list(cards = cards, bet = bet, doubled = FALSE, done = FALSE, split = split,
       split_aces = split_aces, result = "", payout = 0, switched = FALSE)
}

bj_is_natural <- function(hand) length(hand$cards) == 2 && !hand$split && bj_total(hand$cards) == 21

# Perfect Pairs side bet result for the first two player cards.
perfect_pairs_result <- function(c1, c2) {
  if (card_rank(c1) != card_rank(c2)) return(list(kind = "none", pays = 0))
  if (card_suit(c1) == card_suit(c2)) return(list(kind = "Perfect Pair", pays = 25))
  if (SUIT_COLOR[card_suit(c1)] == SUIT_COLOR[card_suit(c2)]) return(list(kind = "Coloured Pair", pays = 12))
  list(kind = "Mixed Pair", pays = 6)
}

# Coins required up-front for a deal.
bj_deal_cost <- function(tb, bet, side = 0) {
  (if (tb$rules$switch) 2 else 1) * bet + (if (tb$rules$perfect_pairs) side else 0)
}

# Deal a new round.
bj_deal <- function(tb, bet, side = 0) {
  stopifnot(tb$phase %in% c("betting", "settled"))
  tb$shuffled <- FALSE
  if (length(tb$shoe) <= tb$cut) tb <- bj_shuffle(tb)
  nh <- if (tb$rules$switch) 2L else 1L
  pc <- vector("list", nh); for (h in seq_len(nh)) pc[[h]] <- integer(0)
  dealer <- integer(0)
  # round 1: each hand, then dealer up card; round 2: each hand, then hole card
  for (round in 1:2) {
    for (h in seq_len(nh)) { d <- bj_draw(tb); tb <- d$tb; pc[[h]] <- c(pc[[h]], d$card) }
    if (round == 1 || !tb$rules$enhc) { d <- bj_draw(tb); tb <- d$tb; dealer <- c(dealer, d$card) }
  }
  tb$hands <- lapply(pc, new_hand, bet = bet)
  tb$dealer <- dealer
  tb$hole_hidden <- TRUE
  tb$bet <- bet
  tb$insurance <- 0; tb$insurance_payout <- 0
  tb$side_bet <- if (tb$rules$perfect_pairs) side else 0
  tb$side_result <- NULL
  if (tb$side_bet > 0) {
    pp <- perfect_pairs_result(pc[[1]][1], pc[[1]][2])
    tb$side_result <- list(kind = pp$kind, pays = pp$pays,
                    payout = if (pp$pays > 0) tb$side_bet * (pp$pays + 1) else 0)
  }
  tb$active <- 1L
  tb$hands_played <- tb$hands_played + 1L
  if (tb$rules$switch) {
    tb$phase <- "switch"
    tb$message <- tr("Switch the top cards between your hands?")
    return(tb)
  }
  bj_after_deal(tb)
}

# Blackjack Switch: swap the second card of the two hands (or keep them).
bj_switch <- function(tb, do_switch = TRUE) {
  stopifnot(tb$phase == "switch")
  if (isTRUE(do_switch)) {
    a <- tb$hands[[1]]$cards[2]; b <- tb$hands[[2]]$cards[2]
    tb$hands[[1]]$cards[2] <- b; tb$hands[[2]]$cards[2] <- a
    tb$hands[[1]]$switched <- TRUE; tb$hands[[2]]$switched <- TRUE
  }
  tb$phase <- "dealt"
  bj_after_deal(tb)
}

up_is_ace <- function(tb) card_rank(tb$dealer[1]) == 1L
up_is_ten <- function(tb) card_rank(tb$dealer[1]) >= 10L

bj_after_deal <- function(tb, insurance_done = FALSE) {
  r <- tb$rules
  if (r$insurance && up_is_ace(tb) && !insurance_done) {
    tb$phase <- "insurance"
    tb$message <- tr("Dealer shows an Ace. Insurance?")
    return(tb)
  }
  if (r$peek && (up_is_ace(tb) || up_is_ten(tb)) && bj_total(tb$dealer) == 21) {
    tb$hole_hidden <- FALSE
    if (tb$insurance > 0) tb$insurance_payout <- tb$insurance * 3
    tb$message <- tr("Dealer has Blackjack.")
    return(bj_settle(tb))
  }
  # Player naturals are finished immediately
  for (h in seq_along(tb$hands)) if (bj_total(tb$hands[[h]]$cards) == 21) tb$hands[[h]]$done <- TRUE
  tb$phase <- "player"
  bj_advance(tb, start = 1L)
}

# Insurance costs half the main bet and pays 2:1 if the dealer has Blackjack.
bj_insurance_cost <- function(tb) tb$bet / 2

bj_insurance <- function(tb, take = FALSE) {
  stopifnot(tb$phase == "insurance")
  tb$insurance <- if (isTRUE(take)) bj_insurance_cost(tb) else 0
  bj_after_deal(tb, insurance_done = TRUE)
}

# Move to the next unfinished hand or hand over to the dealer.
bj_advance <- function(tb, start = tb$active) {
  nxt <- which(!vapply(tb$hands, `[[`, logical(1), "done"))
  nxt <- nxt[nxt >= start]
  if (length(nxt)) {
    tb$active <- nxt[1]
    tb$phase <- "player"
    v <- bj_value(tb$hands[[tb$active]]$cards)
    tb$message <- if (v$soft) tr("Hand %d: soft %d. Hit or stand?", tb$active, v$total)
                  else tr("Hand %d: %d. Hit or stand?", tb$active, v$total)
    return(tb)
  }
  bj_dealer_play(tb)
}

# Which actions are legal right now.
bj_actions <- function(tb) {
  if (tb$phase == "switch") return(c("switch", "keep"))
  if (tb$phase == "insurance") return(c("insure", "no_insure"))
  if (tb$phase != "player") return(character(0))
  h <- tb$hands[[tb$active]]
  acts <- c("hit", "stand")
  if (h$split_aces) return("stand")
  if (length(h$cards) == 2) {
    t <- bj_value(h$cards)
    dbl_ok <- switch(tb$rules$double,
                     any = TRUE,
                     `9-11` = !t$soft && t$total %in% 9:11)
    if (h$split && !tb$rules$das) dbl_ok <- FALSE
    if (dbl_ok) acts <- c(acts, "double")
    same <- min(card_rank(h$cards[1]), 10L) == min(card_rank(h$cards[2]), 10L)
    if (same && length(tb$hands) < tb$rules$max_hands) acts <- c(acts, "split")
  }
  acts
}

# Extra coins an action needs (double / split cost one more hand bet).
bj_action_cost <- function(tb, action) {
  if (action %in% c("double", "split")) return(tb$hands[[tb$active]]$bet)
  if (action == "insure") return(bj_insurance_cost(tb))
  0
}

bj_hit <- function(tb) {
  stopifnot("hit" %in% bj_actions(tb))
  d <- bj_draw(tb); tb <- d$tb
  h <- tb$hands[[tb$active]]
  h$cards <- c(h$cards, d$card)
  t <- bj_total(h$cards)
  if (t >= 21) h$done <- TRUE
  tb$hands[[tb$active]] <- h
  if (h$done) tb <- bj_advance(tb, start = tb$active + 1L)
  else tb$message <- tr("Hand %d: %d. Hit or stand?", tb$active, t)
  tb
}

bj_stand <- function(tb) {
  stopifnot(tb$phase == "player")
  tb$hands[[tb$active]]$done <- TRUE
  bj_advance(tb, start = tb$active + 1L)
}

bj_double <- function(tb) {
  stopifnot("double" %in% bj_actions(tb))
  d <- bj_draw(tb); tb <- d$tb
  h <- tb$hands[[tb$active]]
  h$cards <- c(h$cards, d$card)
  h$bet <- h$bet * 2; h$doubled <- TRUE; h$done <- TRUE
  tb$hands[[tb$active]] <- h
  bj_advance(tb, start = tb$active + 1L)
}

bj_split <- function(tb) {
  stopifnot("split" %in% bj_actions(tb))
  i <- tb$active
  h <- tb$hands[[i]]
  aces <- card_rank(h$cards[1]) == 1L
  d1 <- bj_draw(tb); tb <- d1$tb
  d2 <- bj_draw(tb); tb <- d2$tb
  a <- new_hand(c(h$cards[1], d1$card), h$bet, split = TRUE, split_aces = aces)
  b <- new_hand(c(h$cards[2], d2$card), h$bet, split = TRUE, split_aces = aces)
  if (aces) { a$done <- TRUE; b$done <- TRUE }
  if (bj_total(a$cards) == 21) a$done <- TRUE
  if (bj_total(b$cards) == 21) b$done <- TRUE
  tb$hands <- append(tb$hands[-i], list(a, b), after = i - 1L)
  if (a$done) bj_advance(tb, start = i) else { tb$active <- i; bj_advance(tb, start = i) }
}

# Dealer draws (reveals the hole card, or draws it under no-hole-card rules).
bj_dealer_play <- function(tb) {
  tb$phase <- "dealer"
  tb$hole_hidden <- FALSE
  if (length(tb$dealer) < 2) { d <- bj_draw(tb); tb <- d$tb; tb$dealer <- c(tb$dealer, d$card) }
  live <- vapply(tb$hands, function(h) bj_total(h$cards) <= 21 && !bj_is_natural(h), logical(1))
  dealer_bj <- bj_total(tb$dealer) == 21 && length(tb$dealer) == 2
  if (any(live) && !dealer_bj) {
    repeat {
      v <- bj_value(tb$dealer)
      if (v$total > 17 || (v$total == 17 && !(v$soft && tb$rules$h17))) break
      d <- bj_draw(tb); tb <- d$tb
      tb$dealer <- c(tb$dealer, d$card)
    }
  }
  bj_settle(tb)
}

# Settle every hand. `payout` is the total returned (stake + winnings).
bj_settle <- function(tb) {
  r <- tb$rules
  tb$hole_hidden <- FALSE
  dt <- bj_total(tb$dealer)
  dealer_bj <- dt == 21 && length(tb$dealer) == 2
  for (i in seq_along(tb$hands)) {
    h <- tb$hands[[i]]
    pt <- bj_total(h$cards)
    nat <- bj_is_natural(h)
    if (pt > 21) { h$result <- "Bust"; h$payout <- 0 }
    else if (dealer_bj) {
      if (nat) { h$result <- "Push"; h$payout <- h$bet }
      else { h$result <- "Dealer Blackjack"; h$payout <- 0 }
    }
    else if (nat) { h$result <- "Blackjack!"; h$payout <- h$bet * (1 + r$bj_pays) }
    else if (dt > 21) {
      if (r$dealer22push && dt == 22) { h$result <- "Push (dealer 22)"; h$payout <- h$bet }
      else { h$result <- "Win - dealer busts"; h$payout <- h$bet * 2 }
    }
    else if (pt > dt) { h$result <- "Win"; h$payout <- h$bet * 2 }
    else if (pt == dt) { h$result <- "Push"; h$payout <- h$bet }
    else { h$result <- "Lose"; h$payout <- 0 }
    h$done <- TRUE
    tb$hands[[i]] <- h
  }
  tb$phase <- "settled"
  net <- bj_round_return(tb) - bj_round_stake(tb)
  tb$message <- paste(
    if (dealer_bj) tr("Dealer has Blackjack.") else if (dt > 21) tr("Dealer busts with %d.", dt) else tr("Dealer has %d.", dt),
    if (net > 0) tr("You win %s coins!", fmt_coins(net))
    else if (net == 0) tr("Push.") else tr("You lose %s coins.", fmt_coins(-net)))
  tb
}

# Total staked this round (main hands incl. doubles/splits + insurance + side).
bj_round_stake <- function(tb) {
  sum(vapply(tb$hands, `[[`, numeric(1), "bet")) + tb$insurance + tb$side_bet
}

# Total returned this round.
bj_round_return <- function(tb) {
  sum(vapply(tb$hands, `[[`, numeric(1), "payout")) + tb$insurance_payout +
    (tb$side_result$payout %||% 0)
}

# Basic-strategy style hint (hard/soft totals, pairs) for the active hand.
bj_hint <- function(tb) {
  if (tb$phase != "player") return("")
  h <- tb$hands[[tb$active]]; acts <- bj_actions(tb)
  up <- min(card_rank(tb$dealer[1]), 10L); if (up == 1L) up <- 11L
  v <- bj_value(h$cards)
  pair <- "split" %in% acts
  pr <- min(card_rank(h$cards[1]), 10L)
  rec <- if (pair && (pr == 1L || pr == 8L)) "split"
    else if (pair && pr %in% c(2L, 3L, 7L) && up <= 7) "split"
    else if (pair && pr == 9L && up %in% c(2:6, 8:9)) "split"
    else if (pair && pr == 6L && up <= 6) "split"
    else if (v$soft) {
      if (v$total >= 19) "stand"
      else if (v$total == 18) { if (up %in% 3:6) "double" else if (up <= 8) "stand" else "hit" }
      else if (up %in% 4:6 || (v$total >= 17 && up == 3)) "double" else "hit"
    } else {
      t <- v$total
      if (t >= 17) "stand"
      else if (t >= 13) { if (up <= 6) "stand" else "hit" }
      else if (t == 12) { if (up %in% 4:6) "stand" else "hit" }
      else if (t == 11) "double"
      else if (t == 10) { if (up <= 9) "double" else "hit" }
      else if (t == 9) { if (up %in% 3:6) "double" else "hit" }
      else "hit"
    }
  if (rec == "double" && !"double" %in% acts) rec <- if (v$total >= 18) "stand" else "hit"
  if (rec == "split" && !"split" %in% acts) rec <- "hit"
  rec
}
