# ---------------------------------------------------------------------------
# engine_cards.R -- playing cards shared by Blackjack and Video Poker.
# A card is an integer 1..52 (53 = Joker):
#   rank = ((card - 1) %% 13) + 1   -> 1 = Ace, 11 = J, 12 = Q, 13 = K
#   suit = ((card - 1) %/% 13) + 1  -> 1 spades, 2 hearts, 3 diamonds, 4 clubs
# ---------------------------------------------------------------------------

JOKER <- 53L
RANK_LABELS <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
SUIT_SYMBOLS <- c("\u2660", "\u2665", "\u2666", "\u2663")
SUIT_NAMES <- c("spades", "hearts", "diamonds", "clubs")
SUIT_COLOR <- c("black", "red", "red", "black")

card_rank <- function(card) ((as.integer(card) - 1L) %% 13L) + 1L
card_suit <- function(card) ((as.integer(card) - 1L) %/% 13L) + 1L
make_card <- function(rank, suit) as.integer((suit - 1L) * 13L + rank)

card_label <- function(card) {
  ifelse(card == JOKER, "JOKER",
         paste0(RANK_LABELS[card_rank(card)], SUIT_SYMBOLS[card_suit(card)]))
}

# A freshly shuffled deck / shoe (uniform permutation via sample()).
new_shoe <- function(decks = 1L, jokers = 0L) {
  cards <- c(rep(1:52, decks), rep(JOKER, jokers))
  cards[sample.int(length(cards))]
}
