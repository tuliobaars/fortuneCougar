# ---------------------------------------------------------------------------
# engine_roulette.R -- roulette wheels, bet catalogue and settlement.
#
# Pockets are integers 0..36, with 37 standing for "00" on the American wheel.
# A bet key is "<type>:<n1>-<n2>-..." for inside bets (e.g. "split:17-20") or
# a named outside bet ("red", "dozen2", "col3", ...). Payout ratios follow
# the standard 36/n - 1 formula (n = numbers covered), except the American
# five-number "basket" (0-00-1-2-3), which pays 6:1.
# ---------------------------------------------------------------------------

RL_RED <- c(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
RL_ORDER_EU <- c(0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27, 13, 36, 11, 30, 8, 23, 10,
                 5, 24, 16, 33, 1, 20, 14, 31, 9, 22, 18, 29, 7, 28, 12, 35, 3, 26)
RL_ORDER_US <- c(0, 28, 9, 26, 30, 11, 7, 20, 32, 17, 5, 22, 34, 15, 3, 24, 36, 13, 1,
                 37, 27, 10, 25, 29, 12, 8, 19, 31, 18, 6, 21, 33, 16, 4, 23, 35, 14, 2)

RL_VARIANTS <- c(european = "European Roulette", american = "American Roulette",
                 french = "French Roulette", multi = "Multi-Wheel Roulette")

rl_wheel <- function(variant) if (variant == "american") RL_ORDER_US else RL_ORDER_EU
rl_label <- function(n) ifelse(n == 37, "00", as.character(n))
rl_color <- function(n) ifelse(n %in% c(0, 37), "green", ifelse(n %in% RL_RED, "red", "black"))

OUTSIDE_BETS <- list(
  red = RL_RED, black = setdiff(1:36, RL_RED),
  odd = seq(1, 35, 2), even = seq(2, 36, 2),
  low = 1:18, high = 19:36,
  dozen1 = 1:12, dozen2 = 13:24, dozen3 = 25:36,
  col1 = seq(1, 34, 3), col2 = seq(2, 35, 3), col3 = seq(3, 36, 3)
)
EVEN_MONEY <- c("red", "black", "odd", "even", "low", "high")

# Numbers covered by a bet key.
rl_numbers <- function(key) {
  if (!is.null(OUTSIDE_BETS[[key]])) return(OUTSIDE_BETS[[key]])
  as.numeric(strsplit(sub("^[a-z0-9]+:", "", key), "-")[[1]])
}

rl_type <- function(key) if (grepl(":", key)) sub(":.*$", "", key) else key

# Payout ratio (x:1) for a bet key.
rl_ratio <- function(key) {
  n <- length(rl_numbers(key))
  if (n == 5) return(6)                # American basket 0-00-1-2-3
  36 / n - 1
}

# Is a key a legal bet on this wheel?
rl_valid <- function(key, variant) {
  nums <- tryCatch(rl_numbers(key), error = function(e) NA, warning = function(w) NA)
  if (anyNA(nums) || !length(nums)) return(FALSE)
  if (variant != "american" && any(nums == 37)) return(FALSE)
  if (length(nums) == 5 && variant != "american") return(FALSE)
  all(nums %in% 0:37) && !anyDuplicated(nums) && length(nums) %in% c(1, 2, 3, 4, 5, 6, 12, 18)
}

# Spin: a uniformly random pocket of the wheel.
rl_spin <- function(variant) {
  w <- rl_wheel(variant)
  w[sample.int(length(w), 1L)]
}

# Settle a set of bets (named numeric: key -> stake) against one result.
# French roulette applies La Partage: even-money bets lose only half on zero.
# @return list(total = coins returned incl. stakes, lines = data.frame)
rl_settle <- function(bets, result, variant) {
  if (!length(bets)) return(list(total = 0, lines = data.frame()))
  keys <- names(bets)
  ret <- numeric(length(bets)); note <- character(length(bets))
  for (i in seq_along(bets)) {
    k <- keys[i]; amt <- bets[[i]]
    if (result %in% rl_numbers(k)) {
      ret[i] <- amt * (rl_ratio(k) + 1); note[i] <- tr("wins %s:1", format(rl_ratio(k)))
    } else if (variant == "french" && result == 0 && k %in% EVEN_MONEY) {
      ret[i] <- amt / 2; note[i] <- tr("La Partage: half back")
    } else note[i] <- tr("loses")
  }
  list(total = sum(ret),
       lines = data.frame(bet = keys, stake = as.numeric(bets), returned = ret, note = note,
                          stringsAsFactors = FALSE))
}

# French announced (call) bets expanded into their component chips.
# Each element: bet key -> number of chips.
RL_CALL_BETS <- list(
  voisins = list(label = "Voisins du Z\u00e9ro", chips = c(
    "street:0-2-3" = 2, "split:4-7" = 1, "split:12-15" = 1, "split:18-21" = 1,
    "split:19-22" = 1, "split:32-35" = 1, "corner:25-26-28-29" = 2)),
  tiers = list(label = "Tiers du Cylindre", chips = c(
    "split:5-8" = 1, "split:10-11" = 1, "split:13-16" = 1, "split:23-24" = 1,
    "split:27-30" = 1, "split:33-36" = 1)),
  orphelins = list(label = "Orphelins", chips = c(
    "straight:1" = 1, "split:6-9" = 1, "split:14-17" = 1, "split:17-20" = 1, "split:31-34" = 1)),
  jeu0 = list(label = "Jeu Z\u00e9ro", chips = c(
    "split:0-3" = 1, "split:12-15" = 1, "straight:26" = 1, "split:32-35" = 1))
)

# Add `amount` on `key` to a bet list (named numeric).
rl_add_bet <- function(bets, key, amount) {
  bets <- bets %||% numeric(0)
  if (key %in% names(bets)) bets[[key]] <- bets[[key]] + amount
  else bets <- c(bets, stats::setNames(amount, key))
  bets
}

# House edge (theoretical) for display.
rl_house_edge <- function(variant) {
  switch(variant, american = 2 / 38, french = 1 / 74, 1 / 37)
}

# Human-readable bet names.
RL_OUTSIDE_NAMES <- c(red = "Red", black = "Black", odd = "Odd", even = "Even", low = "1-18",
                      high = "19-36", dozen1 = "1st 12", dozen2 = "2nd 12", dozen3 = "3rd 12",
                      col1 = "Column 1", col2 = "Column 2", col3 = "Column 3")
RL_TYPE_NAMES <- c(straight = "Straight up", split = "Split", street = "Street", corner = "Corner",
                   sixline = "Six line", basket = "Basket")

rl_bet_label <- function(key) {
  if (!is.na(RL_OUTSIDE_NAMES[key])) return(tr(unname(RL_OUTSIDE_NAMES[key])))
  t <- rl_type(key)
  nums <- paste(rl_label(rl_numbers(key)), collapse = "/")
  paste0(tr(RL_TYPE_NAMES[[t]] %||% tools::toTitleCase(t)), " ", nums)
}
