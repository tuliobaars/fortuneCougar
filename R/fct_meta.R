# ---------------------------------------------------------------------------
# meta_data.R -- data & pure logic for the metagame:
#   Cougar Album "Like a Pro" (cards, packs, Lucky Cards), Star Market,
#   Wheel of Fun, Playtika Rewards tiers, levels/XP, Daily Dash missions and
#   Monsterpedia chapters. No Shiny code here.
# ---------------------------------------------------------------------------

# ---- Bets & levels -------------------------------------------------------------
BET_LADDER <- c(100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000)
BET_UNLOCK_LEVEL <- c(1, 1, 1, 1, 1, 1, 3, 6, 10, 15)   # level needed per ladder step
START_COINS <- 1000000

xp_needed <- function(level) round(500 * level^1.5)
max_bet_index <- function(level) max(which(BET_UNLOCK_LEVEL <= level))
level_reward <- function(level) 25000 * level

# ---- Cougar Album: "Like a Pro" -------------------------------------------------------
ALBUM_SETS <- data.frame(
  set = 1:9,
  name = c("Vegas Nights", "High Rollers", "Pharaoh's Tomb", "Olympus & Myth",
           "High Seas", "Fairy Tales", "Wild Kingdom", "Sweet Treats", "Monster Mash"),
  icon = c("\U0001F3B0", "\U0001F3E6", "\U0001F53A", "\u26A1", "\U0001F3F4\u200D\u2620\uFE0F",
           "\U0001F3F0", "\U0001F42F", "\U0001F36D", "\U0001F9DB"),
  reward = c(250e3, 300e3, 400e3, 500e3, 650e3, 800e3, 1e6, 1.25e6, 1.5e6),
  stringsAsFactors = FALSE)
ALBUM_COMPLETE_REWARD <- 25e6

ALBUM_CARDS <- local({
  cards <- list(
    c("Lucky 7", "7\uFE0F\u20E3"), c("Cherry Bomb", "\U0001F352"), c("Golden Bell", "\U0001F514"),
    c("High Roller", "\U0001F3B2"), c("Showgirl", "\U0001F483"), c("Neon Star", "\U0001F31F"),
    c("Cocktail Hour", "\U0001F378"), c("Bankroll", "\U0001F4B5"), c("Jackpot!", "\U0001F3B0"),
    c("Vault", "\U0001F510"), c("Golden Key", "\U0001F511"), c("Money Bag", "\U0001F4B0"),
    c("Diamond", "\U0001F48E"), c("Top Hat", "\U0001F3A9"), c("Champagne", "\U0001F942"),
    c("The Ring", "\U0001F48D"), c("Limousine", "\U0001F698"), c("Queen's Crown", "\U0001F451"),
    c("Scarab", "\U0001FAB2"), c("Pyramid", "\U0001F53A"), c("Canopic Urn", "\U0001F3FA"),
    c("Camel", "\U0001F42B"), c("Eye of Ra", "\U0001F441\uFE0F"), c("Ankh", "\u2625"),
    c("Sphinx", "\U0001F981"), c("Mummy", "\U0001F9DF"), c("Cat Pharaoh", "\U0001F63C"),
    c("Laurel", "\U0001F33F"), c("Amphora", "\U0001F3FA"), c("Shield", "\U0001F6E1\uFE0F"),
    c("Sword", "\U0001F5E1\uFE0F"), c("Thunderbolt", "\u26A1"), c("Statue", "\U0001F5FF"),
    c("Phoenix", "\U0001F985"), c("Dragon", "\U0001F409"), c("Medusa", "\U0001F40D"),
    c("Anchor", "\u2693"), c("Cutlass", "\U0001F5E1\uFE0F"), c("Parrot", "\U0001F99C"),
    c("Sailboat", "\u26F5"), c("Octopus", "\U0001F419"), c("Treasure Map", "\U0001F5FA\uFE0F"),
    c("Skull", "\U0001F480"), c("Mermaid", "\U0001F9DC\u200D\u2640\uFE0F"), c("Jolly Roger", "\U0001F3F4\u200D\u2620\uFE0F"),
    c("Pumpkin", "\U0001F383"), c("Mouse", "\U0001F42D"), c("Glass Slipper", "\U0001F460"),
    c("Beanstalk", "\U0001F331"), c("Golden Egg", "\U0001F95A"), c("Fairy", "\U0001F9DA"),
    c("Palace", "\U0001F3F0"), c("Princess", "\U0001F478"), c("Carriage", "\U0001F3A0"),
    c("Panda", "\U0001F43C"), c("Owl", "\U0001F989"), c("Frog", "\U0001F438"),
    c("Kitty", "\U0001F63B"), c("Elephant", "\U0001F418"), c("Gorilla", "\U0001F98D"),
    c("Lion", "\U0001F981"), c("Jaguar", "\U0001F406"), c("Tiger King", "\U0001F42F"),
    c("Cookie", "\U0001F36A"), c("Candy", "\U0001F36C"), c("Chocolate", "\U0001F36B"),
    c("Donut", "\U0001F369"), c("Ice Cream", "\U0001F366"), c("Cupcake", "\U0001F9C1"),
    c("Lollipop", "\U0001F36D"), c("Birthday Cake", "\U0001F382"), c("Gummy King", "\U0001F9F8"),
    c("Spider", "\U0001F577\uFE0F"), c("Ghost", "\U0001F47B"), c("Bat", "\U0001F987"),
    c("Witch", "\U0001F9D9\u200D\u2640\uFE0F"), c("Werewolf", "\U0001F43A"), c("Vile Vixen", "\U0001F608"),
    c("Frankenstein", "\U0001F9DF"), c("Gorgon", "\U0001F40D"), c("The Count", "\U0001F9DB")
  )
  set <- rep(1:9, each = 9)
  pos <- rep(1:9, 9)
  data.frame(
    id = sprintf("c%02d%d", set, pos),
    set = set, pos = pos,
    name = vapply(cards, `[[`, "", 1),
    glyph = vapply(cards, `[[`, "", 2),
    stars = c(1, 1, 2, 2, 3, 3, 4, 4, 5)[pos],
    lucky = pos == 6,                      # one golden Lucky Card per set
    stringsAsFactors = FALSE)
})

PACKS <- list(
  bronze  = list(name = "Bronze Pack",  cards = 2L, min_star = 1L, icon = "\U0001F7EB"),
  silver  = list(name = "Silver Pack",  cards = 3L, min_star = 2L, icon = "\u2B1C"),
  gold    = list(name = "Gold Pack",    cards = 4L, min_star = 3L, icon = "\U0001F7E8"),
  diamond = list(name = "Diamond Pack", cards = 5L, min_star = 4L, icon = "\U0001F48E")
)
STAR_WEIGHTS <- c(40, 28, 17, 10, 5)        # card draw weight by star rating
DUPLICATE_STARS <- c(1, 2, 4, 7, 12)        # Star Market value of a spare card

# Draw a pack. One card is guaranteed at least `min_star` stars.
# @param owned named integer vector of card counts (names = card ids)
open_pack <- function(tier, owned = integer(0)) {
  p <- PACKS[[tier]]
  w <- STAR_WEIGHTS[ALBUM_CARDS$stars]
  ids <- character(p$cards)
  g <- which(ALBUM_CARDS$stars >= p$min_star)
  ids[1] <- ALBUM_CARDS$id[g[sample.int(length(g), 1, prob = w[g])]]
  if (p$cards > 1) ids[-1] <- ALBUM_CARDS$id[sample.int(nrow(ALBUM_CARDS), p$cards - 1, replace = TRUE, prob = w)]
  ids <- ids[sample.int(length(ids))]
  before <- owned[ids]; before[is.na(before)] <- 0L
  is_new <- before == 0L & !duplicated(ids)
  list(tier = tier, ids = ids, new = is_new,
       lucky = ids[ALBUM_CARDS$lucky[match(ids, ALBUM_CARDS$id)]])
}

# Pack tier for a random in-game drop, by stake.
drop_tier <- function(bet) {
  if (bet >= 50000) "diamond" else if (bet >= 10000) "gold" else if (bet >= 1000) "silver" else "bronze"
}
PACK_DROP_CHANCE <- 0.04

# Sets completed given card counts.
album_progress <- function(owned) {
  have <- ALBUM_CARDS$id %in% names(owned)[owned > 0]
  per_set <- tapply(have, ALBUM_CARDS$set, sum)
  list(have = sum(have), total = nrow(ALBUM_CARDS), per_set = as.integer(per_set),
       complete_sets = unname(which(per_set == 9)))
}

# Stars earned by trading every spare (duplicate) card.
spare_value <- function(owned) {
  if (!length(owned)) return(0)
  extra <- pmax(0, owned - 1)
  st <- ALBUM_CARDS$stars[match(names(owned), ALBUM_CARDS$id)]
  sum(extra * DUPLICATE_STARS[st], na.rm = TRUE)
}

# ---- Lucky Cards mini game ----------------------------------------------------------
make_lucky_cards <- function(level = 1) {
  prizes <- list(
    list(kind = "coins", value = sample(c(100e3, 250e3, 500e3, 1e6, 2e6), 1, prob = c(35, 30, 20, 10, 5)) * (1 + level / 20)),
    list(kind = "stars", value = sample(c(10, 20, 35, 50), 1, prob = c(40, 30, 20, 10))),
    list(kind = "pack",  value = 0, tier = sample(c("silver", "gold", "diamond"), 1, prob = c(55, 35, 10)))
  )
  prizes <- prizes[sample.int(3)]
  lab <- vapply(prizes, function(p) switch(p$kind,
    coins = paste("\U0001FA99", tr("%s coins", fmt_coins(p$value))),
    stars = paste("\u2B50", tr("%s stars", p$value)),
    pack  = paste(PACKS[[p$tier]]$icon, tr(PACKS[[p$tier]]$name))), "")
  b <- new_bonus("choose", tr("Lucky Cards!"), tr("You pulled a golden Lucky Card - pick one of three for a surprise prize."),
                 bonus_tiles(vapply(prizes, `[[`, "", "kind"), vapply(prizes, function(p) as.numeric(p$value), 0), lab),
                 face = "\U0001F340")
  b$tiers <- vapply(prizes, function(p) p$tier %||% "", "")
  b
}

# ---- Star Market -----------------------------------------------------------------------
MARKET_ITEMS <- data.frame(
  id = c("coins_1m", "pack_silver", "pack_gold", "pack_diamond", "boost_xp", "boost_win",
         "wheel_reset", "av_lion", "av_robot", "av_dragon", "av_royal", "av_unicorn", "av_genie"),
  kind = c("coins", "pack", "pack", "pack", "boost", "boost", "wheel",
           "avatar", "avatar", "avatar", "avatar", "avatar", "avatar"),
  name = c("Coin Chest", "Silver Pack", "Gold Pack", "Diamond Pack", "Double XP",
           "Win Booster", "Wheel Recharge", "Lion Heart", "Robo Roller", "Dragon Lord",
           "Royal Flush", "Unicorn Dream", "The Genie"),
  desc = c("1,000,000 coins", "3 cards, one 2\u2605+", "4 cards, one 3\u2605+", "5 cards, one 4\u2605+",
           "2x XP for your next 50 plays", "+20% on all slot wins for 25 spins",
           "Recharge the hourly Wheel of Fun now", "Exclusive avatar", "Exclusive avatar",
           "Exclusive avatar", "Exclusive avatar", "Exclusive avatar", "Exclusive avatar"),
  icon = c("\U0001FA99", "\u2B1C", "\U0001F7E8", "\U0001F48E", "\u26A1", "\U0001F680", "\U0001F3A1",
           "\U0001F981", "\U0001F916", "\U0001F432", "\U0001F451", "\U0001F984", "\U0001F9DE"),
  value = c("1000000", "silver", "gold", "diamond", "xp2:50", "win20:25", "",
            "\U0001F981", "\U0001F916", "\U0001F432", "\U0001F451", "\U0001F984", "\U0001F9DE"),
  price = c(60, 40, 90, 200, 50, 120, 80, 50, 100, 150, 300, 500, 750),
  stringsAsFactors = FALSE)
DEFAULT_AVATAR <- "\U0001F60E"

# ---- Wheel of Fun ---------------------------------------------------------------------------
WHEEL_SEGMENTS <- data.frame(
  value = c(25e3, 100e3, 50e3, 250e3, 75e3, 500e3, 50e3, 150e3, 25e3, 1e6, 75e3, 2e6),
  weight = c(16, 10, 14, 6, 12, 3, 14, 8, 16, 1.5, 12, 0.5),
  color = rep(c("#ff2e88", "#7b2cbf", "#ffcc00", "#00c2ff"), 3),
  stringsAsFactors = FALSE)
WHEEL_MULTS <- data.frame(mult = c(1, 2, 3, 5, 10), weight = c(40, 30, 18, 9, 3))
WHEEL_HOURLY_SECS <- 3600

spin_fun_wheel <- function(daily = FALSE, vip_bonus = 0) {
  k <- sample.int(nrow(WHEEL_SEGMENTS), 1, prob = WHEEL_SEGMENTS$weight)
  m <- if (daily) WHEEL_MULTS$mult[sample.int(nrow(WHEEL_MULTS), 1, prob = WHEEL_MULTS$weight)] else 1
  base <- WHEEL_SEGMENTS$value[k]
  list(segment = k, base = base, mult = m, vip_bonus = vip_bonus,
       total = round(base * m * (1 + vip_bonus)), pack = if (daily) "silver" else NULL)
}

# ---- Playtika Rewards ------------------------------------------------------------------------
VIP_TIERS <- data.frame(
  tier = c("Bronze", "Silver", "Gold", "Platinum", "Diamond", "Royal Diamond"),
  min_points = c(0, 2500, 10000, 40000, 150000, 500000),
  wheel_bonus = c(0, 0.10, 0.25, 0.50, 1.00, 1.50),
  daily_gift = c(50e3, 100e3, 250e3, 500e3, 1e6, 2.5e6),
  color = c("#cd7f32", "#c0c0c0", "#ffd700", "#8fd3ff", "#b9f2ff", "#ff7ad9"),
  icon = c("\U0001F949", "\U0001F948", "\U0001F947", "\U0001F4A0", "\U0001F48E", "\U0001F451"),
  stringsAsFactors = FALSE)
REWARD_FAMILIES <- c(slots = "fortuneCougar Slots", blackjack = "Blackjack Tables",
                     poker = "Video Poker", roulette = "Roulette", side = "Side Games")
# Loyalty points: 1 point per 1,000 coins wagered.
loyalty_points <- function(wager) wager / 1000

vip_index <- function(points) max(which(VIP_TIERS$min_points <= points))

# ---- Daily Dash ----------------------------------------------------------------------------
MISSION_CATALOG <- data.frame(
  metric = c("spins", "spins_classic", "spins_myth", "spins_animal", "win_x", "big_wins",
             "features", "bj_wins", "bj_naturals", "vp_hands", "vp_good", "rl_spins",
             "rl_straight", "wager"),
  text = c("Spin any slot %s times", "Spin Classic & Vegas slots %s times",
           "Spin Mythological slots %s times", "Spin Animal 3D slots %s times",
           "Win a total of %s bets", "Land %s Big Wins (8x+)",
           "Trigger %s slot features", "Win %s blackjack hands",
           "Get %s blackjacks", "Play %s video poker hands",
           "Make Three of a Kind or better %s times", "Spin the roulette wheel %s times",
           "Hit %s straight-up roulette numbers", "Wager %s coins"),
  daily = c(30, 20, 20, 20, 25, 2, 3, 5, 1, 15, 3, 10, 1, 250e3),
  stringsAsFactors = FALSE)
DASH_PERIODS <- c(day = 1, week = 5, month = 15)      # target multipliers
DASH_REWARD <- list(
  day   = list(mission = 50e3,  chest = 150e3, pack = "silver",  points = 10),
  week  = list(mission = 250e3, chest = 1e6,   pack = "gold",    points = 30),
  month = list(mission = 1e6,   chest = 5e6,   pack = "diamond", points = 100))

# Deterministic missions for a period key (same for everyone that day).
dash_missions <- function(period, key) {
  with_seed(str_seed(paste(period, key)), {
    i <- sample.int(nrow(MISSION_CATALOG), 3)
    m <- MISSION_CATALOG[i, ]
    target <- ceiling(m$daily * DASH_PERIODS[[period]])
    data.frame(id = paste(period, key, m$metric, sep = "|"), metric = m$metric,
               text = vapply(seq_along(target), function(k) tr(m$text[k], fmt_coins(target[k])), ""), target = target,
               stringsAsFactors = FALSE)
  })
}

# ---- Monsterpedia -----------------------------------------------------------------------------
MONSTER_CHAPTERS <- list(
  list(id = "medusa", name = "Medusa's Lair", game = "stonegold", icon = "\U0001F40D",
       reward = 1e6, avatar = "\U0001F40D", color = "#8fbf6a"),
  list(id = "lab", name = "The Laboratory", game = "frankenstein", icon = "\U0001F9DF",
       reward = 2e6, avatar = "\U0001F9DF", color = "#62d2a2"),
  list(id = "crypt", name = "Vixen's Crypt", game = "vixens", icon = "\U0001F608",
       reward = 3e6, avatar = "\U0001F608", color = "#e05297"),
  list(id = "castle", name = "Count's Castle", game = "vampire", icon = "\U0001F9DB",
       reward = 5e6, avatar = "\U0001F9DB", color = "#c0392b")
)

# Five capture challenges per chapter. metric: spins | win_x (sum of win/bet)
# | single_x (one win >= value x bet, counted once) | event:<name>
MONSTER_CHALLENGES <- data.frame(
  chapter = rep(c("medusa", "lab", "crypt", "castle"), each = 5),
  monster = c("Stone Serpent", "Gorgon Sister", "Basilisk", "Cyclops", "Medusa Queen",
              "Igor", "Lab Rat", "Brain in a Jar", "The Bride", "Frankenstein",
              "Cat Familiar", "Banshee", "Sticky Succubus", "Harpy", "Vile Vixen",
              "Bat Swarm", "Ghoul", "Blood Moon Wolf", "Nosferatu", "The Count"),
  glyph = c("\U0001F40D", "\U0001F9DD\u200D\u2640\uFE0F", "\U0001F98E", "\U0001F441\uFE0F", "\U0001F478",
            "\U0001F9CC", "\U0001F400", "\U0001F9E0", "\U0001F470", "\U0001F9DF",
            "\U0001F408\u200D\u2B1B", "\U0001F47B", "\U0001F608", "\U0001F985", "\U0001F483",
            "\U0001F987", "\U0001F480", "\U0001F43A", "\U0001F9DB", "\U0001F9DB\u200D\u2642\uFE0F"),
  metric = c("spins", "event:stone_respin", "win_x", "single_x", "event:stone_respin",
             "spins", "event:revive", "win_x", "event:lab", "single_x",
             "spins", "event:feature", "event:sticky3", "win_x", "single_x",
             "spins", "event:blood_moon", "win_x", "single_x", "event:blood_moon"),
  target = c(25, 10, 50, 15, 40,
             25, 10, 60, 2, 20,
             25, 20, 3, 75, 25,
             30, 2, 90, 30, 6),
  stringsAsFactors = FALSE)

MONSTER_EVENT_TEXT <- list(
  stone_respin = c("Trigger Medusa respins %d time", "Trigger Medusa respins %d times"),
  revive       = c("Revive wilds %d time", "Revive wilds %d times"),
  lab          = c("Enter the Laboratory %d time", "Enter the Laboratory %d times"),
  feature      = c("Spin with sticky wilds on the reels %d time", "Spin with sticky wilds on the reels %d times"),
  sticky3      = c("Hold 3+ sticky wilds at once %d time", "Hold 3+ sticky wilds at once %d times"),
  blood_moon   = c("Raise the Blood Moon %d time", "Raise the Blood Moon %d times"))

monster_challenge_text <- function(metric, target) {
  target <- as.integer(target)
  if (metric == "spins") return(tr("Spin %d times", target))
  if (metric == "win_x") return(tr("Win a total of %dx your bet", target))
  if (metric == "single_x") return(tr("Land a single %dx win", target))
  ev <- MONSTER_EVENT_TEXT[[sub("event:", "", metric)]]
  trn(target, ev[1], ev[2], target)
}
