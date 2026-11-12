# ---------------------------------------------------------------------------
# fct_demo.R -- demo mode (run_app(demo = TRUE)), used to film the promo videos
# and for showcases:
#   * the store gets a returning player "Lucky Cougar" (level 40, album
#     progress, 420 plays of statistics, 3 past sessions) and a community of 36
#     players, so My Stats and the k-means clusters have something to show;
#   * the input `__seed` sets R's random seed, so a scripted demo (DirectoR
#     shot recipes) can pin the outcome of the next spin, deal or wheel.
# Outside demo mode none of this exists. Play money only, as everywhere.
# ---------------------------------------------------------------------------

FC_DEMO_PLAYER <- "lucky-cougar-demo-0001"
FC_DEMO_NAMES <- c("Ace", "Bella", "Carlos", "Dara", "Eli", "Fatima", "Gus", "Hana", "Ivo", "Jade", "Kofi", "Lia",
                   "Mateo", "Nina", "Omar", "Pia", "Quinn", "Rui", "Sofia", "Tomas", "Uma", "Vera", "Wes", "Xena",
                   "Yara", "Zeke", "Ana", "Beto", "Caio", "Duda", "Enzo", "Flor", "Gabi", "Hugo", "Iris", "Joao")

fc_demo_enabled <- function() isTRUE(getOption("fortuneCougar.demo", FALSE))

# Add the demo players to a store (once: skipped when "Lucky Cougar" exists).
# Uses fixed seeds, so the demo data is identical every time; R's random
# stream is re-seeded from the clock afterwards.
fc_demo_seed <- function(store) {
  if (!isTRUE(store$server_side)) return(invisible(FALSE))
  if (!is.null(store$find_player_by_name("Lucky Cougar"))) return(invisible(FALSE))
  on.exit(set.seed(NULL), add = TRUE)
  p <- player_state(session = NULL, store = store)
  shiny::isolate({
    p$rv$name <- "Lucky Cougar"
    p$rv$level <- 40L; p$rv$xp <- 0; p$rv$stars <- 640; p$rv$vip_points <- 13250
    p$rv$vip_by_family <- c(slots = 9100, blackjack = 2100, poker = 900, roulette = 1150, side = 0)
    set.seed(3); own <- sample(ALBUM_CARDS$id, 55); al <- stats::setNames(sample(1:3, 55, TRUE), own)
    al[ALBUM_CARDS$id[ALBUM_CARDS$set == 2]] <- 1L
    p$rv$album <- al; p$rv$recent_pulls <- sample(own, 12)
    p$rv$avatars <- c(DEFAULT_AVATAR, "\U0001F981", "\U0001F432"); p$rv$avatar <- "\U0001F981"
    p$rv$monster <- list(medusa.1 = 25, medusa.2 = 10, medusa.3 = 50, medusa.4 = 15, medusa.5 = 22, lab.1 = 12, lab.2 = 4, lab.3 = 17)
    p$rv$monster_claimed <- "medusa"
    set.seed(11)
    t <- now_secs() - 3 * 86400; tk <- track_new(t); bal <- 2.4e6
    cats <- c("classic", "myth", "animal", "blackjack", "poker", "roulette")
    for (i in 1:420) {
      cat_i <- sample(cats, 1, prob = c(.3, .2, .15, .15, .12, .08))
      w <- sample(c(1000, 2500, 5000, 10000), 1, prob = c(.3, .35, .25, .1))
      win <- if (stats::runif(1) < .34) round(w * stats::rexp(1, 1 / 2.95)) else 0
      if (i %in% c(120, 300)) win <- w * 60
      bal <- bal - w + win
      t <- t + if (i %% 140 == 0) 20000 else stats::runif(1, 3, 8)
      tk <- track_add(tk, t, bal, w, win, cat_i, cat_i %in% c("classic", "animal") && stats::runif(1) < .5)
    }
    tk <- track_add_session(tk, now_secs() - 3 * 86400, 2460, 140, 2.4e6, 2.29e6)
    tk <- track_add_session(tk, now_secs() - 2 * 86400, 1740, 140, 2.29e6, 2.61e6)
    tk <- track_add_session(tk, now_secs() - 86400, 1980, 140, 2.61e6, bal)
    p$rv$track <- tk; p$rv$coins <- bal
    p$rv$stats <- utils::modifyList(p$rv$stats, list(plays = 1260, wagered = 9.8e6, won = 9.61e6, biggest = 845000,
                                                     biggest_game = "bankjackpots", spins = 880, hands = 380, packs = 23))
  })
  store$save_player(FC_DEMO_PLAYER, shiny::isolate(p$snapshot()), "en", features = p$features())
  ref <- archetype_reference()
  set.seed(5)
  pick <- ref[sample(nrow(ref), 36), ]
  for (i in seq_len(nrow(pick)))
    store$save_player(sprintf("community-%04d", i),
                      list(v = 1L, coins = round(stats::runif(1, 2e5, 6e6)), level = sample(3:30, 1), avatar = "", name = FC_DEMO_NAMES[i]),
                      "en", features = unlist(pick[i, names(ARCHETYPE_SD)]))
  invisible(TRUE)
}

# Value of the `__seed` input as an integer seed (NULL if unusable).
fc_demo_seed_value <- function(x) {
  x <- suppressWarnings(as.integer(x))
  if (length(x) != 1 || is.na(x)) NULL else x
}
