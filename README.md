# fortuneCougar

A social casino (play money only) built as an R **Shiny package**: 22 feature
slot machines, 12 table games and a full metagame, all sharing one reactive
player session. Progress and the linked jackpots persist in **SQLite**, and the
whole interface is available in **English and Brazilian Portuguese**.

## Running it

```r
# one-time: packages not yet in renv.lock
renv::install(c("DBI", "RSQLite", "testthat", "withr", "pkgload"))
renv::snapshot()

shiny::runApp()                                  # from this folder (loads the sources)
fortuneCougar::run_app()                         # after devtools::install()
fortuneCougar::run_app(lang = "pt_BR")           # Portuguese by default
fortuneCougar::run_app(db_path = "casino.sqlite", storage = "sqlite")
```

On the first visit the app asks for a **player name**; coins, cards and
statistics are saved under it, and typing the same name later (on any browser)
continues that profile. Click the name in the header for **My Stats**.

Keyboard: **Space** spins the visible machine, **T** toggles turbo animations.
The **EN / PT** button in the header switches language (remembered per
browser; `?lang=pt_BR` in the URL works too).

| option (or `run_app()` argument) | values | default |
|---|---|---|
| `fortuneCougar.lang` / `lang` | `"en"`, `"pt_BR"` | `"en"` |
| `fortuneCougar.storage` / `storage` | `"sqlite"`, `"browser"`, `"memory"` | `"sqlite"` if RSQLite is installed, else `"browser"` |
| `fortuneCougar.db_path` / `db_path` | file path | `tools::R_user_dir("fortuneCougar", "data")/fortuneCougar.sqlite` |

## Package structure

```
DESCRIPTION, NAMESPACE      package metadata (roxygen-managed imports)
app.R                       dev entry point: pkgload::load_all() + run_app()
R/
  run_app.R                 exported run_app()
  app_ui.R, app_server.R    top-level UI (bslib, dark theme) and server
  app_config.R              options: language, storage backend, database path
  mod_*.R                   Shiny modules
    mod_slot_machine.R        generic slot machine (one instance per machine)
    mod_slot_category.R       category lobby + machine switching
    mod_classic_slots.R, mod_myth_slots.R, mod_animal_slots.R
    mod_blackjack.R, mod_video_poker.R, mod_roulette.R, mod_table_games.R
    mod_meta.R                Album, Lucky Cards, Wheel of Fun, Star Market,
                              Monsterpedia, Rewards (+ leaderboard), Daily Dash
    mod_stats.R               My Stats: money, time played, drawdown, tendency,
                              category, kNN archetype, k-means community clusters
    mod_profile.R             player name dialog (new player / returning / switch)
    mod_lobby.R, mod_hud.R    lobby, header HUD (name, coins, level, VIP, EN/PT, turbo)
  fct_*.R                   business logic, no Shiny
    fct_slots_engine.R        reel strips, paylines, ways, cascades, bonuses
    fct_slots_classic.R / _myth.R / _animal.R   the 22 machines
    fct_slots_registry.R      machine catalogue + RTP calibration
    fct_cards.R, fct_blackjack.R, fct_poker.R, fct_roulette.R
    fct_meta.R                album, packs, market, wheel, VIP, missions, monsters
    fct_player_state.R        the global reactive player session + linked jackpots
    fct_stats.R               statistics track, drawdown, tendency, kNN, k-means
    fct_storage.R             SQLite / memory / browser persistence
  utils_helpers.R, utils_ui.R          formatting and shared UI builders
  utils_i18n.R, utils_i18n_pt_BR.R     tr()/trn() and the pt_BR dictionary
inst/app/www/               casino.css (theme) and casino.js (animations, HUD,
                            toasts, language switch, player id), served at fc-assets/
tests/testthat/             the test suite (see below)
tools/                      simulate_rtp.R (RTP calibration), run_tests.R
man/                        run_app.Rd, package docs
```

**Design rule:** all game maths lives in plain R functions (`fct_*`). Modules
only move coins, render and forward events. A spin is computed instantly on the
server and sent to the browser as animation frames (spin → expand → cascade →
respin …) that `casino.js` plays, so the UI stays fast while R decides every
outcome.

**Global player state:** `player_state()` (one per browser session) is passed
to every module. It owns the coin balance and every progression: XP/level
(higher levels unlock bigger bets), Cougar Album cards, stars, avatars, boosts,
loyalty points per game family, Daily Dash progress, Monsterpedia captures,
wheel cooldowns and per-machine meters. Modules call `player$debit()` /
`player$credit()` and `player$record(...)` after every play; `record()` fans
the event out to XP, loyalty points, card-pack drops (and Lucky Cards), Daily
Dash, Monsterpedia and the play log.

## Persistent data (SQLite)

`fct_storage.R` creates the schema on first use (`PRAGMA user_version = 2`, WAL
mode) and upgrades version-1 databases in place (new columns via `ALTER TABLE`,
data kept):

| table | contents |
|---|---|
| `players` | one row per player: the full profile as JSON (`state`) plus `name` (unique, case-insensitive), `coins`, `level`, `avatar`, `lang`, behaviour `features` (JSON, for the community clustering), timestamps |
| `jackpots` | the server-wide linked progressive pots (survive restarts) |
| `plays` | append-only log of every play: player, time, game family, game, wager, win, balance after the play, category |
| `sessions` | one row per play session: start, end, seconds, start/end balance, plays |
| `jackpot_wins` | every linked jackpot hit |

The browser keeps an anonymous player id (`localStorage`). On connect it sends
the id and the server loads that player's row. A player without a name is asked
for one first (nothing is saved before that); a name that already exists loads
that player's profile, so returning players continue on any browser. Profiles
are saved after every change (once per spin or hand) and when the session ends
(which also logs the session). A profile saved by the old browser-only storage
is migrated automatically. All queries are parameterised; one connection per
database file is shared by the sessions of an R process. The Rewards page
shows a leaderboard (by name) read from the `players` table. Storage errors
become warnings, never crashes. Play money only: names are not passwords, so
anyone who types a name plays that profile.

## Player statistics (My Stats)

Every play appends to a compact *track* saved inside the profile (so it works
with every storage backend): a ring buffer of the last 500 plays (time,
balance after the play, wager, net result, category) plus lifetime aggregates.
The page (header name button, or *Meta & Side Games → My Stats*) shows:

* **Money** — balance and peak, net result, wagered / won, return to player,
  biggest win.
* **Time played** — active time (gaps over 2 minutes count as idle) and total
  time online over all sessions; a table of recent sessions with duration,
  plays and result.
* **Drawdown** — maximum and current peak-to-trough fall (coins and % of the
  peak), and an SVG balance chart with the running peak and the drawdown area.
* **Tendency** — balance trend (least-squares slope over the last 100 plays),
  bet-size trend (last 50 vs previous 50 bets), loss chasing (mean bet after a
  loss ÷ after a win), current streak and the recent return.
* **Category** — share of coins wagered per category (3 slot categories,
  blackjack, video poker, roulette, side games), favourite category, stake class
  (Micro → VIP) and style (slots player / table player / all-rounder).
* **Classification** — five behaviour features (average bet (log10), plays per
  minute, high-risk share = high-volatility slots + double-or-nothing, table-game
  share, bet escalation after losses) classify the player with **k-nearest
  neighbours** (k = 9, standardised features, majority vote) against a
  deterministic synthetic training set of five archetypes: Casual Spinner,
  Grinder, Thrill Seeker, High Roller, Card Shark (needs 20 plays).
* **Community clusters** — with server-side storage the features of every
  player are stored in the database and clustered with **k-means**
  (`stats::kmeans`, 2–4 clusters, 10 starts, once 8+ players have 20+ plays);
  each cluster is named after the archetype nearest its centre and your cluster
  is highlighted.

## Languages (en / pt_BR)

Every user-facing string is written in English and wrapped in `tr()` (or
`trn()` for plurals); `utils_i18n_pt_BR.R` maps ~1,000 strings to Brazilian
Portuguese, and numbers follow the locale (1.000.000 / 95,5%). The language of
each session comes from `?lang=`, the player's remembered choice or the
`fortuneCougar.lang` option; engine code called outside a session (tests, the
RTP simulator) stays in English. Game titles (e.g. *Vegas Strip*) are product
names and are not translated. `tests/testthat/test-i18n.R` fails if a string
is added without a translation or if a translation breaks a `sprintf()`
placeholder.

## Checklist coverage

| todo item | implementation |
|---|---|
| Cougar Album (Like a Pro) — the todo's "HoF Album" | 81 cards in 9 sets, 1–5★ rarity, set prizes (250K–1.5M) and 25M album prize; packs drop on 4% of plays, on level-ups, from Dash chests, Monsterpedia and the market |
| Wheel of Fun | hourly wheel + once-a-day bonus wheel with x1–x10 multiplier ring, +VIP bonus, SVG wheel animated to the server's result |
| Star Market | trade spare cards for stars (1/2/4/7/12★ by rarity); buy coins, packs, Double XP, +20% Win Booster, wheel recharge, 6 exclusive avatars |
| Lucky Cards | one golden card per set; pulling it opens a 3-card pick (coins / stars / pack) |
| Monsterpedia | 4-chapter map tied to Stone Gold, Frankenstein Rising, Vile Vixens, Vampire's Kiss; 5 capture challenges each, sequential unlock, coin + avatar rewards |
| Playtika Rewards | 1 point per 1,000 coins wagered across every game family, 6 tiers (Bronze→Royal Diamond) boosting wheel prizes and the daily status gift |
| Daily Dash | 3 daily, 3 weekly, 3 monthly missions (deterministic per period), claimable rewards and period chests |
| Classic 777 Cash | 3-reel stepper with blanks, 5 lines, Cash Wilds x2/x3/x5 that multiply, ANY 7 / ANY BAR |
| Bank of Jackpots | 5x25 lines; 3+ Vaults → match-3 pick for Mini/Minor/Major/Grand **linked progressive** |
| Vegas Strip | Neon Star scatter pays from 2, 3+ = 10 free spins at x2, retriggerable |
| Classic Ruby | stacked Ruby wilds on real reel strips; the machine nudges a reel ±1 stop to complete a line |
| City of Queens | crowns on reels 1/3/5 → pick-until-COLLECT jewel bonus with x2 |
| Rapid Fire Jackpot | heat meter grows with spins <3 s apart and adds Rapid Fire bolts; 6–9+ bolts = 15x–1000x jackpots |
| Purrymid Prince | cat-pharaoh wilds expand over reels 2–4; scarabs → 4-chamber Tomb Raid (doors, mummy trap, treasure) |
| Stone Gold | winners turn to stone & lock; other cells respin while new wins appear |
| Blackbeard's Riches | cascading reels; maps sail a 15-island voyage with prizes at 5/10/15 (average-bet based) |
| Giant's Treasure | beanstalk wilds grow upwards to the top of the reel; full-reel stalks climb toward the Giant's Treasure |
| Cinderella | midnight countdown (clocks speed it up); at midnight pumpkins & mice become carriage wilds |
| Mighty Phoenix | 243 ways; 6+ fireballs → Hold & Spin (3 resetting respins, values, Mini/Minor/Major, full screen = Grand) |
| Dragon's Fury | random Dragon's Fury turns 1–3 whole reels wild; very high volatility |
| Mermaid's Pearl | oysters add 1–3 pearls; 100 pearls = Grand Jackpot wheel |
| Wild West Bounty | dual screen: badges open a saloon shooting gallery on the top screen (3+ bullets) |
| 3 Tigers | 3+ paws → choose 15 FS x2, 10 FS x3 or 6 FS x5 (equal value, different variance) |
| Kitty Gems | 30 lines, low symbols pay from 2, diamond wilds x2/x3 multiply; ~54% hit rate |
| Gummy King 2 | 5x4, 1024 ways, cascades with x1→x2→x3→x5→x8→x12→x20 multiplier |
| Frankenstein Rising | wilds revive in place (45%); 3+ flasks → Laboratory potion pick; feeds Monsterpedia |
| Vile Vixens | every wild becomes sticky for the next 3 spins (counter shown) |
| Dark Jaguar | stacked mystery symbols all reveal the same random symbol |
| Vampire's Kiss | 3+ Blood Moons → 3 respins where bat wilds transfer to new positions and grow in number |
| Classic Blackjack | 6-deck shoe (75% penetration), S17, double any two, DAS, split to 4, peek + insurance, BJ 3:2 |
| European Blackjack | no hole card (dealer draws after the player), dealer BJ takes doubles/splits, double 9–11 |
| Blackjack Switch | two hands, switch the top cards, H17, BJ pays 1:1, dealer 22 pushes |
| Perfect Pairs | classic rules + side bet: mixed 6:1, coloured 12:1, perfect 25:1 |
| Video poker (all 4 games) | 1, 3, 5, 10 or 25 hands per deal: the held cards are copied to every hand and each hand draws from its own shuffle of the remaining deck; then **double or nothing** (beat the dealer's card, aces high, ties push, up to 5 doubles) |
| Jacks or Better | 9/6 full pay |
| Deuces Wild | full pay; deuces wild, four deuces 200, wild royal 25 |
| Bonus Poker | 8/5; four aces 80, four 2s–4s 40 |
| Joker Poker | 53-card deck, Kings or Better, five of a kind 200 |
| European Roulette | 37 pockets, every inside/outside bet (straight, split, street, corner, six-line, first four, dozens, columns, even money) |
| American Roulette | 38 pockets with 00: 0-00 split, 00 trios and the 0-00-1-2-3 basket (6:1) |
| French Roulette | single zero + La Partage (half back on even money) + Voisins / Tiers / Orphelins / Jeu Zéro call bets |
| Multi-Wheel Roulette | one bet layout on 1–6 independently spinning European wheels |

## Mathematics

* **RNG**: R's Mersenne-Twister via `sample()` / `runif()`. Reel strips are
  built once per machine from symbol weights (seeded by the machine id, so
  every session plays the same physical strips); each spin picks a uniform
  stop per reel (χ² tested). Cards are uniform permutations of 1/6-deck shoes.
* **Slot RTP**: each machine has a single pay `scale` solved with
  `tools/simulate_rtp.R`, which plays the exact module code path (free spins,
  retriggers, persistent meters, bonus games, linked pots). The table below is
  generated from the final simulation run.
* **Table games** use the standard rules/paytables, so their published
  theoretical returns apply: Classic Blackjack ≈ 99.6% with basic strategy;
  Jacks or Better 9/6 99.54%, Deuces Wild (full pay) 100.76%, Bonus Poker 8/5
  99.17%, Joker Poker (Kings or Better, full pay) 100.65% with optimal holds.
  Roulette edges (2.70% European, 5.26% American with the basket at 7.89%,
  1.35% on French even-money bets) are verified exactly in
  `tests/test_tables.R`, together with blackjack rule scenarios, a brute-force
  check of the wild-card poker evaluator and dealt-hand frequencies.
  Multi-hand video poker keeps the same return per hand (each hand is an
  independent draw from the 47 unseen cards), and double or nothing is a fair
  bet (P(win) = P(lose) = 24/51, push 3/51), so it adds variance, not edge.

Slot calibration (target 95.5%; ± is one standard error of the pooled
Monte-Carlo estimate, 180k–340k spins per machine):

| machine | volatility | pay scale | RTP | hit rate | feature rate |
|---|---|---|---|---|---|
| Classic 777 Cash | High | 0.4988 | 95.5% ± 1.6% | 24% | — |
| Bank of Jackpots | High | 1.472 | 95.5% ± 1.5% | 40% | 1 in 333 |
| Vegas Strip | Medium | 0.9888 | 95.5% ± 1.6% | 45% | 1 in 50 |
| Classic Ruby | Medium | 0.7648 | 95.5% ± 0.8% | 48% | 20% |
| City of Queens | Medium | 1.801 | 95.5% ± 2.9% | 32% | 1 in 100 |
| Rapid Fire Jackpot | High | 2.475 | 95.5% ± 0.8% | 44% | — |
| Purrymid Prince | Medium | 0.384 | 95.5% ± 1.3% | 38% | 35% |
| Stone Gold | Medium | 0.5983 | 95.5% ± 0.8% | 34% | 34% |
| Blackbeard's Riches | Medium | 0.2373 | 95.5% ± 0.3% | 43% | 43% |
| Giant's Treasure | Medium | 0.7134 | 95.5% ± 0.6% | 36% | 14% |
| Cinderella | Low-Medium | 0.9816 | 95.5% ± 1.4% | 34% | 11% |
| Mighty Phoenix | High | 1.929 | 95.5% ± 6.5% | 28% | 1 in 213 |
| Dragon's Fury | Very High | 1.805 | 95.5% ± 1.8% | 38% | 11% |
| Mermaid's Pearl | Medium | 1.232 | 95.5% ± 3.3% | 34% | 1 in 103 |
| Wild West Bounty | Medium | 1.07 | 95.5% ± 1.7% | 30% | 1 in 41 |
| 3 Tigers | High | 1.89 | 95.5% ± 2.2% | 36% | 1 in 38 |
| Kitty Gems | Low | 1.837 | 95.5% ± 1.1% | 54% | — |
| Gummy King 2 | Medium-High | 0.2425 | 95.5% ± 1.2% | 70% | 70% |
| Frankenstein Rising | Medium | 0.9245 | 95.5% ± 1.1% | 41% | 32% |
| Vile Vixens | Medium | 1.793 | 95.5% ± 1.1% | 45% | 60% |
| Dark Jaguar | Medium-High | 3.828 | 95.5% ± 1.2% | 34% | 59% |
| Vampire's Kiss | Medium-High | 2.515 | 95.5% ± 1.1% | 32% | 1 in 40 |

Feature rate = share of spins with a feature event (bonus, free spins,
expansion, respin, cascade, transform...). Bank of Jackpots' linked pots
return ≈27 points of its RTP (analytic renewal estimate, confirmed by simulation).

## Tests

```r
source("tools/simulate_rtp.R")            # RTP simulator (project folder)
simulate_slot("vegasstrip", n = 1e5)      # one machine
simulate_rtp(n = 2e4)                     # every machine, one row each
# terminal: Rscript tools/simulate_rtp.R [ids|all] [n]

devtools::test()             # or: Rscript tools/run_tests.R
devtools::check()            # R CMD check, runs the same suite
Sys.setenv(FC_E2E = "1")     # also run the shinytest2 browser test
```

`tests/testthat/` (testthat 3rd edition):

| file | what it checks |
|---|---|
| `test-slots-engine.R` | payline evaluator vs a naive reference on every machine, ways, scatters, strips (χ² uniform), cascades |
| `test-slots-mechanics.R` | every machine's signature mechanic (sticky, midnight, beanstalks, mystery, stone lock, Hold & Spin…), free spins, bonus games |
| `test-slots-rtp.R` | Monte-Carlo RTP sanity on low-variance machines |
| `test-blackjack.R` | rigged shoes for all four rule sets, payouts, basic-strategy return |
| `test-poker.R` | hand ranking, wild cards vs brute-force substitution, exact deal frequencies, multi-hand draws (held cards, independent decks, unbiased odds), double or nothing |
| `test-stats.R` | track, idle time, drawdown, tendency, loss chasing, category, features, kNN accuracy, k-means purity, JSON round-trip, chart |
| `test-roulette.R` | wheels, payouts, La Partage, call bets, exact house edges, uniform spins |
| `test-meta.R` | album, packs, levels, VIP tiers, wheel EV, missions, Monsterpedia |
| `test-player-state.R` | coins, JSON round-trip, `record()` fan-out, claims, market, Lucky Cards, statistics track, name, sessions |
| `test-storage.R` | storage contract on the memory and SQLite backends (names, features, sessions), persistence across restarts, v1 → v2 migration, errors → warnings |
| `test-i18n.R` | dictionary completeness, placeholders, number formats, translated engines |
| `test-modules.R` | slot machine, blackjack, video poker (incl. 10 hands + double), roulette, Daily Dash and My Stats modules via `shiny::testServer()` |
| `test-app.R` | `run_app()`, UI in both languages, app server load/save, name required before saving, returning player on another browser, switching player |
| `test-e2e.R` | optional shinytest2 browser test (set `FC_E2E=1`) |

Pick a licence (e.g. `usethis::use_mit_license()`) to clear the remaining
`R CMD check` licence warning.
