# ---------------------------------------------------------------------------
# app_server.R -- wires every module to one shared player state, handles the
# language of the session and loading / saving the player's progress.
# ---------------------------------------------------------------------------

# Language of a session: ?lang= in the URL, else the default option.
fc_session_lang <- function(session) {
  q <- tryCatch(shiny::isolate(session$clientData$url_search), error = function(e) NULL)
  fc_normalize_lang(if (length(q) && nzchar(q)) shiny::parseQueryString(q)$lang else NULL) %||% fc_default_lang()
}

fc_app_server <- function(input, output, session) {
  session$userData$fc_lang <- fc_session_lang(session)
  store <- fc_storage()

  # One reactive player state per browser session, shared by every module
  player <- player_state(session, store = store)
  # Linked progressive jackpot pools, shared by every session on this server
  linked <- linked_jackpots(store)

  # Demo mode (run_app(demo = TRUE)): demo players, and scripted randomness
  if (fc_demo_enabled()) {
    fc_demo_seed(store)
    observeEvent(input[["__seed"]], {
      seed <- fc_demo_seed_value(input[["__seed"]])
      if (!is.null(seed)) set.seed(seed)
    })
  }

  hud_server("hud", player)
  lobby_server("lobby", player, linked)
  classic_slots_server("classic", player, linked)
  myth_slots_server("myth", player)
  animal_slots_server("animal", player)
  table_games_server("tables", player)
  meta_server("meta", player)
  stats_server("stats", player)
  profile_server("profile", player)

  # Cross-module navigation requests (lobby tiles, Monsterpedia "Play", HUD)
  observeEvent(player$rv$nav, {
    nav_select("main_nav", player$rv$nav$tab)
  })

  # ---- load the player --------------------------------------------------------
  # The browser sends {id, saved}: `id` is its anonymous player id and `saved`
  # a profile kept in localStorage (browser backend, or older versions).
  observeEvent(input$fc_hello, {
    hello <- input$fc_hello
    id <- hello$id
    if (!fc_valid_player_id(id)) {
      id <- fc_new_player_id()
      player$send_id(id)
    }
    player$set_id(id)
    ok <- store$server_side && player$load(id)
    if (!ok && is.character(hello$saved) && nzchar(hello$saved)) {
      saved <- tryCatch(jsonlite::fromJSON(hello$saved, simplifyVector = FALSE), error = function(e) NULL)
      ok <- !is.null(saved) && isTRUE(player$restore(saved))
    }
    player$rv$restored <- TRUE
    if (nzchar(player$name())) {
      player$session_begin()
      player$toast(tr("Welcome back, %s!", player$name()), tr("Balance restored: %s coins", fmt_coins(player$coins())), "success", "\U0001F44B")
    } else {
      # first visit (or a profile saved before names existed): ask for a name
      player$ask_name("welcome")
    }
  }, once = TRUE)

  # ---- save the player -----------------------------------------------------------
  # Save whenever the profile changes (at most once per reactive flush, i.e.
  # once per spin / hand), plus a final save when the session ends.
  snapshot <- reactive({
    req(player$rv$restored, nzchar(player$rv$name))
    player$snapshot()
  })
  observe({
    snap <- snapshot()      # read first: registers the dependency (save() may return early)
    player$save(snap)
  })
  session$onSessionEnded(function() {
    shiny::isolate(if (isTRUE(player$rv$restored)) {
      player$session_end()
      player$save()
    })
  })
}
