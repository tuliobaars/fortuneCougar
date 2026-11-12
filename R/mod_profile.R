# ---------------------------------------------------------------------------
# mod_profile.R -- the player name: asked at startup, used to save the player
# and to find the profile again in later sessions.
#
#   * "welcome" mode (first visit): a new name is claimed by the current
#     profile; a name that already exists in the database loads that profile
#     (so a returning player continues on any browser).
#   * "switch" mode (from My Stats): an existing name loads that profile, a new
#     name starts a fresh profile. With browser-only storage there is a single
#     local profile, so the name is simply changed.
# Names are unique (case-insensitive) in the database. There are no passwords:
# coins are play money.
# ---------------------------------------------------------------------------

name_modal <- function(ns, mode = "welcome", server_side = TRUE, value = "", error = NULL) {
  welcome <- identical(mode, "welcome")
  modalDialog(
    title = if (welcome) paste("\U0001F406", tr("Welcome to fortuneCougar!"))
            else if (server_side) paste("\U0001F465", tr("Switch player")) else paste("\u270F\uFE0F", tr("Change name")),
    div(class = "fc-name-modal",
        p(if (welcome) tr("Pick a player name. Your coins, cards and statistics are saved under this name between sessions.")
          else if (server_side) tr("Type the name of another player profile, or a new name to start a fresh profile.")
          else tr("Choose a new player name.")),
        if (server_side && welcome) p(class = "fc-name-hint", tr("Played here before? Type the same name to continue where you left off.")),
        textInput(ns("name"), tr("Player name"), value = value, placeholder = tr("e.g. LuckyCougar"), width = "100%"),
        div(class = "fc-name-rules", tr("2-20 characters: letters, digits, spaces, dots, dashes or underscores.")),
        if (!is.null(error)) div(class = "fc-name-error", role = "alert", error)),
    footer = tagList(
      if (!welcome) modalButton(tr("Cancel")),
      actionButton(ns("ok"), if (welcome) tr("Start playing") else tr("Continue"), class = "btn-deal fc-name-ok")),
    easyClose = FALSE, fade = TRUE, size = "m")
}

profile_server <- function(id, player) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    store <- player$store
    mode <- reactiveVal("welcome")

    show <- function(m, value = "", error = NULL) {
      mode(m)
      showModal(name_modal(ns, m, store$server_side, value, error))
    }
    observeEvent(player$rv$ask_name, show(player$rv$ask_name$mode))

    observeEvent(input$ok, {
      name <- fc_clean_name(input$name %||% "")
      m <- isolate(mode())
      if (!fc_valid_name(name)) {
        show(m, name, tr("Please use 2-20 letters or digits (spaces, dots, dashes and underscores are allowed)."))
        return()
      }
      me <- player$id()
      other <- if (store$server_side) store$find_player_by_name(name) else NULL
      same <- identical(tolower(name), tolower(player$name()))
      if (!is.null(other) && !identical(other, me) && is.null(store$load_player(other))) {
        show(m, name, tr("This name is not available. Please choose another one."))
        return()
      }
      removeModal()

      if (m == "welcome") {
        if (!is.null(other) && !identical(other, me) && player$load(other)) {
          player$send_id(other)
          player$session_begin()
          player$toast(tr("Welcome back, %s!", player$name()), tr("Balance restored: %s coins", fmt_coins(player$coins())), "success", "\U0001F44B")
          return()
        }
        player$set_name(name)
        player$session_begin()
        if (player$coins() == START_COINS && isolate(player$rv$stats$plays) == 0)
          player$toast(tr("Welcome, %s!", name), tr("Here are 1,000,000 coins to get you started."), "success", "\U0001F389")
        else player$toast(tr("Welcome, %s!", name), tr("Your progress is now saved under this name."), "success", "\U0001F44B")
        return()
      }

      # switch / rename
      if (same && (is.null(other) || identical(other, me))) {
        player$set_name(name)                             # only the capitalisation changed
        return()
      }
      if (!store$server_side) {
        player$set_name(name)
        player$toast(tr("Name changed"), tr("You are now playing as %s.", name), "success", "\u270F\uFE0F")
        return()
      }
      # close the current player's session and save it before leaving
      player$session_end()
      player$save()
      if (!is.null(other) && player$load(other)) {
        player$send_id(other)
        player$session_begin()
        player$toast(tr("Welcome back, %s!", player$name()), tr("Balance restored: %s coins", fmt_coins(player$coins())), "success", "\U0001F44B")
      } else {
        new_id <- fc_new_player_id()
        player$set_id(new_id)
        player$send_id(new_id)
        player$reset()
        player$set_name(name)
        player$session_begin()
        player$toast(tr("Welcome, %s!", name), tr("Here are 1,000,000 coins to get you started."), "success", "\U0001F389")
      }
      player$go("lobby")
    })
  })
}
