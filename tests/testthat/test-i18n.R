test_that("every translatable string has a pt_BR translation", {
  keys <- i18n_data_keys()
  src <- source_r_dir()
  if (!is.null(src)) keys <- c(keys, i18n_literal_keys(src))
  keys <- unique(keys[grepl("[A-Za-z]", keys)])
  missing <- setdiff(keys, names(I18N_PT_BR))
  expect_equal(missing, character(0))
})

test_that("translations keep the sprintf placeholders", {
  spec <- function(x) regmatches(x, gregexpr("%[-+0#]*[0-9]*(\\.[0-9]+)?[sdgfxi%]", x))[[1]]
  bad <- names(I18N_PT_BR)[vapply(names(I18N_PT_BR), function(k) !identical(spec(k), spec(I18N_PT_BR[[k]])), TRUE)]
  expect_equal(bad, character(0))
  expect_false(anyDuplicated(names(I18N_PT_BR)) > 0)
})

test_that("tr() follows the active language and falls back to English", {
  expect_equal(with_lang("en", tr("Lobby")), "Lobby")
  expect_equal(with_lang("pt_BR", tr("DEAL")), "DAR CARTAS")
  expect_equal(with_lang("pt_BR", tr("Spin %d times", 3L)), "Gire 3 vezes")
  expect_equal(with_lang("pt_BR", tr("a string nobody translated")), "a string nobody translated")
  expect_equal(with_lang("pt_BR", trn(1, "%d spin", "%d spins", 1L)), "1 giro")
  expect_equal(with_lang("pt_BR", trn(2, "%d spin", "%d spins", 2L)), "2 giros")
  expect_equal(with_lang("pt_BR", tr(c("Red", "Black"))), c("Vermelho", "Preto"))
})

test_that("numbers use the locale's separators", {
  expect_equal(with_lang("en", fmt_coins(1234567)), "1,234,567")
  expect_equal(with_lang("pt_BR", fmt_coins(1234567)), "1.234.567")
  expect_equal(with_lang("pt_BR", fmt_short(2500000)), "2,5M")
  expect_equal(with_lang("pt_BR", fmt_num(95.5)), "95,5")
})

test_that("language tags are normalised", {
  expect_equal(fc_normalize_lang("pt-BR"), "pt_BR")
  expect_equal(fc_normalize_lang("PT"), "pt_BR")
  expect_equal(fc_normalize_lang("en-US"), "en")
  expect_null(fc_normalize_lang("de"))
  withr::local_options(fortuneCougar.lang = "pt_BR")
  expect_equal(fc_lang(), "pt_BR")
})

test_that("the engines speak the player's language", {
  with_lang("pt_BR", {
    tb <- bj_new_table("classic")
    expect_equal(tb$message, "Fa\u00e7a sua aposta e pressione DAR CARTAS.")
    expect_equal(rl_bet_label("split:17-20"), "Cavalo 17/20")
    expect_match(monster_challenge_text("spins", 25), "^Gire 25 vezes$")
  })
})

test_that("browser strings are sent translated", {
  js <- fc_js_i18n("pt_BR")
  expect_equal(js[["BIG WIN!"]], "GRANDE PR\u00caMIO!")
  expect_setequal(names(js), FC_JS_STRINGS)
})
