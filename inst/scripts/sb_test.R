devtools::load_all()

competitions <- sb_get_competitions()

competitions_copa_do_brasil <- competitions |>
  dplyr::filter(competition_name == "Copa do Brasil", season_name == "2026")

matches_copa_do_brasil <- sb_get_matches(competitions_copa_do_brasil)
