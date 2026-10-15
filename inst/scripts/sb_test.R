devtools::load_all()

# Versions

sb_api_versions()

competitions <- sb_get_competitions()

competitions_copa_do_brasil <- competitions |>
  dplyr::filter(competition_name == "Copa do Brasil", season_name == "2026")

matches_copa_do_brasil <- sb_get_matches(competitions_copa_do_brasil)

matches_id <- matches_copa_do_brasil |> 
  dplyr::filter(match_status == "available") |> 
  dplyr::pull(match_id) |> 
  unique()

events_copa_do_brasil <- sb_get_events(
  matches = matches_id
)



