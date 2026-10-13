# Live tests: check that each endpoint returns the columns of the expected
# version. The API silently falls back to a basic format when a version does
# not exist, so missing columns are the only sign of a wrong version.

skip_if_no_credentials <- function() {
  skip_on_cran()
  if (Sys.getenv("STATSBOMB_USERNAME") == "" || Sys.getenv("STATSBOMB_PASSWORD") == "") {
    skip("StatsBomb credentials not available")
  }
}

# Brazilian Serie A 2026 and Grêmio x São Paulo
serie_a <- data.frame(competition_id = 71, season_id = 316)
match_id <- 4039348

test_that("sb_get_competitions() returns the v4 columns", {
  skip_if_no_credentials()
  comp <- sb_get_competitions()
  expect_true(all(c("competition_id", "season_id", "competition_gender", "match_available_360") %in% names(comp)))
})

test_that("sb_get_matches() iterates over competitions", {
  skip_if_no_credentials()
  two <- data.frame(competition_id = c(71, 1456), season_id = c(316, 316))
  matches <- sb_get_matches(two)
  expect_setequal(unique(matches$competition.competition_id), c(71, 1456))
  expect_true(all(c("match_id", "match_status_360", "home_team.managers") %in% names(matches)))
})

test_that("sb_get_events() returns OBV and match_id", {
  skip_if_no_credentials()
  events <- sb_get_events(match_id)
  expect_equal(names(events)[1], "match_id")
  expect_true(all(c("id", "obv_total_net", "location") %in% names(events)))
})

test_that("sb_get_lineups() returns positions and formations", {
  skip_if_no_credentials()
  lineups <- sb_get_lineups(data.frame(match_id = match_id))
  expect_equal(nrow(lineups), 2)
  expect_true(all(c("match_id", "lineup", "formations") %in% names(lineups)))
  expect_true("positions" %in% names(lineups$lineup[[1]]))
})

test_that("sb_get_360() returns the 360 metrics", {
  skip_if_no_credentials()
  frames <- sb_get_360(match_id)
  expect_true(all(c("match_id", "event_uuid", "freeze_frame", "line_breaking_pass") %in% names(frames)))
})

test_that("match stats endpoints return one row per player/team", {
  skip_if_no_credentials()
  player <- sb_get_player_match_stats(match_id)
  team <- sb_get_team_match_stats(match_id)
  expect_true(all(c("match_id", "player_id", "player_match_np_xg") %in% names(player)))
  expect_equal(nrow(team), 2)
})

test_that("season endpoints return data for the competition", {
  skip_if_no_credentials()
  expect_true("player_id" %in% names(sb_get_player_season_stats(serie_a)))
  expect_equal(nrow(sb_get_team_season_stats(serie_a)), 20)
  expect_true(all(c("offline_player_id", "live_player_id") %in% names(sb_get_player_mapping(serie_a))))
})

test_that("matches without data return an empty tibble with a warning", {
  skip_if_no_credentials()
  expect_warning(events <- sb_get_events(999999999), "No data returned")
  expect_equal(nrow(events), 0)
})

test_that("several matches are combined, skipping those without data", {
  skip_if_no_credentials()
  expect_warning(events <- sb_get_events(c(match_id, 999999999)), "match 999999999")
  expect_equal(unique(events$match_id), match_id)
})
