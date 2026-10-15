# Live tests with the StatsBomb Open Data (no credentials needed).

# 2022 World Cup and its final (Argentina x France)
world_cup_2022 <- data.frame(competition_id = 43, season_id = 106)
final_id <- 3869685

test_that("sb_open_competitions() has the same columns as the paid API", {
  skip_on_cran()
  skip_if_offline()
  comp <- suppressMessages(sb_open_competitions())
  expect_true(all(c("competition_id", "season_id", "competition_gender", "match_available_360") %in% names(comp)))
})

test_that("sb_open_matches() returns the matches of the competition", {
  skip_on_cran()
  skip_if_offline()
  matches <- suppressMessages(sb_open_matches(world_cup_2022))
  expect_equal(nrow(matches), 64)
  expect_true(final_id %in% matches$match_id)
})

test_that("match-level open data functions add match_id", {
  skip_on_cran()
  skip_if_offline()
  events <- suppressMessages(sb_open_events(final_id))
  lineups <- suppressMessages(sb_open_lineups(data.frame(match_id = final_id)))
  frames <- suppressMessages(sb_open_360(final_id))

  expect_equal(names(events)[1], "match_id")
  expect_true(all(c("id", "type.name", "location") %in% names(events)))
  expect_equal(nrow(lineups), 2)
  expect_true("positions" %in% names(lineups$lineup[[1]]))
  expect_true(all(c("match_id", "event_uuid", "freeze_frame") %in% names(frames)))
})

test_that("matches without open data return a warning", {
  skip_on_cran()
  skip_if_offline()
  expect_warning(
    frames <- suppressMessages(sb_open_360(c(final_id, 12345678))),
    "match 12345678"
  )
  expect_equal(unique(frames$match_id), final_id)
})

test_that("the terms of use notice is shown once per session", {
  sb_state$open_data_notice_shown <- NULL
  withr::defer(sb_state$open_data_notice_shown <- TRUE)
  expect_message(sb_open_data_notice(), "StatsBomb")
  expect_silent(sb_open_data_notice())
})
