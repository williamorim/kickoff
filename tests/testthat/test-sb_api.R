test_that("sb_api_versions() lists the defaults", {
  versions <- sb_api_versions()
  expect_named(versions, c("endpoint", "version", "default"))
  expect_equal(versions$version, versions$default)
  expect_equal(versions$version[versions$endpoint == "events"], "v11")
})

test_that("the statsbomb.versions option overrides the defaults", {
  withr::local_options(statsbomb.versions = list(events = "v10"))
  versions <- sb_api_versions()
  expect_equal(versions$version[versions$endpoint == "events"], "v10")
  expect_equal(versions$default[versions$endpoint == "events"], "v11")
  expect_equal(sb_version("lineups"), "v5")
})

test_that("match ids are extracted from vectors and data frames", {
  expect_equal(sb_match_ids(c(4039348, 4039348, 1e6)), c("4039348", "1000000"))
  expect_equal(sb_match_ids(data.frame(match_id = 1:2)), c("1", "2"))
  expect_error(sb_match_ids(data.frame(id = 1)), "match_id")
})

test_that("competition/season pairs are validated and deduplicated", {
  comp <- data.frame(competition_id = c(71, 71), season_id = c(316, 316), x = 1:2)
  expect_equal(nrow(sb_competition_seasons(comp)), 1)
  expect_error(sb_competition_seasons(data.frame(competition_id = 71)), "season_id")
})
