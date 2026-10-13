#' Retrieve StatsBomb Competition Data
#'
#' Function to retrieve the competitions and seasons available to your account.
#' You can have access to competition_id, season_id, competition_name, competition_gender,
#' competition_youth, competition_international, country_name, season_name, match_updated,
#' match_updated_360, match_available, match_available_360.
#'
#' @param version Character. API version to use. Default is the most recent
#'   version as of 2026-10-12. Versions that do not exist do not raise an error
#'   and return fewer columns. See [sb_api_versions()].
#'
#' @return A tibble containing competition information with columns including competition_id,
#'   season_id, competition_name, competition_gender, competition_youth, competition_international,
#'   country_name, season_name, match_updated, match_updated_360, match_available, match_available_360.
#'
#' @examples
#' \dontrun{
#' competitions <- sb_get_competitions()
#' }
#'
#' @export
sb_get_competitions <- function(version = sb_version("competitions")) {
  sb_perform(list(sb_request(version, "competitions")), ids = "competitions")
}

#' Retrieve StatsBomb Match Data
#'
#' Function to retrieve match data from StatsBomb API for one or more competitions and seasons.
#' Returns detailed match information including teams, scores, dates, and match status.
#'
#' @param competitions A data frame containing competition and season IDs, typically
#' a subset of the data.frame returned by [sb_get_competitions()].
#' Must include columns `competition_id` and `season_id`. Matches are
#' retrieved for each row and combined into a single tibble.
#' @inheritParams sb_get_competitions
#' @param remove_deleted_matches Logical. Whether to remove matches with "deleted" status. Default is TRUE.
#'
#' @return A tibble containing match information with columns for match details, teams, scores,
#'   dates, and match status. Deleted matches are excluded if remove_deleted_matches is TRUE.
#'
#' @examples
#' \dontrun{
#' competitions <- sb_get_competitions()
#'
#' # Get matches for Brazilian Serie A 2026
#' serie_a <- competitions |>
#'   dplyr::filter(competition_name == "Serie A", season_name == "2026")
#' matches <- sb_get_matches(serie_a)
#'
#' # Get matches for several competitions at once, including deleted ones
#' brazil <- competitions |>
#'   dplyr::filter(country_name == "Brazil", season_name == "2026")
#' all_matches <- sb_get_matches(brazil, remove_deleted_matches = FALSE)
#' }
#'
#' @export
sb_get_matches <- function(competitions, version = sb_version("matches"),
                           remove_deleted_matches = TRUE) {
  comp_seasons <- sb_competition_seasons(competitions)
  paths <- paste0(
    "competitions/", comp_seasons$competition_id,
    "/seasons/", comp_seasons$season_id, "/matches"
  )

  matches <- sb_perform(
    lapply(paths, sb_request, version = version),
    ids = sb_competition_season_labels(comp_seasons)
  )

  # Optionally, remove deleted matches if specified
  if (remove_deleted_matches && "match_status" %in% names(matches)) {
    matches <- matches[!matches$match_status %in% "deleted", ]
  }

  # Return the match data
  return(matches)
}

#' Retrieve StatsBomb Event Data
#'
#' Function to retrieve the event data of one or more matches, including the
#' On-Ball Value (OBV) columns. Matches are retrieved in parallel.
#'
#' @param matches A vector of match ids or a data frame with the column
#'   `match_id`, typically returned by [sb_get_matches()].
#' @inheritParams sb_get_competitions
#'
#' @return A tibble with one row per event. The column `match_id` is added
#'   to identify the match. Nested fields (e.g. `location`, `tactics.lineup`)
#'   are kept as list columns.
#'
#' @examples
#' \dontrun{
#' events <- sb_get_events(4039348)
#'
#' # All events of the available matches of a competition
#' matches <- sb_get_matches(serie_a)
#' events <- matches |>
#'   dplyr::filter(match_status == "available") |>
#'   sb_get_events()
#' }
#'
#' @export
sb_get_events <- function(matches, version = sb_version("events")) {
  sb_get_match_endpoint(matches, version, path = "events/{match_id}")
}

#' Retrieve StatsBomb Lineup Data
#'
#' Function to retrieve the lineups of one or more matches, including player
#' positions (with time on and off the pitch), formations and cards.
#'
#' @inheritParams sb_get_events
#'
#' @return A tibble with one row per team and match. The column `match_id` is
#'   added to identify the match. Players are in the list column `lineup`.
#'
#' @examples
#' \dontrun{
#' lineups <- sb_get_lineups(4039348)
#' }
#'
#' @export
sb_get_lineups <- function(matches, version = sb_version("lineups")) {
  sb_get_match_endpoint(matches, version, path = "lineups/{match_id}")
}

#' Retrieve StatsBomb 360 Data
#'
#' Function to retrieve the 360 freeze frames of one or more matches, with
#' the visible area, the location of the visible players and 360 metrics.
#' Can be joined to the events using `event_uuid` (360) and `id` (events).
#'
#' @inheritParams sb_get_events
#'
#' @return A tibble with one row per event with 360 data. The column
#'   `match_id` is added to identify the match. Player locations are in the
#'   list column `freeze_frame`.
#'
#' @examples
#' \dontrun{
#' frames <- sb_get_360(4039348)
#' }
#'
#' @export
sb_get_360 <- function(matches, version = sb_version("frames_360")) {
  sb_get_match_endpoint(matches, version, path = "360-frames/{match_id}")
}

#' Retrieve StatsBomb Player Match Stats
#'
#' Function to retrieve aggregated stats of each player in one or more matches.
#'
#' @inheritParams sb_get_events
#'
#' @return A tibble with one row per player and match.
#'
#' @examples
#' \dontrun{
#' player_stats <- sb_get_player_match_stats(4039348)
#' }
#'
#' @export
sb_get_player_match_stats <- function(matches, version = sb_version("player_match_stats")) {
  sb_get_match_endpoint(matches, version, path = "matches/{match_id}/player-stats", add_match_id = FALSE)
}

#' Retrieve StatsBomb Team Match Stats
#'
#' Function to retrieve aggregated stats of each team in one or more matches.
#'
#' @inheritParams sb_get_events
#'
#' @return A tibble with one row per team and match.
#'
#' @examples
#' \dontrun{
#' team_stats <- sb_get_team_match_stats(4039348)
#' }
#'
#' @export
sb_get_team_match_stats <- function(matches, version = sb_version("team_match_stats")) {
  sb_get_match_endpoint(matches, version, path = "matches/{match_id}/team-stats", add_match_id = FALSE)
}

#' Retrieve StatsBomb Player Season Stats
#'
#' Function to retrieve aggregated stats of each player in one or more
#' competitions and seasons.
#'
#' @inheritParams sb_get_matches
#'
#' @return A tibble with one row per player, team, competition and season.
#'
#' @examples
#' \dontrun{
#' player_season <- sb_get_player_season_stats(serie_a)
#' }
#'
#' @export
sb_get_player_season_stats <- function(competitions, version = sb_version("player_season_stats")) {
  sb_get_competition_endpoint(competitions, version, endpoint = "player-stats")
}

#' Retrieve StatsBomb Team Season Stats
#'
#' Function to retrieve aggregated stats of each team in one or more
#' competitions and seasons.
#'
#' @inheritParams sb_get_matches
#'
#' @return A tibble with one row per team, competition and season.
#'
#' @examples
#' \dontrun{
#' team_season <- sb_get_team_season_stats(serie_a)
#' }
#'
#' @export
sb_get_team_season_stats <- function(competitions, version = sb_version("team_season_stats")) {
  sb_get_competition_endpoint(competitions, version, endpoint = "team-stats")
}

#' Retrieve StatsBomb Player Mapping
#'
#' Function to retrieve the mapping between offline and live ids of players,
#' teams and countries in one or more competitions and seasons, along with
#' player information such as birth date, height, weight and preferred foot.
#'
#' @inheritParams sb_get_matches
#'
#' @return A tibble with one row per player, team, competition and season.
#'
#' @examples
#' \dontrun{
#' mapping <- sb_get_player_mapping(serie_a)
#' }
#'
#' @export
sb_get_player_mapping <- function(competitions, version = sb_version("player_mapping")) {
  comp_seasons <- sb_competition_seasons(competitions)

  reqs <- lapply(seq_len(nrow(comp_seasons)), function(i) {
    sb_request(version, "player-mapping", query = list(
      `competition-id` = comp_seasons$competition_id[i],
      `season-id` = comp_seasons$season_id[i]
    ))
  })

  sb_perform(reqs, ids = sb_competition_season_labels(comp_seasons))
}

#' Retrieve data from a match-level endpoint
#'
#' @param path Character. Endpoint path with the placeholder `{match_id}`.
#' @param add_match_id Logical. Whether to add the `match_id` column, for
#'   endpoints that do not return it.
#'
#' @noRd
sb_get_match_endpoint <- function(matches, version, path, add_match_id = TRUE) {
  match_ids <- sb_match_ids(matches)
  paths <- vapply(match_ids, function(id) sub("{match_id}", id, path, fixed = TRUE), character(1))

  keys <- if (add_match_id) tibble::tibble(match_id = as.integer(match_ids))

  sb_perform(
    lapply(paths, sb_request, version = version),
    ids = paste0("match ", match_ids),
    keys = keys
  )
}

#' Retrieve data from a competition-season-level endpoint
#'
#' @param endpoint Character. Endpoint name after the season, e.g. "team-stats".
#'
#' @noRd
sb_get_competition_endpoint <- function(competitions, version, endpoint) {
  comp_seasons <- sb_competition_seasons(competitions)
  paths <- paste0(
    "competitions/", comp_seasons$competition_id,
    "/seasons/", comp_seasons$season_id, "/", endpoint
  )

  sb_perform(
    lapply(paths, sb_request, version = version),
    ids = sb_competition_season_labels(comp_seasons)
  )
}

#' Labels for competition/season pairs used in messages
#'
#' @noRd
sb_competition_season_labels <- function(comp_seasons) {
  paste0("competition ", comp_seasons$competition_id, "/season ", comp_seasons$season_id)
}
