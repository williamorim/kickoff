#' Retrieve StatsBomb Open Data Competitions
#'
#' Function to retrieve the competitions and seasons freely available in the
#' StatsBomb Open Data repository (<https://github.com/hudl/open-data>). No
#' credentials are needed.
#'
#' The output has the same columns as [sb_get_competitions()], so the open data
#' can be used with the same workflow as the paid API.
#'
#' @section Terms of use:
#' The StatsBomb Open Data is provided for research and non-commercial use
#' under the StatsBomb Public Data User Agreement
#' (<https://github.com/hudl/open-data/blob/master/LICENSE.pdf>). In short:
#' \itemize{
#'   \item any published analysis must credit StatsBomb as the data source
#'     and use the StatsBomb logo;
#'   \item the data cannot be redistributed or used commercially.
#' }
#' For this reason the package does not include any open data: it is always
#' downloaded from the official repository.
#'
#' @section Differences from the paid API:
#' The open data files are exported from the StatsBomb API, but some data is
#' not included: matches come without attendance and collection status
#' columns, events come without the On-Ball Value (OBV) columns and other
#' derived columns (e.g. `is_transition`, `possession_directness`), lineups
#' come without formations, height and weight, and 360 frames come without
#' the 360 metrics. Player and team stats and the player mapping are not
#' available.
#'
#' The base URL can be changed with the `statsbomb.open_data_url` option.
#' Default is `"https://raw.githubusercontent.com/hudl/open-data/master/data/"`.
#'
#' @return A tibble with one row per competition and season, with the same
#'   columns as [sb_get_competitions()].
#'
#' @examples
#' \donttest{
#' competitions <- sb_open_competitions()
#' }
#'
#' @export
sb_open_competitions <- function() {
  sb_perform(list(sb_open_request("competitions.json")), ids = "competitions")
}

#' Retrieve StatsBomb Open Data Matches
#'
#' Function to retrieve the matches of one or more competitions and seasons
#' from the StatsBomb Open Data. No credentials are needed.
#'
#' @param competitions A data frame containing competition and season IDs, typically
#' a subset of the data.frame returned by [sb_open_competitions()].
#' Must include columns `competition_id` and `season_id`. Matches are
#' retrieved for each row and combined into a single tibble.
#' @param remove_deleted_matches Logical. Whether to remove matches with "deleted" status. Default is TRUE.
#'
#' @inheritSection sb_open_competitions Terms of use
#'
#' @return A tibble with one row per match, with the same columns as
#'   [sb_get_matches()].
#'
#' @examples
#' \donttest{
#' world_cup_2022 <- sb_open_competitions() |>
#'   dplyr::filter(competition_name == "FIFA World Cup", season_name == "2022")
#' matches <- sb_open_matches(world_cup_2022)
#' }
#'
#' @export
sb_open_matches <- function(competitions, remove_deleted_matches = TRUE) {
  comp_seasons <- sb_competition_seasons(competitions)
  paths <- paste0("matches/", comp_seasons$competition_id, "/", comp_seasons$season_id, ".json")

  matches <- sb_perform(
    lapply(paths, sb_open_request),
    ids = sb_competition_season_labels(comp_seasons)
  )

  # Optionally, remove deleted matches if specified
  if (remove_deleted_matches && "match_status" %in% names(matches)) {
    matches <- matches[!matches$match_status %in% "deleted", ]
  }

  matches
}

#' Retrieve StatsBomb Open Data Events
#'
#' Function to retrieve the event data of one or more matches from the
#' StatsBomb Open Data. Matches are retrieved in parallel. No credentials are
#' needed.
#'
#' @param matches A vector of match ids or a data frame with the column
#'   `match_id`, typically returned by [sb_open_matches()].
#'
#' @inheritSection sb_open_competitions Terms of use
#'
#' @return A tibble with one row per event, in the same format as
#'   [sb_get_events()] but without the OBV and other derived columns. The column `match_id` is
#'   added to identify the match.
#'
#' @examples
#' \donttest{
#' # 2022 World Cup final
#' events <- sb_open_events(3869685)
#' }
#'
#' @export
sb_open_events <- function(matches) {
  sb_open_match_endpoint(matches, "events")
}

#' Retrieve StatsBomb Open Data Lineups
#'
#' Function to retrieve the lineups of one or more matches from the StatsBomb
#' Open Data, including player positions (with time on and off the pitch) and
#' cards. No credentials are needed.
#'
#' @inheritParams sb_open_events
#'
#' @inheritSection sb_open_competitions Terms of use
#'
#' @return A tibble with one row per team and match, in the same format as
#'   [sb_get_lineups()] but without formations. The column `match_id` is
#'   added to identify the match. Players are in the list column `lineup`.
#'
#' @examples
#' \donttest{
#' lineups <- sb_open_lineups(3869685)
#' }
#'
#' @export
sb_open_lineups <- function(matches) {
  sb_open_match_endpoint(matches, "lineups")
}

#' Retrieve StatsBomb Open Data 360 Frames
#'
#' Function to retrieve the 360 freeze frames of one or more matches from the
#' StatsBomb Open Data. Only some matches have 360 data: check the column
#' `match_status_360` of [sb_open_matches()]. Matches without 360 data return
#' no rows and generate a warning. No credentials are needed.
#'
#' @inheritParams sb_open_events
#'
#' @inheritSection sb_open_competitions Terms of use
#'
#' @return A tibble with one row per event with 360 data, in the same format
#'   as [sb_get_360()] but without the 360 metrics. The column `match_id` is
#'   added to identify the match.
#'
#' @examples
#' \donttest{
#' frames <- sb_open_360(3869685)
#' }
#'
#' @export
sb_open_360 <- function(matches) {
  sb_open_match_endpoint(matches, "three-sixty")
}

#' Build a request to the StatsBomb Open Data repository
#'
#' @param path Character. File path inside the `data` folder of the repository.
#'
#' @noRd
sb_open_request <- function(path) {
  sb_open_data_notice()

  getOption("statsbomb.open_data_url", "https://raw.githubusercontent.com/hudl/open-data/master/data/") |>
    httr2::request() |>
    httr2::req_url_path_append(path) |>
    httr2::req_user_agent("kickoff (R package)") |>
    httr2::req_retry(max_tries = 3) |>
    # 404 means the file does not exist (e.g. a match without 360 data)
    httr2::req_error(is_error = function(resp) {
      httr2::resp_status(resp) >= 400 && httr2::resp_status(resp) != 404
    })
}

#' Retrieve data from a match-level file of the open data
#'
#' @param folder Character. Folder of the repository ("events", "lineups" or "three-sixty").
#'
#' @noRd
sb_open_match_endpoint <- function(matches, folder) {
  match_ids <- sb_match_ids(matches)
  paths <- paste0(folder, "/", match_ids, ".json")

  sb_perform(
    lapply(paths, sb_open_request),
    ids = paste0("match ", match_ids),
    keys = tibble::tibble(match_id = as.integer(match_ids))
  )
}

# Environment to store package state during the session
sb_state <- new.env(parent = emptyenv())

#' Show the open data terms of use once per session
#'
#' @noRd
sb_open_data_notice <- function() {
  if (isTRUE(sb_state$open_data_notice_shown)) return(invisible())
  sb_state$open_data_notice_shown <- TRUE

  message(
    "Data provided by StatsBomb Open Data (https://github.com/hudl/open-data).\n",
    "Published analyses must credit StatsBomb and use its logo. ",
    "The data cannot be redistributed or used commercially. ",
    "See ?sb_open_competitions."
  )
}
