#' Set StatsBomb API Credentials
#'
#' Function to set and manage StatsBomb API credentials. First attempts to retrieve
#' credentials from environment variables, and if not found, prompts the user to enter
#' them interactively. The credentials are then stored as environment variables for
#' future use in the current session.
#'
#' @return Invisibly returns a list containing username and password. The function
#'   primarily sets environment variables for credential storage.
#'
#' @details
#' The function looks for the following environment variables:
#' \itemize{
#'   \item STATSBOMB_USERNAME - Your StatsBomb username
#'   \item STATSBOMB_PASSWORD - Your StatsBomb password
#' }
#'
#' If these are not set, the function will prompt for interactive input and then
#' set these environment variables for the current session.
#'
#' @examples
#' \dontrun{
#' # Set credentials interactively or from environment
#' sb_set_credentials()
#'
#' # Credentials are now available for other functions
#' }
#'
#' @seealso \code{\link{Sys.getenv}}, \code{\link{Sys.setenv}}
#'
#' @export
sb_set_credentials <- function() {
  # Retrieve StatsBomb credentials from environment variables
  username <- Sys.getenv("STATSBOMB_USERNAME")
  password <- Sys.getenv("STATSBOMB_PASSWORD")

  # If empty, ask credentials
  if (username == "" || password == "") {
    username <- readline(prompt = "Enter your StatsBomb username: ")
    password <- getPass::getPass(msg = "Enter your StatsBomb password: ")

    Sys.setenv(STATSBOMB_USERNAME = username)
    Sys.setenv(STATSBOMB_PASSWORD = password)
  }

  invisible(list(username = username, password = password))
}

#' StatsBomb API Versions
#'
#' Lists the API version used by each endpoint function of the package.
#'
#' @details
#' The default versions are the most recent ones available in the StatsBomb API
#' as of 2026-10-12.
#'
#' **Calling a version that does not exist does not raise an error.** The
#' StatsBomb API responds with status 200 to any version number, but when the
#' version does not exist (or is no longer served) it silently falls back to a
#' basic format with fewer columns. For example, events `v12` comes without the
#' OBV columns, lineups `v6` comes without positions, formations and cards,
#' 360 frames `v3` comes without the 360 metrics and competitions `v5` comes
#' without gender, youth, international and 360 availability columns. If you
#' change a version, always check that the columns you need are present.
#'
#' The defaults can be overridden for the whole session with the
#' `statsbomb.versions` option, or for a single call with the `version`
#' argument of each function:
#'
#' ```r
#' options(statsbomb.versions = list(events = "v10"))
#' ```
#'
#' Other options used by the package:
#' \itemize{
#'   \item `statsbomb.base_url`: base URL of the API. Default is
#'     `"https://data.statsbomb.com/api/"`.
#'   \item `statsbomb.max_active`: maximum number of simultaneous requests
#'     when several matches or competitions are retrieved. Default is 5.
#' }
#'
#' @return A tibble with the columns `endpoint`, `version` (the version in use)
#'   and `default` (the package default).
#'
#' @examples
#' sb_api_versions()
#'
#' @export
sb_api_versions <- function() {
  defaults <- sb_default_versions()

  tibble::tibble(
    endpoint = names(defaults),
    version = vapply(names(defaults), sb_version, character(1), USE.NAMES = FALSE),
    default = unname(defaults)
  )
}

#' Default StatsBomb API versions
#'
#' @noRd
sb_default_versions <- function() {
  c(
    competitions = "v4",
    matches = "v6",
    events = "v11",
    lineups = "v5",
    frames_360 = "v2",
    player_match_stats = "v8",
    team_match_stats = "v4",
    player_season_stats = "v7",
    team_season_stats = "v4",
    player_mapping = "v1"
  )
}

#' Get the API version in use for an endpoint
#'
#' Returns the version set in the `statsbomb.versions` option, or the package
#' default if none is set.
#'
#' @param endpoint Character. Name of the endpoint, as in `sb_default_versions()`.
#'
#' @noRd
sb_version <- function(endpoint) {
  overrides <- getOption("statsbomb.versions", list())
  version <- overrides[[endpoint]]
  if (is.null(version)) version <- sb_default_versions()[[endpoint]]
  version
}

#' Build a request to the StatsBomb API
#'
#' Internal helper that creates an authenticated `httr2` request with retries
#' for transient errors.
#'
#' @param version Character. API version, e.g. "v11".
#' @param path Character. Endpoint path after the version, e.g. "events/123".
#' @param query Named list of query parameters.
#'
#' @return An `httr2_request`.
#'
#' @noRd
sb_request <- function(version, path, query = list()) {
  # Credentials
  sb_creds <- sb_set_credentials()

  getOption("statsbomb.base_url", "https://data.statsbomb.com/api/") |>
    httr2::request() |>
    httr2::req_url_path_append(version, path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_auth_basic(sb_creds$username, sb_creds$password) |>
    httr2::req_user_agent("FlaSB (R package)") |>
    httr2::req_retry(max_tries = 3) |>
    # 404 means "no data" for some endpoints (e.g. player-mapping)
    httr2::req_error(
      is_error = function(resp) httr2::resp_status(resp) >= 400 && httr2::resp_status(resp) != 404,
      body = function(resp) {
        if (httr2::resp_status(resp) == 401) "Check your StatsBomb credentials."
      }
    )
}

#' Perform requests to the StatsBomb API and combine the results
#'
#' Performs the requests in parallel, stops if any of them fails and combines
#' the parsed responses in a single tibble. Requests that return no data
#' generate a single warning listing their ids.
#'
#' @param reqs List of `httr2_request`s.
#' @param ids Character. One label per request, used in error and warning messages.
#' @param keys Optional data frame with one row per request. Its columns are
#'   added to the start of the corresponding parsed response (e.g. `match_id`
#'   for endpoints that do not return it).
#'
#' @return A tibble.
#'
#' @noRd
sb_perform <- function(reqs, ids, keys = NULL) {
  if (length(reqs) == 0) return(tibble::tibble())

  resps <- httr2::req_perform_parallel(
    reqs,
    on_error = "continue",
    progress = length(reqs) > 1,
    max_active = getOption("statsbomb.max_active", 5)
  )

  # Stop if any request failed
  failed <- vapply(resps, inherits, logical(1), what = "error")
  if (any(failed)) {
    msgs <- vapply(resps[failed], conditionMessage, character(1))
    stop(
      "StatsBomb API request failed for ", sum(failed), " of ", length(reqs),
      " request(s):\n",
      paste0("  ", ids[failed], ": ", msgs, collapse = "\n"),
      call. = FALSE
    )
  }

  data <- lapply(resps, sb_parse_json)

  # Warn about requests that returned no data
  empty <- vapply(data, function(d) nrow(d) == 0, logical(1))
  if (any(empty)) {
    warning(
      "No data returned for ", sum(empty), " of ", length(reqs), ": ",
      paste(utils::head(ids[empty], 20), collapse = ", "),
      if (sum(empty) > 20) ", ...",
      call. = FALSE
    )
  }

  # Add the key columns to each response
  if (!is.null(keys)) {
    data <- lapply(seq_along(data), function(i) {
      if (empty[i]) return(data[[i]])
      dplyr::bind_cols(keys[i, , drop = FALSE], data[[i]])
    })
  }

  dplyr::bind_rows(data)
}

#' Parse a StatsBomb API response
#'
#' @param resp An `httr2_response`.
#'
#' @return A tibble with the flattened JSON. Empty responses (no body, `[]`
#'   or status 404) return an empty tibble.
#'
#' @noRd
sb_parse_json <- function(resp) {
  if (httr2::resp_status(resp) == 404 || !httr2::resp_has_body(resp)) {
    return(tibble::tibble())
  }

  parsed <- httr2::resp_body_string(resp, encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)

  if (length(parsed) == 0) return(tibble::tibble())

  tibble::as_tibble(parsed)
}

#' Extract match ids from a vector or data frame
#'
#' @noRd
sb_match_ids <- function(x) {
  if (is.data.frame(x)) {
    if (!"match_id" %in% names(x)) {
      stop("`matches` must be a vector of match ids or a data frame with the column `match_id`.", call. = FALSE)
    }
    x <- x$match_id
  }

  unique(sb_format_id(x))
}

#' Extract the unique competition/season pairs from a data frame
#'
#' @noRd
sb_competition_seasons <- function(competitions) {
  required_cols <- c("competition_id", "season_id")
  missing_cols <- setdiff(required_cols, names(competitions))
  if (!is.data.frame(competitions) || length(missing_cols) > 0) {
    stop(
      "`competitions` must be a data frame with the columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  out <- unique(competitions[required_cols])
  out$competition_id <- sb_format_id(out$competition_id)
  out$season_id <- sb_format_id(out$season_id)
  out
}

#' Format ids for use in URLs (avoids scientific notation)
#'
#' @noRd
sb_format_id <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE)
  as.character(x)
}
