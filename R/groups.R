#' Overview of access to data sources
#'
#' Lists the groups (data sources) that your account has access to. Use this to
#' find the group identifiers that the other functions in this package expect as
#' their `group` argument.
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param from Start of the date range, as a `Date` or a `"YYYY-MM-DD"` character string. Default is `"1900-01-01"`, so that all data is counted.
#' @param to End of the date range, as a `Date` or a `"YYYY-MM-DD"` character string. Default is `"2999-12-31"`, so that all data is counted.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#'
#' @return A data frame with one row per group you have access to and the
#' following character columns:
#' - `name`: Human-readable name of the group.
#' - `count`: Number of observations and tracks in the group within the given
#'   date range.
#' - `value`: Group identifier, to be used as the `group` argument of, for
#'   example, [get_observations()] and [get_tracks()].
#'
#' Returns an empty data frame if you do not have access to any group.
#' @export
#'
#' @examples
#' \dontrun{
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' df <- get_groups(cookie)
#' }
get_groups <- function(cookie,
                       from = "1900-01-01",
                       to = "2999-12-31",
                       url = "https://focus.sensingclues.org/") {
  # facets
  url_search_results <- paste0(url, "api/search/all/facets")
  query <- paste0('
    {"filters":
        {"dateTimeRange":
            {"to":"', to, 'T24:00:00.000Z",
              "from":"', from, 'T00:00:00.000Z"
            },
          "entities":["Observation","track"]
        },
      "options": {
        "start": 1,
        "pageLength": 0
      }
    }
  ')
  httr::handle_reset(url_search_results)
  result <- httr::POST(url_search_results, body = query, encode = "raw",
                       httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value))) # verbose())
  facet_result <- httr::content(result)
  # unpack the content
  data_sources <- facet_result$facets$dataSources
  Ngroups <- length(data_sources$facetValues)
  GROUPS <- data.frame()
  if(Ngroups != 0) {
    for (i in 1:Ngroups) {
      # get to content
      name <- data_sources$facetValues[[i]]$name
      count <- data_sources$facetValues[[i]]$count
      value <- data_sources$facetValues[[i]]$value

      GROUPS <- rbind(GROUPS, c(name, count, value))
      # names
      names(GROUPS) <- c("name", "count", "value")
    }
  } else {
    message("No groups received from backend, returning empty dataframe")
  }
  return(GROUPS)
}
