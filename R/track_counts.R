#' Frequency of track concepts
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param group One or multiple group identification character string(s), see which groups you have access to with [get_groups()].
#' @param from Start date.
#' @param to End date.
#' @param hierarchy A Sensing Clues knowledge graph object, to be retrieved with [get_hierarchy()].
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#'
#' @return A data frame of track concept definitions and their frequencies collected by the defined group(s), within the given date range. See [https://sensingclues.poolparty.biz/GraphViews/](https://sensingclues.poolparty.biz/GraphViews/) for all available concepts.
#' @export
#'
#' @examples
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' df <- get_track_counts(cookie, group = "focus-project-1234") # demo group
get_track_counts <- function(cookie,
                             group,
                             from = "1900-01-01",
                             to = "2999-12-31",
                             hierarchy = NULL,
                             url = "https://focus.sensingclues.org/",
                             lang = "en") {
  # ontology counts
  url_search_results <- paste0(url, "api/ontology/all/counts?language=", lang)
  grouparray <- jsonlite::toJSON(unlist(group)) # if multiple groups selected

  query <- paste0('
    {"filters":
        {
          "dateTimeRange":
            {"to":"', to, 'T24:00:00.000Z",
              "from":"', from, 'T00:00:00.000Z"
            },
          "entities":["track"],
          "dataSources": ', grouparray, '
        }
    }
  ')

  httr::handle_reset(url_search_results)
  result <- httr::POST(url_search_results, body = query, encode = "raw", httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
  counts <- httr::content(result)$"values-response"$"distinct-value"

  if (is.null(hierarchy)) {
    h <- sensingcluesr::get_hierarchy(url = url, lang = lang)
  } else {h <- hierarchy}

  if (length(counts) > 0) {
    track_counts <- data.frame(
      id = sapply(counts, function(x) x$`_value`),
      name = sapply(counts, function(x) sensingcluesr::get_label(x$`_value`, h)),
      count = sapply(counts, function(x) x$frequency)
    )
  } else {
    message("No track counts received from backend returning NULL")
    track_counts <- NULL
  }
  return(track_counts)
}
