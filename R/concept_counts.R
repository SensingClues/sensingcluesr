#' Frequency of concepts
#'
#' Counts how often each concept occurs in the observations of one or more
#' groups, without downloading the observations themselves. See
#' [get_track_counts()] for the same information on tracks.
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param group One or multiple group identification character string(s), see which groups you have access to with [get_groups()].
#' @param from Start of the date range, as a `Date` or a `"YYYY-MM-DD"` character string. Default is `"1900-01-01"`, so that all observations are counted.
#' @param to End of the date range, as a `Date` or a `"YYYY-MM-DD"` character string. Default is `"2999-12-31"`, so that all observations are counted.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#'
#' @return A list of concept definitions and their frequencies collected by the
#' defined group(s), within the given date range. Each element is a list with a
#' `_value` element holding the concept identifier and a `frequency` element
#' holding the number of observations labelled with that concept. See
#' [https://sensingclues.poolparty.biz/GraphViews/](https://sensingclues.poolparty.biz/GraphViews/)
#' for all available concepts, or use [get_label()] to resolve the identifiers to
#' readable labels.
#' @export
#'
#' @examples
#' \dontrun{
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' lst <- get_concept_counts(cookie, group = 'focus-project-1234') # demo group
#'
#' # turn the result into a data frame with readable concept labels
#' hierarchy <- get_hierarchy()
#' df <- data.frame(
#'   id = sapply(lst, function(x) x$`_value`),
#'   name = sapply(lst, function(x) get_label(x$`_value`, hierarchy)),
#'   count = sapply(lst, function(x) x$frequency)
#' )
#' }
get_concept_counts <- function(cookie,
                               group,
                               from = "1900-01-01",
                               to = "2999-12-31",
                               url = "https://focus.sensingclues.org/",
                               lang = "en") {
  # ontology counts
  url_search_results <- paste0(url, "api/ontology/all/counts?language=", lang)
  grouparray <- jsonlite::toJSON(unlist(group))

  query <- paste0('
    {"filters":
        {"dateTimeRange":
            {"to":"', to, 'T24:00:00.000Z",
              "from":"', from, 'T00:00:00.000Z"
            },
          "entities":["Observation"],
          "dataSources": ', grouparray, '
        }
    }
  ')

  httr::handle_reset(url_search_results)
  result <- httr::POST(url_search_results, body = query, encode = "raw", httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
  counts <- httr::content(result)$"values-response"$"distinct-value"

  return(counts)
}
