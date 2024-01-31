#' Frequency of concepts
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param group One or multiple group identification character string(s), see which groups you have access to with [get_groups()].
#' @param from Start date.
#' @param to End date.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#'
#' @return A list of concept definitions and their frequencies collected by the defined group(s), within the given date range. See [https://sensingclues.poolparty.biz/GraphViews/](https://sensingclues.poolparty.biz/GraphViews/) for all available concepts.
#' @export
#'
#' @examples
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' lst <- get_concept_counts(cookie, group = 'focus-project-1234') # demo group
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
