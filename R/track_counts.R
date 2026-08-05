#' Frequency of track concepts
#'
#' Counts how often each track concept, such as a patrol type, occurs, without
#' downloading the tracks themselves. Unlike [get_concept_counts()], the concept
#' labels are resolved for you, and the ontologies' top concepts are left out of
#' the result.
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param group One or multiple group identification character string(s), see which groups you have access to with [get_groups()].
#' @param from Start of the date range, as a `Date` or a `"YYYY-MM-DD"` character string. Default is `"1900-01-01"`, so that all tracks are counted.
#' @param to End of the date range, as a `Date` or a `"YYYY-MM-DD"` character string. Default is `"2999-12-31"`, so that all tracks are counted.
#' @param hierarchy A Sensing Clues knowledge graph object, to be retrieved with [get_hierarchy()]. Default `NULL` retrieves the hierarchy for you; pass one explicitly to avoid downloading it again on repeated calls.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#'
#' @return A data frame of track concept definitions and their frequencies
#' collected by the defined group(s), within the given date range, with one row
#' per concept and the following columns:
#' - `id`: Concept identifier. See
#'   [https://sensingclues.poolparty.biz/GraphViews/](https://sensingclues.poolparty.biz/GraphViews/)
#'   for all available concepts.
#' - `name`: Label of the concept, in the language given by `lang`.
#' - `count`: Number of tracks labelled with the concept.
#'
#' Returns `NULL` if no track concepts are found.
#' @export
#'
#' @examples
#' \dontrun{
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' df <- get_track_counts(cookie, group = "focus-project-1234") # demo group
#'
#' # re-use the hierarchy when calling the function several times
#' lst <- get_hierarchy()
#' df <- get_track_counts(cookie, group = "focus-project-1234", hierarchy = lst)
#' }
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
    track_counts <- dplyr::filter(track_counts, !(.data$id %in% h$topConcepts))
  } else {
    message("No track counts received from backend returning NULL")
    track_counts <- NULL
  }
  return(track_counts)
}
