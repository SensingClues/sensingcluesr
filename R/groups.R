#' Overview of access to data sources
#'
#' @param cookie A cookie obtained by login_cluey().
#' @param from Start date.
#' @param to End date.
#' @param url A Sensing Clues URL, default is https://focus.sensingclues.org/.
#'
#' @return A data frame representing the data sources you have access to.
#' There are three columns including a data source identifier, a name and the
#' number of observations made in that group.
#' @export
#'
#' @examples
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' df <- get_groups(cookie)
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
