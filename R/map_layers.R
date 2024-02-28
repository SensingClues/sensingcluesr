#' Access to geodata
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#'
#' @return A data frame representing the geodata you have access to. There are
#' four columns including a name, a data source identifier consisting of two
#' parts, a project id and a layer id, and the type of geometry of the layer.
#' @export
#'
#' @examplesIf !is.null(login_cluey())
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' df <- get_layer_details(cookie)
get_layer_details <- function(cookie, url = "https://focus.sensingclues.org/") {
  # get all layers
  l <- get_all_layers(cookie, url)
  m <- l$models
  aoi <- list()
  # sequence along models
  for (i in seq_along(m)) {
    # parse model
    for (j in seq_along(m[[i]]$layers)) {
      # find layers in model
      aoi[[length(aoi)+1]] <- list(m[[i]]$layers[[j]]$name, m[[i]]$layers[[j]]$id, m[[i]]$id, m[[i]]$layers[[j]]$geometryType)
    }
  }
  df <- as.data.frame(do.call(rbind, aoi))
  names(df) <- c("layerName", "lid", "pid", "geometryType")
  # drop default and track layers
  df <- dplyr::filter(df, !df$pid %in% c("track", "default"))
  return(df)
}

# Helpers ---------------------------------------------------------------------

get_all_layers <- function(cookie, url = "https://focus.sensingclues.org/") {
  url_search_results <- paste0(url, "api/map/all/describe")
  # initial call to get total and number of pages to get
  httr::handle_reset(url_search_results)
  result <- httr::GET(url_search_results, httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
  layers <- httr::content(result)
  return(layers)
}
