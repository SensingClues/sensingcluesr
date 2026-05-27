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
#'
#' # retrieve a layer in simple feature collection (sfc) format
#' projectId <- df$pid[1]
#' layerId <- df$lid[1]
#' sf <- get_layer_features(projectId, layerId, cookie)
get_layer_details <- function(cookie, url = "https://focus.sensingclues.org/") {
  m <- get_all_layers(cookie, url)$models
  df <- dplyr::bind_rows(lapply(m, function(model) {
    dplyr::bind_rows(lapply(model$layers, function(layer) {
      data.frame(
        layerName    = layer$name,
        lid          = layer$id,
        projectName  = model$description,
        pid          = model$id,
        geometryType = layer$geometryType,
        stringsAsFactors = FALSE
      )
    }))
  }))
  if (nrow(df) == 0) return(df)
  dplyr::filter(df, !pid %in% c("track", "default"))
}

#' @rdname get_layer_details
#' @param projectId An integer representing a project identifier, obtained by [get_layer_details()].
#' @param layerId An integer representing a layer identifier, obtained by [get_layer_details()].
#' @export
get_layer_features <- function(projectId, layerId, cookie, url = "https://focus.sensingclues.org/") {
  # /api/map/{searchType}/{projectId}/{layerId}/features
  searchType <- "all"

  # select the proper source URL
  url_search_results <- paste0(url, "api/map/", searchType, "/", projectId, "/", layerId, "/features")

  # initial call to get total and number of pages to get
  httr::handle_reset(url_search_results)
  result <- httr::POST(url_search_results, httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
  layer <- httr::content(result)
  return(geojsonsf::geojson_sf(jsonlite::toJSON(layer, auto_unbox = TRUE)))
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
