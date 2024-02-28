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
