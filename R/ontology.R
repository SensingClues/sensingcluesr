#' Querying Sensing Clues' ontologies
#'
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#'
#' @return Nested list representing multiple ontologies.
#' @export
#'
#' @examples
#' lst <- get_hierarchy() # you have to be logged in to https://focus.sensingclues.org/ to get access
#'
#' # inspect a single concept
#' cncpt <- lst$concepts[[1]]
get_hierarchy <- function(url = "https://focus.sensingclues.org/", lang = "en") {
  url_onto <- paste0(url, "api/ontology/all/hierarchy?language=", lang)
  # "https://focus.sensingclues.org/api/ontology/all/hierarchy?language=en"
  httr::content(httr::GET(url_onto))
}

get_id <- function(label, hierarchy) {
  hierarchy$concepts[sapply(hierarchy$concepts, function(y) label %in% y$label)][[1]]$id
}

get_label <- function(id, hierarchy) {
  hierarchy$concepts[sapply(hierarchy$concepts, function(y) id %in% y$id)][[1]]$label
}
