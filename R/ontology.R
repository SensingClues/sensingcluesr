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
#'
#' # look up the ID for a given concept label
#' get_id("Human-wildlife conflict", lst)
#'
#' # look up the label for a given concept ID
#' get_label("https://sensingclues.poolparty.biz/SCCSSOntology/106", lst)
get_hierarchy <- function(url = "https://focus.sensingclues.org/", lang = "en") {
  url_onto <- paste0(url, "api/ontology/all/hierarchy?language=", lang)
  # "https://focus.sensingclues.org/api/ontology/all/hierarchy?language=en"
  httr::content(httr::GET(url_onto))
}

#' @rdname get_hierarchy
#' @param label Textual concept definiton, for example `Human-wildlife conflict`.
#' @param hierarchy Object retrieved by [get_hierarchy()].
#' @export
get_id <- function(label, hierarchy) {
  hierarchy$concepts[sapply(hierarchy$concepts, function(y) label %in% y$label)][[1]]$id
}

#' @rdname get_hierarchy
#' @param id Concept identification, for example `https://sensingclues.poolparty.biz/SCCSSOntology/106`.
#' @param hierarchy Object retrieved by [get_hierarchy()].
#' @export
get_label <- function(id, hierarchy) {
  hierarchy$concepts[sapply(hierarchy$concepts, function(y) id %in% y$id)][[1]]$label
}

get_parent_id <- function(id, hierarchy) {
  hierarchy$concepts[sapply(hierarchy$concepts, function(y) id %in% y$id)][[1]]$parent
}

get_parent_label <- function(label, hierarchy) {
  parent <- hierarchy$concepts[sapply(hierarchy$concepts, function(y) label %in% y$label)][[1]]$parent
  get_label(parent, hierarchy)
}
