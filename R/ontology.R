#' Querying Sensing Clues' ontologies
#'
#' Observations and tracks refer to concepts in Sensing Clues' ontologies by
#' identifier, for example
#' `https://sensingclues.poolparty.biz/SCCSSOntology/106`. `get_hierarchy()`
#' retrieves those ontologies, and the other functions documented here translate
#' between identifiers and labels and walk up and down the hierarchy.
#'
#' Retrieving the hierarchy is a download, so retrieve it once and pass the same
#' object to the lookup functions rather than calling `get_hierarchy()`
#' repeatedly.
#'
#' All lookup functions warn and return `NULL` when the concept is not found in
#' the hierarchy, so they are safe to call on unknown identifiers or labels.
#'
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#' @param hierarchy Object retrieved by [get_hierarchy()].
#'
#' @return `get_hierarchy()` returns a nested list representing multiple
#' ontologies, with two elements of interest:
#' - `concepts`: One element per concept, each a list with an `id`, one or more
#'   `label`s, the `parent` concept identifier and the `child` concept
#'   identifier(s).
#' - `topConcepts`: The identifiers of the concepts at the root of each ontology.
#'
#' Returns `NULL` with a warning if the ontologies cannot be retrieved.
#' @export
#'
#' @examples
#' \dontrun{
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
#'
#' # get the parent ID/label for a given concept ID/label
#' get_parent_id("https://sensingclues.poolparty.biz/SCCSSOntology/106", lst)
#' get_parent_label("Human-wildlife conflict", lst)
#'
#' # get the children ID/label(s) for a given concept ID/label
#' get_children_id("https://sensingclues.poolparty.biz/SCCSSOntology/106", lst)
#' get_children_label("Human-wildlife conflict", lst)
#'
#' # retrieve the ontologies in another language
#' lst_nl <- get_hierarchy(lang = "nl")
#' }
get_hierarchy <- function(url = "https://focus.sensingclues.org/", lang = "en") {
  tryCatch({
    url_onto <- paste0(url, "api/ontology/all/hierarchy?language=", lang)
    # "https://focus.sensingclues.org/api/ontology/all/hierarchy?language=en"
    result <- httr::content(httr::GET(url_onto))
    if ("statusText" %in% names(result)) {
      warning(result$statusText)
      return(NULL)
    } else {return(result)}
  }, error = function(e) {
    warning(paste("Unsuccessful at retrieving the hierarchy for:", url_onto))
    return(NULL)
  })
}

#' @rdname get_hierarchy
#' @param label Textual concept definition, for example `Human-wildlife conflict`.
#' @return `get_id()` returns the identifier of the concept with the given
#' `label`, as a character string.
#' @export
get_id <- function(label, hierarchy) {
  # Try to extract the concept ID
  tryCatch({
    hierarchy$concepts[sapply(hierarchy$concepts, function(y) label %in% y$label)][[1]]$id
  }, error = function(e) {
    warning(paste("No concept ID found in hierarchy for:", label))
    return(NULL) # Return NULL if an error occurs
  })
}

#' @rdname get_hierarchy
#' @param id Concept identification, for example `https://sensingclues.poolparty.biz/SCCSSOntology/106`.
#' @return `get_label()` returns the label of the concept with the given `id`, in
#' the language the hierarchy was retrieved in.
#' @export
get_label <- function(id, hierarchy) {
  # Try to extract the concept label
  tryCatch({
    hierarchy$concepts[sapply(hierarchy$concepts, function(y) id %in% y$id)][[1]]$label
  }, error = function(e) {
    warning(paste("No concept label found in hierarchy for:", id))
    return(NULL) # Return NULL if an error occurs
  })
}

#' @rdname get_hierarchy
#' @return `get_parent_id()` returns the identifier of the parent of the concept
#' with the given `id`.
#' @export
get_parent_id <- function(id, hierarchy) {
  # Try to extract the parent concept ID
  tryCatch({
    hierarchy$concepts[sapply(hierarchy$concepts, function(y) id %in% y$id)][[1]]$parent
  }, error = function(e) {
    warning(paste("No parent concept ID found in hierarchy for:", id))
    return(NULL) # Return NULL if an error occurs
  })
}

#' @rdname get_hierarchy
#' @return `get_parent_label()` returns the label of the parent of the concept
#' with the given `label`.
#' @export
get_parent_label <- function(label, hierarchy) {
  # Try to extract the parent concept label
  tryCatch({
    parent <- hierarchy$concepts[sapply(hierarchy$concepts, function(y) label %in% y$label)][[1]]$parent
    get_label(parent, hierarchy)
  }, error = function(e) {
    warning(paste("No parent concept label found in hierarchy for:", label))
    return(NULL) # Return NULL if an error occurs
  })
}

#' @rdname get_hierarchy
#' @return `get_children_id()` returns a character vector with the identifiers of
#' the direct children of the concept with the given `id`, or `NULL` if the
#' concept has no children.
#' @export
get_children_id <- function(id, hierarchy) {
  # Try to extract the children concept ID(s)
  tryCatch({
    unlist(hierarchy$concepts[sapply(hierarchy$concepts, function(y) id %in% y$id)][[1]]$child)
  }, error = function(e) {
    warning(paste("No children concept ID(s) found in hierarchy for:", id))
    return(NULL) # Return NULL if an error occurs
  })
}

#' @rdname get_hierarchy
#' @return `get_children_label()` returns a character vector with the labels of
#' the direct children of the concept with the given `label`, or `NULL` if the
#' concept has no children.
#' @export
get_children_label <- function(label, hierarchy) {
  # Try to extract the children concept label(s)
  tryCatch({
    children <- unlist(hierarchy$concepts[sapply(hierarchy$concepts, function(y) label %in% y$label)][[1]]$child)
    result <- c()
    for (item in children) {
      result <- c(result, get_label(item, hierarchy))
    }
    return(result)
  }, error = function(e) {
    warning(paste("No children concept labels found in hierarchy for:", label))
    return(NULL) # Return NULL if an error occurs
  })
}
