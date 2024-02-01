get_hierarchy <- function(url = "https://focus.sensingclues.org/", lang = "en") {
  url_onto <- paste0(url, "api/ontology/all/hierarchy?language=", lang)
  # "https://focus.sensingclues.org/api/ontology/all/hierarchy?language=en"
  httr::content(httr::GET(url_onto))
}
