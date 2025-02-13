get_agent <- function(cookie, agentId, url = "https://focus.sensingclues.org/", lang = "en") {
  # https://focus.test.sensingclues.org/api/crud/Agent/A0
  url_search_results <- paste0(url, "api/crud/Agent/", agentId, "?language=", lang)
  httr::handle_reset(url_search_results)
  result <- httr::GET(url_search_results, httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
  agent_data <- httr::content(result)

  if (result$status_code != 200) {
    warning(paste("Unsuccessful at retrieving agent details for:", agentId))
    return(NULL)
    }

  agent <- list()
  agent$agentId <- agent_data$envelope$instance$Agent$agentId
  agent$agentName <- agent_data$envelope$instance$Agent$name
  agent$description <- agent_data$envelope$instance$Agent$description
  return(agent)
}
