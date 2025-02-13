#' Retrieve agent details
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param agentId Identification character string of a single agent. You can find the agent id of your group members in [Focus](https://focus.sensingclues.org/).
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#'
#' @return A list with agent details, such as the username. Returns `NULL` if you do not have access to the agent or if the `agentId` is incorrect.
#' @export
#'
#' @examples
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' lst <- get_agent(cookie, agentId = 'YOUR AGENT ID')
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
