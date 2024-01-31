#' Log in to Sensing Clues Cluey
#'
#' @param username Your Cluey username.
#' @param password Your Cluey password.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#'
#' @return A cookie object
#' @export
#'
#' @examples
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
login_cluey <- function(username = "", password = "", url = "https://focus.sensingclues.org/") {
  # login
  url_login <- paste0(url, "api/auth/login")
  json_body <- jsonlite::toJSON(list(username = username, password = password),
                                auto_unbox = TRUE)
  # we set up an authenticated session
  # force empty cookie (new handle pool)
  httr::handle_reset(url_login)
  rl <- httr::POST(url_login, body = json_body, encode = "raw", httr::content_type_json()) # verbose())

  # for logging
  message("Login attempt by ", username)
  if (httr::status_code(rl) == 200) {
    message("Successfully logged in")
    return(httr::cookies(rl))
  } else {
    warning("Login failed!")
    return(NULL)
  }
}
