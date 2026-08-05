#' Log in to Sensing Clues Cluey
#'
#' Sets up an authenticated session with the Sensing Clues platform. The cookie
#' that is returned identifies your session and is the first argument of every
#' function in this package that retrieves your data.
#'
#' @param username Your Cluey username.
#' @param password Your Cluey password.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#'
#' @return A cookie object: a one-row data frame as returned by
#' [httr::cookies()], to be passed on as the `cookie` argument of the other
#' functions in this package. Returns `NULL` with a warning if the login fails,
#' for example because the credentials are incorrect.
#'
#' @section Keeping your credentials out of your scripts:
#' Rather than typing your password into a script, store it in your user
#' environment file with `usethis::edit_r_environ()` and read it back with
#' [Sys.getenv()]:
#'
#' ```r
#' cookie <- login_cluey(Sys.getenv("CLUEY_USER"), Sys.getenv("CLUEY_PASSWORD"))
#' ```
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' }
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
