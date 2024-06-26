% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/login.R
\name{login_cluey}
\alias{login_cluey}
\title{Log in to Sensing Clues Cluey}
\usage{
login_cluey(
  username = "",
  password = "",
  url = "https://focus.sensingclues.org/"
)
}
\arguments{
\item{username}{Your Cluey username.}

\item{password}{Your Cluey password.}

\item{url}{A Sensing Clues URL, default is \url{https://focus.sensingclues.org/}.}
}
\value{
A cookie object
}
\description{
Log in to Sensing Clues Cluey
}
\examples{
cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
}
