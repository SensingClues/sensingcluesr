% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concept_counts.R
\name{get_concept_counts}
\alias{get_concept_counts}
\title{Frequency of concepts}
\usage{
get_concept_counts(
  cookie,
  group,
  from = "1900-01-01",
  to = "2999-12-31",
  url = "https://focus.sensingclues.org/",
  lang = "en"
)
}
\arguments{
\item{cookie}{A cookie obtained by \code{\link[=login_cluey]{login_cluey()}}.}

\item{group}{One or multiple group identification character string(s), see which groups you have access to with \code{\link[=get_groups]{get_groups()}}.}

\item{from}{Start date.}

\item{to}{End date.}

\item{url}{A Sensing Clues URL, default is \url{https://focus.sensingclues.org/}.}

\item{lang}{Language in which the concepts are shown, default is English.}
}
\value{
A list of concept definitions and their frequencies collected by the defined group(s), within the given date range. See \url{https://sensingclues.poolparty.biz/GraphViews/} for all available concepts.
}
\description{
Frequency of concepts
}
\examples{
cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
lst <- get_concept_counts(cookie, group = 'focus-project-1234') # demo group
}
