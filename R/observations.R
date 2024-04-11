#' Retrieve observation data
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param group One or multiple group identification character string(s), see which groups you have access to with [get_groups()].
#' @param bounds Bounding box coordinates (latitude and longitude) in list(north, east, south, west) format
#' @param from Start date.
#' @param to End date.
#' @param aoi Area of interest.
#' @param filteredConcepts One or multiple concept definitions, for example `https://sensingclues.poolparty.biz/SCCSSOntology/186`. See [https://sensingclues.poolparty.biz/GraphViews/](https://sensingclues.poolparty.biz/GraphViews/) for all available concepts.
#' @param updateProgress A function to update a progress bar object, default is NULL.
#' @param allAttributes A boolean. Allows you to collect more attributes for the observations.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#'
#' @return A data frame with all observations collected by the defined group(s), within the given date range.
#' @export
#'
#' @examplesIf !is.null(login_cluey())
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' df <- get_observations(cookie, group = 'focus-project-1234') # demo group
get_observations <- function(cookie,
                             group,
                             bounds = list(north = 90, east = 180, south = -89, west = -179),
                             from = Sys.Date() - 30,
                             to = Sys.Date(),
                             aoi = "",
                             filteredConcepts = NULL,
                             updateProgress = NULL,
                             allAttributes = FALSE,
                             url = "https://focus.sensingclues.org/",
                             lang = "en") { # default language is English

  message("Start downloading observations ...")

  # process time
  ptm <- proc.time()

  # concepts to json array
  if (!is.null(filteredConcepts)) {
    cncpts <- jsonlite::toJSON(unlist(filteredConcepts))
  } else {
    cncpts <- "[]"
  }

  grouparray <- jsonlite::toJSON(unlist(group))

  # page length
  page_length <- 500 # optimal value regarding query performance

  # select the proper source URL
  url_search_results <- paste0(url, "api/search/all/results?language=", lang)

  # build search query
  query <- paste0('
    {"filters":
        {"geoQuery":
            {"operator":"intersects",
              "mapBounds":{"south":', bounds$south, ',"west":', bounds$west, ',"north":', bounds$north, ',"east":', bounds$east, '},
              "drawings": [', aoi, ']
            },
          "dateTimeRange":
            {"to":"', to, 'T24:00:00.000Z",
              "from":"', from, 'T00:00:00.000Z"
            },
          "entities":["Observation"],
          "dataSources": ', grouparray, ',
          "concepts": ', cncpts, '
        },
      "options": {
        "start": 0,
        "pageLength": ', page_length, '
      }
    }
  ')

  #message(paste0('QUERY : ',query))

  # initial call to get total and number of pages to get
  httr::handle_reset(url_search_results)
  result <- httr::POST(url_search_results, body = query, encode = "raw",
                       httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value))) # verbose())
  searchResult <- httr::content(result)

  # setup empty dataframe
  OBSERVATIONS <- data.frame()

  # total results to get
  total <- searchResult$total
  # pages to fetch
  pages <- ceiling(total/page_length)

  # set progress
  #updateProgress$set(value = total)

  # expected number of results
  message(paste0("Expecting total ", total, " results in ", pages, " number of pages"))

  # pagination
  for (p in 1:pages) {
    # adept query for next page
    query <- paste0('
      {"filters":
          {"geoQuery":
              {"operator":"intersects",
                "mapBounds":{"south":', bounds$south, ',"west":', bounds$west, ',"north":', bounds$north, ',"east":', bounds$east, '},
                "drawings": [', aoi, ']
              },
            "dateTimeRange":
              {"to":"', to, 'T24:00:00.000Z",
                "from":"', from, 'T00:00:00.000Z"
              },
            "entities":["Observation"],
            "dataSources": ', grouparray, ',
            "concepts": ', cncpts, '
          },
        "options": {
          "start": ', (p-1)*page_length+1, ',
          "pageLength": ', page_length, '
        }
      }
    ')

    # call to get next page
    httr::handle_reset(url_search_results)
    result <- httr::POST(url_search_results, body = query, encode = "raw",
                         httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
    searchResult <- httr::content(result)

    # number of results in first page
    Nobservations <- length(searchResult$results)

    if(Nobservations != 0) {
      message(paste0(" ", Nobservations, " results on this page; processing ..." ))
      for (i in 1:Nobservations) {
        # get to content
        content <- searchResult$results[[i]]$extracted$content

        # list the order of the elements of the content
        contentNames <- c()
        for (element in content) {
          contentNames <- c(contentNames, names(element))
        }

        # headers
        headers <- content[[which(contentNames == "headers")]]$headers
        entityId <- headers$entityId
        entityName <- headers$entityName
        entityType <- headers$entityType
        projectId <- headers$projectId
        projectName <- headers$projectName

        # observation
        observation <- content[[which(contentNames == "Observation")]]$Observation
        observationType <- observation$observationType
        agentName <- observation$agent$agentName
        when <- observation$when
        where <- jsonlite::toJSON(observation$where)
        description <- observation$description
        if (length(description) == 0) description <- ""

        # Increment the progress bar, and update the detail text.
        #progress$inc(1/Ntracks, detail = paste("Doing part", i))
        # If we were passed a progress update function, call it
        if (is.function(updateProgress)) {
          text <- paste0("Observation ", (p-1)*page_length+i , " out of ", total)
          updateProgress(value = (p-1)*page_length+i, detail = text)
        }

        # for each concept in observation add a row if it is in filteredConcepts
        concepts <- observation$concepts
        # if (length(concepts) == 0 ) {
        #   print(concepts)
        #   print(observation)
        # }
        Nconcepts <- length(concepts)
        for (j in 1:Nconcepts) {
          conceptLabel <- concepts[[j]]$label
          conceptId  <- concepts[[j]]$conceptId
          # if concept is in filteredConcepts then we need it
          if(is.null(filteredConcepts)) {
            # we want all concepts
            OBSERVATIONS <- rbind(OBSERVATIONS,c(entityType,entityId,entityName,projectId,projectName,observationType,when,where,agentName,conceptLabel,conceptId,description))
            #OBSERVATIONS[nrow(OBSERVATIONS)+1,] <- c(entityType,entityId,entityName,projectId,projectName,observationType,when,where,agentName,conceptLabel,conceptId,description)
          } else {
            if(conceptId %in% filteredConcepts){
              OBSERVATIONS <- rbind(OBSERVATIONS,c(entityType,entityId,entityName,projectId,projectName,observationType,when,where,agentName,conceptLabel,conceptId,description))
              #OBSERVATIONS[nrow(OBSERVATIONS)+1,] <- c(entityType,entityId,entityName,projectId,projectName,observationType,when,where,agentName,conceptLabel,conceptId,description)
            }
          }
        } # end for concepts
      } # end for observations

      # only if extra attributes required for export (in ObservationReport app)
      if (allAttributes == TRUE) {
        message("GET ALL ATTRIBUTES")
        # Do another loop over all observations to get and store the attributes in data frame dfa
        if (p == 1) { # first page: initialise dfa and (overall) row numbering
          dfa <- data.frame()
          row <- 0
        }

        for (i in 1:Nobservations) { # i numbers the observations in the current page
          row <- row + 1

          # get to content
          content <- searchResult$results[[i]]$extracted$content

          # list the order of the elements of the content
          contentNames <- c()
          for (element in content) {
            contentNames <- c(contentNames, names(element))
          }

          # message("Observation", row)
          observation <- content[[which(contentNames == "Observation")]]$Observation
          dfa[row, "observationId"] <- observation$observationId
          # attributes
          attributes <- content[[which(contentNames == "Observation")]]$Observation$attributes
          # debugging
          # message(print(attributes))
          for (j in 1:length(attributes)) {
            key <- attributes[[j]]$key
            if (key == "description") next # skip "description" attribute, which we already have at observation level
            # treat geometry/tags separately, because they will/may have more than 1 value
            if (key == "geometry") {
              varname <- "longitude"
              value <- attributes[[j]]$value$coordinates[[1]]
              dfa[row, varname] <- value
              varname <- "latitude"
              value <- attributes[[j]]$value$coordinates[[2]]
              dfa[row, varname] <- value
            } else if (key == "tags") {
              varname <- key
              value <- paste(unlist(attributes[[j]]$value),collapse = "|")
              dfa[row, varname] <- value
            } else if (length(attributes[[j]]$value) == 1 & !is.list(attributes[[j]]$value)) { # the other attributes will have only 1 value and not be a list
              varname <- attributes[[j]]$key
              value <- attributes[[j]]$value
              dfa[row, varname] <- value
            }
          } # endfor attributes
        } # endif allAttributes
      } # endif Nobservations !=0

      # check if OBSERVATIONS
      if (nrow(OBSERVATIONS) != 0 & ncol(OBSERVATIONS) == 12) {
        # add names
        names(OBSERVATIONS) <- c("entityType", "entityId", "entityName",
                                 "projectId", "projectName", "observationType",
                                 "when", "where", "agentName", "conceptLabel",
                                 "conceptId", "description")
      } else {
        message("NO observation in results")
      }

    } else {
      message(paste0("NO results found on page ", p))
    }
  } # end pagination

  # process time
  elapsed <- round(proc.time()["elapsed"] - ptm["elapsed"])
  message(paste("Successfully processed observations in", elapsed, "seconds"))

  result <- OBSERVATIONS
  if (allAttributes == TRUE) result <- list(OBSERVATIONS, dfa)

  return(result)

}
