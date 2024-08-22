#' Retrieve tracks metadata
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param group One or multiple group identification character string(s), see which groups you have access to with [get_groups()].
#' @param bounds Bounding box coordinates (latitude and longitude) in list(north, east, south, west) format. For example `list(north=15, east=10, south=-25, west=50)`.
#' @param from Start date.
#' @param to End date.
#' @param aoi Area of interest.
#' @param patrolType One or multiple concept definitions, for example `https://sensingclues.poolparty.biz/SCCSSOntology/631`. See [https://sensingclues.poolparty.biz/GraphViews/](https://sensingclues.poolparty.biz/GraphViews/) for all available concepts.
#' @param updateProgress A function to update a progress bar object, default is NULL.
#' @param allAttributes A boolean. Allows you to collect more attributes for the tracks.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#'
#' @return A data frame with all tracks collected by the defined group(s), within the given date range.
#' @export
#'
#' @examplesIf !is.null(login_cluey())
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' df <- get_tracks(cookie, group = 'focus-project-1234') # demo group
#'
#' # get extra details on a single track
#' id <- df$entityId[1]
#' lst <- get_track(cookie, trackId = id)
#'
#' # get a single track in JSON format
#' json <- get_track_as_geojson(cookie, trackId = id)
get_tracks <- function(cookie,
                       group,
                       bounds = NULL, # list(north = 90, east = 180, south = -89, west = -179)
                       from = Sys.Date() - 30,
                       to = Sys.Date(),
                       aoi = "",
                       patrolType = NULL,
                       updateProgress = NULL,
                       allAttributes = FALSE,
                       url = "https://focus.sensingclues.org/",
                       lang = "en") {

  message(paste0("Start downloading tracks from ", from, " to ", to,
                 " on group ", group, " for patrolType ", patrolType,
                 " within ", paste(bounds, collapse = " ")))

  # process time
  ptm <- proc.time()

  # bounds check
  if (!is.null(bounds)) {
    bounds <- check_bounds(bounds)
    boundaries <- paste0('{"south":', bounds$south, ',"west":', bounds$west, ',"north":', bounds$north, ',"east":', bounds$east, '}')
    message(paste("North ", bounds$north, "East ", bounds$east, "South ", bounds$south, "West ", bounds$west))
  } else {
    boundaries <- "[]"
  }

  # concepts to json array
  if (!is.null(patrolType)) {
    cncpts <- jsonlite::toJSON(unlist(patrolType))
  } else {
    cncpts <- "[]"
  }

  # page length
  page_length <- 50 # optimal value regarding query performance and user experience

  # select the proper source URL
  url_search_results <- paste0(url, "api/search/all/results?language=", lang)
  grouparray <- jsonlite::toJSON(unlist(group)) # if multiple groups selected

  # build search query
  query <- paste0('
    {"filters":
        {"geoQuery":
            {"operator":"intersects",
              "mapBounds": ', boundaries,',
              "drawings":[', aoi, ']
            },
          "dateTimeRange":
            {"to":"', to, 'T24:00:00.000Z",
              "from":"', from, 'T00:00:00.000Z"
            },
          "entities":["track"],
          "dataSources": ', grouparray, ',
          "concepts": ', cncpts, '
        },
      "options": {
        "start": 1,
        "pageLength": 0
      }
    }
  ')

  # initial call to get total and number of pages to get
  httr::handle_reset(url_search_results)
  result <- httr::POST(url_search_results, body = query, encode = "raw", httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
  searchResult <- httr::content(result)
  # names(searchResult$results[[1]]$extracted$content[[2]]$GeoFeature)

  # setup empty dataframe
  TRACKS <- data.frame()

  # total results to get
  total <- searchResult$total
  # pages to fetch
  pages <- ceiling(total / page_length)

  # set progress
  #updateProgress$set(value = total)

  # expected number of results
  message(paste0("Expecting total ", as.character(total), " results in ", as.character(pages), " number of pages"))

  # if pages < 1 then skip

  # pagination
  for (p in 1:pages) {
    # message(paste0("We are on page ", p))
    # adept query for next page
    query <- paste0('
      {"filters":
          {"geoQuery":
              {"operator":"intersects",
                "mapBounds": ', boundaries,',
                "drawings":[', aoi, ']
              },
            "dateTimeRange":
              {"to":"', to, 'T24:00:00.000Z",
                "from":"', from, 'T00:00:00.000Z"
              },
            "entities":["track"],
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
    result <- httr::POST(url_search_results, body = query, encode = "raw", httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
    searchResult <- httr::content(result)

    # number of results in first page
    Ntracks <- length(searchResult$results)

    if(Ntracks != 0) {
      message(paste0("We have ", Ntracks, " results on this page; processing ..." ))
      for (i in 1:Ntracks) {
        # get to content
        content <- searchResult$results[[i]]$extracted$content

        # list the order of the elements of the content
        contentNames <- c()
        for (element in content) {
          contentNames <- c(contentNames, names(element))
        }

        # headers
        #browser() # check to see if "team size" and "description" are now in the result
        headers <- content[[which(contentNames == "headers")]]$headers
        entityId <- headers$entityId
        entityType <- headers$entityType
        projectId <- headers$projectId
        projectName <- headers$projectName

        # geofeature
        geofeature <- content[[which(contentNames == "GeoFeature")]]$GeoFeature
        featureType <- geofeature$featureType
        # length in km
        length <- geofeature$length

        # as.POSIXct(strptime(dt1, "%Y-%m-%dT%H:%M:%S"))
        startWhenChar <- geofeature$startWhen
        endWhenChar <- geofeature$endWhen

        # agent
        agentName <- geofeature$agent$agentName

        # Increment the progress bar, and update the detail text.
        #progress$inc(1/Ntracks, detail = paste("Doing part", i))
        # If we were passed a progress update function, call it
        if (is.function(updateProgress)) {
          text <- paste0("Track ", (p-1)*page_length+i, " out of ", total)
          updateProgress(value = (p-1)*page_length+i, detail = text)
        }
        TRACKS <- rbind(TRACKS, c(entityType,entityId,projectId,projectName,featureType,length,startWhenChar,endWhenChar,agentName)) # skip patrolDuration here

        # retrieve the attributes of a track
        if (allAttributes == TRUE) {
          if (!exists("dfa")) dfa <- data.frame() # initialize dfa

          # unpack the attributes
          attributes <- content[[which(contentNames == "GeoFeature")]]$GeoFeature$attributes
          dfa <- dplyr::bind_rows(dfa, unpack_attributes(entityId, attributes))
        }
      }
      # names
      names(TRACKS) <- c("entityType", "entityId", "projectId", "projectName",
                         "featureType", "length", "startWhenChar",
                         "endWhenChar", "agentName")
    } else {
      message("No tracks found on this page")
    }
  } # end pagination

  result <- TRACKS
  if (allAttributes == TRUE) {
    message("Adding attributes to the dataframe")
    result <- dplyr::left_join(TRACKS, dfa, by = "entityId")
  }

  # process time
  dt <- proc.time() - ptm
  message(paste("Successfully fetched", nrow(result), "tracks in", dt["elapsed"], "seconds"))

  return(result)
}

#' @rdname get_tracks
#' @param trackId Identification character string of a single track.
#' @export
get_track <- function(cookie, trackId, url = "https://focus.sensingclues.org/", lang = "en") {
  # https://focus.sensingclues.org/api/crud/GeoFeature/G3588368-69d14fa1-64d9-40fd-a11a-1112ecca1e95?language=en
  url_search_results <- paste0(url, "api/crud/GeoFeature/", trackId, "?language=", lang)
  httr::handle_reset(url_search_results)
  trackEnveloppe <- httr::content(httr::GET(url_search_results, httr::set_cookies(focus2 = utils::URLdecode(cookie$value))))
  trackEnveloppe
}

#' @rdname get_tracks
#' @export
get_track_as_geojson <- function(cookie, trackId, url = "https://focus.sensingclues.org/", lang = "en") {
  message(paste0("Start fetching track ", trackId, " as geojson"))

  # process time
  ptm <- proc.time()
  # select the proper source URL
  url_search_results <- paste0(url, "api/map/all/track/0/features?language=", lang)

  query <- paste0('
    {"filters":
        {"queryText": "entityId:\\"', trackId, '\\""},
      "options": {},
      "start": 1,
      "pageLength": 500
    }
  ')

  # reset cookies
  httr::handle_reset(url_search_results)
  result <- httr::POST(url_search_results, body = query, httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
  trackGeojson <- httr::content(result)

  return(jsonlite::toJSON(trackGeojson, auto_unbox = TRUE))
}

# Helpers ---------------------------------------------------------------------

check_bounds <- function(bounds) {
  # check if bounds are in range so marklogic does not return zero results
  if (bounds$north > 90) {
    bounds$north <- 90
  }
  if (bounds$south < -89) {
    bounds$south <- -89
  }
  if (bounds$east > 180){
    bounds$east <- 180
  }
  if (bounds$west < -179) {
    bounds$west <- -179
  }
  return(bounds)
}
