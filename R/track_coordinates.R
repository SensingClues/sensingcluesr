get_track_coordinates <- function(cookie,
                                  group,
                                  bounds = list(north = 90, east = 180, south = -89, west = -179),
                                  from = Sys.Date() - 30,
                                  to = Sys.Date(),
                                  aoi = "",
                                  concepts = NULL,
                                  updateProgress = NULL,
                                  total_tracks = 500,
                                  url = "https://focus.sensingclues.org/",
                                  lang = "en") {
  message(paste0("Start downloading tracks for group ", group, " from ", from, " to ", to))

  # process time
  ptm <- proc.time()

  # select the proper source URL
  url_search_results <- paste0(url, "api/map/all/track/0/features?language=", lang)

  grouparray <- jsonlite::toJSON(unlist(group))

  # concepts to json array
  if (!is.null(concepts)) {
    cncpts <- jsonlite::toJSON(unlist(concepts))
  } else {
    cncpts <- "[]"
  }

  page_length <- 500 # standard maximum on the api, decreasing causes longer processing times
  p <- 1 # page
  Ntracks <- page_length # to start the while loop
  TRACKS <- NULL # initiate the result
  t <- 0 # track counter

  # get the list with tracking data locations
  while (Ntracks == page_length) {
    query <- paste0('
      {"filters":
          {"geoQuery":
              {"operator":"intersects",
                "mapBounds":{"south":', bounds$south, ',"west":', bounds$west, ',"north":', bounds$north, ',"east":', bounds$east, '},
                "drawings":[', aoi, ']},
            "dateTimeRange":{"to":"', to, 'T24:00:00.000Z","from":"', from, 'T00:00:00.000Z"},
            "dataSources": ', grouparray, ',
            "concepts": ', cncpts, '
          },
        "start": ', (p-1)*page_length+1, ',
        "pageLength": ', page_length, '
      }
    ') # for /api/map it is not possible yet to use parameters start and pageLength within options
    # reset cookies
    httr::handle_reset(url_search_results)
    result <- httr::POST(url_search_results, body = query, encode = "raw", httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
    trackDATA <- httr::content(result)
    Ntracks <- length(trackDATA$features)

    if (Ntracks != 0) {
      message(paste0("We have ", Ntracks, " results on this page, processing ..."))
      for (i in 1:Ntracks) {
        # get the detailed coordinates of the tracks
        coords <- as.numeric(unlist(trackDATA$features[[i]]$geometry)[-1])
        coords <- t(matrix(unlist(coords), nrow = 2, ncol = length(coords)))
        coords <- data.frame(coords)
        names(coords) <- c("lon", "lat")

        # Increment the progress bar, and update the detail text.
        #progress$inc(1/Ntracks, detail = paste("Doing part", i))
        # If we were passed a progress update function, call it
        if (is.function(updateProgress)) {
          text <- paste0("Track ", t, " out of ", total_tracks)
          updateProgress(value = t, detail = text)
        }
        # debug counter
        # message(paste0("Track ", i, " out of ", Ntracks))
        # add timestamp data
        # first try this one:
        timestamps <- strsplit(trackDATA$features[[i]]$properties$DateTimes, ",")[[1]]

        # if that doesn't work:
        if (is.null(timestamps)) {
          coords$time <- NA
          coords$timeslot <- NA
        } else {
          coords$time <- timestamps
          # remove duplicates
          coords <- coords[!duplicated(coords$time),]
          # calculate the time difference between subsequent fixes
          coords$time <- `substr<-`(coords$time, 11, 11, " ")
          coords$time <- as.POSIXlt(coords$time)
          coords$dt <- c(as.numeric(coords$time[-nrow(coords)] - coords$time[-1]),0)
          # order the data so that it increases in time
          coords <- coords[order(coords$time),]
          # devide dataset into 5-minute time slots and save only 1 5-minute slot
          coords$timeslot <- cumsum(coords$dt)%/%(60*5)
          coords <- coords[!duplicated(coords$timeslot), -4]
        }

        # add the track identification
        id <- trackDATA$features[[i]]$properties$EntityId
        coords$trackId <- id

        TRACKS <- rbind(TRACKS, coords)
        t <- t+1

      } # for tracks
      p <- p+1
    } # if TRACK != 0
  }

  # process time
  dt <- proc.time() - ptm
  message(paste("Successfully fetched", t, "tracks in", dt["elapsed"], "seconds"))

  return(TRACKS)
}
