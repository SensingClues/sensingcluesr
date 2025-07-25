#' Retrieve track coordinate data
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param group One or multiple group identification character string(s), see which groups you have access to with [get_groups()].
#' @param bounds Bounding box coordinates (latitude and longitude) in list(north, east, south, west) format. For example `list(north=15, east=10, south=-25, west=50)`.
#' @param from Start date.
#' @param to End date.
#' @param aoi Area of interest.
#' @param concepts One or multiple concept definitions, for example `https://sensingclues.poolparty.biz/SCCSSOntology/631`. See [https://sensingclues.poolparty.biz/GraphViews/](https://sensingclues.poolparty.biz/GraphViews/) for all available concepts.
#' @param updateProgress A function to update a progress bar object, default is NULL.
#' @param total_tracks The number of tracks to retrieve per page of the query.
#' @param downsample A boolean. Allows you to downsample the traces per track.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#'
#' @return A data frame where each row represents a node in a track. All nodes of all tracks collected by the defined group(s), within the given date range, are returned in the same data frame.
#' @export
#'
#' @examples
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' df <- get_track_coordinates(cookie, group = 'focus-project-1234') # demo group
get_track_coordinates <- function(cookie,
                                  group,
                                  bounds = NULL, # list(north = 90, east = 180, south = -89, west = -179)
                                  from = Sys.Date() - 30,
                                  to = Sys.Date(),
                                  aoi = "",
                                  concepts = NULL,
                                  updateProgress = NULL,
                                  total_tracks = 500,
                                  downsample = TRUE,
                                  url = "https://focus.sensingclues.org/",
                                  lang = "en") {
  message(paste0("Start downloading tracks for group ", group, " from ", from, " to ", to))

  # process time
  ptm <- proc.time()

  # select the proper source URL
  url_search_results <- paste0(url, "api/map/all/track/0/features?language=", lang)

  grouparray <- jsonlite::toJSON(unlist(group))

  # bounds check
  if (!is.null(bounds)) {
    bounds <- check_bounds(bounds) # helper function from tracks.R
    boundaries <- paste0('{"south":', bounds$south, ',"west":', bounds$west, ',"north":', bounds$north, ',"east":', bounds$east, '}')
    message(paste("North ", bounds$north, "East ", bounds$east, "South ", bounds$south, "West ", bounds$west))
  } else {
    boundaries <- "[]"
  }

  # concepts to json array
  if (!is.null(concepts)) {
    cncpts <- jsonlite::toJSON(unlist(concepts))
  } else {
    cncpts <- "[]"
  }

  page_length <- 500 # optimal value regarding query performance
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
                "mapBounds": ', boundaries,',
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
          if (downsample) {
            # devide dataset into 5-minute time slots and save only 1 5-minute slot
            coords$timeslot <- cumsum(coords$dt)%/%(60*5)
            coords <- coords[!duplicated(coords$timeslot), -4]
          }
        }

        # add the track identification, agent and patrol type
        coords$trackId <- trackDATA$features[[i]]$properties$EntityId
        coords$agent <- trackDATA$features[[i]]$properties$AgentRef
        coords$patrolType <- sub("^track/", "", trackDATA$features[[i]]$properties$EntityType)

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
