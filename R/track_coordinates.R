#' Retrieve track coordinate data
#'
#' Downloads the individual coordinates (nodes) that make up the tracks, which is
#' what you need to plot the tracks on a map. Use [get_tracks()] instead if one
#' row of metadata per track is enough.
#'
#' Duplicate timestamps within a track are dropped and the remaining nodes are
#' ordered by time. By default the nodes are also downsampled to at most one
#' node per five minutes, which keeps long tracks manageable; set
#' `downsample = FALSE` to get every node.
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param group One or multiple group identification character string(s), see which groups you have access to with [get_groups()].
#' @param bounds Bounding box coordinates (latitude and longitude) in list(north, east, south, west) format. For example `list(north=15, east=10, south=-25, west=50)`. Values outside the range supported by the platform are clamped to it (north 90, east 180, south -89, west -179). Default `NULL` places no restriction on the location.
#' @param from Start of the date range, as a `Date` or a `"YYYY-MM-DD"` character string. Default is 30 days ago.
#' @param to End of the date range, as a `Date` or a `"YYYY-MM-DD"` character string. Default is today.
#' @param aoi Area of interest: a GeoJSON geometry as a character string, restricting the results to that area. Default `""` places no restriction on the area.
#' @param concepts One or multiple concept definitions, for example `https://sensingclues.poolparty.biz/SCCSSOntology/631`. See [https://sensingclues.poolparty.biz/GraphViews/](https://sensingclues.poolparty.biz/GraphViews/) for all available concepts. Default `NULL` returns all concepts.
#' @param updateProgress A function to update a progress bar object, default is NULL. It is called once per track with the arguments `value` (the number of tracks processed so far) and `detail` (a progress message), which makes it suitable for a Shiny progress bar.
#' @param total_tracks The number of tracks to retrieve per page of the query. Only used in the progress messages passed to `updateProgress`, not to limit the results.
#' @param downsample A boolean. Allows you to downsample the traces per track. If `TRUE` (the default), at most one node per five minutes is kept per track.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#' @param lang Language in which the concepts are shown, default is English.
#'
#' @return A data frame where each row represents a node in a track. All nodes of
#' all tracks collected by the defined group(s), within the given date range, are
#' returned in the same data frame, with the following columns:
#' - `lon`, `lat`: Coordinates of the node, as numbers.
#' - `time`: Timestamp of the node, as the character string returned by the
#'   platform, or `NA` if the track carries no timestamps.
#' - `trackId`: Identifier of the track the node belongs to, matching the
#'   `entityId` column of [get_tracks()]. Group by this column to draw one line
#'   per track.
#' - `agent`: Reference to the agent that recorded the track.
#' - `patrolType`: Patrol type of the track.
#'
#' Returns `NULL` if no tracks are found.
#' @export
#'
#' @examples
#' \dontrun{
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' df <- get_track_coordinates(cookie, group = 'focus-project-1234') # demo group
#'
#' # keep every node instead of one node per five minutes
#' df <- get_track_coordinates(cookie,
#'                             group = 'focus-project-1234',
#'                             downsample = FALSE)
#' }
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

        # keep the original timestamp strings, use UTC-normalised
        # POSIXct for deduplication, ordering, and downsampling
        timestamps <- strsplit(trackDATA$features[[i]]$properties$DateTimes, ",")[[1]]
        if (is.null(timestamps)) {
          coords$time <- NA
        } else {
          posix_time <- as.POSIXct(timestamps, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
          idx <- which(!duplicated(posix_time))
          idx <- idx[order(posix_time[idx])]
          if (downsample) {
            elapsed <- as.numeric(difftime(posix_time[idx], posix_time[idx[1]], units = "secs"))
            idx <- idx[!duplicated(elapsed %/% (60 * 5))]
          }
          coords <- coords[idx, ]
          coords$time <- timestamps[idx]
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
