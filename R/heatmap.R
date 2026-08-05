#' Retrieve a heatmap raster
#'
#' @param cookie A cookie obtained by [login_cluey()].
#' @param group One or multiple group identification character string(s), see which groups you have access to with [get_groups()].
#' @param bounds Bounding box coordinates (latitude and longitude) in list(north, east, south, west) format. For example `list(north=15, east=10, south=-25, west=50)`.
#' @param from Start date.
#' @param to End date.
#' @param aoi Area of interest.
#' @param concepts One or multiple concept definitions, for example `https://sensingclues.poolparty.biz/SCCSSOntology/631`. See [https://sensingclues.poolparty.biz/GraphViews/](https://sensingclues.poolparty.biz/GraphViews/) for all available concepts.
#' @param resolution The number of rows and columns of the output raster grid, default is 25.
#' @param type The type of data to build the heatmap from, either `"observation"` (default) or `"track"`.
#' @param url A Sensing Clues URL, default is [https://focus.sensingclues.org/](https://focus.sensingclues.org/).
#'
#' @return A `terra` SpatRaster with `resolution` x `resolution` grid cells, where each cell holds the number of results collected by the defined group(s) within the given date range and bounds. Returns `NULL` when no results are found.
#' @export
#'
#' @examples
#' \dontrun{
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' r <- get_heatmap(cookie, group = 'focus-project-1234') # demo group
#' }
get_heatmap <- function(cookie,
                        group,
                        bounds = NULL, # list(north = 90, east = 180, south = -89, west = -179)
                        from = Sys.Date() - 30,
                        to = Sys.Date(),
                        aoi = "",
                        concepts = NULL,
                        resolution = 25,
                        type = "observation", # or "track"
                        url = "https://focus.sensingclues.org/") {
  message(paste0("Start downloading heatmap for group ", group, " from ", from, " to ", to))

  # process time
  ptm <- proc.time()

  # entity type to include in the heatmap: 0 for observations, 1 for tracks
  if (type == "observation") {
    entity_code <- 0
  } else if (type == "track") {
    entity_code <- 1
  } else {
    stop('`type` must be either "observation" or "track"')
  }

  # select the proper source URL
  url_search_results <- paste0(url, "api/map/all/default/", entity_code, "/features?returnQuery=true&returnFeatures=false&heatmap=", resolution, "x", resolution)

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

  # get the list with tracking data locations # "[]" for all groups
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
        "start": 0,
        "pageLength": 1
      }
    ') # for /api/map it is not possible yet to use parameters start and pageLength within options
  # reset cookies
  httr::handle_reset(url_search_results)
  result <- httr::POST(url_search_results, body = query, encode = "raw", httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
  content <- httr::content(result)
  total <- content$metadata$total

  if (total > 0) {
    boundaries <- content$metadata$bounds
    boxes <- content$boxes
    message(paste0("We have ", total, " results, processing ", length(boxes), " boxes ..."))

    # Create raster with resolution x resolution cells, all values NA
    # boundaries is a JSON string "[south, west, north, east]"
    boundaries <- jsonlite::fromJSON(boundaries)
    ext <- terra::ext(boundaries[[2]], boundaries[[4]], boundaries[[1]], boundaries[[3]])
    r <- terra::rast(ext, nrows = resolution, ncols = resolution, crs = "EPSG:4326")
    terra::values(r) <- NA

    # Fill in box counts using box center coordinates
    # box$bounds is a JSON string "[south, west, north, east]"
    # boundary sentinel values (-90, -180, 90, 180) are replaced with the actual boundaries
    centers_x <- sapply(boxes, function(b) {
      bnd <- jsonlite::fromJSON(b$bounds)
      west  <- ifelse(bnd[[2]] == -180, boundaries[[2]], bnd[[2]])
      east  <- ifelse(bnd[[4]] ==  180, boundaries[[4]], bnd[[4]])
      (west + east) / 2
    })
    centers_y <- sapply(boxes, function(b) {
      bnd <- jsonlite::fromJSON(b$bounds)
      south <- ifelse(bnd[[1]] ==  -90, boundaries[[1]], bnd[[1]])
      north <- ifelse(bnd[[3]] ==   90, boundaries[[3]], bnd[[3]])
      (south + north) / 2
    })
    counts    <- sapply(boxes, function(b) b$count)
    cell_idx  <- terra::cellFromXY(r, cbind(centers_x, centers_y))
    valid     <- !is.na(cell_idx)
    r[cell_idx[valid]] <- counts[valid]
  } else {
    r <- NULL
  }

  # process time
  dt <- proc.time() - ptm
  message(paste("Successfully fetched heatmap in", dt["elapsed"], "seconds"))
  return(r)
}
