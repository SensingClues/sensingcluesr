#' @keywords internal
#'
#' @details
#' Every function that returns your own data needs a cookie obtained by
#' [login_cluey()], so a typical session starts with a login and a look at the
#' groups you have access to:
#'
#' ```r
#' cookie <- login_cluey("YOUR_USERNAME", "YOUR_PASSWORD")
#' groups <- get_groups(cookie)
#' ```
#'
#' The identifiers in the `value` column of `groups` are what the other
#' functions expect as their `group` argument.
#'
#' @section Observations:
#' - [get_observations()] retrieves observations as a data frame.
#' - [get_concept_counts()] counts how often each concept occurs in
#'   observations, without downloading the observations themselves.
#'
#' @section Tracks:
#' - [get_tracks()] retrieves one row of metadata per track.
#' - [get_track()] and [get_track_as_geojson()] retrieve a single track in
#'   full detail.
#' - [get_track_coordinates()] retrieves the individual coordinates that make
#'   up the tracks.
#' - [get_track_counts()] counts how often each patrol type occurs.
#'
#' @section Maps and geodata:
#' - [get_layer_details()] lists the map layers you have access to, and
#'   [get_layer_features()] retrieves one of them as a simple feature
#'   collection.
#' - [get_heatmap()] aggregates observations or tracks into a raster grid.
#'
#' @section Ontologies:
#' Observations and tracks refer to concepts in Sensing Clues' ontologies by
#' identifier. [get_hierarchy()] retrieves those ontologies, after which
#' [get_id()], [get_label()], [get_parent_id()], [get_parent_label()],
#' [get_children_id()] and [get_children_label()] let you translate between
#' identifiers and labels and walk up and down the hierarchy.
#'
#' @section Agents:
#' - [get_agent()] retrieves the details of a single agent, for example to
#'   resolve the agent identifiers found in track data.
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom sf st_as_sf
## usethis namespace: end
NULL
