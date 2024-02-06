get_track_counts <- function(cookie,
                             group,
                             from = "1900-01-01",
                             to = "2999-12-31",
                             url = "https://focus.sensingclues.org/",
                             lang = "en") {
  # ontology counts
  url_search_results <- paste0(url, "api/ontology/all/counts?language=", lang)
  grouparray <- jsonlite::toJSON(unlist(group)) # if multiple groups selected

  query <- paste0('
    {"filters":
        {
          "dateTimeRange":
            {"to":"', to, 'T24:00:00.000Z",
              "from":"', from, 'T00:00:00.000Z"
            },
          "entities":["track"],
          "dataSources": ', grouparray, '
        }
    }
  ')

  httr::handle_reset(url_search_results)
  result <- httr::POST(url_search_results, body = query, encode = "raw", httr::content_type_json(), httr::set_cookies(focus2 = utils::URLdecode(cookie$value)))
  counts <- httr::content(result)$"values-response"$"distinct-value"

  NTrackTypes <- length(counts)
  track_counts <- data.frame()
  if (NTrackTypes != 0) {
    h <- wildcatr::get_hierarchy(url = url, lang = lang)
    for (i in 1:NTrackTypes) {
      id <- counts[[i]]$'_value' # e.g. https://sensingclues.poolparty.biz/SCCSSOntology/0
      # filter out scheme concepts
      if (!id %in% unlist(h$schemes)) {
        parent <- wildcatr::get_parent_id(id, h)
        # filter out concepts that do not belong to the top concept for tracks
        if (parent == "https://sensingclues.poolparty.biz/SCCSSOntology/42" | # concept 42 = Tracks
            parent == "https://sensingclues.poolparty.biz/SCCSSOntologyv3/136") { # concept 136 = Animal tracks
          name <- wildcatr::get_label(id, h)
          count <- counts[[i]]$frequency
          track_counts <- rbind(track_counts, c(id, name, count))
        }
      }
    }
    # names
    names(track_counts) <- c("id", "name", "count")
  } else {
    message("No track counts received from backend returning NULL")
    track_counts <- NULL
  }
  return(track_counts)
}
