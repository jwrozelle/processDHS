#' Calculate Metrics Within Buffers for Point Data
#'
#' This function calculates specified metrics for `spoke_sf` points within a buffer around `hub_sf` points. 
#' The buffer radius can be uniform or vary based on a grouping variable. Metrics are calculated for each 
#' `hub_sf` point and then merged back into the `hub_sf` object.
#'
#' @param hub_sf An sf object containing points around which buffers will be created.
#' @param spoke_sf An sf object containing points for which metrics will be calculated within the buffers of `hub_sf`.
#' @param bufferRadius Numeric, the radius of the buffer(s) in meters. If a single value is provided, 
#'        it applies to all `hub_sf` points. If a named vector is provided, it specifies variable buffer 
#'        radii based on `bufferGroupVar` categories. See the example.
#' @param bufferGroupVar Character vector, the name of the variable in `hub_sf` based on which variable buffer 
#'        radii are defined. Required if `bufferRadius` is a named vector.
#' @param metric Character vector, names of the variables in `spoke_sf` for which metrics (e.g., mean) 
#'        will be calculated within the buffers.
#' @param suffix Character, an optional suffix to append to the metric variable names in the output.
#' @param type The type of calculation to be done. Options are "mean", "median", "max", "min".
#' @param count Character, name of count variable. You may also calculate the count of points that fall within a buffer - this will have the same suffix.dssssss
#' 
#'
#' @return An sf object (`hub_sf`) with new columns added for each of the metrics calculated for the points 
#'         in `spoke_sf` that fall within the buffer radius of each point in `hub_sf`.
#'
#' @examples
#' # Assuming you have two sf objects, hub_sf and spoke_sf, with appropriate CRS and variables:
#' # result_sf <- pointMetricsInBuffer(hub_sf, spoke_sf, bufferRadius = c("value1" = 10000, "value2" = 5000), 
#'                                     bufferGroupVar = "category", metric = c("var1", "var2"))
#'
#' @importFrom dplyr filter arrange
#' @importFrom sf st_buffer st_join st_within st_drop_geometry
#' @export
#'
#' @note This function requires the `sf` and `dplyr` packages. Also, ensure that `hub_sf` and `spoke_sf` 
#'       are in the same coordinate reference system (CRS) and that `bufferRadius` is appropriate for the CRS's 
#'       units (typically meters for projected CRS).


pointMetricsInBuffer <- function(
    hub_sf,
    spoke_sf, 
    bufferRadius = 1e4, 
    bufferGroupVar = NULL, 
    # bufferRadius = c("U" = 1e4, "R" = 4e3), 
    # bufferGroupVar = "URBAN_RURA", 
    metric, 
    suffix = "",
    type = "mean",
    countVar = NULL
    ) {
  
  # load(Sys.getenv("TESTING_DHS_GEDATA"))
  # GEdata <- GEdata.list$MW7
  # load(Sys.getenv("TESTING_SPA_DATA"))
  # FCdata <- spa.list$MW_SPA13$FC
  # 
  # hub_sf <- GEdata
  # spoke_sf <- FCdata
  # 
  # metric <- c("sri_score", "sri_basicamenities")
  # 
  # suffix = ".test"
  # countVar = "countHF"
  
  require(dplyr)
  
  
  # Throw an error if buffer radius is >1 and the group variable is not named
  if(length(bufferRadius) > 1 & is.null(bufferGroupVar)) {
    stop("If bufferRadius has length > 1, then bufferGroupVar must be specified.")
  }
  
  # Throw an error if buffer radius names do not match the buffer group variable values
  if (sum(names(bufferRadius) %in% hub_sf[[bufferGroupVar]]) != length(bufferRadius)) {
    stop("All names of bufferRadius must be values in the bufferGroupVar.")
  }
  
  # throw an error if all unique bufferGroupVar values are not captured
  if (length(bufferRadius) != length(unique(hub_sf[[bufferGroupVar]]))) {
    warning("There are values in bufferGroupVar that are not in the names of bufferRadius. Some rows will have missing values.")
  }
  
  # # check that the hubID variable is unique
  # if (length(unique(hub_sf[[hubIDVar]])) != nrow(hub_sf)) {
  #   stop("hubIDVar must be a uniquely valid ID for each row of hub_sf.")
  # }
  
  # check to make sure the new variables that are created aren't already in hub_sf
  if (sum(paste0(metric, suffix) %in% names(hub_sf)) > 1) {
    stop("At least one variable name specified in metric already exists in hub_sf. Either drop the duplicate or use a different suffix.")
  }
  
  if (!type %in% c("mean", "median", "min", "max")) {
    stop("type must be specified as 'mean', 'median', 'min', or 'max'.")
  }
  
  # preserve the original order and create a unique ID
  hub_sf$temp_2cec8e12b8794706bf596fdb6ead814d <- 1:nrow(hub_sf)
  
  # if there are different buffer lengths, lapply for each length, otherwise just apply buffer radius to all rows
  if (length(bufferRadius) > 1 ) {
    # first extract the group categories, and name the vector by the groups
    bufferRadiusGroups <- names(bufferRadius)
    names(bufferRadiusGroups) <- bufferRadiusGroups
    
    # now create the buffers
    buffers.list <- lapply(bufferRadiusGroups, function(group) {
      ## Get the subset of buffers
      group_hub_buffer.sf <- dplyr::filter(hub_sf, .data[[bufferGroupVar]] == group)
      
      hub_buff.sf <- sf::st_buffer(group_hub_buffer.sf, bufferRadius[group])
      
      return(hub_buff.sf)
    })
    
    # initial rbind
    buff_sf <- rbind(buffers.list[[1]], buffers.list[[2]])
    
    # all additional rbinds
    if (length(buffers.list) > 2) {
      for (i in 3:length(buffers.list)) {
        buff_sf <- rbind(buff_sf, buffers.list[[i]])
      }
    }
      
  } else { # if there's only one, it's much simpler
    buff_sf <- sf::st_buffer(hub.sf, bufferRadius)
  }

  # drop unnecessary stuff
  rm(bufferRadiusGroups, buffers.list)
  
  # join the health facility points to the cluster buffer
  joinedObject <- sf::st_join(spoke_sf, buff_sf, join = sf::st_within, suffix = suffix)
  # drop the geometry of the joined object
  joinedObject <- sf::st_drop_geometry(joinedObject)
  
  hubIDs <- hub_sf$temp_2cec8e12b8794706bf596fdb6ead814d
  names(hubIDs) <- hubIDs
  
  # get health facility scores, returned as vector
  hseScores <- sapply(hubIDs, function(hubID) {
    relevantHFs <- dplyr::filter(joinedObject, temp_2cec8e12b8794706bf596fdb6ead814d == hubID)
    
    hseScore <- c()
    for (i in 1:length(metric)) {
      
      if (type == "mean") {
        hseScore[i] <- mean(relevantHFs[[paste0(metric[i], suffix)]], na.rm = TRUE)
      } else if (type == "median") {
        hseScore[i] <- median(relevantHFs[[paste0(metric[i], suffix)]], na.rm = TRUE)
      } else if (type == "max") {
        hseScore[i] <- max(relevantHFs[[paste0(metric[i], suffix)]], na.rm = TRUE)
      } else if (type == "min") {
        hseScore[i] <- min(relevantHFs[[paste0(metric[i], suffix)]], na.rm = TRUE)
      }
      
    }
    
    # if a  count variable is specified - add it to the stats
    if (!is.null(countVar)) {
      hseScore[length(metric) + 1] <- nrow(relevantHFs)
      names(hseScore) <- paste0(c(metric, countVar), suffix)
    } else { # otherwise ignore it
      names(hseScore) <- paste0(metric, suffix)
    }
    
    
    
    
    return(hseScore)
    
  })
  
  hseScores <- t(hseScores) |> as.data.frame()
  hseScores$temp_2cec8e12b8794706bf596fdb6ead814d <- row.names(hseScores) |> as.numeric()
  
  # merge the new values back into hub_sf
  output_sf <- merge(hub_sf, hseScores, by = "temp_2cec8e12b8794706bf596fdb6ead814d", all.x = T, all.y = F)
  
  # sort by the original order
  output_sf <- output_sf %>% 
    arrange(temp_2cec8e12b8794706bf596fdb6ead814d)
  
  # drop the temporary variable name
  output_sf <- output_sf[,!names(output_sf) %in% "temp_2cec8e12b8794706bf596fdb6ead814d"]
  
  
  return(output_sf)
  
}