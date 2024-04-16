#' Nearest Neighbor Filtering and Extraction
#'
#' This function applies a filtering criterion to a spatial data frame (point_sf),
#' calculates the minimum distances between points, and extracts a specified column
#' from the nearest neighbors that meet the filtering criteria.
#'
#' @param point_sf A spatial data frame containing point data.
#' @param filterVar A string indicating the column name in point_sf used for filtering.
#'                 If NULL, no filtering is applied based on variable values.
#' @param filterValue A value (or vector of values) used for filtering the points
#'                    in the column specified by filterVar. If NULL, no filtering
#'                    is applied based on variable values.
#' @param extractColumn A string indicating the column name from which data is
#'                      extracted once the nearest neighbor is determined.
#'
#' @return A vector extracted from the nearest neighbors' specified column after
#'         applying the filtering conditions.
#'
#' @details The function first checks if the filterVar and filterValue are provided and valid.
#'          If they are, it filters the point_sf to include only those points where
#'          the filterVar matches the filterValue. If no filtering is required or specified,
#'          it uses all points in point_sf. After filtering, it calculates the distances
#'          between all points and the filtered points, excludes self-comparisons by setting
#'          distances from a point to itself as infinity, and identifies the nearest neighbor
#'          for each point. Finally, it extracts the desired column from these nearest neighbors.
#'
#' @examples
#' # Load data (assuming point_sf is already loaded and appropriately formatted)
#' # nn_filterExtract(point_sf, filterVar = "svc_antenatalCare", filterValue = 1, extractColumn = "typeDHS_category")
#'
#' @export
nn_filterExtract <- function(point_sf, filterVar=NULL, filterValue=NULL, extractColumn, y_dedup = T) {
  
  require(dplyr)
  
  # load(Sys.getenv("TESTING_SPA_DATA"))
  # point_sf <- spa.list$MW_SPA13$FC
  # filterVar <- "svc_antenatalCare"
  # filterVar <- "svc_caesarean"
  # filterValue <- 1
  # uniqueID <- "facID"
  # extractColumn <- "typeDHS_category"
  
  tmpID_var <- "tmpID_3dc99750ca4247f1ad048e86dec6e57c"
  tmpOutput_var <- "tmpOutput_385ea504c5eb48ac8f7ff58d5621c30c"
  tmpSort_var <- "tmpSort_3dc99750ca4247f1ad048e86dec6e57c"
  
  # # Assuming FCdata.sf is already loaded and has a unique identifier column named 'id'
  point_sf[[tmpSort_var]] <- 1:nrow(point_sf)
  
  rownames(point_sf) <- paste0("obs_", 1:nrow(point_sf))
  point_sf[[tmpID_var]] <- paste0("obs_", 1:nrow(point_sf))
  
  if (!is.null(filterVar) | !is.null(filterValue)) {
    if (!filterVar %in% names(point_sf)) {
      warning(paste0("The point_sf object has no column '", filterVar, "'. Returning a vector of NA."))
      min_distances <- rep(NA, nrow(point_sf))
      return(min_distances)
    }
    
    if (sum(filterValue %in% point_sf[[filterVar]]) == 0) {
      warning(paste0("There is no value ", filterValue, " in ", filterVar, ". Returning a vector of NA."))
      min_distances <- rep(NA, nrow(point_sf))
      return(min_distances)
    } else {
      
      
      # Filter to get only the points where svc_art == 1
      points_of_interest <- dplyr::filter(point_sf, .data[[filterVar]] %in% filterValue)
    } 
  } else {
    warning("No filterVar or filterValue has been provided. Thus, simply extracting the nearest neighbor.")
    points_of_interest <- point_sf
  }
  
  
  
  # Calculate distances from all points in FCdata.sf to the points_of_interest
  distances <- sf::st_distance(point_sf, points_of_interest)
  
  rownames(distances) <- point_sf[[tmpID_var]]
  colnames(distances) <- points_of_interest[[tmpID_var]]
  
  # # Set the distance from a point to itself as Inf (if IDs match)
  # # This is efficient if the order of points in FCdata.sf and points_of_interest is the same and they have the same rows
  # if (all(rownames(point_sf) == rownames(points_of_interest))) {
  #   diag(distances) <- Inf
  # }
  
  # Create a matrix to determine which distances to set as Inf (self-comparisons)
  self_comparison <- outer(point_sf[[tmpID_var]], points_of_interest[[tmpID_var]], FUN = "==")
  
  # Set distances from a point to itself as Inf
  distances[self_comparison] <- Inf
  
    # get the count of 0's
  matching.count <- apply(distances, 1, function(row) {
    row.0s <- 0 %in% as.numeric(row)
  }) |> unlist() |> sum()
  
  # give warning if there are shared coordinates, correct if requested.
  if (y_dedup & matching.count > 0) {
    distances[as.numeric(distances) == 0] <- Inf
    warning(paste0("There were ", matching.count, " unique observations in point_sf that shared one or more coordinates observations in point_sf. Shared coordinates have been excluded."))
  } else if (0 %in% as.numeric(distances)) {
    warning(paste0("There were ", matching.count, " unique observations in point_sf that shared one or more coordinates observations in point_sf. Shared coordinates are included and may be unreliable."))
  }
  
  
  # Compute the minimum distance for each point to any point in points_of_interest (excluding itself)
  min_distances_index <- apply(distances, 1, function(row) {
    
    # row <- distances[1,]
    
    # minimum index
    min_index <- which.min(row) |> names()
    
    return(min_index)
  }) |> unlist()
  
  # filter down to the 
  notNA_subset <- dplyr::filter(st_drop_geometry(point_sf)[,c(tmpID_var, extractColumn)], .data[[tmpID_var]] %in% names(min_distances_index))
  
  # extract the column values using r * c names
  notNA_subset[[tmpOutput_var]] <- sf::st_drop_geometry(point_sf)[min_distances_index, extractColumn]
  
  # merge back to the original data frame. This is to ensure that things are sorted appropriately and there are missing when there should be.
  output.df <- merge(sf::st_drop_geometry(point_sf)[, c(tmpSort_var, tmpID_var)], notNA_subset, by = tmpID_var, all.x = T, all.y = F)
  output.df <- output.df %>% dplyr::arrange(.data[[tmpSort_var]])
  
  
  rm(point_sf, notNA_subset, self_comparison, distances, tmpID_var)
  
  output.vec <- output.df[[tmpOutput_var]]
  
  return(output.vec)
}







