#' Nearest Neighbor Filtered Distance
#'
#' This function calculates the minimum distance from each point in an sf object to the nearest point that meets a specified criterion. 
#' It ensures that a point does not calculate the distance to itself by using a temporary unique identifier.
#'
#' @param point_sf An sf object representing the point layer.
#' @param filterVar The name of the column in `point_sf` used for filtering.
#' @param filterValue The value in `filterVar` to filter by; points meeting this criterion are considered in distance calculations.
#' @param uniqueID Optional; the name of the column in `point_sf` that uniquely identifies each point. 
#'        If NULL, a temporary identifier will be automatically created.
#'
#' @return A numeric vector containing the minimum distance from each point to the nearest point that meets the criterion, 
#'         excluding the distance from a point to itself.
#'
#' @export
#'
#' @examples
#' # Load required libraries
#' library(sf)
#' library(dplyr)
#'
#' # Assume point_sf is an sf object loaded with an 'id' column and a 'svc_art' column
#' # Example data creation for demonstration (not executable as is)
#' point_sf <- st_as_sf(data.frame(id = 1:10, x = rnorm(10), y = rnorm(10), svc_art = sample(0:1, 10, replace = TRUE)),
#'                     coords = c("x", "y"), crs = 4326)
#' uniqueID <- "id"
#' filterVar <- "svc_art"
#' filterValue <- 1
#'
#' # Calculate minimum distances excluding self-distance
#' distances <- nn_filterDist(point_sf, filterVar, filterValue, uniqueID)
#' print(distances)
nn_filterDist <- function(point_sf, filterVar, filterValue) {
  
  # load(Sys.getenv("TESTING_SPA_DATA"))
  # point_sf <- spa.list$MW_SPA13$FC
  # filterVar <- "svc_antenatalCare"
  # filterVar <- "svc_caesarean"
  # filterValue <- 1
  # uniqueID <- "facID"
  
  tmpID_var <- "tmpID_3dc99750ca4247f1ad048e86dec6e57c"
  
  # # Assuming FCdata.sf is already loaded and has a unique identifier column named 'id'
  rownames(point_sf) <- 1:nrow(point_sf)
  point_sf[[tmpID_var]] <- 1:nrow(point_sf)
  
  # Filter to get only the points where svc_art == 1
  points_of_interest <- dplyr::filter(point_sf, .data[[filterVar]] %in% filterValue)
  
  # Calculate distances from all points in FCdata.sf to the points_of_interest
  distances <- st_distance(point_sf, points_of_interest)
  
  # # Set the distance from a point to itself as Inf (if IDs match)
  # # This is efficient if the order of points in FCdata.sf and points_of_interest is the same and they have the same rows
  # if (all(rownames(point_sf) == rownames(points_of_interest))) {
  #   diag(distances) <- Inf
  # }
  
  # Create a matrix to determine which distances to set as Inf (self-comparisons)
  self_comparison <- outer(point_sf[[tmpID_var]], points_of_interest[[tmpID_var]], FUN = "==")
  
  # Set distances from a point to itself as Inf
  distances[self_comparison] <- Inf
  
  # Compute the minimum distance for each point to any point in points_of_interest (excluding itself)
  min_distances <- apply(distances, 1, min)
  
  # 
  return(min_distances)
  
}







