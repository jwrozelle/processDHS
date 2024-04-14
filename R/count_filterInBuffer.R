#' Count Points Within a Specified Radius Meeting a Filter Criterion
#'
#' This function calculates the number of points within a specified radius of each point in an sf object that meet a given criterion. 
#' It also ensures that each point does not count itself by setting the distance from a point to itself as infinity.
#'
#' @param point_sf An `sf` object representing the point layer.
#' @param filterVar The name of the column in `point_sf` used for filtering.
#' @param filterValue The value in `filterVar` to filter by; only points meeting this criterion are counted within the specified radius.
#' @param radius The radius within which to count points, specified in the same units as the spatial data in `point_sf`.
#'
#' @return An integer vector where each element is the count of points meeting the criterion within the specified radius 
#'         from each corresponding point in `point_sf`.
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
#' filterVar <- "svc_art"
#' filterValue <- 1
#' radius <- 1000  # Assuming radius is in meters if CRS is projected
#'
#' # Calculate counts within the specified radius
#' counts <- count_filterInBuffer(point_sf, filterVar, filterValue, radius)
#' print(counts)

count_filterInBuffer <- function(point_sf, filterVar, filterValue, radius) {
  
  # load(Sys.getenv("TESTING_SPA_DATA"))
  # point_sf <- spa.list$MW_SPA13$FC
  # filterVar <- "svc_antenatalCare"
  # filterVar <- "svc_caesarean"
  # filterValue <- 1
  # uniqueID <- "facID"
  # radius <- 5e4
  
  if (!filterVar %in% names(point_sf)) {
    warning(paste0("The point_sf object has no column '", filterVar, "'. Returning a vector of NA."))
    countInBuffer <- rep(NA, nrow(point_sf))
    return(countInBuffer)
  }
  
  if (sum(filterValue %in% point_sf[[filterVar]]) == 0) {
    warning(paste0("There is no value ", filterValue, " in ", filterVar, ". Returning a vector of NA."))
    countInBuffer <- rep(NA, nrow(point_sf))
    return(countInBuffer)
  } else {
    tmpID_var <- "tmpID_3dc99750ca4247f1ad048e86dec6e57c"
    
    # # Assuming FCdata.sf is already loaded and has a unique identifier column named 'id'
    rownames(point_sf) <- paste0("obs_", 1:nrow(point_sf))
    point_sf[[tmpID_var]] <- paste0("obs_", 1:nrow(point_sf))
    
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
    
    countInBuffer <- apply(distances, 1, function(obs) {
      
      # obs <- distances[10,]
      
      sum_inRadius <- sum(as.numeric(obs) <= radius, na.rm = T)
      
    })
    
    # clean up
    rm(distances, self_comparison, points_of_interest)
    
    return(countInBuffer)
  }
  
  
  
}