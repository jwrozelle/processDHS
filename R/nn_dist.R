#' Calculate Nearest Neighbor Distance for Each Observation in an sf Object
#'
#' This function calculates the Euclidean distance from each observation to its nearest neighbor 
#' within the same sf object. It returns a vector of distances, with each element representing the 
#' nearest neighbor distance for the corresponding observation in the sf object.
#'
#' @param GEdata An `sf` object containing the geographic data with coordinates.
#'               The sf object should be properly projected for distance calculation,
#'               ideally in a coordinate system that uses meters (e.g., UTM).
#'
#' @return A numeric vector with the nearest neighbor distances for each observation
#'         in the `GEdata` sf object. The distances are in the same unit as the sf object's
#'         coordinate reference system.
#'
#' @examples
#' # Assuming you have an sf object named GEdata with proper CRS
#' library(sf)
#' # Example data creation (you would typically load this from your data source)
#' GEdata <- st_as_sf(data.frame(id = 1:3,
#'                               x = c(10, 20, 30),
#'                               y = c(50, 60, 70),
#'                               crs = 4326),
#'                    coords = c("x", "y"), crs = 4326)
#' # Convert CRS to a more appropriate one for distance measurement
#' GEdata <- st_transform(GEdata, 32633) # Transforming to a UTM zone
#'
#' # Calculate nearest neighbor distances
#' distances <- nn_dist(GEdata)
#' print(distances)
#'
#' @importFrom sf st_distance
#' @export

nn_dist <- function(GEdata) {
  
  # load(Sys.getenv("TESTING_SPA_DATA"))
  
  
  # Calculate the distance matrix
  distance_matrix <- st_distance(GEdata)
  
  # Set diagonal to NA to ignore zero distances (point to itself)
  diag(distance_matrix) <- NA
  
  # Find the nearest neighbor distance for each point
  nearest_neighbor_distance <- apply(distance_matrix, 1, min, na.rm = TRUE)
  
  # Add this data back to the sf object if desired
  return(nearest_neighbor_distance)
}






