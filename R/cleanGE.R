#' Clean and Enrich Geospatial Data with Facility Proximity Metrics
#'
#' Processes geospatial facility data by assigning unique facility IDs, computing pairwise distances between facilities, and counting the number of nearby facilities within specified distances.
#'
#' @param GEdata A spatial data frame (e.g., an `sf` object) containing facility data with at least a `SPAFACID` column.
#'
#' @return A modified spatial data frame with additional columns:
#' \describe{
#'   \item{\code{facID}}{Facility ID copied from \code{SPAFACID}.}
#'   \item{\code{HFwithin_5km}}{Number of facilities within 5 kilometers.}
#'   \item{\code{HFwithin_10km}}{Number of facilities within 10 kilometers.}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Copies the \code{SPAFACID} column to create a \code{facID} column.
#'   \item Calculates pairwise distances between all facilities using \code{\link[sf]{st_distance}}.
#'   \item Counts the number of other facilities within 5 km and 10 km for each facility.
#' }
#'
#' @importFrom sf st_distance
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Load your spatial data
#' facilities_sf <- st_read("path_to_your_data.shp")
#' # Ensure that 'SPAFACID' column exists
#' cleaned_data <- cleanGE(facilities_sf)
#' }
#'
#' @export


# GEdata <- htSPA.list$GE

# GEdata <- spa.list$NP_SPA15$GE


cleanGE <- function(GEdata) {
  
  GEdata$facID <- GEdata$SPAFACID
  
  # Create a matrix of distances between all facilities
  dist_matrix <- st_distance(GEdata)
  
  # For each facility, count the number of other facilities within 5 kilometers
  GEdata$HFwithin_5km <- apply(dist_matrix, 1, function(x) sum(x < 5000, na.rm = T))
  # For each facility, count the number of other facilities within 10 kilometers
  GEdata$HFwithin_10km <- apply(dist_matrix, 1, function(x) sum(x < 10000, na.rm = T))
  
  return(GEdata)
  
}























