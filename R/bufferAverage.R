#' Calculate Average Raster Values Within Buffer Around Points
#'
#' This function calculates the average value of raster cells within a specified
#' radius (buffer) around each point in an `sf` object. The function creates buffers
#' around each point and then extracts the raster values within these buffers to compute
#' the average.
#'
#' @param points_sf An `sf` object containing points.
#' @param raster_obj A raster object.
#' @param radius The radius within which to calculate the average raster value, in meters.
#'
#' @return A numeric vector containing the average raster value for each point.
#'
#' @examples
#' # Assuming points_sf is a predefined sf object and raster is a predefined raster
#' result <- bufferAverage(points_sf, raster, 1000) # Radius of 1000 meters
#'
#' @export
bufferAverage <- function(points_sf, raster_obj, radius) {
  # # Convert sf object to SpatialPoints
  # points_sp <- as(points_sf, "SpatialPoints")
  
  # load(Sys.getenv())
  # raster_obj <- malawi_2015_hfp.raster
  # radius <- 2000
  
  # # # set the crs of the raster to the crs of the sf object
  # raster_obj <- terra::project(raster_obj, crs = st_crs(points_sf))
  
  # Create buffers around each point
  buffers <- st_buffer(points_sf, dist = radius)
  
  # Extract raster values within each buffer and calculate the average
  avg_values <- raster::extract(raster_obj, buffers, fun = mean, na.rm = TRUE)[,2]
  
  return(avg_values)
}
