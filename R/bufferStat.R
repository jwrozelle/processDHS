#' Calculate Statistics within a Specified Radius Around Points
#'
#' This function takes a set of points in an sf object and a raster object, creates buffers
#' around each point with a specified radius, and then computes various statistical summaries
#' for the raster values within each buffer.
#'
#' @param points_sf An sf object containing points around which buffers will be created.
#' @param raster_obj A RasterLayer object from which values will be extracted.
#' @param radius Numeric value specifying the radius of the buffer around each point, in the units of the coordinate reference system of `points_sf`.
#' @param type Character string specifying the type of statistic to compute within each buffer. Supported types are "mean", "median", "sum", "max", "min", "count", and "mode".
#' @param na.rm Logical indicating whether to remove missing values from the raster data before computing statistics. Defaults to TRUE.
#'
#' @return Depending on the `type` parameter, the function returns a vector with the calculated statistic for each buffer. If the `type` is "count" or "mode", the function adjusts to handle raster values accordingly.
#'
#' @examples
#' # Load necessary libraries
#' library(sf)
#' library(raster)
#'
#' # Example data (Assuming the data is loaded)
#' # points_sf <- st_as_sf(data.frame(x = c(1, 2), y = c(2, 3), crs = 4326), coords = c("x", "y"))
#' # raster_obj <- raster(matrix(1:100, 10, 10))
#' # radius <- 500  # Example radius
#'
#' # Calculate mean values within buffers
#' mean_values <- bufferStat(points_sf, raster_obj, radius, type = "mean")
#'
#' @note The function assumes the raster and point spatial data are adequately projected and aligned. Users must ensure that the spatial data used as input follow the same coordinate reference system or are appropriately transformed to align.
#' @export

bufferStat <- function(points_sf, raster_obj, radius, type = "mean", na.rm = T) {
  
  require(raster)
  require(sf)
  require(terra)
  
  # define the get mode function
  get_mode <- function(x, na.rm = na.rm) {
    if (na.rm == TRUE) {
      ux <- unique(na.omit(x))
      mode <- ux[which.max(tabulate(match(x, ux)))]
    } else if (na.rm == F) {
      ux <- unique(x)
      mode <- ux[which.max(tabulate(match(x, ux)))]
    }
    return(mode)
  }
  
  # Create buffers around each point
  buffers <- sf::st_buffer(points_sf, dist = radius)
  
  # Extract raster values within each buffer and calculate the average
  
  if (type == "mean") {
    rast_values <- raster::extract(raster_obj, buffers, fun = mean, na.rm = na.rm)[,2]
  } else if (type == "median") {
    rast_values <- raster::extract(raster_obj, buffers, fun = median, na.rm = na.rm)[,2]
  } else if (type == "sum") {
    rast_values <- raster::extract(raster_obj, buffers, fun = sum, na.rm = na.rm)[,2]
  } else if (type == "max") {
    rast_values <- raster::extract(raster_obj, buffers, fun = max, na.rm = na.rm)[,2]
  } else if (type == "min") {
    rast_values <- raster::extract(raster_obj, buffers, fun = min, na.rm = na.rm)[,2]
  } else if (type == "count") {
    # Add a constant field to `buffers` for rasterization
    buffers$constant_field <- 1
    # Rasterize `buffers` using this new field
    buffers_raster <- raster::rasterize(buffers, raster_obj, field = "constant_field", fun = max, background = 0)
    # Mask the raster with the buffer
    masked_raster <- raster::mask(raster_obj, buffers_raster)
    if (na.rm == TRUE) {
      rast_values <- sum(!is.na(raster::values(masked_raster)))
    } else if (na.rm == FALSE) {
      rast_values <- length(raster::values(masked_raster))
    }
  } else if (type == "mode") {
    # Add a constant field to `buffers` for rasterization
    buffers$constant_field <- 1
    # Rasterize `buffers` using this new field
    buffers_raster <- raster::rasterize(buffers, raster_obj, field = "constant_field", fun = max, background = 0)
    # Mask the raster with the buffer
    masked_raster <- raster::mask(raster_obj, buffers_raster)
    # extract the values
    raster_values <- raster::values(masked_raster)
    # get the mode
    rast_values <- get_mode(raster_values, na.rm = na.rm)
  }
  
  
  
  
  return(rast_values)
}
