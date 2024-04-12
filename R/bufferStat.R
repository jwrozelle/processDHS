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
  get_mode <- function(x, na.rm) {
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
    rast_values <- terra::extract(raster_obj, buffers, fun = sum, na.rm = na.rm, exact = T, ID = F)[,1]
  } else if (type == "median") {
    rast_values <- terra::extract(raster_obj, buffers, fun = median, na.rm = na.rm, ID = F)[,1]
  } else if (type == "sum") {
    rast_values <- terra::extract(raster_obj, buffers, fun = sum, na.rm = na.rm, exact = T, ID = F)[,1]
  } else if (type == "max") {
    rast_values <- terra::extract(raster_obj, buffers, fun = max, na.rm = na.rm, exact = T, ID = F)[,1]
  } else if (type == "min") {
    rast_values <- terra::extract(raster_obj, buffers, fun = min, na.rm = na.rm, exact = T, ID = F)[,1]
  } else if (type == "count") {
    binaryRaster <- raster_obj
    if (na.rm == TRUE) {
      values(binaryRaster)[!is.na(values(binaryRaster))] <- 1
    } else if (na.rm == FALSE) {
      values(binaryRaster)[1:length(values(binaryRaster))] <- 1
    }
    rast_values <- raster::extract(binaryRaster, buffers, fun = sum, na.rm = na.rm, exact = T)[,2]
  } else if (type == "mode") {
    points_sf$tempID_367d611194ae422f9dbedff788200911 <- 1:nrow(points_sf)
    
    raster_values.df <- terra::extract(raster_obj, buffers, raw = T) |> as.data.frame()
    names(raster_values.df) <- c("ID", "raster_values")
    
    raster_values_mode.df <- raster_values.df %>% 
      dplyr::group_by(ID) %>% 
      summarise(mode = get_mode(raster_values, na.rm = na.rm))
    rast_values <- raster_values.df$raster_values
  }
  
  return(rast_values)
}
