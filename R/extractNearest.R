#' Extract Nearest Non-Missing Raster Value
#'
#' For each point in an `sf` object, this function extracts the value of the nearest non-missing raster cell, 
#' within a specified maximum radius. It handles points falling on raster cells with missing values or outside the raster extent.
#'
#' @param sf_object An `sf` object containing point geometries.
#' @param raster_object A `RasterLayer` object containing the raster data.
#' @param maxRadius Numeric. Maximum search radius for non-missing raster values, in the same units as the raster.
#'
#' @return A numeric vector. Each element corresponds to a point in the `sf` object and contains 
#'         the value of the nearest non-missing raster cell within `maxRadius`. Returns `NA` for points 
#'         with no non-missing raster cells found within `maxRadius`.
#'
#' @examples
#' # Example usage:
#' # sf_obj <- st_as_sf(data.frame(x = c(1, 2), y = c(3, 4)), coords = c("x", "y"))
#' # rast_obj <- raster(matrix(runif(100, 0, 1), 10, 10))
#' # result <- extractNearest(sf_obj, rast_obj, maxRadius = 1000)
#'
#' @export
#' @importFrom raster rasterToPoints
#' @importFrom sf st_coordinates
#' @importFrom sp spDistsN1


extractNearest <- function(sf_object, raster_object, maxRadius) {
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)
  
  sf_coords <- st_coordinates(sf_object)
  surrounding_cells <- rasterToPoints(raster_object, fun = function(x) !is.na(x))
  
  extract_value <- function(point) {
    if (nrow(surrounding_cells) > 0) {
      distances <- sp::spDistsN1(surrounding_cells[, 1:2], point, longlat = FALSE)
      valid_cells <- which(distances <= maxRadius)
      
      if (length(valid_cells) > 0) {
        nearest_cell_index <- valid_cells[which.min(distances[valid_cells])]
        return(surrounding_cells[nearest_cell_index, 3])
      }
    }
    return(NA)
  }
  
  sapply(1:nrow(sf_coords), function(i) extract_value(sf_coords[i, ]))
}

