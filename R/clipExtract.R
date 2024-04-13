#' Clip and Extract Raster Values at Points
#'
#' This function transforms the coordinates of a point layer to match the CRS of a raster layer,
#' creates an extent around the transformed points, clips the raster to this extent,
#' and then extracts the raster values at the original point locations.
#'
#' @param rast A \code{\link[terra]{SpatRaster}} object representing the raster layer.
#' @param points_sf An \code{\link[sf]{sf}} object representing the point layer.
#'
#' @return A numeric vector containing the extracted raster values at the locations
#'         of the points.
#'
#' @importFrom sf st_transform
#' @importFrom terra crs crop ext extract project
#' @export
#'
#' @examples
#' # Load libraries
#' library(terra)
#' library(sf)
#'
#' # Load example raster and points
#' rast <- rast(system.file("ex/elev.tif", package = "terra"))
#' points_sf <- st_as_sf(data.frame(x = c(-90, -90.5), y = c(49, 49.5)), coords = c("x", "y"), crs = 4326)
#'
#' # Perform clipping and extraction
#' extracted_values <- clipExtract(rast, points_sf)
#' print(extracted_values)

clipExtract <- function(rast, points_sf) {
  
  points_transformed <- sf::st_transform(points_sf, terra::crs(rast))
  # create extent
  point_extent <- ext(
    min(st_coordinates(points_transformed)[,1]) - (max(st_coordinates(points_transformed)[,1]) - min(st_coordinates(points_transformed)[,1]))*.2,
    max(st_coordinates(points_transformed)[,1]) + (max(st_coordinates(points_transformed)[,1]) - min(st_coordinates(points_transformed)[,1]))*.2,
    min(st_coordinates(points_transformed)[,2]) - (max(st_coordinates(points_transformed)[,2]) - min(st_coordinates(points_transformed)[,2]))*.2,
    max(st_coordinates(points_transformed)[,2]) + (max(st_coordinates(points_transformed)[,2]) - min(st_coordinates(points_transformed)[,2]))*.2,
  )
  
  # clip the raster by the extent
  crop.rast <- terra::crop(rast, point_extent)
  
  extractedVals <- terra::extract(terra::project(crop.rast, terra::crs(points_sf)), points_sf, ID = F)[,1]
  
  return(extractedVals)
  
}