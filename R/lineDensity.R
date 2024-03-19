#' Calculate Road Network Density Around Points
#'
#' This function calculates the density of the road network within a specified
#' radius around each point in a given set of points. The density is calculated
#' as the ratio of the total length of the road network within the buffer zone
#' to the area of the buffer zone. Both the road network and points are provided 
#' as `sf` objects, and they should be in the same coordinate system.
#'
#' @param points_sf An `sf` object containing points.
#' @param roads_sf An `sf` object representing the road network.
#' @param radius The radius within which to calculate road density, in meters.
#'
#' @return A vector of the same length as points_sf with road density.
#'
#' @examples
#' # Assuming points_sf and roads_sf are predefined sf objects
#' result <- lineDensity(points_sf, roads_sf, 1000)
#'
#' @export
#'
#' @importFrom sf st_transform st_buffer st_intersection st_length st_area
#' @importFrom units set_units

lineDensity <- function(points_sf, roads_sf, radius, uniqueID = "SPAID") {
  #points_sf <- htSPA.list[["GE"]]
  #roads_sf <- HTI_road_data.sf
  #radius <- 2000
  
  
  # add uniqueID column
  points_sf$uniqueID_ <- points_sf[[uniqueID]]
  
  # Ensure that both sf objects are in the same coordinate system
  roads_sf <- st_transform(roads_sf, st_crs(points_sf))
  
  # Calculate buffer around each point
  buffers <- st_buffer(points_sf, units::set_units(radius, "meters"))
  
  # Calculate area of each buffer
  buffer_area <- median(st_area(buffers)) |> as.numeric()
  
  # Intersect the road network with the buffers and calculate length
  intersected_roads <- st_intersection(roads_sf, buffers)
  intersected_roads$road_length <- st_length(intersected_roads)
  
  buffers_roadlengths <- st_drop_geometry(intersected_roads) %>% 
    dplyr::group_by(uniqueID_) %>%
    dplyr::summarise(roadLengthSum = sum(road_length, na.rm = T))
  
  # merge this back to buffers
  buffers <- merge(buffers, buffers_roadlengths, by = "uniqueID_", all.x = T, all.y = F)
  
  # set missing to 0
  buffers$roadLengthSum <- ifelse(is.na(buffers$roadLengthSum), 0, buffers$roadLengthSum)
  
  # Calculate road density (length/area) for each buffer
  road_density <- buffers$roadLengthSum / buffer_area
  
  return(road_density)
}