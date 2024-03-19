#' Convert Neighborhood Objects to LINESTRING Spatial Features
#'
#' This function takes a neighborhood object, an sf (simple feature) object,
#' and optionally a listw (spatial weights) object from the `spdep` package
#' to create LINESTRING geometries representing the connections between neighbors.
#' Weights from the listw object are incorporated as an attribute if provided.
#'
#' @param nb_object A neighbor object (e.g., from `poly2nb` function in the `spdep` package).
#' @param sf_object An sf object containing the spatial features corresponding to the neighbors.
#' @param listw_object Optionally, a listw object containing the spatial weights for each pair
#'   of neighbors. If not provided, weights are set to NA.
#'
#' @return An sf object containing LINESTRING geometries for each neighbor pair,
#'   with an optional weight attribute if a listw object is provided.
#' @examples
#' \dontrun{
#'   # Assuming 'nb', 'sf_data', and 'listw' are already defined:
#'   lines_sf <- nb2linestring(nb, sf_data, listw)
#'   plot(lines_sf)
#' }
#' @export
#'
#' @importFrom sf st_coordinates st_linestring st_sfc st_sf st_crs
#' @importFrom spdep card
nb2linestring <- function(nb_object, sf_object, listw_object = NULL) {
  
  # Extract coordinates from the sf object
  coords <- st_coordinates(sf_object)
  
  # Initialize coordinates for start and end points of lines
  x <- coords[, 1]
  y <- coords[, 2]
  n <- length(nb_object)
  cardnb <- card(nb_object) # Number of neighbors for each feature
  i <- rep(1:n, cardnb) # Replicate indices for start points
  j <- unlist(nb_object) # Flatten list to get end point indices
  nb_df <- data.frame(x=x[i], xend=x[j], y=y[i], yend=y[j])
  
  # Add weights to the dataframe if a listw object is provided
  if (!is.null(listw_object)) {
    weights <- unlist(listw_object$weights, use.names = FALSE)
  } else {
    weights <- rep(NA, nrow(nb_df)) # Use NA for weights if listw is not provided
  }
  
  # Create LINESTRING geometries from the point pairs
  lines_list <- vector("list", nrow(nb_df))
  for(i in 1:nrow(nb_df)) {
    # Define coordinates for each LINESTRING
    coordinates <- matrix(
      as.numeric(nb_df[i, c("x", "y", "xend", "yend")]),
      ncol = 2, byrow = TRUE)
    # Create LINESTRING from coordinates
    lines_list[[i]] <- st_linestring(coordinates)
  }
  
  # Combine LINESTRINGs into an sf object with weights as an attribute
  nb_df_lines <- st_sf(weight = weights, geom = st_sfc(lines_list, crs = st_crs(sf_object)))
  
  return(nb_df_lines)
}
