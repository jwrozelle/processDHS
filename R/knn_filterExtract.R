
knn_filterExtract <- function(point_sf, y = NULL, filterVar=NULL, filterValue=NULL, extractColumn, knn = 1, type = "mean", y_dedup = F) {
  
  # load(Sys.getenv("TESTING_SPA_DATA"))
  # point_sf <- spa.list$MW_SPA13$FC
  # filterVar <- "svc_antenatalCare"
  # filterVar <- "svc_caesarean"
  # filterValue <- 1
  # uniqueID <- "facID"
  # extractColumn <- "sri_score"
  # y = point_sf
  # y_dedup = T
  # type = "mean"
  
  tmpID_x_var <- "tmpID_x_3dc99750ca4247f1ad048e86dec6e57c"
  tmpID_y_var <- "tmpID_y_3dc99750ca4247f1ad048e86dec6e57c"
  tmpOutput_var <- "tmpOutput_385ea504c5eb48ac8f7ff58d5621c30c"
  
  # Set error messages
  ## Check if 'type' is set to a possible value
  if (!type %in% c("mean", "median", "min", "max", "sum", "mode")) {
    stop("You must set type to a valid value. Possible types are 'mean', 'median', 'min', 'max', 'sum', and 'mode'.")
  }
  ## make sure that it's numeric
  if (type %in% c("mean", "median", "min", "max", "sum") & !is.numeric(point_sf[[extractColumn]])) {
    stop(paste0("Only numeric vectors are allowed for type = '", type, "'."))
  }
  ## if y is specified, make sure it's in the right format
  if (!is.null(y) & !"sf" %in% class(y)) {
    stop("Object `y` must be of class sf.")
  }
  
  # set points of interest, either the original points_sf object, or y (if specified).
  # also set the name to refer to in errors.
  if (!is.null(y)) {
    points_of_interest <- y
    poi_error_name <- "y"
  } else {
    points_of_interest <- point_sf
    poi_error_name <- "point_sf"
  }
  
  
  if (!is.null(filterVar) | !is.null(filterValue)) {
    
    if (!filterVar %in% names(points_of_interest)) {
      warning(paste0("The ", poi_error_name, " object has no column '", filterVar, "'. Returning a vector of NA."))
      min_distances <- rep(NA, nrow(points_of_interest))
      return(min_distances)
    }
    
    if (sum(filterValue %in% points_of_interest[[filterVar]]) == 0) {
      warning(paste0("There is no value ", filterValue, " in ", filterVar, ". Returning a vector of NA."))
      min_distances <- rep(NA, nrow(points_of_interest))
      return(min_distances)
    } else {
      # Filter to get only the points where svc_art == 1
      points_of_interest <- dplyr::filter(points_of_interest, .data[[filterVar]] %in% filterValue)
    } 
  } else {
    warning("No filterVar or filterValue has been provided. Thus, simply extracting the nearest neighbor.")
  }
  
  # set point_sf and points_of_interest id names
  ## point_sf
  rownames(point_sf) <- paste0("obs_x_", 1:nrow(point_sf))
  point_sf[[tmpID_x_var]] <- paste0("obs_x_", 1:nrow(point_sf))
  ## points_of_interest
  rownames(points_of_interest) <- paste0("obs_y_", 1:nrow(points_of_interest))
  points_of_interest[[tmpID_y_var]] <- paste0("obs_y_", 1:nrow(points_of_interest))
  
  
  # Calculate distances from all points in FCdata.sf to the points_of_interest
  distances <- sf::st_distance(point_sf, points_of_interest)
  
  rownames(distances) <- point_sf[[tmpID_x_var]]
  colnames(distances) <- points_of_interest[[tmpID_y_var]]
  
  # # Set the distance from a point to itself as Inf (if IDs match)
  # # This is efficient if the order of points in FCdata.sf and points_of_interest is the same and they have the same rows
  # if (all(rownames(point_sf) == rownames(points_of_interest))) {
  #   diag(distances) <- Inf
  # }
  
  # Create a matrix to determine which distances to set as Inf (self-comparisons)
  if (is.null(y)) { # in cases where point_sf is it's own neighbor
    self_comparison <- outer(
      substr(point_sf[[tmpID_x_var]], 6, nchar(point_sf[[tmpID_x_var]])), # need to remove leading characters
      substr(points_of_interest[[tmpID_y_var]], 6, nchar(points_of_interest[[tmpID_y_var]])), 
      FUN = "==")
    # Set distances from a point to itself as Inf
    distances[self_comparison] <- Inf
  }
  
  if (y_dedup & length(distances[as.numeric(sb) == 0]) > 0) {
    distances[as.numeric(sb) == 0] <- Inf
    warning(paste0("There were ", length(distances[as.numeric(sb) == 0]), " pairs that shared the same coordinates. These have been excluded."))
  } else {
    warning(paste0("There were ", length(distances[as.numeric(sb) == 0]), " pairs that shared the same coordinates. These are included."))
  }

  
  # Compute the minimum distance for each point to any point in points_of_interest (excluding itself)
  min_distances_index <- apply(distances, 1, function(row) {
    
    # row <- distances[1,]
    
    # minimum index
    min_index <- processDHS::which_kmin(row) |> names()
    
    return(min_index)
  }) |> unlist()
  
  # filter down to the 
  notNA_subset <- dplyr::filter(st_drop_geometry(point_sf)[,c(tmpID_var, extractColumn)], .data[[tmpID_var]] %in% names(min_distances_index))
  
  # extract the column values using r * c names
  notNA_subset[[tmpOutput_var]] <- sf::st_drop_geometry(point_sf)[min_distances_index, extractColumn]
  
  # merge back to the original data frame. This is to ensure that things are sorted appropriately and there are missing when there should be.
  output.df <- merge(sf::st_drop_geometry(point_sf)[, c(names(point_sf)[1], tmpID_var)], notNA_subset, by = tmpID_var, all.x = T, all.y = F)
  
  rm(point_sf, notNA_subset, self_comparison, distances, tmpID_var)
  
  output.vec <- ifelse((output.df[[tmOutput_var]]))
  
  return(output.df[[tmpOutput_var]])
}







