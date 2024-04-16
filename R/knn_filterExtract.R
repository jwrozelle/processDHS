
knn_filterExtract <- function(point_sf, y = NULL, filterVar=NULL, filterValue=NULL, extractColumn, knn = 1, type = "mean", y_dedup = T, na.rm = T) {
  
  require(dplyr)
  
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
  # knn = 1
  
  tmpID_x_var <- "tmpID_x_3dc99750ca4247f1ad048e86dec6e57c"
  tmpID_y_var <- "tmpID_y_3dc99750ca4247f1ad048e86dec6e57c"
  tmpOutput_var <- "tmpOutput_385ea504c5eb48ac8f7ff58d5621c30c"
  tmpSort_var <- "tmpSort_3dc99750ca4247f1ad048e86dec6e57c"
  
  point_sf[[tmpSort_var]] <- 1:nrow(point_sf)
  
  # Set error messages
  ## Check if 'type' is set to a possible value
  if (!type %in% c("mean", "median", "min", "max", "sum")) {
    stop("You must set type to a valid value. Possible types are 'mean', 'median', 'min', 'max', and 'sum'.")
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
  
  # get the count of 0's
  matching.count <- apply(distances, 1, function(row) {
    row.0s <- 0 %in% as.numeric(row)
  }) |> unlist() |> sum()
  
  # give warning if there are shared coordinates, correct if requested.
  if (y_dedup & matching.count > 0) {
    distances[as.numeric(distances) == 0] <- Inf
    warning(paste0("There were ", matching.count, " unique observations in point_sf that shared one or more coordinates observations in ", poi_error_name,". Shared coordinates have been excluded."))
  } else if (0 %in% as.numeric(distances)) {
    warning(paste0("There were ", matching.count, " unique observations in point_sf that shared one or more coordinates observations in ", poi_error_name,". Shared coordinates are included and may be unreliable."))
  }

  
  # Compute the minimum distance for each point to any point in points_of_interest (excluding itself)
  minDistanceEmpty.list <- list()
  minDistanceEmpty.list[1:nrow(distances)] <- 1:nrow(distances)
  names(minDistanceEmpty.list) <- rownames(distances)
  
  min_distances_index <- lapply(minDistanceEmpty.list, function(rowNum) {
    
    row <- distances[rowNum,]
    
    # minimum index
    min_index <- processDHS::which_kmin(row, knn)
    
    return(min_index)
  })
  
  # do the calculation
  outputValues.vec <- sapply(min_distances_index, function(obs) {
    # obs <- min_distances_index[[1]]
    
    # extract the specific values
    extractValues.vec <- sf::st_drop_geometry(points_of_interest)[names(obs), extractColumn]
    
    if (type == "mean") {
      output.value <- mean(extractValues.vec, na.rm = na.rm)
    } else if (type == "median") {
      output.value <- median(extractValues.vec, na.rm = na.rm)
    } else if (type == "min") {
      output.value <- min(extractValues.vec, na.rm = na.rm)
    } else if (type == "max") {
      output.value <- max(extractValues.vec, na.rm = na.rm)
    } else if (type == "sum") {
      output.value <- sum(extractValues.vec, na.rm = na.rm)
    }
    
    return(output.value)
  })
  
  outputValues.df <- data.frame(names(outputValues.vec), outputValues.vec)
  names(outputValues.df) <- c(tmpID_x_var, tmpOutput_var)

  # merge back to the original data frame. This is to ensure that things are sorted appropriately and there are missing when there should be.
  # This is probably uneccessary, could just return outputValues.vec - but my paranoia
  output.df <- merge(sf::st_drop_geometry(point_sf)[, c(tmpSort_var, tmpID_x_var)], outputValues.df, by = tmpID_x_var, all.x = T, all.y = F)
  output.df <- output.df %>% arrange(.data[[tmpSort_var]])


  output.vec <- output.df[[tmpOutput_var]]
  
  return(output.vec)
}







