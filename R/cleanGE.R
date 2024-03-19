
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























