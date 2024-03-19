

# dhs_sf <- GEdata.list$HT7
# spa_sf <- spa.list$HT_SPA17$FC
# metric <- c("sri_score", "sri_basicamenities", "sri_basicequip", "sri_diagcapacity", "sri_infprev", "sri_med")



hseBuffer <- function(spa_sf, dhs_sf, bufferSize_R = 1e4, bufferSize_U = 4e3, metric) {
  
  # get urban and rural subsets for different buffers
  ## Urban subset
  dhs_u.sf <- subset(dhs_sf, URBAN_RURA == "U")
  ## Rural subset
  dhs_r.sf <- subset(dhs_sf, URBAN_RURA == "R")
  
  # create a buffer around the DHS cluster
  buff_u.sf <- st_buffer(dhs_u.sf, bufferSize_U)
  buff_r.sf <- st_buffer(dhs_r.sf, bufferSize_R)
  
  # join the two back together
  buff_sf <- rbind(buff_u.sf, buff_r.sf)
  
  # drop unnecessary stuff
  rm(dhs_u.sf, dhs_r.sf, buff_u.sf, buff_r.sf)
  
  # join the health facility points to the cluster buffer
  joinedObject <- st_join(spa_sf, buff_sf, join = st_within)
  # drop the geometry of the joined object
  joinedObject <- st_drop_geometry(joinedObject)
  
  dhsIDs <- dhs_sf$DHSID
  names(dhsIDs) <- dhsIDs
  
  # get health facility scores
  hseScores <- sapply(dhsIDs, function(x) {
    relevantHFs <- dplyr::filter(joinedObject, DHSID == x)
    
    hseScore <- c()
    for (i in 1:length(metric)) {
      hseScore[i] <- mean(relevantHFs[[metric[i]]], na.rm = TRUE)
    }
    
    names(hseScore) <- metric
    
    return(hseScore)
    
  })
  
  hseScores <- t(hseScores) |> as.data.frame()
  hseScores$DHSID <- row.names(hseScores)
  
  return(hseScores)
}