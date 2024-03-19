
# clusterID <- htIR_t2$v001
# prefix <- "HT2016"
# totalChars <- 8

genDHSID <- function(clusterID, prefix, totalChars) {
  
  # create a character vector with 0's to pad
  zeroPads <- sapply(clusterID, function(x) {
    return(paste0(rep(0, times = totalChars - nchar(x)), collapse = ""))
  })
  
  dhsid <- paste0(prefix, 
                  zeroPads,
                  clusterID
  )
  return(dhsid)
}

