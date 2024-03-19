# FPdata <- htSPA.list$FP

cleanFP <- function(FPdata) {
  
  # rename facility ID
  FPdata$facID <- FPdata$c004
  
  # wait time
  FPdata$waitFP <- NA
  FPdata$waitFP <- ifelse(FPdata$c501 %in% c(998), FPdata$waitFP, FPdata$c501)
  
  # Bypassed
  FPdata$bypassFP <- NA
  FPdata$bypassFP <- ifelse(FPdata$c507 %in% c(0), 1, FPdata$bypassFP)
  FPdata$bypassFP <- ifelse(FPdata$c507 %in% c(1), 0, FPdata$bypassFP)
  
  # Fee
  FPdata$feeFP <- NA
  FPdata$feeFP <- ifelse(FPdata$c504 %in% c(1), 1, FPdata$feeFP)
  FPdata$feeFP <- ifelse(FPdata$c504 %in% c(2), 0, FPdata$feeFP)
  
  # Fee amount
  FPdata$feeAmount_FP <- NA
  FPdata$feeAmount_FP <- ifelse(!FPdata$c505 %in% c(999998), FPdata$c505, FPdata$feeAmount_FP)
  
  # client problems
  #   Wait time
  FPdata$cp_waitTime <- NA
  FPdata$cp_waitTime <- ifelse(FPdata$c502a %in% c(1,2), 1, FPdata$cp_waitTime)
  FPdata$cp_waitTime <- ifelse(FPdata$c502a %in% c(0), 0, FPdata$cp_waitTime)
  
  #   ability to discuss problems
  FPdata$cp_discProbs <- NA
  FPdata$cp_discProbs <- ifelse(FPdata$c502b %in% c(1,2), 1, FPdata$cp_discProbs)
  FPdata$cp_discProbs <- ifelse(FPdata$c502b %in% c(0), 0, FPdata$cp_discProbs)
  
  #   Explanation of problems / treatments
  FPdata$cp_explain <- NA
  FPdata$cp_explain <- ifelse(FPdata$c502c %in% c(1,2), 1, FPdata$cp_explain)
  FPdata$cp_explain <- ifelse(FPdata$c502c %in% c(0), 0, FPdata$cp_explain)
  
  #   Visual privacy
  FPdata$cp_privVisual <- NA
  FPdata$cp_privVisual <- ifelse(FPdata$c502e %in% c(1,2), 1, FPdata$cp_privVisual)
  FPdata$cp_privVisual <- ifelse(FPdata$c502e %in% c(0), 0, FPdata$cp_privVisual)
  
  #   Availability of medicines
  FPdata$cp_privAudio <- NA
  FPdata$cp_privAudio <- ifelse(FPdata$c502f %in% c(1,2), 1, FPdata$cp_privAudio)
  FPdata$cp_privAudio <- ifelse(FPdata$c502f %in% c(0), 0, FPdata$cp_privAudio)
  
  #   Explanation of problems / treatments
  FPdata$cp_medAvail <- NA
  FPdata$cp_medAvail <- ifelse(FPdata$c502g %in% c(1,2), 1, FPdata$cp_medAvail)
  FPdata$cp_medAvail <- ifelse(FPdata$c502g %in% c(0), 0, FPdata$cp_medAvail)
  
  #   Service hours
  FPdata$cp_svcHours <- NA
  FPdata$cp_svcHours <- ifelse(FPdata$c502h %in% c(1,2), 1, FPdata$cp_svcHours)
  FPdata$cp_svcHours <- ifelse(FPdata$c502h %in% c(0), 0, FPdata$cp_svcHours)
  
  #   Service days
  FPdata$cp_svcDays <- NA
  FPdata$cp_svcDays <- ifelse(FPdata$c502i %in% c(1,2), 1, FPdata$cp_svcDays)
  FPdata$cp_svcDays <- ifelse(FPdata$c502i %in% c(0), 0, FPdata$cp_svcDays)
  
  #   Cleanliness
  FPdata$cp_clean <- NA
  FPdata$cp_clean <- ifelse(FPdata$c502j %in% c(1,2), 1, FPdata$cp_clean)
  FPdata$cp_clean <- ifelse(FPdata$c502j %in% c(0), 0, FPdata$cp_clean)
  
  #   Wait time
  FPdata$cp_staffTreat <- NA
  FPdata$cp_staffTreat <- ifelse(FPdata$c502k %in% c(1,2), 1, FPdata$cp_staffTreat)
  FPdata$cp_staffTreat <- ifelse(FPdata$c502k %in% c(0), 0, FPdata$cp_staffTreat)
  
  #   Wait time
  FPdata$cp_cost <- NA
  FPdata$cp_cost <- ifelse(FPdata$c502l %in% c(1,2), 1, FPdata$cp_cost)
  FPdata$cp_cost <- ifelse(FPdata$c502l %in% c(0), 0, FPdata$cp_cost)
  
  # define a vector with the client problem names
  cp_names <- c("cp_waitTime", "cp_discProbs", "cp_explain", "cp_privVisual", "cp_privAudio", "cp_svcHours", "cp_medAvail", "cp_svcDays", "cp_clean", "cp_staffTreat", "cp_cost")
  
  
  # sum of problems
  FPdata$cp_sum <- NA
  FPdata$cp_sum <- rowSums(FPdata[,cp_names])
  
  # Get the complaints index
  mca_result <- MCA(FPdata[cp_names])
  # create cp index
  FPdata$cp_index <- mca_result$ind$coord[, 1]
  FPdata$cp_index <- scale(FPdata$cp_index)
  FPdata$cp_index_pct <- Hmisc::cut2(FPdata$cp_index, g = 4) |> as.numeric()
  
  
  return(FPdata)
  
}







