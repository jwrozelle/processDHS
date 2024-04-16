# ANdata <- htSPA.list$AN

cleanAN <- function(ANdata) {
  
  # rename facility ID
  ANdata$facID <- ANdata$c004
  
  # client age
  ANdata$age_client <- ifelse(ANdata$c511 == 98, NA, ANdata$c511)
  
  # wait time
  ANdata$waitAN <- NA
  ANdata$waitAN <- ifelse(ANdata$c501 %in% c(998), ANdata$waitAN, ANdata$c501)
  
  # Bypassed
  ANdata$bypassAN <- NA
  ANdata$bypassAN <- ifelse(ANdata$c507 %in% c(0), 1, ANdata$bypassAN)
  ANdata$bypassAN <- ifelse(ANdata$c507 %in% c(1), 0, ANdata$bypassAN)
  
  # Fee # checked for AF, HT, MW, NP, TZ
  ANdata$feeAN <- NA
  ANdata$feeAN <- ifelse(ANdata$c504 %in% c(1), 1, ANdata$feeAN)
  ANdata$feeAN <- ifelse(ANdata$c504 %in% c(0), 0, ANdata$feeAN)
  
  # Fee amount
  ANdata$feeAmount_AN <- NA
  ANdata$feeAmount_AN <- ifelse(!ANdata$c505 %in% c(999998), ANdata$c505, ANdata$feeAmount_AN)
  
  # client problems
  #   Wait time
  ANdata$cp_waitTime <- NA
  ANdata$cp_waitTime <- ifelse(ANdata$c502a %in% c(1,2), 1, ANdata$cp_waitTime)
  ANdata$cp_waitTime <- ifelse(ANdata$c502a %in% c(0), 0, ANdata$cp_waitTime)
  
  #   ability to discuss problems
  ANdata$cp_discProbs <- NA
  ANdata$cp_discProbs <- ifelse(ANdata$c502b %in% c(1,2), 1, ANdata$cp_discProbs)
  ANdata$cp_discProbs <- ifelse(ANdata$c502b %in% c(0), 0, ANdata$cp_discProbs)
  
  #   Explanation of problems / treatments
  ANdata$cp_explain <- NA
  ANdata$cp_explain <- ifelse(ANdata$c502c %in% c(1,2), 1, ANdata$cp_explain)
  ANdata$cp_explain <- ifelse(ANdata$c502c %in% c(0), 0, ANdata$cp_explain)
  
  #   Visual privacy
  ANdata$cp_privVisual <- NA
  ANdata$cp_privVisual <- ifelse(ANdata$c502e %in% c(1,2), 1, ANdata$cp_privVisual)
  ANdata$cp_privVisual <- ifelse(ANdata$c502e %in% c(0), 0, ANdata$cp_privVisual)
  
  #   Availability of medicines
  ANdata$cp_privAudio <- NA
  ANdata$cp_privAudio <- ifelse(ANdata$c502f %in% c(1,2), 1, ANdata$cp_privAudio)
  ANdata$cp_privAudio <- ifelse(ANdata$c502f %in% c(0), 0, ANdata$cp_privAudio)
  
  #   Explanation of problems / treatments
  ANdata$cp_medAvail <- NA
  ANdata$cp_medAvail <- ifelse(ANdata$c502g %in% c(1,2), 1, ANdata$cp_medAvail)
  ANdata$cp_medAvail <- ifelse(ANdata$c502g %in% c(0), 0, ANdata$cp_medAvail)
  
  #   Service hours
  ANdata$cp_svcHours <- NA
  ANdata$cp_svcHours <- ifelse(ANdata$c502h %in% c(1,2), 1, ANdata$cp_svcHours)
  ANdata$cp_svcHours <- ifelse(ANdata$c502h %in% c(0), 0, ANdata$cp_svcHours)
  
  #   Service days
  ANdata$cp_svcDays <- NA
  ANdata$cp_svcDays <- ifelse(ANdata$c502i %in% c(1,2), 1, ANdata$cp_svcDays)
  ANdata$cp_svcDays <- ifelse(ANdata$c502i %in% c(0), 0, ANdata$cp_svcDays)
  
  #   Cleanliness
  ANdata$cp_clean <- NA
  ANdata$cp_clean <- ifelse(ANdata$c502j %in% c(1,2), 1, ANdata$cp_clean)
  ANdata$cp_clean <- ifelse(ANdata$c502j %in% c(0), 0, ANdata$cp_clean)
  
  #   staff treatment
  ANdata$cp_staffTreat <- NA
  ANdata$cp_staffTreat <- ifelse(ANdata$c502k %in% c(1,2), 1, ANdata$cp_staffTreat)
  ANdata$cp_staffTreat <- ifelse(ANdata$c502k %in% c(0), 0, ANdata$cp_staffTreat)
  
  #   cost
  ANdata$cp_cost <- NA
  ANdata$cp_cost <- ifelse(ANdata$c502l %in% c(1,2), 1, ANdata$cp_cost)
  ANdata$cp_cost <- ifelse(ANdata$c502l %in% c(0), 0, ANdata$cp_cost)
  
  # define a vector with the client problem names
  cp_names <- c("cp_waitTime", "cp_discProbs", "cp_explain", "cp_privVisual", "cp_privAudio", "cp_svcHours", "cp_medAvail", "cp_svcDays", "cp_clean", "cp_staffTreat", "cp_cost")
  
  
  # sum of problems
  ANdata$cp_sum <- NA
  ANdata$cp_sum <- rowSums(ANdata[,cp_names])
  
  # Get the complaints index
  mca_result <- MCA(ANdata[cp_names])
  # create cp index
  ANdata$cp_index <- mca_result$ind$coord[, 1]
  ANdata$cp_index <- scale(ANdata$cp_index)
  ANdata$cp_index_pct <- Hmisc::cut2(ANdata$cp_index, g = 4) |> as.numeric()
  
  # Satisfied
  ANdata$satisfied <- NA
  
  if (ANdata$svyID[1] == "NP_SPA15") {
    
    # label define C520    
    # 1 "Very satisfied"
    # 2 "Fairly satisfied"
    # 3 "Neither satisfied not dissatisfied"
    # 4 "Fairly dissatisfied"
    # 5 "Very dissatisfied"
    
    ANdata$satisfied <- ifelse(ANdata$c520 == 1 | ANdata$c520 == 2, 1, 0) # 35 missing
  } else {
    ANdata$satisfied <- ifelse(ANdata$c520 == 1 , 1, 0) # note that included surveys have between 3 and 37 missing
  }
  
  # "Would recommend to friend/family member"
    # label define C521    
    # 0 "No"
    # 1 "Yes"
    # 8 "DK"
  
  ANdata$recommend <- NA
  ANdata$recommend <- ifelse(
    ANdata$c521 == 1, # note that included surveys have between 3 and 37 missing
    1,
    0
  )
  
  # Composite Outcomes
  
  ANdata$satisfy_outcome <- NA
  ANdata$satisfy_outcome <- ifelse(ANdata$recommend == 1 & ANdata$satisfied == 1, 1, 0)
    
  
  ANdata$cp_outcome <- NA
  ANdata$cp_outcome <- ifelse(ANdata$cp_sum >= 1, 1, 0)
    
  
  
  return(ANdata)
  
}







