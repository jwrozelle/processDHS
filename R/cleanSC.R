# SCdata <- htSPA.list$SC

cleanSC <- function(SCdata) {
  
  # rename facility ID
  SCdata$facID <- SCdata$c004
  # Provider ID #!!!
  SCdata$providerID <- paste0(SCdata$facID, "_", SCdata$c004p)
  
  
  SCdata$rural <- ifelse(SCdata$c003 %in% 2, 1, 0)
  
  # client age
  SCdata$age_client <- ifelse(SCdata$c511 == 98, NA, SCdata$c511)
  
  # Child age in months
  SCdata$age_childMonths <- ifelse(SCdata$c253 == 99, NA, SCdata$c253)
  
  # wait time
  SCdata$waitSC <- NA
  SCdata$waitSC <- ifelse(SCdata$c501 %in% c(998), SCdata$waitSC, SCdata$c501)
  
  # Bypassed
  SCdata$bypassSC <- NA
  SCdata$bypassSC <- ifelse(SCdata$c507 %in% c(0), 1, SCdata$bypassSC)
  SCdata$bypassSC <- ifelse(SCdata$c507 %in% c(1), 0, SCdata$bypassSC)
  
  # Fee
  SCdata$feeSC <- NA
  SCdata$feeSC <- ifelse(SCdata$c504 %in% c(1), 1, SCdata$feeSC)
  SCdata$feeSC <- ifelse(SCdata$c504 %in% c(0), 0, SCdata$feeSC)
  
  # Fee amount
  SCdata$feeAmount_SC <- NA
  SCdata$feeAmount_SC <- ifelse(!SCdata$c505 %in% c(999998), SCdata$c505, SCdata$feeAmount_SC)
  
  # client problems
  #   Wait time
  SCdata$cp_waitTime <- NA
  SCdata$cp_waitTime <- ifelse(SCdata$c502a %in% c(1,2), 1, SCdata$cp_waitTime)
  SCdata$cp_waitTime <- ifelse(SCdata$c502a %in% c(0), 0, SCdata$cp_waitTime)
  
  #   ability to discuss problems
  SCdata$cp_discProbs <- NA
  SCdata$cp_discProbs <- ifelse(SCdata$c502b %in% c(1,2), 1, SCdata$cp_discProbs)
  SCdata$cp_discProbs <- ifelse(SCdata$c502b %in% c(0), 0, SCdata$cp_discProbs)
  
  #   Explanation of problems / treatments
  SCdata$cp_explain <- NA
  SCdata$cp_explain <- ifelse(SCdata$c502c %in% c(1,2), 1, SCdata$cp_explain)
  SCdata$cp_explain <- ifelse(SCdata$c502c %in% c(0), 0, SCdata$cp_explain)
  
  #   Visual privacy
  SCdata$cp_privVisual <- NA
  SCdata$cp_privVisual <- ifelse(SCdata$c502e %in% c(1,2), 1, SCdata$cp_privVisual)
  SCdata$cp_privVisual <- ifelse(SCdata$c502e %in% c(0), 0, SCdata$cp_privVisual)
  
  #   Availability of medicines
  SCdata$cp_privAudio <- NA
  SCdata$cp_privAudio <- ifelse(SCdata$c502f %in% c(1,2), 1, SCdata$cp_privAudio)
  SCdata$cp_privAudio <- ifelse(SCdata$c502f %in% c(0), 0, SCdata$cp_privAudio)
  
  #   Explanation of problems / treatments
  SCdata$cp_medAvail <- NA
  SCdata$cp_medAvail <- ifelse(SCdata$c502g %in% c(1,2), 1, SCdata$cp_medAvail)
  SCdata$cp_medAvail <- ifelse(SCdata$c502g %in% c(0), 0, SCdata$cp_medAvail)
  
  #   Service hours
  SCdata$cp_svcHours <- NA
  SCdata$cp_svcHours <- ifelse(SCdata$c502h %in% c(1,2), 1, SCdata$cp_svcHours)
  SCdata$cp_svcHours <- ifelse(SCdata$c502h %in% c(0), 0, SCdata$cp_svcHours)
  
  #   Service days
  SCdata$cp_svcDays <- NA
  SCdata$cp_svcDays <- ifelse(SCdata$c502i %in% c(1,2), 1, SCdata$cp_svcDays)
  SCdata$cp_svcDays <- ifelse(SCdata$c502i %in% c(0), 0, SCdata$cp_svcDays)
  
  #   Cleanliness
  SCdata$cp_clean <- NA
  SCdata$cp_clean <- ifelse(SCdata$c502j %in% c(1,2), 1, SCdata$cp_clean)
  SCdata$cp_clean <- ifelse(SCdata$c502j %in% c(0), 0, SCdata$cp_clean)
  
  #   Wait time
  SCdata$cp_staffTreat <- NA
  SCdata$cp_staffTreat <- ifelse(SCdata$c502k %in% c(1,2), 1, SCdata$cp_staffTreat)
  SCdata$cp_staffTreat <- ifelse(SCdata$c502k %in% c(0), 0, SCdata$cp_staffTreat)
  
  #   Wait time
  SCdata$cp_cost <- NA
  SCdata$cp_cost <- ifelse(SCdata$c502l %in% c(1,2), 1, SCdata$cp_cost)
  SCdata$cp_cost <- ifelse(SCdata$c502l %in% c(0), 0, SCdata$cp_cost)
  
  # define a vector with the client problem names
  cp_names <- c("cp_waitTime", "cp_discProbs", "cp_explain", "cp_privVisual", "cp_privAudio", "cp_svcHours", "cp_medAvail", "cp_svcDays", "cp_clean", "cp_staffTreat", "cp_cost")
  
  
  # sum of problems
  SCdata$cp_sum <- NA
  SCdata$cp_sum <- rowSums(SCdata[,cp_names])
  
  # any reported client problem
  SCdata$cp_any <- NA
  SCdata$cp_any <- ifelse(SCdata$cp_sum > 0, 1, 0)
  
  # child gender
  SCdata$child_female <- NA
  SCdata$child_female <- ifelse(SCdata$c023 == 1, 0, SCdata$child_female)
  SCdata$child_female <- ifelse(SCdata$c023 == 2, 1, SCdata$child_female)
  
  # provider gender
  SCdata$provider_female <- NA
  SCdata$provider_female <- ifelse(SCdata$c022 == 1, 0, SCdata$provider_female)
  SCdata$provider_female <- ifelse(SCdata$c022 == 2, 1, SCdata$provider_female)
  
  # Get the complaints index
  mca_result <- MCA(SCdata[cp_names])
  # create cp index
  SCdata$cp_index <- mca_result$ind$coord[, 1]
  SCdata$cp_index <- scale(SCdata$cp_index)
  SCdata$cp_index_pct <- Hmisc::cut2(SCdata$cp_index, g = 4) |> as.numeric()
  
  
  # client education level
  SCdata$client_edLevel <- NA
  SCdata$client_edLevel <- SCdata$c512
  
  # urgent hospitalization
  SCdata$referred_urgent <- NA
  SCdata$referred_urgent <- ifelse(SCdata$c208h %in% 1, 1, 0)
  
  # any disatisfaction
  SCdata$anyUnsatisfy <- NA
  SCdata$anyUnsatisfy <- ifelse(SCdata$c521 %in% 0 | SCdata$c520 %in% c(2,3), 1, 0)
  
  
  # Satisfied !!! double check, this was borrowed from ANC
  SCdata$satisfied <- NA
  
  if (SCdata$svyID[1] == "NP_SPA15") {
    
    # label define C520    
    # 1 "Very satisfied"
    # 2 "Fairly satisfied"
    # 3 "Neither satisfied not dissatisfied"
    # 4 "Fairly dissatisfied"
    # 5 "Very dissatisfied"
    
    SCdata$satisfied <- ifelse(SCdata$c520 == 1 | SCdata$c520 == 2, 1, 0) # 35 missing
  } else {
    SCdata$satisfied <- ifelse(SCdata$c520 == 1 , 1, 0) # note that included surveys have between 3 and 37 obs missing
  }
  
  # "Would recommend to friend/family member"
  # label define C521    
  # 0 "No"
  # 1 "Yes"
  # 8 "DK"
  
  SCdata$recommend <- NA
  SCdata$recommend <- ifelse(
    SCdata$c521 == 1, # note that included surveys have between 3 and 37 obs missing
    1,
    0
  )
  
  
  # Composite outcomes
  SCdata$satisfy_outcome <- NA
  SCdata$satisfy_outcome <- ifelse(SCdata$recommend == 1 & SCdata$satisfied == 1, 1, 0)
  
  
  SCdata$cp_outcome <- NA
  SCdata$cp_outcome <- ifelse(SCdata$cp_sum >= 1, 1, 0)
  
  
  return(SCdata)
  
}







