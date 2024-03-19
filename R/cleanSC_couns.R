

cleanSC_couns <- function(SCdata) {
  
  # Counseling variables
  ## Caretaker counseling:Provide general information about feeding/breastfeeding when NOT sick
  SCdata$counsSC_feedNotSick <- NA
  SCdata$counsSC_feedNotSick <- ifelse(SCdata$c207a == 1, 1, 0)
  ## Advise extra fluids during this sickness
  SCdata$counsSC_fluids <- NA
  SCdata$counsSC_fluids <- ifelse(SCdata$c207b == 1, 1, 0)
  ## c207c    "Caretaker counseling:Advise continued feeding during sickness"
  SCdata$counsSC_contFeed <- NA
  SCdata$counsSC_contFeed <- ifelse(SCdata$c207c == 1, 1, 0)
  ## c207d    "Caretaker counseling:Name the illness for the caretaker"
  SCdata$counsSC_nameIll <- NA
  SCdata$counsSC_nameIll <- ifelse(SCdata$c207d == 1, 1, 0)
  ## c207e    "Caretaker counseling:Describe symptoms requiring immediate return for care"
  SCdata$counsSC_descDanger <- NA
  SCdata$counsSC_descDanger <- ifelse(SCdata$c207e == 1, 1, 0)
  ## c207f    "Caretaker counseling:Used visual aid to educate the caretaker"
  SCdata$counsSC_visAid <- NA
  SCdata$counsSC_visAid <- ifelse(SCdata$c207f == 1, 1, 0)
  ## c207y    "Caretaker counseling:No caretaker counseling"
  SCdata$counsSC_anyCounseling <- NA
  SCdata$counsSC_anyCounseling <- ifelse(SCdata$c207y == 0, 1, 0)
  
  # Additional counseling
  ## c209a    "Home meds:Oral medications prescribed or provided for home treatment"
  SCdata$oralMedSC_presc <- NA
  SCdata$oralMedSC_presc <- ifelse(SCdata$c209a == 1, 1, 0)
  ## c209b    "Home meds:Oral medication administration explained"
  SCdata$oralMedSC_explain <- NA
  SCdata$oralMedSC_explain <- ifelse(SCdata$c209b == 1, 1, 0)
  ## c209c    "Home meds:Caretaker asked to repeat instructions"\
  SCdata$oralMedSC_repeatIns <- NA
  SCdata$oralMedSC_repeatIns <- ifelse(SCdata$c209c == 1, 1, 0)
  ## c209d    "Home meds:Oral medications - first dose given"
  SCdata$oralMedSC_firstDoseGiven <- NA
  SCdata$oralMedSC_firstDoseGiven <- ifelse(SCdata$c209d == 1, 1, 0)
  ## c209y    "Home meds:Oral medications - none prescribed or given"
  SCdata$oralMedSC_anyPresc <- NA
  SCdata$oralMedSC_anyPresc <- ifelse(SCdata$c209y == 0, 1, 0)
  ## c212     "Follow-up discussed"
  SCdata$counsSC_followUp <- NA
  SCdata$counsSC_followUp <- ifelse(SCdata$c209y == 1, 1, 0)
  
  # Composite indicators
  ## Fluid or feeding
  SCdata$counsSC_compositeFluidFeed <- ifelse(SCdata$counsSC_fluids == 1 | SCdata$counsSC_contFeed == 1, 1, 0)
  
  ## Communication Score !!! Note that because we don't use the %in% function, missing on any will mean a missing commScore (Larson E, Leslie H, Kruk M, 2017)
  commScore_vars <- c("counsSC_compositeFluidFeed",  
                       "counsSC_descDanger", 
                       "counsSC_nameIll",  
                       "counsSC_followUp")
  
  SCdata$commScore <- NA
  SCdata$commScore <- rowSums(st_drop_geometry(SCdata[,commScore_vars])) / length(commScore_vars)
  
  return(SCdata)
  
}










