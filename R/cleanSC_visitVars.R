#' Clean and Recode Visit Variables for Service Data
#'
#' This function processes healthcare service visit data by recoding provider qualifications,
#' symptoms, and other visit-related variables based on specific survey IDs. It supports multiple
#' country-specific datasets by applying different recoding rules for each. It also generates new
#' variables indicating the type of provider, symptoms reported, and visit outcomes.
#'
#' @param SCdata A data frame containing the service consultation data, expected to include specific
#'        variables such as provider codes, symptom codes, and survey identifiers.
#'
#' @return The modified SCdata data frame with new or transformed variables:
#'   - Provider qualifications and types (generalist, specialist, nurses, etc.)
#'   - Recoded symptom variables (cough, diarrhea, fever, etc.)
#'   - Visit outcome variables (outcome of consultation, referrals, etc.)
#'   - Additional derived metrics like symptom indices and referral statuses.
#'
#' @examples
#' # Assuming SCdata is loaded with the necessary survey variables
#' # Example usage:
#' SCdata <- data.frame(
#'   svyID = c("AF_SPA18", "HT_SPA17", "MW_SPA13"),
#'   providerCode = c(1, 21, 3),
#'   c255a = c(1, 0, 1), c255b = c(0, 1, 0), c255c = c(1, 1, 0),
#'   c517a = c("0900", "1000", "1100"), c517b = c("0930", "1030", "1115")
#' )
#' cleaned_data <- cleanSC_visitVars(SCdata)
#' 
#' @export

cleanSC_visitVars <- function(SCdata) {
  
  require(ExPosition)
  
  # Provider position
  SCdata$providerCode <- NA
  SCdata$providerCode <- SCdata$c024
  
  if (SCdata$svyID[1] == "AF_SPA18") {
    SCdata$providerQual <- NA
    SCdata$providerQual <- ifelse(SCdata$providerCode == 1, "Specialist Medical Dr", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 2, "Generalist Medical Dr", SCdata$providerQual)
    # NO OTHER PROVIDERS IN AFGHANISTAN
    # SCdata$providerQual <- ifelse(SCdata$providerCode == 21, "Diploma nurse", SCdata$providerQual)
    # SCdata$providerQual <- ifelse(SCdata$providerCode == 22, "Registered nurse", SCdata$providerQual)
    # SCdata$providerQual <- ifelse(SCdata$providerCode == 23, "Community Nurse", SCdata$providerQual)
    # SCdata$providerQual <- ifelse(SCdata$providerCode == 24, "Registered nurse midwife (bsn)", SCdata$providerQual) #!!! Codebook is: "Registered Midwife (6)"
    # SCdata$providerQual <- ifelse(SCdata$providerCode == 25, "Community midwife", SCdata$providerQual)
    # SCdata$providerQual <- ifelse(SCdata$providerCode == 31, "Pharmacist", SCdata$providerQual)
    # CODEBOOK INCLUDES MORE
    
    # provider dummy variables
    # generalist Medical Doctor
    SCdata$scProv_genMD <- NA
    SCdata$scProv_genMD <- ifelse(SCdata$providerCode %in% 2, 1, 0)
    
    # specialist Medical Doctor
    SCdata$scProv_specMD <- NA
    SCdata$scProv_specMD <- ifelse(SCdata$providerCode %in% 1, 1, 0)
    
  } else if (SCdata$svyID[1] == "HT_SPA17") { # HAITI
    SCdata$providerQual <- NA
    SCdata$providerQual <- ifelse(SCdata$providerCode == 1, "Generalist Medical Dr", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 2, "Specialist Medical Dr", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 21, "Registered nurse (bsn)", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 22, "Registered nurse midwife (bsn)", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 25, "Enrolled nurse", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 31, "Pharmacist", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 71, "Other CHW", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 96, "Other", SCdata$providerQual)
    
    # provider dummy variables
    # generalist Medical Doctor
    SCdata$scProv_genMD <- NA
    SCdata$scProv_genMD <- ifelse(SCdata$providerCode %in% 1, 1, 0)
    
    # specialist Medical Doctor
    SCdata$scProv_specMD <- NA
    SCdata$scProv_specMD <- ifelse(SCdata$providerCode %in% 2, 1, 0)
    
    # Registered nurse (bsn)
    SCdata$scProv_regNurseBSN <- NA
    SCdata$scProv_regNurseBSN <- ifelse(SCdata$providerCode %in% 21, 1, 0)
    
    # Registered nurse midwife (bsn)
    SCdata$scProv_regMidNurseBSN <- NA
    SCdata$scProv_regMidNurseBSN <- ifelse(SCdata$providerCode %in% 22, 1, 0)
    
    # Enrolled Nurse
    SCdata$scProv_enrolNurse <- NA
    SCdata$scProv_enrolNurse <- ifelse(SCdata$providerCode %in% 25, 1, 0)
    
    # Pharmacist
    SCdata$scProv_pharma <- NA
    SCdata$scProv_pharma <- ifelse(SCdata$providerCode %in% 31, 1, 0)
    
    # Other CHW
    SCdata$scProv_otherCHW <- NA
    SCdata$scProv_otherCHW <- ifelse(SCdata$providerCode %in% 71, 1, 0)
    
    # Other
    SCdata$scProv_other <- NA
    SCdata$scProv_other <- ifelse(SCdata$providerCode %in% 96, 1, 0)
  
  } else if (SCdata$svyID[1] == "MW_SPA13") { # MALAWI
    
    SCdata$providerQual <- NA
    SCdata$providerQual <- ifelse(SCdata$providerCode == 1, "Generalist Medical Dr", SCdata$providerQual) # 1 "Generalist Medical Doctor"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 2, "Specialist Medical Dr", SCdata$providerQual) # 2 "Specialist Medical Doctor"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 3, "Clin off (degree)", SCdata$providerQual) # 3 "Clinical Officer (Degree Level)"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 4, "Clin off (diploma)", SCdata$providerQual) # 4 "Clinical Officer (Diploma)"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 5, "Medical assistant", SCdata$providerQual) # 5 "Medical Assistant"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 21, "Registered nurse (bsn)", SCdata$providerQual) # 21 "Registered Nurse (BSN)" DOES NOT APPEAR IN SC
    SCdata$providerQual <- ifelse(SCdata$providerCode == 22, "Registered nurse midwife (bsn)", SCdata$providerQual) # 22 "Registered Nurse Midwife (BSN)"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 24, "Registered nurse (diploma)", SCdata$providerQual) # 24 "Registered Nurse with Diploma"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 25, "Enrolled nurse", SCdata$providerQual) # 25 "Enrolled Nurse"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 26, "Comm health nurse", SCdata$providerQual) # 26 "Community Health Nurse"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 27, "Enrolled nurse", SCdata$providerQual) # 27 "Enrolled Midwife /Nurse Midwife Technical"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 28, "Enrolled nurse midwife", SCdata$providerQual) # 28 "Enrolled Nurse Midwife"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 42, "Health Surveillance assistant", SCdata$providerQual) # 42 "Health Surveillance Assistant (HSA)"
    
    # provider dummy variables
    # generalist Medical Doctor
    SCdata$scProv_genMD <- NA
    SCdata$scProv_genMD <- ifelse(SCdata$providerCode %in% 1, 1, 0)
    
    # specialist Medical Doctor
    SCdata$scProv_specMD <- NA
    SCdata$scProv_specMD <- ifelse(SCdata$providerCode %in% 2, 1, 0)
    
    # Registered nurse (bsn)
    SCdata$scProv_regNurseBSN <- NA
    SCdata$scProv_regNurseBSN <- ifelse(SCdata$providerCode %in% 21, 1, 0)
    
    # Registered nurse midwife (bsn)
    SCdata$scProv_regMidNurseBSN <- NA
    SCdata$scProv_regMidNurseBSN <- ifelse(SCdata$providerCode %in% 22, 1, 0)
    
    # Enrolled Nurse
    SCdata$scProv_enrolNurse <- NA
    SCdata$scProv_enrolNurse <- ifelse(SCdata$providerCode %in% 25, 1, 0)
    
    # !!! HAVE NOT RECODED ALL, SEE ABOVE
    
    # Other
    SCdata$scProv_other <- NA
    SCdata$scProv_other <- ifelse(SCdata$providerCode %in% 96, 1, 0)
  
    
  } else if (SCdata$svyID[1] == "NP_SPA15") { # NEPAL
    
    # 1 "Generalist [non-specialist] (01)"
    # 2 "Gynecologist / obstetrician (02)"
    # 3 "Anesthesiologist (03)"
    # 4 "Pathologist (04)"
    # 5 "General surgeon (05)"
    # 6 "Pediatrician (06)"
    # 7 "Medical officer (MBBS, BDS) (08)"
    # 8 "Other specialists medical doctors (07)"
    # 15 "Nurse/auxillary nurse midwife  (ANM) (10)"
    # 16 "Anesthetic assistant  (09)"
    # 40 "Health assistant (ha) / ahw / sahw / public health inspector (12)"
    # 42 "Other clinical staff not listed above (e.g., dietician) (18)"
    # 50 "Non-clinical staff/no technical qualification (95)"
    
    SCdata$providerQual <- NA
    SCdata$providerQual <- ifelse(SCdata$providerCode == 1, "Generalist Medical Dr", SCdata$providerQual) 
    SCdata$providerQual <- ifelse(SCdata$providerCode == 5, "General surgeon", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 6, "Pediatrician", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 7, "Medical off (degree)", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 8, "Specialist Medical Dr", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 15, "Nurse", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 16, "Anesthetic assistant", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 40, "Health assistant", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 42, "other", SCdata$providerQual)
    
    # provider dummy variables
    # generalist Medical Doctor
    SCdata$scProv_genMD <- NA
    SCdata$scProv_genMD <- ifelse(SCdata$providerCode %in% 1, 1, 0)
    
    # specialist Medical Doctor
    SCdata$scProv_specMD <- NA
    SCdata$scProv_specMD <- ifelse(SCdata$providerCode %in% 2, 1, 0)
    
    # Pediatrician
    SCdata$scProv_pediatrcianMD <- NA
    SCdata$scProv_pediatrcianMD <- ifelse(SCdata$providerCode %in% 6, 1, 0)
    
    # Registered nurse (bsn)
    SCdata$scProv_regNurseBSN <- NA
    SCdata$scProv_regNurseBSN <- ifelse(SCdata$providerCode %in% 15, 1, 0)
    
    # Registered nurse midwife (bsn)
    SCdata$scProv_regMidNurseBSN <- NA
    # SCdata$scProv_regMidNurseBSN <- ifelse(SCdata$providerCode %in% 22, 1, 0)
    
    # Enrolled Nurse
    SCdata$scProv_enrolNurse <- NA
    SCdata$scProv_enrolNurse <- ifelse(SCdata$providerCode %in% 25, 1, 0)
    
    # !!! HAVE NOT RECODED ALL, SEE ABOVE
    
    # Other
    SCdata$scProv_other <- NA
    SCdata$scProv_other <- ifelse(SCdata$providerCode %in% c(16, 40, 42), 1, 0)
  
  } else if (SCdata$svyID[1] == "TZ_SPA14") { # Tanzania
    
    # 1 "Generalist Medical Doctor"
    # 2 "Specialist Medical Doctor"
    # 3 "Assistant Medical Officer"
    # 4 "Clinical Officer"
    # 5 "Assistant Clinical Officer"
    # 21 "Registered Nurse"
    # 22 "Enrolled Nurse"
    # 23 "Nurse Assistant/Attendant"
    # 35 "Laboratory Scientist"
    # 36 "Laboratory Technologist"
    # 37 "Laboratory Technician"
    # 38 "Laboratory Assistant"
    # 96 "Other"
    # 97 "No Technical Qualification"
    
    SCdata$providerQual <- NA
    SCdata$providerQual <- ifelse(SCdata$providerCode == 1, "Generalist Medical Dr", SCdata$providerQual) 
    SCdata$providerQual <- ifelse(SCdata$providerCode == 2, "Specialist Medical Dr", SCdata$providerQual) # 2 "Specialist Medical Doctor"
    SCdata$providerQual <- ifelse(SCdata$providerCode == 3, "Assistant med officer", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 4, "Clin off", SCdata$providerQual) 
    SCdata$providerQual <- ifelse(SCdata$providerCode == 5, "Clin off (assistant)", SCdata$providerQual) 
    SCdata$providerQual <- ifelse(SCdata$providerCode == 21, "Registered nurse (bsn)", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 22, "Enrolled nurse", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 23, "Nurse assistant", SCdata$providerQual)

    
    SCdata$providerQual <- ifelse(SCdata$providerCode == 6, "Pediatrician", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 7, "Medical off (degree)", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 8, "Specialist Medical Dr", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 15, "Nurse", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 16, "Anesthetic assistant", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 40, "Health assistant", SCdata$providerQual)
    SCdata$providerQual <- ifelse(SCdata$providerCode == 42, "other", SCdata$providerQual)
    
    # provider dummy variables
    # generalist Medical Doctor
    SCdata$scProv_genMD <- NA
    SCdata$scProv_genMD <- ifelse(SCdata$providerCode %in% 1, 1, 0)
    
    # specialist Medical Doctor
    SCdata$scProv_specMD <- NA
    SCdata$scProv_specMD <- ifelse(SCdata$providerCode %in% 2, 1, 0)
    
    # Pediatrician
    SCdata$scProv_pediatrcianMD <- NA
    # SCdata$scProv_pediatrcianMD <- ifelse(SCdata$providerCode %in% 6, 1, 0)
    
    # Registered nurse (bsn)
    SCdata$scProv_regNurseBSN <- NA
    SCdata$scProv_regNurseBSN <- ifelse(SCdata$providerCode %in% 21, 1, 0)
    
    # Registered nurse midwife (bsn)
    SCdata$scProv_regMidNurseBSN <- NA
    # SCdata$scProv_regMidNurseBSN <- ifelse(SCdata$providerCode %in% 22, 1, 0)
    
    # Enrolled Nurse
    SCdata$scProv_enrolNurse <- NA
    SCdata$scProv_enrolNurse <- ifelse(SCdata$providerCode %in% 22, 1, 0)
    
    # !!! HAVE NOT RECODED ALL, SEE ABOVE
    
    # Other
    SCdata$scProv_other <- NA
    SCdata$scProv_other <- ifelse(SCdata$providerCode %in% c(96), 1, 0)
    
  } else {
    stop(paste0("The function has not been customized for ", SCdata$svyID[1]))
  }
  
  # Harmonized provider (From Turcotte-Tremblay)
  SCdata$scProv_h <- NA
  
  # Medical Doctors
  SCdata$scProv_h <- ifelse(SCdata$providerQual %in% c(
    "Generalist Medical Dr", 
    "Specialist Medical Dr", 
    "Pediatrician", 
    "General surgeon", 
    "Gynecologist"
  ), 0, SCdata$scProv_h)
  
  # Advanced practice clinicians, paramedical professionals
  SCdata$scProv_h <- ifelse(SCdata$providerQual %in% c(
    "Medical off (degree)", 
    "Clin off (degree)", 
    "Clin off (diploma)", 
    "Clin off",
    "Clin off (assistant)",
    "Medical assistant", 
    "Anesthetic assistant", 
    "Assistant med officer"
  ), 1, SCdata$scProv_h)
  
  # Nurse, midwife
  SCdata$scProv_h <- ifelse(SCdata$providerQual %in% c(
    "Registered nurse (bsn)", 
    "Registered nurse midwife (bsn)", 
    "Nurse or ANM", 
    "Enrolled nurse", 
    "Enrolled midwife", 
    "Community midwife",
    "Comm health nurse",
    "Enrolled nurse midwife",
    "Registered nurse (diploma)",
    "Registered midwife"
  ), 2, SCdata$scProv_h)
  
  # Others—pharm, lab, dental, non-clinical
  SCdata$scProv_h <- ifelse(SCdata$providerQual %in% c(
    "Pharmacist", 
    "Health assistant", 
    "Other CHW", 
    "Nurse assistant", 
    "other"
  ), 3, SCdata$scProv_h)
  
  # If the providerQual doesn't match any known values, categorize as "Unknown"
  SCdata$scProv_h <- ifelse(is.na(SCdata$scProv_h), NA, SCdata$scProv_h)
  
  # Convert anProv_h to a factor with appropriate labels
  SCdata$scProv_h <- factor(SCdata$scProv_h, 
                            levels = c(0, 1, 2, 3),
                            labels = c("Medical Doctors", 
                                       "Advanced practice clinicians, paramedical", 
                                       "Nurse, midwife", 
                                       "Others—pharm, lab, dental, non-clinical"))
  
  
  
  # Reason for visit
  #   Cough
  SCdata$sc_cough <- NA
  SCdata$sc_cough <- ifelse(SCdata$c255a %in% c(1), 1, 0)
  #   Diarrhea
  SCdata$sc_diarrhea <- NA
  SCdata$sc_diarrhea <- ifelse(SCdata$c255b %in% c(1), 1, 0)
  #   fever
  SCdata$sc_fever <- NA
  SCdata$sc_fever <- ifelse(SCdata$c255c %in% c(1), 1, 0)
  #   feeding DOUBLE CHECK THESE (in haiti)!!!
  SCdata$sc_feeding <- NA
  SCdata$sc_feeding <- ifelse(SCdata$c255e %in% c(1), 1, 0)
  #   vomit
  SCdata$sc_vomit <- NA
  SCdata$sc_vomit <- ifelse(SCdata$c255d %in% c(1), 1, 0)
  #   convulse
  SCdata$sc_convulse <- NA
  SCdata$sc_convulse <- ifelse(SCdata$c255f %in% c(1), 1, 0)
  #   lethargy
  SCdata$sc_lethargy <- NA
  SCdata$sc_lethargy <- ifelse(SCdata$c255g %in% c(1), 1, 0)
  # #   abdominal Pain # Haiti specific
  # SCdata$sc_abdomPain <- NA
  # SCdata$sc_abdomPain <- ifelse(SCdata$c256e %in% c(1), 1, 0)
  #   Other Reasons
  
  SCdata$sc_other <- NA
  
  if (SCdata$svyID[1] == "AF_SPA18") { # Afghanistan
    
    # label variable c256a    "Other reason for visit:Eye problems"
    # label variable c256b    "Other reason for visit:Skin sore"
    # label variable c256c    "Other reason for visit:Injury"
    # label variable c256d    "Other reason for visit:Ear problem"
    # label variable c256x    "Other reason for visit:Other"
    # label variable c256y    "Other reason for visit:No other reason"

    SCdata$sc_other <- ifelse(SCdata$c256a %in% c(1) | SCdata$c256b %in% c(1)  | SCdata$c256c %in% c(1) | SCdata$c256d %in% c(1) | SCdata$c256x %in% c(1), 1, 0)
  } else if (SCdata$svyID[1] == "HT_SPA17") { # Haiti
    
    # label variable c256a    "NA - Other reason for visit: Eye problems"
    # label variable c256b    "Other reason for visit: Skin sore"
    # label variable c256c    "Other reason for visit: Injury"
    # label variable c256d    "Other reason for visit: Ear problem"
    # label variable c256e    "CS - Other reason for visit: Abdominal pain"
    # label variable c256x    "Other reason for visit: Other"
    # label variable c256y    "Other reason for visit: No other reason"
    
    SCdata$sc_other <- ifelse(SCdata$c256a %in% c(1) | SCdata$c256b %in% c(1)  | SCdata$c256c %in% c(1) | SCdata$c256d %in% c(1) | SCdata$c256e %in% c(1) | SCdata$c256x %in% c(1), 1, 0)
    
    
  } else if (SCdata$svyID[1] == "MW_SPA13") {
    
    # label variable c256a    "Other reason for visit:Eye problems"
    # label variable c256b    "Other reason for visit:Skin sore"
    # label variable c256c    "Other reason for visit:Injury"
    # label variable c256d    "Other reason for visit:Ear problem"
    # label variable c256e    "CS Other reason for visit:Abdominal pain/ stomach ache"
    # label variable c256x    "Other reason for visit:Other"
    # label variable c256y    "Other reason for visit:No other reason"
    
    SCdata$sc_other <- ifelse(SCdata$c256a %in% c(1) | SCdata$c256b %in% c(1)  | SCdata$c256c %in% c(1) | SCdata$c256d %in% c(1) | SCdata$c256e %in% c(1) | SCdata$c256x %in% c(1), 1, 0)
    
  } else if (SCdata$svyID[1] == "NP_SPA15") {
    
    # label variable c256a    "Other reason for visit:Eye problems"
    # label variable c256b    "Other reason for visit:Skin sore"
    # label variable c256c    "Other reason for visit:Injury"
    # label variable c256d    "Other reason for visit:Ear problems"
    # label variable c256e    "Other reason for visit:Weight loss - CS"
    # label variable c256f    "Other reason for visit:Common cold/cough - CS"
    # label variable c256g    "Other reason for visit:Abdominal pain - CS"
    # label variable c256x    "Other reason for visit:Other"
    # label variable c256y    "Other reason for visit:No other reason"
    
    SCdata$sc_other <- ifelse(SCdata$c256a %in% c(1) | 
                                SCdata$c256b %in% c(1) | 
                                SCdata$c256c %in% c(1) | 
                                SCdata$c256d %in% c(1) | 
                                SCdata$c256e %in% c(1) | 
                                SCdata$c256f %in% c(1) | 
                                SCdata$c256g %in% c(1) | 
                                SCdata$c256x %in% c(1), 
                              1, 0)
    
  } else if (SCdata$svyID[1] == "TZ_SPA14") {
    
    # label variable c256a    "Other reason for visit:Eye problems"
    # label variable c256b    "Other reason for visit:Skin sore"
    # label variable c256c    "Other reason for visit:Injury"
    # label variable c256d    "Other reason for visit:Ear problem"
    # label variable c256x    "Other reason for visit:Other"
    # label variable c256y    "Other reason for visit:No other reason"
    
    SCdata$sc_other <- ifelse(SCdata$c256a %in% c(1) | 
                                SCdata$c256b %in% c(1) | 
                                SCdata$c256c %in% c(1) | 
                                SCdata$c256d %in% c(1) | 
                                # SCdata$c256e %in% c(1) | 
                                # SCdata$c256f %in% c(1) | 
                                # SCdata$c256g %in% c(1) | 
                                SCdata$c256x %in% c(1), 
                              1, 0)
    
    
  } else {
    stop(paste0("Error processing sc_other for svyID ", SCdata$svyID[1]))
  }
  
  
  
  
  # Main symptoms
  SCdata$ms_cough <- NA
  SCdata$ms_cough <- ifelse(SCdata$c203a %in% 1, 1, 0)
  
  # sum of symptoms
  SCdata$sc_symptoms_count <- NA
  SCdata$sc_symptoms_count <- SCdata$sc_cough + 
    SCdata$sc_diarrhea + 
    SCdata$sc_fever + 
    SCdata$sc_feeding + 
    SCdata$sc_vomit + 
    SCdata$sc_convulse + 
    SCdata$sc_lethargy + 
    # SCdata$sc_abdomPain + 
    SCdata$sc_other
  
  # symptoms list
  symptoms.vec <- c("sc_cough", 
                    "sc_diarrhea", 
                    "sc_fever", 
                    "sc_feeding", 
                    "sc_vomit", 
                    "sc_convulse", 
                    "sc_lethargy", 
                    # "sc_abdomPain", 
                    "sc_other"
                    )
  
  
  # Get the complaints index
  mca_result <- ExPosition::epMCA(SCdata[symptoms.vec], graphs = F)
  # create cp index
  SCdata$symptoms_index <- mca_result$ExPosition.Data$fi[,1]
  
  if (cor(SCdata$symptoms_index, SCdata$sc_symptoms_count) < 0) {
    SCdata$symptoms_index <- -SCdata$symptoms_index
  }
  
  # SCdata$symptoms_index <- scale(SCdata$symptoms_index)
  SCdata$symptoms_index_pct <- Hmisc::cut2(SCdata$symptoms_index, g = 3) |> as.numeric()
  
  # Caretaker age
  SCdata$caretaker_age <- SCdata$c511
  
  # Referred to other facility
  SCdata$referred <- NA
  SCdata$referred <- ifelse(SCdata$c208y %in% 1, 0, 1) # the variable is "no referrals"
  
  # Visit observation length
  SCdata$obsLengthTime_SC <- NA
  SCdata$obsLengthTime_SC <- processDHS::obsLengthCalc(SCdata$c517a, SCdata$c517b, correct = T)
  
  # Visit outcomes
  # c213     "Outcome of consultation"
  # label define C213    
  # 1 "Child sent home"
  # 2 "Child referred to provider at same facility"
  # 3 "Child admitted to same facility"
  # 4 "Child sent to lab"
  # 5 "Child referred to other facility"
  
  SCdata$SC_visitOutcome <- NA
  SCdata$SC_visitOutcome <- ifelse(SCdata$c213 == 1, "Child sent home", SCdata$SC_visitOutcome)
  SCdata$SC_visitOutcome <- ifelse(SCdata$c213 == 2, "Child referred to provider at same facility", SCdata$SC_visitOutcome)
  SCdata$SC_visitOutcome <- ifelse(SCdata$c213 == 3, "Child admitted to same facility", SCdata$SC_visitOutcome)
  SCdata$SC_visitOutcome <- ifelse(SCdata$c213 == 4, "Child sent to lab", SCdata$SC_visitOutcome)
  SCdata$SC_visitOutcome <- ifelse(SCdata$c213 == 5, "Child referred to other facility", SCdata$SC_visitOutcome)
  
  # visit outcome dummy
  # Sent home
  SCdata$sc_VO_sentHome <- ifelse(SCdata$c213 == 1, 1, 0)
  SCdata$sc_VO_referSameHF <- ifelse(SCdata$c213 == 2, 1, 0)
  SCdata$sc_VO_admit <- ifelse(SCdata$c213 == 3, 1, 0)
  SCdata$sc_VO_sentLab <- ifelse(SCdata$c213 == 4, 1, 0)
  SCdata$sc_VO_referOtherHF <- ifelse(SCdata$c213 == 5, 1, 0)
  
  
  
  return(SCdata)
  
}




