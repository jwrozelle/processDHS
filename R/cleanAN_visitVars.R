#' Clean and Harmonize Provider Variables for SPA Survey Data
#'
#' The `cleanAN_visitVars` function takes a dataset from the SPA survey and 
#' harmonizes the provider qualification codes into a simplified and categorized 
#' variable (`anProv_h`). This new variable categorizes providers into four 
#' groups: Medical Doctors, Advanced Practice Clinicians/Paramedical Professionals, 
#' Nurse/Midwife, and Other (pharmacists, lab staff, dental, non-clinical).
#'
#' @param ANdata A data frame containing SPA survey data with at least the 
#'   variables `svyID` and `c024`. The variable `c024` is used as the 
#'   provider code, and `svyID` indicates the survey country and year.
#'
#' @details The function works by first assigning provider qualifications based 
#'   on the `providerCode` variable, which is initialized from `c024`. 
#'   Different provider codes are mapped to specific provider qualifications 
#'   depending on the survey country (`svyID`). Then, a harmonized provider 
#'   category variable (`anProv_h`) is created with four levels:
#'   \itemize{
#'     \item \code{1}: Medical Doctors (e.g., generalist and specialist doctors, pediatricians, surgeons)
#'     \item \code{2}: Advanced Practice Clinicians, Paramedical Professionals (e.g., clinical officers, medical assistants, anesthetic assistants)
#'     \item \code{3}: Nurse/Midwife (e.g., registered nurses, enrolled nurses, midwives)
#'     \item \code{4}: Other - Pharm, Lab, Dental, Non-clinical (e.g., pharmacists, health assistants, nurse assistants)
#'   }
#'
#' The function includes mappings for Afghanistan (`AF_SPA18`), Haiti (`HT_SPA17`), 
#' Malawi (`MW_SPA13`), Nepal (`NP_SPA15`), and Tanzania (`TZ_SPA14`). 
#' An error is raised if the `svyID` is not one of these.
#'
#' @return A modified version of the input data frame with a new factor variable 
#'   \code{anProv_h}, which represents the harmonized provider categories. 
#'   The factor levels are labeled as follows:
#'   \itemize{
#'     \item \code{"Medical Doctors"}
#'     \item \code{"Advanced practice clinicians, paramedical"}
#'     \item \code{"Nurse, midwife"}
#'     \item \code{"Others—pharm, lab, dental, non-clinical"}
#'   }
#'
#' @examples
#' # Example usage
#' cleaned_data <- cleanAN_visitVars(ANdata)
#'
#' @export

cleanAN_visitVars <- function(ANdata) {
  
  # Provider position
  ANdata$providerCode <- NA
  ANdata$providerCode <- ANdata$c024
  
  if (ANdata$svyID[1] == "AF_SPA18") {
    
    # 1 "Specialist Medical Doctor (1)" *
    # 2 "General Medical Doctor (2)" *
    # 21 "Diploma Nurse (3)"
    # 22 "Registered Nurse(4)" *
    # 23 "Community Nurse (5)"
    # 24 "Registered Midwife (6)" *
    # 25 "Community Midwife (7)" *
    # 31 "Pharmacist (14)"
    # 32 "Pharmacy Technician (15)"
    # 35 "Laboratory Technician (8)"
    # 41 "Radiology Technician (9)"
    # 42 "Anesthesia  Technician (10)"
    # 43 "Dental  Technician (11)"
    # 44 "Blood Bank  Technician (12)"
    # 45 "Vaccinator  Technician (13)"
    # 96 "Other Clinical staff (96)"
    # 97 "No Technical Qualification (95)"
    
    ANdata$providerQual <- NA
    ANdata$providerQual <- ifelse(ANdata$providerCode == 1, "Specialist Medical Dr", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 2, "Generalist Medical Dr", ANdata$providerQual)
    # NO OTHER PROVIDERS IN AFGHANISTAN
    # ANdata$providerQual <- ifelse(ANdata$providerCode == 21, "Diploma nurse", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 22, "Registered nurse", ANdata$providerQual)
    # ANdata$providerQual <- ifelse(ANdata$providerCode == 23, "Community Nurse", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 24, "Registered midwife", ANdata$providerQual) #!!! Codebook is: "Registered Midwife (6)"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 25, "Community midwife", ANdata$providerQual)
    # ANdata$providerQual <- ifelse(ANdata$providerCode == 31, "Pharmacist", ANdata$providerQual)
    # CODEBOOK INCLUDES MORE
    
  } else if (ANdata$svyID[1] == "HT_SPA17") { # HAITI
    
    # 1 "Generalist Medical Doctor"
    # 2 "Specialist Medical Doctor"
    # 3 "Clinical Officer (Degree Level)"
    # 4 "Clinical Technician (Diploma)"
    # 5 "Medical Assistant"
    # 21 "Registered Nurse (BSN)"
    # 22 "Registered Nurse Midwife (BSN)"
    # 23 "Registered Psychiatric Nurse"
    # 24 "Registered Nurse with Diploma"
    # 25 "Enrolled Nurse"
    # 26 "Community Health Nurse"
    # 27 "Enrolled Midwife /Nurse Midwife Technical"
    # 28 "Enrolled Nurse Midwife"
    # 31 "Pharmacist"
    # 32 "Pharmacy Assistant"
    # 35 "Laboratory Technician"
    # 36 "Laboratory Assistant"
    # 37 "Radiology Technician"
    # 38 "Medical Imaging Technician"
    # 41 "Environment Health Officer"
    # 42 "Health Surveillance Assistant (HSA)"
    # 43 "HIV Testing and Counseling Counselors (NON-HSA)"
    # 44 "Dental Hygienist"
    # 71 "Other Community Health Workers"
    # 96 "Other"
    # 97 "No Technical Qualification"
    # 99 "Missing"
    
    ANdata$providerQual <- NA
    ANdata$providerQual <- ifelse(ANdata$providerCode == 1, "Generalist Medical Dr", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 2, "Specialist Medical Dr", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 21, "Registered nurse (bsn)", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 22, "Registered nurse midwife (bsn)", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 25, "Enrolled nurse", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 27, "Enrolled midwife", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 31, "Pharmacist", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 71, "Other CHW", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 96, "Other", ANdata$providerQual)
    
  } else if (ANdata$svyID[1] == "MW_SPA13") { # MALAWI
    
    # 1 "Generalist Medical Doctor"
    # 2 "Specialist Medical Doctor"
    # 3 "Clinical Officer (Degree Level)"
    # 4 "Clinical Technician (Diploma)"
    # 5 "Medical Assistant"
    # 21 "Registered Nurse (BSN)"
    # 22 "Registered Nurse Midwife (BSN)"
    # 23 "Registered Psychiatric Nurse"
    # 24 "Registered Nurse with Diploma"
    # 25 "Enrolled Nurse"
    # 26 "Community Health Nurse"
    # 27 "Enrolled Midwife /Nurse Midwife Technical"
    # 28 "Enrolled Nurse Midwife"
    # 41 "Environment Health Officer"
    # 42 "Health Surveillance Assistant (HSA)"
    # 43 "HIV Testing and Counseling Counselors (NON-HSA)"
    # 96 "Other"
    # 97 "No Technical Qualification"
    # 99 "Missing"
    
    
    ANdata$providerQual <- NA
    ANdata$providerQual <- ifelse(ANdata$providerCode == 1, "Generalist Medical Dr", ANdata$providerQual) # 1 "Generalist Medical Doctor"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 2, "Specialist Medical Dr", ANdata$providerQual) # 2 "Specialist Medical Doctor"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 3, "Clin off (degree)", ANdata$providerQual) # 3 "Clinical Officer (Degree Level)"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 4, "Clin off (diploma)", ANdata$providerQual) # 4 "Clinical Officer (Diploma)"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 5, "Medical assistant", ANdata$providerQual) # 5 "Medical Assistant"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 21, "Registered nurse (bsn)", ANdata$providerQual) # 21 "Registered Nurse (BSN)" DOES NOT APPEAR IN SC
    ANdata$providerQual <- ifelse(ANdata$providerCode == 22, "Registered nurse midwife (bsn)", ANdata$providerQual) # 22 "Registered Nurse Midwife (BSN)"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 24, "Registered nurse (diploma)", ANdata$providerQual) # 24 "Registered Nurse with Diploma"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 25, "Enrolled nurse", ANdata$providerQual) # 25 "Enrolled Nurse"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 26, "Comm health nurse", ANdata$providerQual) # 26 "Community Health Nurse"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 27, "Enrolled nurse", ANdata$providerQual) # 27 "Enrolled Midwife /Nurse Midwife Technical"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 28, "Enrolled nurse midwife", ANdata$providerQual) # 28 "Enrolled Nurse Midwife"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 42, "Health Surveillance assistant", ANdata$providerQual) # 42 "Health Surveillance Assistant (HSA)"
    
    
  } else if (ANdata$svyID[1] == "NP_SPA15") { # NEPAL
    
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
    
    ANdata$providerQual <- NA
    ANdata$providerQual <- ifelse(ANdata$providerCode == 1, "Generalist Medical Dr", ANdata$providerQual) 
    ANdata$providerQual <- ifelse(ANdata$providerCode == 2, "Gynecologist", ANdata$providerQual) 
    ANdata$providerQual <- ifelse(ANdata$providerCode == 5, "General surgeon", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 6, "Pediatrician", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 7, "Medical off (degree)", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 8, "Specialist Medical Dr", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 15, "Nurse or ANM", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 16, "Anesthetic assistant", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 40, "Health assistant", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 42, "other", ANdata$providerQual)
    
    
  } else if (ANdata$svyID[1] == "TZ_SPA14") { # Tanzania
    
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
    
    ANdata$providerQual <- NA
    ANdata$providerQual <- ifelse(ANdata$providerCode == 1, "Generalist Medical Dr", ANdata$providerQual) 
    ANdata$providerQual <- ifelse(ANdata$providerCode == 2, "Specialist Medical Dr", ANdata$providerQual) # 2 "Specialist Medical Doctor"
    ANdata$providerQual <- ifelse(ANdata$providerCode == 3, "Assistant med officer", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 4, "Clin off", ANdata$providerQual) 
    ANdata$providerQual <- ifelse(ANdata$providerCode == 5, "Clin off (assistant)", ANdata$providerQual) 
    ANdata$providerQual <- ifelse(ANdata$providerCode == 21, "Registered nurse (bsn)", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 22, "Enrolled nurse", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 23, "Nurse assistant", ANdata$providerQual)
    
    
    ANdata$providerQual <- ifelse(ANdata$providerCode == 6, "Pediatrician", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 7, "Medical off (degree)", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 8, "Specialist Medical Dr", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 15, "Nurse", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 16, "Anesthetic assistant", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 40, "Health assistant", ANdata$providerQual)
    ANdata$providerQual <- ifelse(ANdata$providerCode == 42, "other", ANdata$providerQual)
    

    
  } else {
    stop(paste0("The function has not been customized for ", ANdata$svyID[1]))
  }
  
  # Harmonize provider categories into anProv_h
  ANdata$anProv_h <- NA
  
  # Medical Doctors
  ANdata$anProv_h <- ifelse(ANdata$providerQual %in% c(
    "Generalist Medical Dr", 
    "Specialist Medical Dr", 
    "Pediatrician", 
    "General surgeon", 
    "Gynecologist"
  ), 0, ANdata$anProv_h)
  
  # Advanced practice clinicians, paramedical professionals
  ANdata$anProv_h <- ifelse(ANdata$providerQual %in% c(
    "Medical off (degree)", 
    "Clin off (degree)", 
    "Clin off (diploma)", 
    "Clin off",
    "Clin off (assistant)",
    "Medical assistant", 
    "Anesthetic assistant", 
    "Assistant med officer"
  ), 1, ANdata$anProv_h)
  
  # Nurse, midwife
  ANdata$anProv_h <- ifelse(ANdata$providerQual %in% c(
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
  ), 2, ANdata$anProv_h)
  
  # Others—pharm, lab, dental, non-clinical
  ANdata$anProv_h <- ifelse(ANdata$providerQual %in% c(
    "Pharmacist", 
    "Health assistant", 
    "Other CHW", 
    "Nurse assistant", 
    "other"
  ), 3, ANdata$anProv_h)
  
  # Convert anProv_h to a factor with appropriate labels
  ANdata$anProv_h <- factor(ANdata$anProv_h, 
                            levels = c(0, 1, 2, 3),
                            labels = c("Medical Doctors", 
                                       "Advanced practice clinicians, paramedical", 
                                       "Nurse, midwife", 
                                       "Others—pharm, lab, dental, non-clinical"))
  
  return(ANdata)
  
  
}