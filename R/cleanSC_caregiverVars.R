#' Clean and Transform Caregiver Variables
#'
#' This function modifies a dataset with specific transformations on caregiver-related variables.
#' It checks the dataset for a specific survey code and updates variables related to education level,
#' caregiver relationship to child, and literacy status. The function adds warnings if used on unvalidated
#' data codes.
#'
#' @param SCdata A data frame containing the survey data, expected to include specific variables like `c512`, `c510`, and `c514`.
#'
#' @return The modified data frame with new or transformed variables:
#'   - `edLevel_r`: Recoded education level, where code `8` is transformed to `NA`.
#'   - `related_mother`, `related_father`, `related_other`: Binary variables indicating the caregiver's relation to the child (mother, father, or other).
#'   - `readWrite_cg`: Binary variable indicating whether the caregiver can read and write.
#'
#' @export

cleanSC_caregiverVars <- function(SCdata) {
  
  # load(file = Sys.getenv("TESTING_SPA_DATA"))
  # SCdata <- spa.list$MW_SPA13$SC
  
  
  
  if (!SCdata$c000[1] %in% c("MW6")) {
    warning("cleanSC_caregiverVars has not been validated/checked for ", SCdata$v000[1], ".")
  }
  
  # level of education
  SCdata$edLevel_r <- SCdata$c512
  SCdata$edLevel_r <- ifelse(SCdata$c512 == 8, NA, SCdata$c512)
  
  # caregiver relationship
  ## Mother
  SCdata$related_mother <- NA
  SCdata$related_mother <- ifelse(SCdata$c510 == 1, 1, 0)
  # Father
  SCdata$related_father <- NA
  SCdata$related_father <- ifelse(SCdata$c510 == 2, 1, 0)
  # Other
  SCdata$related_other <- NA
  SCdata$related_other <- ifelse(SCdata$c510 != 1 & SCdata$c510 != 2, 1, 0)
  
  # read and write
  SCdata$readWrite_cg <- NA
  SCdata$readWrite_cg <- ifelse(SCdata$c514 == 2, 1, 0)
  
  return(SCdata)
  
}
























