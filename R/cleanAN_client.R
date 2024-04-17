#' Clean and Recode Antenatal Client Data
#'
#' This function preprocesses antenatal client data, including verifying survey phases, recoding several
#' variables related to educational level, reading and writing skills, number of antenatal visits,
#' pregnancy information, and financial readiness for delivery. It also evaluates the client's awareness
#' of various danger signs related to pregnancy and computes indices based on this knowledge.
#'
#' @param ANdata A dataframe containing antenatal care client data with specific survey question
#'   codes expected (e.g., c512 for education level).
#'
#' @return Returns the modified ANdata dataframe with additional variables:
#'   - edLevel_r: Recoded education level with "Don't know" recoded to NA.
#'   - readWrite_r: Binary recoding of reading and writing skills.
#'   - ancVisit_n_obs: Number of antenatal visits as observed (with specific codes recoded to NA).
#'   - ancVisit_n_resp: Number of antenatal visits as reported by the respondent.
#'   - firstPregnancy_obs: Observation of whether it's the client's first pregnancy.
#'   - firstPregnancy_resp: Client's report of whether it's their first pregnancy.
#'   - weeksPreg_prov: Weeks of pregnancy as provided by the healthcare provider.
#'   - weeksPreg_card: Weeks of pregnancy according to the health card.
#'   - moneyDel_enough: Financial readiness for delivery, recoded to binary.
#'   - ds_knows_count: Count of known danger signs.
#'   - ds_knows_any: Binary indicator if any danger sign is known.
#'   - ds_knows_pct: Percentage of known danger signs.
#'
#' @examples
#' cleaned_data <- cleanAN_client(ANdata = my_antenatal_data)
#'
#' @importFrom sf st_drop_geometry
#' @export
#'
#' @note It is critical to ensure that ANdata contains all the required columns with specific
#'   coding as used in the function. The function currently checks for compatibility with specific
#'   survey phases ('AF7', 'HT7', 'MW6', 'NP7', 'TZ7') and issues a warning if other phases are used.
cleanAN_client <- function(ANdata) {
  
  if(!ANdata$c000[1] %in% c("AF7", "HT7", "MW6", "NP7", "TZ7")) {
    warning('cleanAN_client() has not been verified for survey phase ', ANdata$c000[1], ".")
  }
  
  # education level
  # label define C512    
  # 0 "Never attended school"
  # 1 "Primary"
  # 2 "Secondary"
  # 3 "Higher"
  # 8 "Don't know"
  
  ANdata$edLevel_r <- NA
  ANdata$edLevel_r <- ifelse(ANdata$c512 == 8, NA, ANdata$c512)
  
  # read and write
  # label define C514    
  # 0 "Cannot read or write"
  # 1 "Read only"
  # 2 "Read and write"
  # 8 "Don;t know"
  
  ANdata$readWrite_r <- NA
  ANdata$readWrite_r <- ifelse(ANdata$c514 == 2, 1, 0)
  
  # c101a    "Number of visits to this facility for this pregnancy (OBSERVATION only)
  ANdata$ancVisit_n_obs <- NA
  ANdata$ancVisit_n_obs <- ifelse(ANdata$c101a == 8, NA, ANdata$c101a)
  
  # c155a    "Number of visits to this facility for this pregnancy"
  ANdata$ancVisit_n_resp <- NA
  ANdata$ancVisit_n_resp <- ANdata$c101a # there's no "don't know" option
  
  # c102     "Client first pregnancy (OBSERVATION only)"
  ANdata$firstPregnancy_obs <- NA
  ANdata$firstPregnancy_obs <- ifelse(ANdata$c102 == 8, NA, ANdata$c102)
  
  # c154     "First pregnancy according to respondent"
  ANdata$firstPregnancy_resp <- NA
  ANdata$firstPregnancy_resp <- ANdata$c154
  
  # c117 weeks pregnant according to provider
  ANdata$weeksPreg_prov <- NA
  ANdata$weeksPreg_prov <- ifelse(ANdata$c117 == 98, NA, ANdata$c152)
  
  
  # c152     "Weeks pregnant according to card"
  ANdata$weeksPreg_card <- NA
  ANdata$weeksPreg_card <- ifelse(ANdata$c152 == 95, NA, ANdata$c152)
  
  # c168     "Has enough money for delivery"
  # label define C168    
  # 0 "No"
  # 1 "Yes, enough"
  # 2 "Yes, but not enough"
  
  ANdata$moneyDel_enough <- NA
  ANdata$moneyDel_enough <- ifelse(ANdata$c168 == 1, 1, 0)
  
  danger_sign_vars <- c(
    "ds_knows_bleed",
    "ds_knows_fever",
    "ds_knows_swollen",
    "ds_knows_tired",
    "ds_knows_headache",
    "ds_knows_convulsions",
    "ds_knows_movement",
    "ds_knows_premrupt",
    "ds_knows_cough"#,
    # "ds_knows_other",
    # "ds_knows_dk"
  )
  
  # DANGER SIGNS
  # label variable c170a    "Client knows danger sign: Bleeding"
  ANdata$ds_knows_bleed <- NA
  ANdata$ds_knows_bleed <- ifelse(ANdata$c170a == 1, 1, 0)
  # label variable c170b    "Client knows danger sign: Fever"
  ANdata$ds_knows_fever <- NA
  ANdata$ds_knows_fever <- ifelse(ANdata$c170b == 1, 1, 0)
  # label variable c170c    "Client knows danger sign: Swollen hand, face or extremities"
  ANdata$ds_knows_swollen <- NA
  ANdata$ds_knows_swollen <- ifelse(ANdata$c170c == 1, 1, 0)
  # label variable c170d    "Client knows danger sign: Tiredness or breathlessness"
  ANdata$ds_knows_tired <- NA
  ANdata$ds_knows_tired <- ifelse(ANdata$c170d == 1, 1, 0)
  # label variable c170e    "Client knows danger sign: Headache or blurred vision"
  ANdata$ds_knows_headache <- NA
  ANdata$ds_knows_headache <- ifelse(ANdata$c170e == 1, 1, 0)
  # label variable c170f    "Client knows danger sign: Convulsions"
  ANdata$ds_knows_convulsions <- NA
  ANdata$ds_knows_convulsions <- ifelse(ANdata$c170f == 1, 1, 0)
  # label variable c170g    "Client knows danger sign: Reduced or absence of fetal movement"
  ANdata$ds_knows_movement <- NA
  ANdata$ds_knows_movement <- ifelse(ANdata$c170g == 1, 1, 0)
  # label variable c170h    "Client knows danger sign: Premature rupture of membranes"
  ANdata$ds_knows_premrupt <- NA
  ANdata$ds_knows_premrupt <- ifelse(ANdata$c170h == 1, 1, 0)
  # label variable c170i    "NA - Client knows danger sign: Cough or difficulty breathing for 3 weeks or long" NA in Haiti
  ANdata$ds_knows_cough <- NA
  ANdata$ds_knows_cough <- ifelse(ANdata$c170i == 1, 1, 0)
  # label variable c170x    "Client knows danger sign: Other"
  ANdata$ds_knows_other <- NA
  ANdata$ds_knows_other <- ifelse(ANdata$c170x == 1, 1, 0)
  
  danger_sign_vars_inDF <- c()
  for(ds in danger_sign_vars) {
    if (sum(is.na(ANdata[[ds]])) < nrow(ANdata)) {
      danger_sign_vars_inDF <- c(danger_sign_vars_inDF, ds)
    }
  }
  
  ANdata$ds_knows_count <- NA
  ANdata$ds_knows_count <- rowSums(sf::st_drop_geometry(ANdata)[,danger_sign_vars_inDF], na.rm = T)
  
  ANdata$ds_knows_any <- NA
  ANdata$ds_knows_any <- ifelse(ANdata$ds_knows_count > 0, 1, 0)
  
  ANdata$ds_knows_pct <- NA
  ANdata$ds_knows_pct <- ANdata$ds_knows_count / length(danger_sign_vars_inDF)
  
  
  # c503     "Part of prepayment plan/insurance that pays part of cost"

  
}