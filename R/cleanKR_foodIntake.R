#' Clean and Process Food Intake Data for Children
#'
#' This function processes food intake data for children in the KR dataset. It creates various indicators for food intake and calculates composite variables such as vitamin A rich foods and iron rich foods. It also computes dietary diversity scores and minimum dietary diversity for children aged 6-23 months.
#'
#' @param KRdata A dataframe containing the KR dataset.
#' @return A dataframe with new variables for food intake indicators, composite variables, dietary diversity scores, and minimum dietary diversity.
#' @examples
#' \dontrun{
#' # Assuming KRdata is already loaded
#' cleaned_data <- cleanKR_foodIntake(KRdata)
#' }
#' @details
#' The function first checks if the dataset is from a specific country (in this case, "RW7") and warns the user if it is not. It then processes the interview date and creates basic food intake indicators. Composite variables for vitamin A rich foods and iron rich foods are created. The function also creates indicators for each food group defined in the Minimum Dietary Diversity for Infants and Young Children (MDD-IYCF) and calculates the dietary diversity score. Finally, it determines whether children aged 6-23 months meet the minimum dietary diversity criteria.
#' @note This function has been verified only for the "RW7" dataset. The nutrition questions are country-specific, so the function might not work correctly with datasets from other countries.
#' @export
cleanKR_foodIntake <- function(KRdata) {
  
  if (!KRdata$v000[1] %in% c("RW7")) {
    warning(paste0("cleanKR_foodIntake has not been verified for ", KRdata$v000[1], ".\nThe nutrition questions are very country dependent."))
  }
  
  
  if (KRdata$v000[1] %in% c("RW7")) {
    
    # Interview date
    KRdata$c_interviewDate_dd <- KRdata$v016
    KRdata$c_interviewDate_mm <- KRdata$v006
    KRdata$c_interviewDate_yyyy <- KRdata$v007
    
    # interview date
    KRdata$c_interviewDate <- as.Date(paste(KRdata$c_interviewDate_yyyy, KRdata$c_interviewDate_mm, KRdata$c_interviewDate_dd, sep = "-"))
    
    
    
    # Basic food intake ####
    # 0 "No"
    # 1 "Yes"
    # 8 "Don't know"
    
    
    # v409     "Gave child plain water"
    KRdata$cfood_water <- NA
    KRdata$cfood_water <- ifelse(KRdata$v409 == 1, 1, 0)
    # v410     "Gave child juice"
    KRdata$cfood_juice <- NA
    KRdata$cfood_juice <- ifelse(KRdata$v410 == 1, 1, 0)
    # v411     "Gave child tinned, powdered or fresh milk"
    KRdata$cfood_milk <- NA
    KRdata$cfood_milk <- ifelse(KRdata$v411 == 1, 1, 0)
    # v411a    "Gave child baby formula"
    KRdata$cfood_formula <- NA
    KRdata$cfood_formula <- ifelse(KRdata$v411a == 1, 1, 0)
    # v412a    "Gave child fortified baby food (cerelac, etc)"
    KRdata$cfood_fortified_bf <- NA
    KRdata$cfood_fortified_bf <- ifelse(KRdata$v412a == 1, 1, 0)
    # v412c    "Gave child soup/clear broth"
    KRdata$cfood_soup <- NA
    KRdata$cfood_soup <- ifelse(KRdata$v412c == 1, 1, 0)
    # v413     "Gave child other liquid"
    KRdata$cfood_other_liquid <- NA
    KRdata$cfood_other_liquid <- ifelse(KRdata$v413 == 1, 1, 0)
    # v414e    "Gave child bread, noodles, other made from grains"
    KRdata$cfood_grains <- NA
    KRdata$cfood_grains <- ifelse(KRdata$v414e == 1, 1, 0)
    # v414f    "Gave child potatoes, cassava, or other tubers"
    KRdata$cfood_tubers <- NA
    KRdata$cfood_tubers <- ifelse(KRdata$v414f == 1, 1, 0)
    # v414g    "Gave child eggs"
    KRdata$cfood_eggs <- NA
    KRdata$cfood_eggs <- ifelse(KRdata$v414g == 1, 1, 0)
    # v414h    "Gave child meat (beef, pork, lamb, chicken, etc)"
    KRdata$cfood_meat <- NA
    KRdata$cfood_meat <- ifelse(KRdata$v414h == 1, 1, 0)
    # v414i    "Gave child pumpkin, carrots, squash (yellow or orange inside)"
    KRdata$cfood_yelOrange <- NA
    KRdata$cfood_yelOrange <- ifelse(KRdata$v414i == 1, 1, 0)
    # v414j    "Gave child any dark green leafy vegetables"
    KRdata$cfood_leafyGreen <- NA
    KRdata$cfood_leafyGreen <- ifelse(KRdata$v414j == 1, 1, 0)
    # v414k    "Gave child mangoes, papayas, other vitamin A fruits"
    KRdata$cfood_vA_fruits <- NA
    KRdata$cfood_vA_fruits <- ifelse(KRdata$v414k == 1, 1, 0)
    # v414l    "Gave child any other fruits"
    KRdata$cfood_ofruits <- NA
    KRdata$cfood_ofruits <- ifelse(KRdata$v414l == 1, 1, 0)
    # v414m    "Gave child liver, heart, other organs"
    KRdata$cfood_organs <- NA
    KRdata$cfood_organs <- ifelse(KRdata$v414m == 1, 1, 0)
    # v414n    "Gave child fish or shellfish"
    KRdata$cfood_fish <- NA
    KRdata$cfood_fish <- ifelse(KRdata$v414n == 1, 1, 0)
    # v414o    "Gave child food made from beans, peas, lentils, nuts"
    KRdata$cfood_legumes <- NA
    KRdata$cfood_legumes <- ifelse(KRdata$v414o == 1, 1, 0)
    # v414p    "Gave child cheese, yogurt, other milk products"
    KRdata$cfood_milkProducts <- NA
    KRdata$cfood_milkProducts <- ifelse(KRdata$v414p == 1, 1, 0)
    # v414s    "Gave child other solid-semisolid food"
    KRdata$cfood_oFood <- NA
    KRdata$cfood_oFood <- ifelse(KRdata$v414s == 1, 1, 0)
    # v414v    "Gave child yogurt"
    KRdata$cfood_yogurt <- NA
    KRdata$cfood_yogurt <- ifelse(KRdata$v414v == 1, 1, 0)
    # v415     "Drank from bottle with nipple yesterday/last night"
    KRdata$cfood_nbottle <- NA
    KRdata$cfood_nbottle <- ifelse(KRdata$v415 == 1, 1, 0)
    
    # h80a     "In the last 7 days given: LOCAL NAME for multiple micronutrient powder"
    KRdata$cfood_mnPowder <- NA
    KRdata$cfood_mnPowder <- ifelse(KRdata$h80a == 1, 1, 0)
    
    # Vitamin A supplements ####
    KRdata$cfood_vAsupp <- NA
    KRdata$cfood_vAsupp <- ifelse(KRdata$h34 == 1, 1, 0)
    
    # most recent vitamin A supplement 
    KRdata$vaxx_vA <- NA
    KRdata$vaxx_vA <- ifelse(KRdata$h33 %in% c(1,2,3), 1, 0)
    KRdata$vaxx_vA <- ifelse(is.na(KRdata$h33), NA, KRdata$vaxx_vA)
    # Date
    KRdata$vaxx_vAdate_dd <- ifelse(KRdata$h33d == 97 | KRdata$h33d == 98, NA, KRdata$h33d)
    KRdata$vaxx_vAdate_mm <- ifelse(KRdata$h33m == 97 | KRdata$h33m == 98, NA, KRdata$h33m)
    KRdata$vaxx_vAdate_yyyy <- ifelse(KRdata$h33y == 9997 | KRdata$h33y == 9998, NA, KRdata$h33y)
    
    KRdata$vaxx_vAdate <- as.Date(paste(KRdata$vaxx_vAdate_yyyy, KRdata$vaxx_vAdate_mm, KRdata$vaxx_vAdate_dd, sep = "-"), format = "%Y-%m-%d")
    
    KRdata$vaxx_vA_6ms <- NA
    KRdata$vaxx_vA_6ms <- ifelse(KRdata$vaxx_vA == 1 & as.numeric(KRdata$c_interviewDate - KRdata$vaxx_vAdate) < 6*30.42, 1, 0)
    
    
    KRdata$vitaminA_6mos <- NA
    KRdata$vitaminA_6mos <- ifelse(KRdata$cfood_vAsupp %in% 1 | KRdata$vaxx_vA_6ms %in% 1, 1, 0)
    
    # Composite variables ####
    ## Vitamin A rich foods ####
    vA_rich_vars <- c(
      "cfood_meat", 
      "cfood_organs", 
      "cfood_fish", 
      "cfood_eggs", 
      "cfood_yelOrange", 
      "cfood_leafyGreen", 
      "cfood_vA_fruits"
    )
    
    KRdata$cfood_vA_rich <- NA
    KRdata$cfood_vA_rich <- ifelse(
      rowSums(KRdata[, vA_rich_vars], na.rm = TRUE) > 0, 
      1, 
      0
    )
    
    ## Iron rich foods ####
    # Define the vector for Iron rich foods
    iron_rich_vars <- c(
      "cfood_meat", 
      "cfood_organs", 
      "cfood_fish", 
      "cfood_eggs"
    )
    
    # Create the cfood_iron_rich variable
    KRdata$cfood_iron_rich <- NA
    KRdata$cfood_iron_rich <- ifelse(
      rowSums(KRdata[, iron_rich_vars], na.rm = TRUE) > 0, 
      1, 
      0
    )
    
    # Define the vectors for each food group
    # breast_milk_var <- 
    grains_tubers_var <- c("cfood_grains", "cfood_tubers")
    beans_peas_nuts_var <- "cfood_legumes"
    dairy_products_var <- c("cfood_milk", "cfood_formula", "cfood_milkProducts", "cfood_yogurt")
    flesh_foods_var <- c("cfood_meat", "cfood_organs", "cfood_fish")
    eggs_var <- "cfood_eggs"
    vitamin_a_fruits_veg_var <- c("cfood_yelOrange", "cfood_leafyGreen", "cfood_vA_fruits")
    other_fruits_veg_var <- c("cfood_ofruits", "cfood_other_liquid")
    
    # Create indicators for each food group
    KRdata$food_breast_milk <- ifelse(KRdata[["m4"]] == 95, 1, 0)
    KRdata$food_grains_tubers <- ifelse(rowSums(KRdata[, grains_tubers_var], na.rm = TRUE) > 0, 1, 0)
    KRdata$food_beans_peas_nuts <- ifelse(KRdata[[beans_peas_nuts_var]] == 1, 1, 0)
    KRdata$food_dairy_products <- ifelse(rowSums(KRdata[, dairy_products_var], na.rm = TRUE) > 0, 1, 0)
    KRdata$food_flesh_foods <- ifelse(rowSums(KRdata[, flesh_foods_var], na.rm = TRUE) > 0, 1, 0)
    KRdata$food_eggs <- ifelse(KRdata[[eggs_var]] == 1, 1, 0)
    KRdata$food_vitamin_a_fruits_veg <- ifelse(rowSums(KRdata[, vitamin_a_fruits_veg_var], na.rm = TRUE) > 0, 1, 0)
    KRdata$food_other_fruits_veg <- ifelse(rowSums(KRdata[, other_fruits_veg_var], na.rm = TRUE) > 0, 1, 0)
    
    # Calculate the dietary diversity score
    KRdata$dd_score <- rowSums(KRdata[, c(
      "food_breast_milk",
      "food_grains_tubers",
      "food_beans_peas_nuts",
      "food_dairy_products",
      "food_flesh_foods",
      "food_eggs",
      "food_vitamin_a_fruits_veg",
      "food_other_fruits_veg"
    )], na.rm = TRUE)
    
    # Minimum dietary diversity for children 6 - 23 months old
    KRdata$mdd_score6to23 <- NA
    KRdata$mdd_score6to23 <- ifelse(KRdata$dd_score >= 5, 1, 0)
    KRdata$mdd_score6to23 <- ifelse(KRdata$age <6 | KRdata$age > 23, NA, KRdata$mdd_score6to23)
    
    # Meal frequency
    KRdata$mealFreq <- NA
    KRdata$mealFreq <- KRdata$m39
    
    
    
  }
  
  # 
  
  
  
  
  return(KRdata)
  
}