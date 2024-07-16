#' #' Clean and Process Food Intake Data for Children
#' #'
#' #' This function processes food intake data for children in the KR dataset. It creates various indicators for food intake and calculates composite variables such as vitamin A rich foods and iron rich foods. It also computes dietary diversity scores and minimum dietary diversity for children aged 6-23 months.
#' #'
#' #' @param IRdata A dataframe containing the KR dataset.
#' #' @return A dataframe with new variables for food intake indicators, composite variables, dietary diversity scores, and minimum dietary diversity.
#' #' @examples
#' #' \dontrun{
#' #' # Assuming IRdata is already loaded
#' #' cleaned_data <- cleanIR_foodIntake(IRdata)
#' #' }
#' #' @details
#' #' The function first checks if the dataset is from a specific country (in this case, "RW7") and warns the user if it is not. It then processes the interview date and creates basic food intake indicators. Composite variables for vitamin A rich foods and iron rich foods are created. The function also creates indicators for each food group defined in the Minimum Dietary Diversity for Infants and Young Children (MDD-IYCF) and calculates the dietary diversity score. Finally, it determines whether children aged 6-23 months meet the minimum dietary diversity criteria.
#' #' @note This function has been verified only for the "RW7" dataset. The nutrition questions are country-specific, so the function might not work correctly with datasets from other countries.
#' #' @export
#' cleanIR_foodIntake <- function(IRdata) {
#'   
#'   if (!IRdata$v000[1] %in% c("RW7")) {
#'     warning(paste0("cleanIR_foodIntake has not been verified for ", IRdata$v000[1], ".\nThe nutrition questions are very country dependent."))
#'   }
#'   
#'   
#'   if (IRdata$v000[1] %in% c("RW7")) {
#'     
#'     # Interview date
#'     IRdata$c_interviewDate_dd <- IRdata$v016
#'     IRdata$c_interviewDate_mm <- IRdata$v006
#'     IRdata$c_interviewDate_yyyy <- IRdata$v007
#'     
#'     # interview date
#'     IRdata$c_interviewDate <- as.Date(paste(IRdata$c_interviewDate_yyyy, IRdata$c_interviewDate_mm, IRdata$c_interviewDate_dd, sep = "-"))
#'     
#'     
#'     
#'     # Basic food intake ####
#'     # 0 "No"
#'     # 1 "Yes"
#'     # 8 "Don't know"
#'     
#'     
#'     # v409     "Gave child plain water"
#'     IRdata$cfood_water <- NA
#'     IRdata$cfood_water <- ifelse(IRdata$v409 == 1, 1, 0)
#'     # v410     "Gave child juice"
#'     IRdata$cfood_juice <- NA
#'     IRdata$cfood_juice <- ifelse(IRdata$v410 == 1, 1, 0)
#'     # v411     "Gave child tinned, powdered or fresh milk"
#'     IRdata$cfood_milk <- NA
#'     IRdata$cfood_milk <- ifelse(IRdata$v411 == 1, 1, 0)
#'     # v411a    "Gave child baby formula"
#'     IRdata$cfood_formula <- NA
#'     IRdata$cfood_formula <- ifelse(IRdata$v411a == 1, 1, 0)
#'     # v412a    "Gave child fortified baby food (cerelac, etc)"
#'     IRdata$cfood_fortified_bf <- NA
#'     IRdata$cfood_fortified_bf <- ifelse(IRdata$v412a == 1, 1, 0)
#'     # v412c    "Gave child soup/clear broth"
#'     IRdata$cfood_soup <- NA
#'     IRdata$cfood_soup <- ifelse(IRdata$v412c == 1, 1, 0)
#'     # v413     "Gave child other liquid"
#'     IRdata$cfood_other_liquid <- NA
#'     IRdata$cfood_other_liquid <- ifelse(IRdata$v413 == 1, 1, 0)
#'     # v414e    "Gave child bread, noodles, other made from grains"
#'     IRdata$cfood_grains <- NA
#'     IRdata$cfood_grains <- ifelse(IRdata$v414e == 1, 1, 0)
#'     # v414f    "Gave child potatoes, cassava, or other tubers"
#'     IRdata$cfood_tubers <- NA
#'     IRdata$cfood_tubers <- ifelse(IRdata$v414f == 1, 1, 0)
#'     # v414g    "Gave child eggs"
#'     IRdata$cfood_eggs <- NA
#'     IRdata$cfood_eggs <- ifelse(IRdata$v414g == 1, 1, 0)
#'     # v414h    "Gave child meat (beef, pork, lamb, chicken, etc)"
#'     IRdata$cfood_meat <- NA
#'     IRdata$cfood_meat <- ifelse(IRdata$v414h == 1, 1, 0)
#'     # v414i    "Gave child pumpkin, carrots, squash (yellow or orange inside)"
#'     IRdata$cfood_yelOrange <- NA
#'     IRdata$cfood_yelOrange <- ifelse(IRdata$v414i == 1, 1, 0)
#'     # v414j    "Gave child any dark green leafy vegetables"
#'     IRdata$cfood_leafyGreen <- NA
#'     IRdata$cfood_leafyGreen <- ifelse(IRdata$v414j == 1, 1, 0)
#'     # v414k    "Gave child mangoes, papayas, other vitamin A fruits"
#'     IRdata$cfood_vA_fruits <- NA
#'     IRdata$cfood_vA_fruits <- ifelse(IRdata$v414k == 1, 1, 0)
#'     # v414l    "Gave child any other fruits"
#'     IRdata$cfood_ofruits <- NA
#'     IRdata$cfood_ofruits <- ifelse(IRdata$v414l == 1, 1, 0)
#'     # v414m    "Gave child liver, heart, other organs"
#'     IRdata$cfood_organs <- NA
#'     IRdata$cfood_organs <- ifelse(IRdata$v414m == 1, 1, 0)
#'     # v414n    "Gave child fish or shellfish"
#'     IRdata$cfood_fish <- NA
#'     IRdata$cfood_fish <- ifelse(IRdata$v414n == 1, 1, 0)
#'     # v414o    "Gave child food made from beans, peas, lentils, nuts"
#'     IRdata$cfood_legumes <- NA
#'     IRdata$cfood_legumes <- ifelse(IRdata$v414o == 1, 1, 0)
#'     # v414p    "Gave child cheese, yogurt, other milk products"
#'     IRdata$cfood_milkProducts <- NA
#'     IRdata$cfood_milkProducts <- ifelse(IRdata$v414p == 1, 1, 0)
#'     # v414s    "Gave child other solid-semisolid food"
#'     IRdata$cfood_oFood <- NA
#'     IRdata$cfood_oFood <- ifelse(IRdata$v414s == 1, 1, 0)
#'     # v414v    "Gave child yogurt"
#'     IRdata$cfood_yogurt <- NA
#'     IRdata$cfood_yogurt <- ifelse(IRdata$v414v == 1, 1, 0)
#'     # v415     "Drank from bottle with nipple yesterday/last night"
#'     IRdata$cfood_nbottle <- NA
#'     IRdata$cfood_nbottle <- ifelse(IRdata$v415 == 1, 1, 0)
#'     
#'     # h80a     "In the last 7 days given: LOCAL NAME for multiple micronutrient powder"
#'     IRdata$cfood_mnPowder <- NA
#'     IRdata$cfood_mnPowder <- ifelse(IRdata$h80a == 1, 1, 0)
#'     
#'     # Vitamin A supplements ####
#'     IRdata$cfood_vAsupp <- NA
#'     IRdata$cfood_vAsupp <- ifelse(IRdata$h34 == 1, 1, 0)
#'     
#'     # most recent vitamin A supplement 
#'     IRdata$vaxx_vA <- NA
#'     IRdata$vaxx_vA <- ifelse(IRdata$h33 %in% c(1,2,3), 1, 0)
#'     IRdata$vaxx_vA <- ifelse(is.na(IRdata$h33), NA, IRdata$vaxx_vA)
#'     # Date
#'     IRdata$vaxx_vAdate_dd <- ifelse(IRdata$h33d == 97 | IRdata$h33d == 98, NA, IRdata$h33d)
#'     IRdata$vaxx_vAdate_mm <- ifelse(IRdata$h33m == 97 | IRdata$h33m == 98, NA, IRdata$h33m)
#'     IRdata$vaxx_vAdate_yyyy <- ifelse(IRdata$h33y == 9997 | IRdata$h33y == 9998, NA, IRdata$h33y)
#'     
#'     IRdata$vaxx_vAdate <- as.Date(paste(IRdata$vaxx_vAdate_yyyy, IRdata$vaxx_vAdate_mm, IRdata$vaxx_vAdate_dd, sep = "-"), format = "%Y-%m-%d")
#'     
#'     IRdata$vaxx_vA_6ms <- NA
#'     IRdata$vaxx_vA_6ms <- ifelse(IRdata$vaxx_vA == 1 & as.numeric(IRdata$c_interviewDate - IRdata$vaxx_vAdate) < 6*30.42, 1, 0)
#'     
#'     
#'     IRdata$vitaminA_6mos <- NA
#'     IRdata$vitaminA_6mos <- ifelse(IRdata$cfood_vAsupp %in% 1 | IRdata$vaxx_vA_6ms %in% 1, 1, 0)
#'     
#'     # Composite variables ####
#'     ## Vitamin A rich foods ####
#'     vA_rich_vars <- c(
#'       "cfood_meat", 
#'       "cfood_organs", 
#'       "cfood_fish", 
#'       "cfood_eggs", 
#'       "cfood_yelOrange", 
#'       "cfood_leafyGreen", 
#'       "cfood_vA_fruits"
#'     )
#'     
#'     IRdata$cfood_vA_rich <- NA
#'     IRdata$cfood_vA_rich <- ifelse(
#'       rowSums(IRdata[, vA_rich_vars], na.rm = TRUE) > 0, 
#'       1, 
#'       0
#'     )
#'     
#'     ## Iron rich foods ####
#'     # Define the vector for Iron rich foods
#'     iron_rich_vars <- c(
#'       "cfood_meat", 
#'       "cfood_organs", 
#'       "cfood_fish", 
#'       "cfood_eggs"
#'     )
#'     
#'     # Create the cfood_iron_rich variable
#'     IRdata$cfood_iron_rich <- NA
#'     IRdata$cfood_iron_rich <- ifelse(
#'       rowSums(IRdata[, iron_rich_vars], na.rm = TRUE) > 0, 
#'       1, 
#'       0
#'     )
#'     
#'     # Define the vectors for each food group
#'     # breast_milk_var <- 
#'     grains_tubers_var <- c("cfood_grains", "cfood_tubers")
#'     beans_peas_nuts_var <- "cfood_legumes"
#'     dairy_products_var <- c("cfood_milk", "cfood_formula", "cfood_milkProducts", "cfood_yogurt")
#'     flesh_foods_var <- c("cfood_meat", "cfood_organs", "cfood_fish")
#'     eggs_var <- "cfood_eggs"
#'     vitamin_a_fruits_veg_var <- c("cfood_yelOrange", "cfood_leafyGreen", "cfood_vA_fruits")
#'     other_fruits_veg_var <- c("cfood_ofruits", "cfood_other_liquid")
#'     
#'     # Create indicators for each food group
#'     IRdata$food_breast_milk <- ifelse(IRdata[["m4"]] == 95, 1, 0)
#'     IRdata$food_grains_tubers <- ifelse(rowSums(IRdata[, grains_tubers_var], na.rm = TRUE) > 0, 1, 0)
#'     IRdata$food_beans_peas_nuts <- ifelse(IRdata[[beans_peas_nuts_var]] == 1, 1, 0)
#'     IRdata$food_dairy_products <- ifelse(rowSums(IRdata[, dairy_products_var], na.rm = TRUE) > 0, 1, 0)
#'     IRdata$food_flesh_foods <- ifelse(rowSums(IRdata[, flesh_foods_var], na.rm = TRUE) > 0, 1, 0)
#'     IRdata$food_eggs <- ifelse(IRdata[[eggs_var]] == 1, 1, 0)
#'     IRdata$food_vitamin_a_fruits_veg <- ifelse(rowSums(IRdata[, vitamin_a_fruits_veg_var], na.rm = TRUE) > 0, 1, 0)
#'     IRdata$food_other_fruits_veg <- ifelse(rowSums(IRdata[, other_fruits_veg_var], na.rm = TRUE) > 0, 1, 0)
#'     
#'     # Calculate the dietary diversity score
#'     IRdata$dd_score <- rowSums(IRdata[, c(
#'       "food_breast_milk",
#'       "food_grains_tubers",
#'       "food_beans_peas_nuts",
#'       "food_dairy_products",
#'       "food_flesh_foods",
#'       "food_eggs",
#'       "food_vitamin_a_fruits_veg",
#'       "food_other_fruits_veg"
#'     )], na.rm = TRUE)
#'     
#'     # Minimum dietary diversity for children 6 - 23 months old
#'     IRdata$mdd_score6to23 <- NA
#'     IRdata$mdd_score6to23 <- ifelse(IRdata$dd_score >= 5, 1, 0)
#'     IRdata$mdd_score6to23 <- ifelse(IRdata$age <6 | IRdata$age > 23, NA, IRdata$mdd_score6to23)
#'     
#'     
#'   }
#'   
#'   # 
#'   
#'   
#'   
#'   
#'   return(IRdata)
#'   
#' }