#' Clean and Process OB Data
#'
#' This function processes the OB dataset by creating a unique linking ID and generating indicators for vitamin B12 and vitamin A insufficiency and deficiency.
#'
#' @param OBdata A dataframe containing the OB dataset.
#' @return A dataframe with new variables for linking ID, and indicators for vitamin B12 and vitamin A insufficiency and deficiency.
#' @examples
#' \dontrun{
#' # Assuming OBdata is already loaded
#' cleaned_OBdata <- cleanOB(OBdata)
#' }
#' @details
#' The function first creates a unique linking ID by concatenating the cluster number, household number, and respondent's line number. It then generates indicators for the risk of vitamin B12 insufficiency and deficiency, and vitamin A insufficiency and deficiency based on specified thresholds.
#' 
#' The specific labels used for the variables in the OB dataset are:
#' 
#' \describe{
#'   \item{mnfclust}{Cluster number}
#'   \item{mnfnumb}{Household number}
#'   \item{mnfline}{Respondent's line number}
#'   \item{mn01}{Bar code}
#'   \item{mn02}{Lab number}
#'   \item{mn03}{Sample weight}
#'   \item{mn03a}{Tested for Ferritin}
#'   \item{mn04}{Ferritin (SF) (2 decimals)}
#'   \item{mn04ad}{Ferritin Adjusted (SF_adj) (women/children)}
#'   \item{mn05}{sTfR- soluble transferrin receptor (1 decimal)}
#'   \item{mn05ad}{sTfR - soluble transferrin receptor - Adjusted (women/children)}
#'   \item{mn06}{BIS - Body Iron Stores (2 decimals)}
#'   \item{mn06ad}{BIS - adjusted (2 decimals)}
#'   \item{mn07}{RBP (retinol-binding protein - 3 decimals)}
#'   \item{mn07ad}{RBP - Adjusted (women/children)}
#'   \item{mn08}{CRP (C-reactive protein - 2 decimals)}
#'   \item{mn09}{AGP (alpha-1-acid glycoprotein - 3 decimals)}
#'   \item{mn10}{Tested for retinol}
#'   \item{mn11}{RETINOL (3 decimals)}
#'   \item{mn11ad}{RETINOL - adjusted (women/children)}
#'   \item{mn12}{Tested for Vitamin B12}
#'   \item{mn13}{Vitamin B12}
#'   \item{mn14}{Tested for UIC (Urinary iodine concentration)}
#'   \item{mn15}{UIC (1 decimal)}
#' }
#' 
#' @note The function assumes that the OB dataset follows the structure outlined in the labels.
#' @export
cleanOB <- function(OBdata) {
  
  # linking to OB data
  OBdata$obLinkID <- NA
  OBdata$obLinkID <- paste(OBdata$mnfclust, OBdata$mnfnumb, OBdata$mnfline, sep = "_")
  
  
  # label variable mnfclust     "Cluster number"
  # label variable mnfnumb     "Household number"
  # label variable mnfline     "Respondent's line number"
  
  # label variable mn01     "Bar code"
  # label variable mn02     "Lab number"
  # label variable mn03     "Sample weight"
  # label variable mn03a    "Tested for Ferritin"
  # label variable mn04     "Ferritin (SF) (2 decimals)"
  # label variable mn04ad   "Ferritin Adjusted (SF_adj) (women/children)"
  # label variable mn05     "sTfR- soluble transferrin receptor  (1 decimals)"
  # label variable mn05ad   "sTfR - soluble transferrin receptor - Adjusted (women/children)"
  # label variable mn06     "BIS - Body Iron Stores (2 decimals)"
  # label variable mn06ad   "BIS - adjusted (2 decimals)"
  # label variable mn07     "RBP (retinol-binding protein - 3 decimals)"
  # label variable mn07ad   "RBP - Adjusted (women/children)"
  # label variable mn08     "CRP (C-reactive protein - 2 decimals)"
  # label variable mn09     "AGP (alpha-1-acid glycoprotein - 3 decimals)"
  # label variable mn10     "Tested for retinol"
  # label variable mn11     "RETINOL (3 decimals)"
  # label variable mn11ad   "RETINOL - adjusted (women/children)"
  # label variable mn12     "Tested for Vitamin B12"
  # label variable mn13     "Vitamin B12"
  # label variable mn14     "Tested for UIC (Urinary iodine concentration)"
  # label variable mn15     "UIC (1 decimal)"
  

  
  # Risk of B12 insufficiency
  OBdata$vB12_insufficient <- NA
  OBdata$vB12_insufficient <- ifelse(OBdata$mn13 < 300, 1, 0)
  
  # B12 deficiency
  OBdata$vB12_deficient <- NA
  OBdata$vB12_deficient <- ifelse(OBdata$mn13 < 203, 1, 0)
  
  # vitamin A insufficient
  OBdata$vA_insufficient <- NA
  OBdata$vA_insufficient <- ifelse(OBdata$mn11ad < 1.05, 1, 0)
  
  # Vitamin A deficiency
  OBdata$vA_deficient <- NA
  OBdata$vA_deficient <- ifelse(OBdata$mn11ad < 0.7, 1, 0)
  
  return(OBdata)
  
}