#' Clean Family Planning Client Data
#'
#' This function cleans the family planning client dataset by handling specific
#' survey phases, adjusting educational levels, and interpreting literacy and
#' age data. It ensures that the dataset is standardized for further analysis.
#'
#' @param FPdata A data frame containing family planning client data. 
#'               It must contain the following columns:
#'               - c000: Survey phase
#'               - c512: Education level of the client
#'               - c514: Literacy status of the client
#'               - c511: Age of the client
#'               Columns are expected to follow specific coding schemes.
#'
#' @return A data frame with the same structure as the input but with cleaned values
#'         in the education level, literacy status, and age fields.
#'
#'
#' @examples
#' # Assume 'FPdata' is a dataframe with the required structure
#' cleaned_data <- cleanFP_client(FPdata)
#'
#' @export



cleanFP_client <- function(FPdata) {
  
  
  # load(Sys.getenv("TESTING_SPA_DATA"))
  # FPdata <- spa.list$MW_SPA13$FP
  
  
  if(!FPdata$c000[1] %in% c("AF7", "HT7", "MW6", "NP7", "TZ7")) {
    warning('cleanAN_client() has not been verified for survey phase ', FPdata$c000[1], ".")
  }
  
  # education level
  # label define C512    
  # 0 "Never attended school"
  # 1 "Primary"
  # 2 "Secondary"
  # 3 "Higher"
  # 8 "Don't know"
  
  FPdata$edLevel_r <- NA
  FPdata$edLevel_r <- ifelse(FPdata$c512 == 8, NA, FPdata$c512)
  
  # read and write
  # label define C514    
  # 0 "Cannot read or write"
  # 1 "Read only"
  # 2 "Read and write"
  # 8 "Don;t know"
  
  FPdata$readWrite_r <- NA
  FPdata$readWrite_r <- ifelse(FPdata$c514 == 2, 1, 0)
  
  # Age of client
  FPdata$age_r <- NA
  FPdata$age_r <- ifelse(FPdata$c511 == 98, NA, FPdata$c511)
  
  
  return(FPdata)
  
  
}