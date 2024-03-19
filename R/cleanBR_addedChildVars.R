#' Add Calculated Child Variables to Birth Record Data
#'
#' This function enhances the birth record dataset by calculating and adding new variables related to children's dates of birth. Specifically, it calculates the CMC (Century Month Code) date of birth for the first child and a refined date of birth for the first child based on specific ID mappings. It relies on auxiliary functions `extractByRC` and `extractByID` for these calculations.
#'
#' @param BRdata A dataframe representing birth records, which must contain specific columns like `v224` for the first birth index, and identifiers for mothers (`femaleID`) and children (`childID`).
#'
#' @return The input dataframe with two new columns added:
#' \itemize{
#'   \item `first_dob_CMC_b3`: The CMC date of birth for the first child, extracted based on the first birth index (`v224`) and converted to an integer format.
#'   \item `first_c_dobR`: A date of birth in R format for the first child.
#' }
#'
#' @details The function requires the `stringr` package for string operations. It uses the `extractByRC` and `extractByID` functions to perform the data extraction and transformation tasks. These auxiliary functions must be defined elsewhere in the package or script. The `extractByRC` function is used to derive the CMC date of birth for the first child based on the index variable `v224` and the `b3` column, applying zero-padding as necessary. The `extractByID` function is tailored to extract and refine the date of birth for the first child by utilizing the identifiers for mothers and children, ensuring accurate linkage and data consistency across records.
#'
#' @examples
#' # Assuming `BRdata` is your dataset containing birth records
#' # with necessary columns like `v224`, `femaleID`, `childID`, etc.
#' BRdata <- cleanBR(BRdata)
#' BRdata_enhanced <- cleanBR_addedChildVars(BRdata)
#'
#' @importFrom stringr str_pad
#' @export

cleanBR_addedChildVars <- function(BRdata) {
  
  require(stringr)
  
  if (!"childID" %in% names(BRdata) | !"c_dobR" %in% names(BRdata) ) {
    stop("Error: this data must first be processed with the `cleanBR()` function.")
  }
  
  # CMC date of birth for first child
  BRdata$first_dob_CMC_b3 <- extractByRC(
    BRdata, 
    idx = "v224", # first birth index
    varName = "b3", 
    zeroPad_numDigits = 2, 
    output.class = "integer") 
  
  BRdata$first_c_dobR <- extractByID(
    BRdata,
    varName = "c_dobR",
    parentID = "femaleID",
    childID = "childID",
    index = "bord",
    sep = "_",
    indexNumber = 1
  )
  
  return(BRdata)
  
}



# 
# keep if m15_1!=. & bord_01==2
# keep if m15_2!=. & bord_02==1
# 
# gen second_del=m15_1
# recode second_del 21/36=1 11/12=0 96/99=.
# 
# gen first_del=m15_2
# recode first_del 21/36=1 11/12=0 96/99=.
# 
# tab second_del first_del, col row
# 
# *first kid died neonatal - only gives us 75 more deaths -- 62% of those that didnt have first kid at hospital did second time around - but only N of 16
# tab second_del first_del if b5_02==0 & b7_02==0, col row
# 
# *first kid did not die - only 49% of those that didnt have first kid at hospital did second time around  - N of 138
# tab second_del first_del if b5_02==1, col row
# 
# forv i=1/9 {
#   rename b3_0`i' b3_`i'
# rename mm8_0`i' mm8_`i'
# rename mm9_0`i' mm9_`i'
# }
# 
# forv i=1/9 {
# replace b3_`i'= b3_`i'-9
# }
# 
# forv i=1/20 {
# gen flag_`i'=1 if mm8_`i'>=b3_2 & mm8_`i'<=b3_1
# }
# 
# forv i=1/20 {
# replace flag_`i'=. if mm9_`i'==1
# replace flag_`i'=. if mm9_`i'==99
# }
# 
# *flag = interval sister maternal death
# tab1 flag*
# 
# egen flag=rowtotal(flag*)
# tab flag
# recode flag 1/2=1
# 
# tab second_del first_del, col row
# 
# *interval sister maternal death  - only gives us 75 interval deaths -- 50% of those that didn't give birth at hopsital first time did second time - N of only 7
# tab second_del first_del if flag==1, col row
# 
# *no interval sister maternal death - same -- 50% converted to hospital birth after having not had one first time
# tab second_del first_del if flag==0, col row



