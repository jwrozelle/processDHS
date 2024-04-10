#' Clean IR Data for Self-Care Barriers
#'
#' This function preprocesses IRdata, specifically handling variables related to self-care barriers. 
#' It converts selected survey responses into binary indicators for various barriers to getting 
#' medical help for oneself, such as knowing where to go, getting permission, financial constraints, 
#' distance, transportation issues, going alone, gender of the health provider, availability of the 
#' provider, and drug availability. It then calculates the total count of barriers faced and flags 
#' if any barriers exist.
#'
#' @param IRdata A data frame containing individual respondent (IR) data. 
#' The data frame must include specific variables (v467a to v467i) that represent survey responses 
#' to questions about barriers to accessing medical help for oneself.
#'
#' @return A modified version of the input data frame `IRdata` with additional columns: 
#' \itemize{
#'   \item `selfMedBarrier_where`: Binary indicator for knowing where to go for medical help. 
#'   (Currently set to `NA` and not used.)
#'   \item `selfMedBarrier_permission`: Binary indicator for getting permission to go for medical help.
#'   \item `selfMedBarrier_money`: Binary indicator for having the money needed for treatment.
#'   \item `selfMedBarrier_distance`: Binary indicator for distance to health facility being a barrier.
#'   \item `selfMedBarrier_transport`: Binary indicator for having to take transport. 
#'   (Currently set to `NA` and not used.)
#'   \item `selfMedBarrier_alone`: Binary indicator for not wanting to go alone.
#'   \item `selfMedBarrier_providerFemale`: Binary indicator for concern about no female health provider.
#'   \item `selfMedBarrier_provider`: Binary indicator for concern about no provider available.
#'   \item `selfMedBarrier_drugs`: Binary indicator for concern about no drugs available.
#'   \item `selfMedBarrier_count`: The total count of barriers faced by the respondent.
#'   \item `selfMedBarrier_any`: Flag indicating whether any self-care barriers are reported.
#' }
#' The function currently processes data specifically for respondents from a certain survey wave (e.g., "MW7"), 
#' and sets most of the new barrier indicators to `NA` initially, later updating them based on the survey responses.
#'
#' @details The function is designed to work with data from specific survey waves, indicated by the `v000` variable.
#' It initially sets most of the new barrier indicators to `NA`, updating them based on the respondent's answers 
#' to relevant questions (v467a to v467i). This preprocessing is crucial for subsequent analysis of barriers to 
#' self-medication and care within the surveyed population. Note that some indicators are currently not in use 
#' and are hardcoded to `NA`, reflecting either the unavailability of data or decisions to exclude certain barriers 
#' from analysis.
#'
#' @examples
#' # Assuming `IRdata` is your dataset loaded with appropriate variables (v467a to v467i)
#' cleanIRdata <- cleanIR_selfCareBarriers(IRdata)
#' head(cleanIRdata)
#'
#' @export

cleanIR_selfCareBarriers <- function(IRdata) {
  if (IRdata$v000[1] == "MW7") {
    
    
    #    0 "No problem" (None in data)
    #    1 "Big problem"
    #    2 "Not a big problem"
    
    # label variable v467a    "NA - Getting medical help for self: know where to go"
    IRdata$selfMedBarrier_where <- NA
    # IRdata$selfMedBarrier_where <- ifelse(IRdata$v467a == 1, 1, 0)
    
    # label variable v467b    "Getting medical help for self: getting permission to go"
    IRdata$selfMedBarrier_permission <- NA
    IRdata$selfMedBarrier_permission <- ifelse(IRdata$v467b == 1, 1, 0)
    
    # label variable v467c    "Getting medical help for self: getting money needed for treatment"
    IRdata$selfMedBarrier_money <- NA
    IRdata$selfMedBarrier_money <- ifelse(IRdata$v467c == 1, 1, 0)
    
    # label variable v467d    "Getting medical help for self: distance to health facility"
    IRdata$selfMedBarrier_distance <- NA
    IRdata$selfMedBarrier_distance <- ifelse(IRdata$v467d == 1, 1, 0)
    
    # label variable v467e    "NA - Getting medical help for self: having to take transport"
    IRdata$selfMedBarrier_transport <- NA
    # IRdata$selfMedBarrier_transport <- ifelse(IRdata$v467e == 1, 1, 0)
    
    # label variable v467f    "Getting medical help for self: not wanting to go alone"
    IRdata$selfMedBarrier_alone <- NA
    IRdata$selfMedBarrier_alone <- ifelse(IRdata$v467f == 1, 1, 0)
    
    # label variable v467g    "Getting medical help for self: concern no female health provider"
    IRdata$selfMedBarrier_providerFemale <- NA
    IRdata$selfMedBarrier_providerFemale <- ifelse(IRdata$v467g == 1, 1, 0)
    
    # label variable v467h    "Getting medical help for self: concern no provider"
    IRdata$selfMedBarrier_provider <- NA
    IRdata$selfMedBarrier_provider <- ifelse(IRdata$v467h == 1, 1, 0)
    # label variable v467i    "Getting medical help for self: concern no drugs available"
    #    0 "No problem" (None in data)
    #    1 "Big problem"
    #    2 "Not a big problem"
    IRdata$selfMedBarrier_drugs <- NA
    IRdata$selfMedBarrier_drugs <- ifelse(IRdata$v467i == 1, 1, 0)
    
    data("selfCareBarriers", package = "processDHS")
    
    # count of barriers for self care
    IRdata$selfMedBarrier_count <- NA
    IRdata$selfMedBarrier_count <- base::rowSums(IRdata[,names(IRdata) %in% selfCareBarriers], na.rm = T)
    
    # any self_careBarriers
    IRdata$selfMedBarrier_any <- NA
    IRdata$selfMedBarrier_any <- ifelse(IRdata$selfMedBarrier_count > 0, 1, 0)
    
    
  } else {
    
    warning(paste0("Double check the coding of barriers to care for self in ", IRdata$v000[1], "."))
    #    0 "No problem" (None in data)
    #    1 "Big problem"
    #    2 "Not a big problem"
    
    # label variable v467a    "NA - Getting medical help for self: know where to go"
    IRdata$selfMedBarrier_where <- NA
    IRdata$selfMedBarrier_where <- ifelse(IRdata$v467a == 1, 1, 0)
    
    # label variable v467b    "Getting medical help for self: getting permission to go"
    IRdata$selfMedBarrier_permission <- NA
    IRdata$selfMedBarrier_permission <- ifelse(IRdata$v467b == 1, 1, 0)
    
    # label variable v467c    "Getting medical help for self: getting money needed for treatment"
    IRdata$selfMedBarrier_money <- NA
    IRdata$selfMedBarrier_money <- ifelse(IRdata$v467c == 1, 1, 0)
    
    # label variable v467d    "Getting medical help for self: distance to health facility"
    IRdata$selfMedBarrier_distance <- NA
    IRdata$selfMedBarrier_distance <- ifelse(IRdata$v467d == 1, 1, 0)
    
    # label variable v467e    "NA - Getting medical help for self: having to take transport"
    IRdata$selfMedBarrier_transport <- NA
    IRdata$selfMedBarrier_transport <- ifelse(IRdata$v467e == 1, 1, 0)
    
    # label variable v467f    "Getting medical help for self: not wanting to go alone"
    IRdata$selfMedBarrier_alone <- NA
    IRdata$selfMedBarrier_alone <- ifelse(IRdata$v467f == 1, 1, 0)
    
    # label variable v467g    "Getting medical help for self: concern no female health provider"
    IRdata$selfMedBarrier_providerFemale <- NA
    IRdata$selfMedBarrier_providerFemale <- ifelse(IRdata$v467g == 1, 1, 0)
    
    # label variable v467h    "Getting medical help for self: concern no provider"
    IRdata$selfMedBarrier_provider <- NA
    IRdata$selfMedBarrier_provider <- ifelse(IRdata$v467h == 1, 1, 0)
    
    # label variable v467i    "Getting medical help for self: concern no drugs available"
    IRdata$selfMedBarrier_drugs <- NA
    IRdata$selfMedBarrier_drugs <- ifelse(IRdata$v467i == 1, 1, 0)
    
    data("selfCareBarriers", package = "processDHS")
    
    # count of barriers for self care
    IRdata$selfMedBarrier_count <- NA
    IRdata$selfMedBarrier_count <- base::rowSums(IRdata[,names(IRdata) %in% selfCareBarriers], na.rm = T)
    
    # any self_careBarriers
    IRdata$selfMedBarrier_any <- NA
    IRdata$selfMedBarrier_any <- ifelse(IRdata$selfMedBarrier_count > 0, 1, 0)
  }
  
  return(IRdata)
  
}
