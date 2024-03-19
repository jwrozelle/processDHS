#' Clean and Enhance Provider Data
#'
#' This function preprocesses a dataset of health service providers, adding calculated variables 
#' and transforming existing ones for easier analysis. It generates unique identifiers for facilities 
#' and providers, calculates years of experience and service, assesses supervision frequency, 
#' and identifies receipt of in-service training and incentives.
#'
#' @param PVdata A dataframe containing data on health service providers.
#'
#' @return The function returns the input dataframe with additional columns including:
#' \itemize{
#'   \item `facID`: A renamed variable for facility ID.
#'   \item `providerID`: A unique identifier for each provider, combining facility ID and provider code.
#'   \item `year_grad`, `year_startFac`, `year_interview`: Extracted or renamed year variables for graduation, start at facility, and interview year.
#'   \item `years_atFac`, `years_sinceGrad`: Calculated years of service at the facility and since graduation.
#'   \item `hoursPerWeek`: Hours worked per week.
#'   \item Supervision variables: Indicators for supervision received within the last 3 or 6 months.
#'   \item `supervisionRank`: A rank based on the frequency of supervision visits.
#'   \item Incentive variables: Indicators for receipt of monetary and non-monetary incentives.
#'   \item In-service training variables: Indicators for various types of in-service training received.
#' }
#'
#' @details The function assumes the dataset comes from a standardized survey such as the Service Provision Assessment (SPA). It requires specific variable naming conventions (e.g., `w004` for facility ID). The function is particularly useful for preparing provider data for analyses related to workload, supervision, incentives, and professional development.
#'
#' @examples
#' # Assuming PVdata is your dataset loaded into R:
#' # PVdata <- read.csv("ProviderData.csv")
#' PVdata_cleaned <- cleanPV(PVdata)
#'
#' @import dplyr
#' @importFrom stringr str_pad
#' @export


cleanPV <- function(PVdata) {
  
  # facility ID rename
  PVdata$facID <- PVdata$w004
  
  # Provider Unique ID
  PVdata$providerID <- paste0(PVdata$facID, "_", PVdata$w004p)
  
  # years
  #   Graduated
  PVdata$year_grad <- PVdata$w105
  PVdata$year_startFac <- PVdata$w106
  PVdata$year_interview <- PVdata$w033
  
  #   Calculated years
  PVdata$years_atFac <- PVdata$year_interview - PVdata$year_startFac
  PVdata$years_sinceGrad <- PVdata$year_interview - PVdata$year_grad
  
  # hours per week
  PVdata$hoursPerWeek <- PVdata$w108
  
  # supervised in past 3 months
  PVdata$supervisedIn3Months <- NA
  PVdata$supervisedIn3Months <- ifelse(PVdata$w111 %in% 1, 1, 0)
  
  # supervised in past 6 months
  PVdata$supervisedIn6Months <- NA
  PVdata$supervisedIn6Months <- ifelse(PVdata$w111 %in% c(1, 2), 1, 0)
  
  # visits in last 6 months (996 = every day)
  PVdata$superIn6Months_count <- ifelse(is.na(PVdata$w112), 0, PVdata$w112)
  
  # supervision rank (Higher number is more visits)
  countRank.df <- data.frame(superIn6Months_count = unique(PVdata$superIn6Months_count))
  countRank.df$supervisionRank <- rank(countRank.df, na.last = NA)
  PVdata <- merge(PVdata, countRank.df, by = "superIn6Months_count", all.x = T, all.y = F)
  
  # bonuses (missing is considered no)
  PVdata$incentive_monetary <- ifelse(PVdata$w116 %in% 1, 1, 0)
  PVdata$incentive_nonMonetary <- ifelse(PVdata$w117 %in% 1, 1, 0)
  
  # in-service training
  PVdata$inService_child <- NA
  PVdata$inService_child <- ifelse(PVdata$w144 %in% 1, 1, 0)
  
  
  
  # child health in-service training
  #   Child health in-service: any in-service
  PVdata$inService_CHcoldChain <- NA
  PVdata$inService_CHcoldChain <- ifelse(PVdata$w144a %in% 1, 1, 0)
  #   ARI dx and tx
  PVdata$inService_CHariDxTx <- NA
  PVdata$inService_CHariDxTx <- ifelse(PVdata$w144b %in% 1, 1, 0)
  #   Diarrhea dx and Tx
  PVdata$inService_CHdiarrDxTx <- NA
  PVdata$inService_CHdiarrDxTx <- ifelse(PVdata$w144c %in% 1, 1, 0)
  #   Nutrition Dx and Tx
  PVdata$inService_CHnutrition <- NA
  PVdata$inService_CHnutrition <- ifelse(PVdata$w144e %in% 1, 1, 0)
  #   Breastfeeding
  PVdata$inService_CHbf <- NA
  PVdata$inService_CHbf <- ifelse(PVdata$w144f %in% 1, 1, 0)
  #   complementary feeding of infant
  PVdata$inService_CHinfFeed <- NA
  PVdata$inService_CHinfFeed <- ifelse(PVdata$w144g %in% 1, 1, 0)
  #   IMCI
  PVdata$inService_CHimci <- NA
  PVdata$inService_CHimci <- ifelse(PVdata$w144h %in% 1, 1, 0)
  #   Malaria dx
  PVdata$inService_CHmalariaDx <- NA
  PVdata$inService_CHmalariaDx <- ifelse(PVdata$w144i %in% 1, 1, 0)
  #   Malaria RDT dx
  PVdata$inService_CHmalariaDxRDT <- NA
  PVdata$inService_CHmalariaDxRDT <- ifelse(PVdata$w144j %in% 1, 1, 0)
  #   case management and treatment of malaria
  PVdata$inService_CHmalariaTx <- NA
  PVdata$inService_CHmalariaTx <- ifelse(PVdata$w144k %in% 1 | PVdata$w143g %in% 1, 1, 0)
  #   Pediatric HIV/AIDs
  PVdata$inService_CHhiv <- NA
  PVdata$inService_CHhiv <- ifelse(PVdata$w144l %in% 1, 1, 0)
  #   Pediatric ART
  PVdata$inService_CHart <- NA
  PVdata$inService_CHart <- ifelse(PVdata$w144m %in% 1, 1, 0)
  
  
  
  
  return(PVdata)
}
  

















