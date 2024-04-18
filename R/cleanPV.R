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
  PVdata$year_grad <- ifelse(PVdata$w105 %in% c(0, 9998), NA, PVdata$w105) # 0 is no technical training, can use year started working at health facility as proxy, but AF, HT, MW, NP, and TZ don't have any 0's
  PVdata$year_startFac <- ifelse(PVdata$w106 == 9998, NA, PVdata$w106)
  PVdata$year_interview <- PVdata$w033
  PVdata$years_edu <- ifelse(PVdata$w103a == 99, NA, PVdata$w103a) # w103a    "Total years of primary, secondary and further education"
  
  #   Calculated years
  PVdata$years_atFac <- PVdata$year_interview - PVdata$year_startFac
  PVdata$years_sinceGrad <- PVdata$year_interview - PVdata$year_grad
  
  
  
  # w108     "Hours per week work in facility"
  # label define W108    
  # 99 "Missing"
  PVdata$hoursPerWeek <- (PVdata$w108)
  
  # supervised in past 3 months
  # w111     "Supervision of work"
  # W111    
  # 0 "No"
  # 1 "Yes, past 3 months"
  # 2 "Yes, past 4-6 months"
  # 3 "Yes, past 7-12 months"
  # 4 "Yes, more than 12 months ago"
  
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
  
  
  # w114     "Has written job description"
  # label define W114    
  # 0 "No"
  # 1 "Yes, observed"
  # 2 "Yes, reported"
  PVdata$pv_jobDescription <- NA
  PVdata$pv_jobDescription <- ifelse(PVdata$w114 == 1 | PVdata$w114 == 2, 1, 0)
  
  
  # child health in-service training
  #   Child health in-service: any in-service
  PVdata$inService_CHany <- NA
  PVdata$inService_CHany <- ifelse(PVdata$w144 %in% 1, 1, 0)
  #   Cold chain
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
  
  inService_CH_vars <- c(
    # "inService_CHany",
    "inService_CHcoldChain",
    "inService_CHariDxTx",
    "inService_CHdiarrDxTx",
    "inService_CHnutrition",
    "inService_CHbf",
    "inService_CHinfFeed",
    "inService_CHimci",
    "inService_CHmalariaDx",
    "inService_CHmalariaDxRDT",
    "inService_CHmalariaTx",
    "inService_CHhiv",
    "inService_CHart"
  )
  
  inService_CH_vars_inDF <- c()
  for(service in inService_AN_vars) {
    if (service %in% names(PVdata) & sum(is.na(PVdata[[service]])) < nrow(PVdata)) {
      inService_CH_vars_inDF <- c(inService_CH_vars_inDF, service)
    }
  }
  
  PVdata$inService_CH_count <- NA
  PVdata$inService_CH_count <- rowSums(sf::st_drop_geometry(PVdata)[,inService_CH_vars_inDF], na.rm = T)
  
  PVdata$inService_CH_pct <- NA
  PVdata$inService_CH_pct <- ifelse(PVdata$inService_CH_count > 0, 1, 0)
  
  
  
  # antenatal care in-service training
  #   ANC/PNC in-service: Any
  PVdata$inService_ANany <- NA
  PVdata$inService_ANany <- ifelse(PVdata$w146 %in% 1, 1, 0)
  #   ANC counseling (nutrition/ FP/ newborn care)
  PVdata$inService_ANcounseling <- NA
  PVdata$inService_ANcounseling <- ifelse(PVdata$w146a %in% 1, 1, 0)
  #   ANC screening (blood pressure, urine glucose/ protein)
  PVdata$inService_ANscreening <- NA
  PVdata$inService_ANscreening <- ifelse(PVdata$w146b %in% 1, 1, 0)
  #   Complications of pregnancy and their management
  PVdata$inService_ANcomplications <- NA
  PVdata$inService_ANcomplications <- ifelse(PVdata$w146d %in% 1, 1, 0)
  #   IPT of malaria in pregnancy
  PVdata$inService_ANmalariaIPT <- NA
  PVdata$inService_ANmalariaIPT <- ifelse(PVdata$w146f %in% 1, 1, 0)
  #   Any topic related to HIV/AIDS or PMTCT
  PVdata$inService_ANhivPMTCT <- NA
  PVdata$inService_ANhivPMTCT <- ifelse(PVdata$w146h %in% 1, 1, 0)
  #   Prevention of PMTCT or HIV/AIDS
  PVdata$inService_ANhivPrevention <- NA
  PVdata$inService_ANhivPrevention <- ifelse(PVdata$w146i %in% 1, 1, 0)
  #   Modified obstetric practices as relates to HIV/AIDS
  PVdata$inService_ANobstetricHIV <- NA
  PVdata$inService_ANobstetricHIV <- ifelse(PVdata$w146j %in% 1, 1, 0)
  #   Antiretroviral prophylactic treatment for PMTCT
  PVdata$inService_ANartPMTCT <- NA
  PVdata$inService_ANartPMTCT <- ifelse(PVdata$w146n %in% 1, 1, 0)
  #   Nutritional counseling for newborn of HIV+ mothers
  PVdata$inService_ANnutrHIVnewborn <- NA
  PVdata$inService_ANnutrHIVnewborn <- ifelse(PVdata$w146o %in% 1, 1, 0)
  #   Nutritional assessment of pregnant woman (BMI, M-UACM)
  PVdata$inService_ANnutrAssess <- NA
  PVdata$inService_ANnutrAssess <- ifelse(PVdata$w146r %in% 1, 1, 0)
  #   Infant and young child feeding
  PVdata$inService_ANinfChildFeed <- NA
  PVdata$inService_ANinfChildFeed <- ifelse(PVdata$w146s %in% 1, 1, 0)
  
  inService_AN_vars <- c(
    "inService_ANany",
    "inService_ANcounseling",
    "inService_ANscreening",
    "inService_ANcomplications",
    "inService_ANmalariaIPT",
    "inService_ANhivPMTCT",
    "inService_ANhivPrevention",
    "inService_ANobstetricHIV",
    "inService_ANartPMTCT",
    "inService_ANnutrHIVnewborn",
    "inService_ANnutrAssess",
    "inService_ANinfChildFeed"
  )
  
  inService_AN_vars_inDF <- c()
  for(service in inService_AN_vars) {
    if (service %in% names(PVdata) & sum(is.na(PVdata[[service]])) < nrow(PVdata)) {
      inService_AN_vars_inDF <- c(inService_AN_vars_inDF, service)
    }
  }
  
  PVdata$inService_AN_count <- NA
  PVdata$inService_AN_count <- rowSums(sf::st_drop_geometry(PVdata)[,inService_AN_vars_inDF], na.rm = T)
  
  PVdata$inService_AN_pct <- NA
  PVdata$inService_AN_pct <- ifelse(PVdata$inService_AN_count > 0, 1, 0)
  
  
  return(PVdata)
}
  

















