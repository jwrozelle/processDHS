#' Aggregate Provider Data to Facility Level
#'
#' This function aggregates provider-level data to the facility level, specifically focusing on the availability of in-service training on child health interventions within health facilities. It checks whether at least one provider within a facility has received training in key child health areas such as cold chain management, diagnosis and treatment of ARI and diarrhea, nutrition, breastfeeding, IMCI, and malaria management among others.
#'
#' @param PVdata A dataframe of provider-level data, presumably processed by `cleanPV()`, containing variables indicating whether each provider has received specific in-service training.
#'
#' @return A dataframe where each row represents a health facility and columns indicate whether any provider within the facility has received training in specific child health interventions. The function calculates aggregated indicators for each facility, such as the presence of any training in areas like breastfeeding support, infant feeding, IMCI, malaria diagnosis and treatment, and management of pediatric HIV/AIDS.
#'
#' @details Before using this function, the input data must be preprocessed with `cleanPV()` to ensure the presence of necessary variables for in-service training. The function iterates over unique facility IDs, checks for the availability of specific types of in-service training among providers at each facility, and aggregates this information to the facility level. This can help in assessing the overall readiness of health facilities to provide comprehensive child health services based on the training of their staff.
#'
#' @examples
#' # Assuming PVdata is your dataset containing provider-level data:
#' # PVdata_processed <- cleanPV(PVdata)
#' # FClvlData <- cleanPV_toFCLevel(PVdata_processed)
#'
#' @importFrom dplyr filter
#' @importFrom iotools fdrbind
#' @export



cleanPV_toFCLevel <- function(PVdata) {
  
  if (!"inService_CHcoldChain" %in% names(PVdata)) {
    stop("PVdata must first be processed with `cleanPV().`")
  }
  
  FClvl.list <- lapply(unique(PVdata$facID), function(facilityID) {
    PV_FC.df <- filter(PVdata, facID == facilityID)
    
    #   Child health in-service: any in-service
    if (sum(PV_FC.df$inService_CHcoldChain, na.rm = T) >= 1) {
      inService_CHcoldChain_any <- 1
    } else {
      inService_CHcoldChain_any <- 0
    }
    #   ARI dx and tx
    if (sum(PV_FC.df$inService_CHariDxTx, na.rm = T) >= 1) {
      inService_CHariDxTx_any <- 1
    } else {
      inService_CHariDxTx_any <- 0
    }
    #   Diarrhea dx and Tx
    if (sum(PV_FC.df$inService_CHdiarrDxTx, na.rm = T) >= 1) {
      inService_CHdiarrDxTx_any <- 1
    } else {
      inService_CHdiarrDxTx_any <- 0
    }
    #   Nutrition Dx and Tx
    if (sum(PV_FC.df$inService_CHnutrition, na.rm = T) >= 1) {
      inService_CHnutrition_any <- 1
    } else {
      inService_CHnutrition_any <- 0
    }
    #   Breastfeeding
    if (sum(PV_FC.df$inService_CHbf, na.rm=TRUE) >= 1) {
      inService_CHbf_any <- 1
    } else {
      inService_CHbf_any <- 0
    }
    #   complementary feeding of infant
    if (sum(PV_FC.df$inService_CHinfFeed, na.rm=TRUE) >= 1) {
      inService_CHinfFeed_any <- 1
    } else {
      inService_CHinfFeed_any <- 0
    }
    #   IMCI
    if (sum(PV_FC.df$inService_CHimci, na.rm=TRUE) >= 1) {
      inService_CHimci_any <- 1
    } else {
      inService_CHimci_any <- 0
    }
    #   Malaria dx
    if (sum(PV_FC.df$inService_CHmalariaDx, na.rm=TRUE) >= 1) {
      inService_CHmalariaDx_any <- 1
    } else {
      inService_CHmalariaDx_any <- 0
    }
    #   Malaria RDT dx
    if (sum(PV_FC.df$inService_CHmalariaDxRDT, na.rm=TRUE) >= 1) {
      inService_CHmalariaDxRDT_any <- 1
    } else {
      inService_CHmalariaDxRDT_any <- 0
    }
    #   case management and treatment of malaria
    if (sum(PV_FC.df$inService_CHmalariaTx, na.rm=TRUE) >= 1) {
      inService_CHmalariaTx_any <- 1
    } else {
      inService_CHmalariaTx_any <- 0
    }
    #   Pediatric HIV/AIDs
    if (sum(PV_FC.df$inService_CHhiv, na.rm=TRUE) >= 1) {
      inService_CHhiv_any <- 1
    } else {
      inService_CHhiv_any <- 0
    }
    #   Pediatric ART
    if (sum(PV_FC.df$inService_CHart, na.rm=TRUE) >= 1) {
      inService_CHart_any <- 1
    } else {
      inService_CHart_any <- 0
    }
    
    # put this into a data frame
    FClvl.df <- data.frame(
      facID = facilityID,
      inService_CHbf_any = inService_CHbf_any, 
      inService_CHinfFeed_any = inService_CHinfFeed_any, 
      inService_CHimci_any = inService_CHimci_any, 
      inService_CHmalariaDx_any = inService_CHmalariaDx_any, 
      inService_CHmalariaDxRDT_any = inService_CHmalariaDxRDT_any, 
      inService_CHmalariaTx_any = inService_CHmalariaTx_any, 
      inService_CHhiv_any = inService_CHhiv_any, 
      inService_CHart_any = inService_CHart_any
    )
    
    return(FClvl.df)
    
  })
  
  PVtoFClvl.df <- iotools::fdrbind(FClvl.list)
  
}