#' Clean Service Level Data and Calculate Facility-Level Statistics
#'
#' This function processes a dataset (`SLdata`) that contains service level data,
#' ensuring it contains a `svyID` column with identical values across observations.
#' It then computes various facility-level statistics based on the survey ID (svyID)
#' and other criteria defined within the function for multiple survey configurations.
#' The function groups data by `facID` and calculates counts of different provider
#' types and services provided. It merges these counts into a final dataset.
#'
#' @param SLdata A data frame containing service level data. This dataset must
#'   include a column named `svyID` with identical survey identifiers for all
#'   rows, and ideally includes an `inv_id` that can be used as `facID` if `facID`
#'   is not present.
#'
#' @return Returns a data frame with facility-level aggregated statistics.
#'   The return includes columns for medical doctors, nurses, surgeons, and other
#'   specialized services, aggregated by facility. Additionally, it merges count
#'   data on various services provided based on specific criteria.
#'
#' @examples
#' # Assuming SLdata is your dataset loaded with appropriate columns:
#' result <- cleanSL_toFCLevel(SLdata)
#'
#' @importFrom dplyr group_by summarise
#' @export


cleanSL_toFCLevel <- function(SLdata) {
  
  require(dplyr)
  
  # require survey identifier
  if (!"svyID" %in% names(SLdata) | length(unique(SLdata$svyID)) > 1) {
    stop("Must include a column svyID with identical values for all observations")
  }
  
  # make facID variable if there is not one
  if(!"facID" %in% names(SLdata)) {
    SLdata$facID <- SLdata$inv_id
  }
  
  # Afghanistan 2018
  if (SLdata$svyID[1] == "AF_SPA18") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE), # 1 Genearlist, 2 Specialist medical doctor
                nurse_bsn = sum(vu13 %in% c(22), na.rm = TRUE), # does not include registered nurse with diploma, or community nurse
                surgeon = sum((vu13 %in% c(1,2) & vu27 == 1), na.rm = T) # medical doctor that provides surgery
      )
  # Bangladesh 2017
  } else if (SLdata$svyID[1] == "BD_SPA17") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1:6), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(22, 23, 24, 26), na.rm=TRUE), 
                surgeon = sum(vu13 %in% c(1:6) & vu27 == 1, na.rm=TRUE)
      )
  # # Congo Democratic Republic 2017-18
  # } else if (SLdata$svyID[1] == "CD_SPA17") {
  #   SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
  #     summarise(mdoctor = sum(vu13 %in% c(1:6), na.rm = TRUE),
  #               nurse_bsn = sum(vu13 %in% c(22, 23, 24, 26), na.rm=TRUE), 
  #               surgeon = sum(vu13 %in% c(1:6) & vu27 == 1, na.rm=TRUE)
  #     )
  # Ethiopia 2021-22
  } else if (SLdata$svyID[1] == "ET_SPA21") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1:2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(21:23), na.rm=TRUE), # does not include registered nurse with diploma
                surgeon = sum(vu13 %in% c(1:2) & vu27 == 1, na.rm=TRUE)
      )
  # Haiti 2017
  } else if (SLdata$svyID[1] == "HT_SPA17") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(21, 22 ,24), na.rm=TRUE), # does not include auxiliary nurses / medical assistants
                surgeon = sum(vu13 %in% c(1,2) & vu27 == 1, na.rm=TRUE) # medical doctor that provides surgery
      )
    
  # Haiti 2013
  } else if (SLdata$svyID[1] == "HT_SPA13") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2, 3), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(21, 22), na.rm = TRUE), # does not include auxiliary nurses / medical assistants
                surgeon = sum((vu13 %in% c(1,3) & vu27 == 1) | (vu13 %in% c(2)), na.rm=TRUE), # medical doctor that provides surgery
                surgeon2 = sum(vu13 == 2, na.rm = TRUE)
      )
  # Malawi 2013-14
  } else if (SLdata$svyID[1] == "MW_SPA13") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(21, 22), na.rm = TRUE), # does NOT include registered nurse with diploma, enrolled nurse, or psychiatric nurse
                surgeon = sum((vu13 %in% c(1,2) & vu27 == 1), na.rm = T) # medical doctor that provides surgery
      )
  # Namibia 2009 (1)
  } else if (SLdata$svyID[1] == "NM_SPA09") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(4), na.rm = TRUE), # does NOT include registered nurse with diploma, enrolled nurse, or psychiatric nurse
                surgeon = NA # sum((vu13 %in% c(2) & vu27 == 1), na.rm = T) # This questionnaire doesn't ask about providing surgery
      )
  # Nepal 2015
  } else if (SLdata$svyID[1] == "NP_SPA15") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(15), na.rm = TRUE), # Note that nepal has one label: NURSE/AUXILLARY NURSE MIDWIFE (ANM)
                surgeon = sum((vu13 %in% c(1, 2, 3, 4, 5, 6, 8) & vu27 == 1) | (vu13 %in% c(5)), na.rm = T) # medical doctor that provides surgery
      )
  # Nepal 2021
  } else if (SLdata$svyID[1] == "NP_SPA21") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1:6, 8), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(15), na.rm = TRUE), # Note that nepal has one label: NURSE/AUXILLARY NURSE MIDWIFE (ANM)
                surgeon = sum((vu13 %in% c(1:6, 8) & vu27 == 1) | (vu13 %in% c(5)), na.rm = T) # medical doctor that provides surgery
      )
  # Senegal 2012
  } else if (SLdata$svyID[1] == "SN_SPA12") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(21), na.rm = TRUE), # Note that nepal has one label: NURSE/AUXILLARY NURSE MIDWIFE (ANM)
                surgeon = sum((vu13 %in% c(1, 2, 3, 4, 5, 6, 8) & vu27 == 1) | (vu13 %in% c(5)), na.rm = T) # medical doctor that provides surgery
      )
  # Tanzania 2014
  } else if (SLdata$svyID[1] == "TZ_SPA14") {
    SLdata_FCLevel <- SLdata %>% group_by(facID) %>% 
      summarise(mdoctor = sum(vu13 %in% c(1, 2), na.rm = TRUE),
                nurse_bsn = sum(vu13 %in% c(21), na.rm = TRUE), # Includes registered nurse, does not include enrolled nurse
                surgeon = sum((vu13 %in% c(1, 2) & vu27 == 1), na.rm = T) # medical doctor that provides surgery
      )
    
  } else {
      stop("Value in svyID not recognized")
  }
  
  # Count of providers who deliver service
  if (!SLdata$svyID[1] %in% "NM_SPA09") {
    providerCounts <- SLdata %>% group_by(facID) %>%
      summarise(
        pv_svc_art_count = sum(vu14 %in% c(1), na.rm = T), # label variable vu14     "Provides ART"
        pv_svc_hivCareSupport_count = sum(vu15 %in% c(1), na.rm = T), # label variable vu15     "Provides HIV counseling/testing"
        pv_svc_hivTesting_count = sum(vu16 %in% c(1), na.rm = T), # label variable vu16     "Provides HIV related"
        pv_svc_malariaTBSTIDxTx_count = sum(vu15 %in% c(1), na.rm = T), # label variable vu17     "Provides any malaria STI TB dx/tx"
        pv_svc_malariaDxTx_count = sum((vu17a %in% c(1) & !svyID %in% "NM_SPA09") | (svyID %in% "NM_SPA09" & vu17 %in% c(1)), na.rm = T), # label variable vu17a    "Provides malaria dx/tx"
        pv_svc_tbDxTx_count = sum(vu17b %in% c(1), na.rm = T), # label variable vu17b    "Provides TB dx/tx"
        pv_svc_stiDxTx_count = sum(vu17c %in% c(1), na.rm = T), # label variable vu17c    "Provides STI dx/tx"
        pv_svc_nonCommDisease_count = sum(vu17d %in% c(1), na.rm = T), # label variable vu17d    "Provides non-communicable disease dx/tx"
        pv_svc_rh_count = sum(vu18 %in% c(1), na.rm = T), # label variable vu18     "Provides any ANC/FP/Delivery"
        pv_svc_other_count = sum(vu19 %in% c(1), na.rm = T), # label variable vu19     "Provides other client services"
        pv_svc_labDiagnostic_count = sum(vu20 %in% c(1), na.rm = T), # label variable vu20     "Conducts lab tests"
        # pv_XXXX_count = sum(vu21 %in% c(1), na.rm = T), # label variable vu21     "Individual interview attempted"
        pv_svc_antenatalCare_count = sum(vu22 %in% c(1), na.rm = T), # label variable vu22     "Provides ANC care"
        pv_svc_pmtct_count = sum(vu23 %in% c(1), na.rm = T), # label variable vu23     "Provides PMTCT care"
        pv_svc_normalDelivery_count = sum(vu24 %in% c(1), na.rm = T), # label variable vu24     "Provides Delivery care"
        pv_svc_familyPlanning_count = sum(vu25 %in% c(1), na.rm = T), # label variable vu25     "Provides FP care"
        pv_svc_curativeCareChild_count = sum(vu26 %in% c(1), na.rm = T), # label variable vu26     "Provides Child health care"
        pv_svc_surgery_count = sum(vu27 %in% c(1), na.rm = T), # label variable vu27     "Provides surgery"
      )
  } else {
    providerCounts <- SLdata %>% group_by(facID) %>%
      summarise(
        pv_svc_art_count = sum(vu14 %in% c(1), na.rm = T), # label variable vu14     "Provides ART"
        pv_svc_hivCareSupport_count = sum(vu15 %in% c(1), na.rm = T), # label variable vu15     "Provides HIV counseling/testing"
        pv_svc_hivTesting_count = sum(vu16 %in% c(1), na.rm = T), # label variable vu16     "Provides HIV related"
        pv_svc_malariaTBSTIDxTx_count = sum(vu15 %in% c(1), na.rm = T), # label variable vu17     "Provides any malaria STI TB dx/tx"
        pv_svc_malariaDxTx_count = sum(vu17 %in% c(1), na.rm = T), # label variable vu17a    "Provides malaria dx/tx"
        pv_svc_tbDxTx_count = sum(vu17 %in% c(1), na.rm = T), # label variable vu17b    "Provides TB dx/tx"
        pv_svc_stiDxTx_count = sum(vu17 %in% c(1), na.rm = T), # label variable vu17c    "Provides STI dx/tx"
        pv_svc_nonCommDisease_count = NA, # sum(vu17d %in% c(1), na.rm = T), # label variable vu17d    "Provides non-communicable disease dx/tx"
        pv_svc_rh_count = sum(vu18 %in% c(1), na.rm = T), # label variable vu18     "Provides any ANC/FP/Delivery"
        pv_svc_other_count = sum(vu19 %in% c(1), na.rm = T), # label variable vu19     "Provides other client services"
        pv_svc_labDiagnostic_count = sum(vu20 %in% c(1), na.rm = T), # label variable vu20     "Conducts lab tests"
        # pv_XXXX_count = sum(vu21 %in% c(1), na.rm = T), # label variable vu21     "Individual interview attempted"
        pv_svc_antenatalCare_count = NA,# sum(vu22 %in% c(1), na.rm = T), # label variable vu22     "Provides ANC care"
        pv_svc_pmtct_count = NA, # sum(vu23 %in% c(1), na.rm = T), # label variable vu23     "Provides PMTCT care"
        pv_svc_normalDelivery_count = NA, # sum(vu24 %in% c(1), na.rm = T), # label variable vu24     "Provides Delivery care"
        pv_svc_familyPlanning_count = NA, # sum(vu25 %in% c(1), na.rm = T), # label variable vu25     "Provides FP care"
        pv_svc_curativeCareChild_count = NA, # sum(vu26 %in% c(1), na.rm = T), # label variable vu26     "Provides Child health care"
        pv_svc_surgery_count = NA# sum(vu27 %in% c(1), na.rm = T), # label variable vu27     "Provides surgery"
      )
  }
  

  
  if(nrow(SLdata_FCLevel) != nrow(providerCounts)) {
    warning("There seems to be incomplete data for part of the SL data. Double check output before merging.")
  }
  
  SLdata_FCLevel <- merge(SLdata_FCLevel, providerCounts, by = "facID", all.x = T, all.y = T)
  
  return(SLdata_FCLevel)
}




























