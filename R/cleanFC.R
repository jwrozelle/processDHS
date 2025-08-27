
#' Clean and Process Facility-Level Data
#'
#' This function processes facility-level data by assigning unique facility IDs, categorizing facilities by type and management authority, and calculating various indicators of health service availability and readiness. It supports data from multiple countries with country-specific categorizations for facility types and management authorities. It also computes indicators related to service availability for family planning, antenatal care, child health services, and more.
#'
#' @param FCdata A dataframe containing facility-level data.
#'
#' @return A dataframe with the original data plus new columns for facility IDs, categorized facility types, management authority categories, and indicators of health service availability and readiness. The function adds variables for each facility indicating the presence of various health services, the readiness to provide those services, and categorizations of facilities according to the DHS and country-specific definitions.
#'
#' @details The function first assigns unique IDs to each facility and categorizes facilities according to their type and management authority, with specific categorizations depending on the country. It then computes indicators for the availability and readiness of services such as family planning, antenatal care, child health services, and others. The readiness indicators are based on the presence of staff, guidelines, equipment, diagnostics, and medicines and commodities necessary to provide these services. The function supports data from Afghanistan (AF7), Haiti (HT7), Malawi (MW6), Nepal (NP7), and Tanzania (TZ7), with specific handling for each country's survey data.
#'
#' @examples
#' # Assuming FCdata is your dataset containing facility-level data:
#' # FCdata_processed <- cleanFC(FCdata)
#'
#' @importFrom dplyr filter mutate select
#' @export


cleanFC <- function(FCdata) {
  
  # Facility ID
  FCdata$facID <- FCdata$v004
  
  # Facility type (country specific)
  if (FCdata$v000[1] == "AF7") {
    
    # label define V007    
    # 1 "Regional/ National Hospital"
    # 2 "Provincial Hospital"
    # 3 "Special Hospital"
    # 4 "Private Hospital"
    # 5 "Private Clinic"
    
    FCdata$type_regHosp <- ifelse(FCdata$v007 %in% 1, 1, 0)
    FCdata$type_provHosp <- ifelse(FCdata$v007 %in% 2, 1, 0)
    FCdata$type_specialHosp <- ifelse(FCdata$v007 %in% 3, 1, 0)
    FCdata$type_privHosp <- ifelse(FCdata$v007 %in% 4, 1, 0)
    FCdata$type_privClinic <- ifelse(FCdata$v007 %in% 5, 1, 0)
    
    # single variable
    FCdata$type_category <- NA
    FCdata$type_category <- ifelse(FCdata$type_regHosp == 1, "regHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_provHosp == 1, "provHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_specialHosp == 1, "specialHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_privHosp == 1, "privHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_privClinic == 1, "privClinic", FCdata$type_category)
    
    
    # # Other country types
    # FCdata$type_uniHosp <- NA
    # FCdata$type_deptHosp <- NA
    # FCdata$type_commRefHosp <- NA
    # FCdata$type_hospOther <- NA
    # FCdata$type_healthCentLit <- NA
    # FCdata$type_healthCenterNoLit <- NA
    # FCdata$type_disp <- NA
    
    # DHS Categories NOTE!!! This is not a dhs category since there is no report, but I'm writing it anyway for consistent variable names
    
    FCdata$typeDHS_hospital <- ifelse(FCdata$v007 %in% c(1,2,3,4), 1, 0)
    FCdata$typeDHS_clinic <- ifelse(FCdata$v007 %in% c(5), 1, 0)
    
    FCdata$typeDHS_category <- NA
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_hospital == 1, "hospital", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_clinic == 1, "clinic", FCdata$typeDHS_category)
    
    
  } else if (FCdata$v000[1] == "HT7") {
    # label define V007    
      # 1 "University hospital"
      # 2 "Department hospital"
      # 3 "Community reference hospital"
      # 4 "Other hospitals"
      # 5 "Health Center with lit"
      # 6 "Health Center without lit"
      # 7 "Dispensary/Community Health Center"
    
    
    FCdata$type_uniHosp <- ifelse(FCdata$v007 %in% c(1), 1, 0)
    FCdata$type_deptHosp <- ifelse(FCdata$v007 %in% c(2), 1, 0)
    FCdata$type_commRefHosp <- ifelse(FCdata$v007 %in% c(3), 1, 0)
    FCdata$type_hospOther <- ifelse(FCdata$v007 %in% c(4), 1, 0)
    FCdata$type_healthCentLit <- ifelse(FCdata$v007 %in% c(5), 1, 0)
    FCdata$type_healthCenterNoLit <- ifelse(FCdata$v007 %in% c(6), 1, 0)
    FCdata$type_disp <- ifelse(FCdata$v007 %in% c(7), 1, 0)
    
    FCdata$type_category <- NA
    FCdata$type_category <- ifelse(FCdata$type_uniHosp == 1, "uniHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_deptHosp == 1, "deptHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_commRefHosp == 1, "commRefHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_hospOther == 1, "hospOther", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_healthCentLit == 1, "healthCentLit", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_healthCenterNoLit == 1, "healthCenterNoLit", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_disp == 1, "disp", FCdata$type_category)
    
    
    # FCdata$type_regHosp <- NA
    # FCdata$type_provHosp <- NA
    # FCdata$type_specialHosp <- NA
    # FCdata$type_privHosp <- NA
    # FCdata$type_privClinic <- NA
    
    # DHS categories
    #   Hospital (131/134)
    #   Health center with bed (163/165) 
    #   Health center without bed (361/375)
    #   Dispensary CCS (352/359)
    
    FCdata$typeDHS_hospital <- ifelse(FCdata$v007 %in% c(1,2,3,4), 1, 0)
    FCdata$typeDHS_hcBed <- ifelse(FCdata$v007 %in% c(5), 1, 0)
    FCdata$typeDHS_hcNoBed <- ifelse(FCdata$v007 %in% c(6), 1, 0)
    FCdata$typeDHS_dispCCS <- ifelse(FCdata$v007 %in% c(7), 1, 0)
    
    FCdata$typeDHS_category <- NA
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_hospital == 1, "hospital", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_hcBed == 1, "hcBed", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_hcNoBed == 1, "hcNoBed", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_dispCCS == 1, "dispCCS", FCdata$typeDHS_category)
    
    
  } else if (FCdata$v000[1] == "MW6") {
    # label define V007    
    # 1 "Central Hospital"
    # 2 "District Hospital"
    # 3 "Rural/Community Hospital"
    # 4 "Other Hospital"
    # 5 "Health Centre"
    # 6 "Maternity"
    # 7 "Dispensary"
    # 8 "Clinic"
    # 9 "Health Post"
    
    FCdata$type_centralHosp <- ifelse(FCdata$v007 %in% c(1), 1, 0)
    FCdata$type_distHosp <- ifelse(FCdata$v007 %in% c(2), 1, 0)
    FCdata$type_commHosp <- ifelse(FCdata$v007 %in% c(3), 1, 0)
    FCdata$type_hospOther <- ifelse(FCdata$v007 %in% c(4), 1, 0)
    FCdata$type_healthCenter <- ifelse(FCdata$v007 %in% c(5), 1, 0)
    FCdata$type_maternity <- ifelse(FCdata$v007 %in% c(6), 1, 0)
    FCdata$type_disp <- ifelse(FCdata$v007 %in% c(7), 1, 0)
    FCdata$type_clinic <- ifelse(FCdata$v007 %in% c(8), 1, 0)
    FCdata$type_healthPost <- ifelse(FCdata$v007 %in% c(9), 1, 0)
    
    FCdata$type_category <- NA
    FCdata$type_category <- ifelse(FCdata$type_centralHosp == 1, "centralHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_distHosp == 1, "distHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_commHosp == 1, "commHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_hospOther == 1, "hospOther", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_healthCenter == 1, "healthCenter", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_maternity == 1, "maternity", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_disp == 1, "disp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_clinic == 1, "clinic", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_healthPost == 1, "healthPost", FCdata$type_category)
    
    
    # # Other country types
    # FCdata$type_uniHosp <- NA
    # FCdata$type_deptHosp <- NA
    # FCdata$type_commRefHosp <- NA
    # FCdata$type_hospOther <- NA
    # FCdata$type_healthCentLit <- NA
    # FCdata$type_healthCenterNoLit <- NA
    # FCdata$type_disp <- NA
    # FCdata$type_regHosp <- NA
    # FCdata$type_provHosp <- NA
    # FCdata$type_specialHosp <- NA
    # FCdata$type_privHosp <- NA
    # FCdata$type_privClinic <- NA
    
    # DHS Categories
    #   Hospital
    #   Health centre
    #   Dispensary
    #   Clinic
    #   Health post
    
    FCdata$typeDHS_hospital <- ifelse(FCdata$v007 %in% c(1,2,3,4), 1, 0)
    FCdata$typeDHS_healthCentre <- ifelse(FCdata$v007 %in% c(5, 6), 1, 0)
    FCdata$typeDHS_disp <- ifelse(FCdata$v007 %in% 7, 1, 0)
    FCdata$typeDHS_clinic <- ifelse(FCdata$v007 %in% c(8), 1, 0)
    FCdata$typeDHS_healthPost <- ifelse(FCdata$v007 %in% c(9), 1, 0)
    
    FCdata$typeDHS_category <- NA
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_hospital == 1, "hospital", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_healthCentre == 1, "healthCentre", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_disp == 1, "dispensary", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_clinic == 1, "clinic", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_healthPost == 1, "healthPost", FCdata$typeDHS_category)
    
    
  } else if (FCdata$v000[1] == "NP7") {
    # label define V007    
    # 1 "Central Govt. Hospital"
    # 2 "Regional Govt. Hospital"
    # 3 "Sub-regional Govt. Hospital"
    # 4 "Zonal Govt. Hospital"
    # 5 "District Govt. Hospital"
    # 6 "Other Hospital (Not state-owned)"
    # 7 "Primary Health Care Center (PHCC)"
    # 8 "Health Post (HP)"
    # 9 "Sub-health post (SHP)"
    # 10 "Urban Health Centre (UHC)"
    # 11 "HTC (stand alone)"
    # 12 "Other Public Hospital"
    # 13 "Central Level Govt. Hospital"
    # 14 "District Level Govt. Hospital"
    
    FCdata$type_centralGovHosp <- ifelse(FCdata$v007 %in% c(1, 13), 1, 0)
    FCdata$type_regGovHosp <- ifelse(FCdata$v007 %in% c(2), 1, 0)
    FCdata$type_subRegGovHosp <- ifelse(FCdata$v007 %in% c(3), 1, 0)
    FCdata$type_zoneGovHosp <- ifelse(FCdata$v007 %in% c(4), 1, 0)
    FCdata$type_distGovHosp <- ifelse(FCdata$v007 %in% c(5, 14), 1, 0)
    FCdata$type_nonGovHosp <- ifelse(FCdata$v007 %in% c(6), 1, 0)
    FCdata$type_primaryhcc <- ifelse(FCdata$v007 %in% c(7), 1, 0)
    FCdata$type_healthPost <- ifelse(FCdata$v007 %in% c(8), 1, 0)
    FCdata$type_subHealthPost <- ifelse(FCdata$v007 %in% c(9), 1, 0)
    FCdata$type_UHC <- ifelse(FCdata$v007 %in% c(10), 1, 0)
    FCdata$type_HTC <- ifelse(FCdata$v007 %in% c(11), 1, 0)
    FCdata$type_otherPubHosp <- ifelse(FCdata$v007 %in% c(12), 1, 0)
    
    FCdata$type_category <- NA
    FCdata$type_category <- ifelse(FCdata$type_centralGovHosp == 1, "centralGovHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_regGovHosp == 1, "regGovHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_subRegGovHosp == 1, "subRegGovHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_zoneGovHosp == 1, "zoneGovHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_distGovHosp == 1, "distGovHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_nonGovHosp == 1, "nonGovHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_primaryhcc == 1, "primaryhcc", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_healthPost == 1, "healthPost", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_subHealthPost == 1, "subHealthPost", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_UHC == 1, "UHC", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_HTC == 1, "HTC", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_otherPubHosp == 1, "otherPubHosp", FCdata$type_category)
    
    
    # DHS Categories
    #   Zonal and above hospitals
    #   District-level hospitals
    #   Private Hospitals
    #   PHCCs
    #   HPs
    #   UHCs
    
    
    FCdata$typeDHS_hospZonePlus <- ifelse(FCdata$v007 %in% c(1,13, 2, 3, 4), 1, 0)
    FCdata$typeDHS_hospDistrict <- ifelse(FCdata$v007 %in% c(5, 14), 1, 0)
    FCdata$typeDHS_hospPrivate <- ifelse(FCdata$v007 %in% c(6), 1, 0)
    FCdata$typeDHS_phcc <- ifelse(FCdata$v007 %in% c(7), 1, 0)
    FCdata$typeDHS_hp <- ifelse(FCdata$v007 %in% c(8,9), 1, 0)
    FCdata$typeDHS_uhc <- ifelse(FCdata$v007 %in% c(10), 1, 0)
    FCdata$typeDHS_htc <- ifelse(FCdata$v007 %in% c(11), 1, 0)
    
    
    FCdata$typeDHS_category <- NA
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_hospZonePlus == 1, "hospZonePlus", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_hospDistrict == 1, "hospDistrict", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_hospPrivate == 1, "hospPrivate", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_phcc == 1, "phcc", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_hp == 1, "hp", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_uhc == 1, "uhc", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_htc == 1, "htc", FCdata$typeDHS_category)
    
    
  } else if (FCdata$v000[1] == "TZ7") {
    
    # label define V007    
    # 1 "National Referral Hospital"
    # 2 "Regional Hospital"
    # 3 "District Hospital"
    # 4 "District-Designated Hospital"
    # 5 "Other Hospital (Private)"
    # 6 "Health Centre"
    # 7 "Clinic"
    # 8 "Dispensary"
    
    FCdata$type_nationalRefHosp <- ifelse(FCdata$v007 %in% c(1), 1, 0)
    FCdata$type_regHosp <- ifelse(FCdata$v007 %in% c(2), 1, 0)
    FCdata$type_distHosp <- ifelse(FCdata$v007 %in% c(3), 1, 0)
    FCdata$type_distDesigHosp <- ifelse(FCdata$v007 %in% c(4), 1, 0)
    FCdata$type_otherHosp <- ifelse(FCdata$v007 %in% c(5), 1, 0)
    FCdata$type_healthCentre <- ifelse(FCdata$v007 %in% c(6), 1, 0)
    FCdata$type_clinic <- ifelse(FCdata$v007 %in% c(7), 1, 0)
    FCdata$type_disp <- ifelse(FCdata$v007 %in% c(8), 1, 0)
    
    
    FCdata$type_category <- NA
    FCdata$type_category <- ifelse(FCdata$type_nationalRefHosp == 1, "nationalRefHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_regHosp == 1, "regHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_distHosp == 1, "distHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_distDesigHosp == 1, "distDesigHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_otherHosp == 1, "otherHosp", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_healthCentre == 1, "healthCentre", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_clinic == 1, "clinic", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_disp == 1, "disp", FCdata$type_category)
    
    # DHS categories for facility type
    #   Hospital
    #   Health Centre
    #   Dispensary
    #   Clinic
    
    FCdata$typeDHS_hospital <- ifelse(FCdata$v007 %in% c(1,2,3,4,5), 1, 0)
    FCdata$typeDHS_healthCentre <- ifelse(FCdata$v007 %in% c(6), 1, 0)
    FCdata$typeDHS_disp <- ifelse(FCdata$v007 %in% c(8), 1, 0)
    FCdata$typeDHS_clinic <- ifelse(FCdata$v007 %in% c(7), 1, 0)
    
    FCdata$typeDHS_category <- NA
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_hospital == 1, "hospital", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_healthCentre == 1, "healthCentre", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_disp == 1, "disp", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_clinic == 1, "clinic", FCdata$typeDHS_category)
    
  } else if (FCdata$v000[1] == "ET8") {    

    # 1 "Referral hospital"
    # 2 "General hospital"
    # 3 "Primary hospital"
    # 4 "Health center"
    # 5 "Health post"
    # 6 "Higher clinic"
    # 7 "Medium clinic"
    # 8 "Lower clinic"
    # 9 "Specialty clinic"
    
    ## Specific type dummies
    FCdata$type_refHosp    <- ifelse(FCdata$v007 %in% 1, 1, 0)
    FCdata$type_genHosp    <- ifelse(FCdata$v007 %in% 2, 1, 0)
    FCdata$type_primHosp   <- ifelse(FCdata$v007 %in% 3, 1, 0)
    FCdata$type_healthCent <- ifelse(FCdata$v007 %in% 4, 1, 0)
    FCdata$type_healthPost <- ifelse(FCdata$v007 %in% 5, 1, 0)
    FCdata$type_highClinic <- ifelse(FCdata$v007 %in% 6, 1, 0)
    FCdata$type_medClinic  <- ifelse(FCdata$v007 %in% 7, 1, 0)
    FCdata$type_lowClinic  <- ifelse(FCdata$v007 %in% 8, 1, 0)
    FCdata$type_specClinic <- ifelse(FCdata$v007 %in% 9, 1, 0)
    
    ## Single convenience string
    FCdata$type_category <- NA
    FCdata$type_category <- ifelse(FCdata$type_refHosp == 1,    "refHosp",    FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_genHosp == 1,    "genHosp",    FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_primHosp == 1,   "primHosp",   FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_healthCent == 1, "healthCent", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_healthPost == 1, "healthPost", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_highClinic == 1, "highClinic", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_medClinic == 1,  "medClinic",  FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_lowClinic == 1,  "lowClinic",  FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_specClinic == 1, "specClinic", FCdata$type_category)
    
    ## DHS-style categories (8 groups, per your mapping)
    FCdata$typeDHS_refHosp          <- ifelse(FCdata$v007 %in% 1,       1, 0)
    FCdata$typeDHS_genHosp          <- ifelse(FCdata$v007 %in% 2,       1, 0)
    FCdata$typeDHS_primHosp         <- ifelse(FCdata$v007 %in% 3,       1, 0)
    FCdata$typeDHS_healthCentre     <- ifelse(FCdata$v007 %in% 4,       1, 0)
    FCdata$typeDHS_healthPost       <- ifelse(FCdata$v007 %in% 5,       1, 0)
    FCdata$typeDHS_specHighClinic   <- ifelse(FCdata$v007 %in% c(6, 9), 1, 0)  # Specialty/higher clinic
    FCdata$typeDHS_medClinic        <- ifelse(FCdata$v007 %in% 7,       1, 0)
    FCdata$typeDHS_lowClinic        <- ifelse(FCdata$v007 %in% 8,       1, 0)
    
    FCdata$typeDHS_category <- NA
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_refHosp == 1,        "Referral hospital",           FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_genHosp == 1,        "General hospital",            FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_primHosp == 1,       "Primary hospital",            FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_healthCentre == 1,   "Health centre",               FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_healthPost == 1,     "Health post",                 FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_specHighClinic == 1, "Specialty/higher clinic",     FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_medClinic == 1,      "Medium clinic",               FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_lowClinic == 1,      "Lower clinic",                FCdata$typeDHS_category)
    
    
  } else if (FCdata$v000[1] == "BD7") {    
    
    # label define V007    
    # 1 "District Hospital (DH)"
    # 2 "Upazila Health Complex (UHC)"
    # 3 "Mother and Child Welfare Center (MCWC)"
    # 4 "Union Health and Family Welfare Center (UnHFWC)"
    # 5 "Union Health and Family Welfare Center (UnHFWC - Upgraded)"
    # 6 "Union Subcenter (UnSC) / Rural Dispensary"
    # 8 "Community Clinic"
    # 9 "NGO Clinic (other than Smiling Sun Clinics)"
    # 10 "Private Hospital (with >20 beds)"
    # 11 "NGO Hospital"
    # 12 "Smiling Sun Clinic of NGO Health Service Delivery Project (NHSDP)"
    
    
    ## specific type dummies
    FCdata$type_dh     <- ifelse(FCdata$v007 %in% 1, 1, 0)
    FCdata$type_uhc    <- ifelse(FCdata$v007 %in% 2, 1, 0)
    FCdata$type_mcwc   <- ifelse(FCdata$v007 %in% 3, 1, 0)
    FCdata$type_uhfwc  <- ifelse(FCdata$v007 %in% c(4,5), 1, 0)
    FCdata$type_usc    <- ifelse(FCdata$v007 %in% 6, 1, 0)
    FCdata$type_cc     <- ifelse(FCdata$v007 %in% 8, 1, 0)
    FCdata$type_ngocln <- ifelse(FCdata$v007 %in% 9, 1, 0)
    FCdata$type_privh  <- ifelse(FCdata$v007 %in% 10, 1, 0)
    FCdata$type_ngoh   <- ifelse(FCdata$v007 %in% 11, 1, 0)
    FCdata$type_suncln <- ifelse(FCdata$v007 %in% 12, 1, 0)
    
    ## single string
    FCdata$type_category <- NA
    FCdata$type_category <- ifelse(FCdata$type_dh == 1,     "dh",     FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_uhc == 1,    "uhc",    FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_mcwc == 1,   "mcwc",   FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_uhfwc == 1,  "uhfwc",  FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_usc == 1,    "usc",    FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_cc == 1,     "cc",     FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_ngocln == 1, "ngocln", FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_privh == 1,  "privh",  FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_ngoh == 1,   "ngoh",   FCdata$type_category)
    FCdata$type_category <- ifelse(FCdata$type_suncln == 1, "suncln", FCdata$type_category)
    
    
    # DHS Categories
    # District and upazila public facilities
    #   DH
    #   MCWC
    #   UHC 
    # Union-level public facilities 
    #   UHFWC 
    #   USC/RD 
    # Community clinic (CC) 
    # NGO clinic/hospital 
    # Private hospital
    
    ## DHS groupings (short single-word labels)
    ## DHS categories (granular, single-word)
    #     District and upazila public facilities
    FCdata$typeDHS_dh     <- ifelse(FCdata$v007 %in% 1,          1, 0)       # District Hospital
    FCdata$typeDHS_mcwc   <- ifelse(FCdata$v007 %in% 3,          1, 0)       # MCWC
    FCdata$typeDHS_uhc    <- ifelse(FCdata$v007 %in% 2,          1, 0)       # UHC
    #     Union-level public facilities
    FCdata$typeDHS_uhfwc  <- ifelse(FCdata$v007 %in% c(4, 5),    1, 0)       # UHFWC (std + upgraded)
    FCdata$typeDHS_uscrd  <- ifelse(FCdata$v007 %in% 6,          1, 0)       # USC/RD
    # community clinic
    FCdata$typeDHS_cc     <- ifelse(FCdata$v007 %in% 8,          1, 0)       # Community clinic
    # NGO clinic/hospital
    FCdata$typeDHS_ngoclnHosp <- ifelse(FCdata$v007 %in% c(9,11,12), 1, 0)       # NGO clinic/hospital (+ Smiling Sun)
    # Private hospital
    FCdata$typeDHS_privh  <- ifelse(FCdata$v007 %in% 10,         1, 0)       # Private hospital
    
    FCdata$typeDHS_category <- NA
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_dh     == 1, "dh",     FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_mcwc   == 1, "mcwc",   FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_uhc    == 1, "uhc",    FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_uhfwc  == 1, "uhfwc",  FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_uscrd  == 1, "uscrd",  FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_cc     == 1, "cc",     FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_ngoclnHosp == 1, "ngoclnHosp", FCdata$typeDHS_category)
    FCdata$typeDHS_category <- ifelse(FCdata$typeDHS_privh  == 1, "privh",  FCdata$typeDHS_category)
    
    
    
  } else {
    stop(paste0("There is a problem with health facility type for survey ", FCdata$v000[1]))
  }
  
  # Initialize type_h with all levels as a factor
  FCdata$type_h <- factor(NA, levels = c(0, 1, 2), 
                          labels = c("Hospital", "Health centre", "Health post/dispensary"))
  
  # Hospitals (0)
  FCdata$type_h <- ifelse(is.na(FCdata$type_h) & FCdata$type_category %in% c(
    "regHosp", 
    "provHosp", 
    "specialHosp", 
    "privHosp", 
    "uniHosp", 
    "deptHosp", 
    "commRefHosp", 
    "hospOther", 
    "centralHosp", 
    "distHosp", 
    "commHosp", 
    "nonGovHosp", 
    "centralGovHosp", 
    "regGovHosp", 
    "subRegGovHosp", 
    "zoneGovHosp", 
    "distGovHosp", 
    "otherPubHosp", 
    "nationalRefHosp", 
    "distDesigHosp", 
    "otherHosp",
    # Add ethiopia
    "refHosp",
    "genHosp",
    "primHosp",
    # Bangladesh
    "dh", "uhc", "privh", "ngoh"
  ), 0, FCdata$type_h)
  
  # Health centres (1)
  FCdata$type_h <- ifelse(is.na(FCdata$type_h) & FCdata$type_category %in% c(
    "privClinic", 
    "healthCentLit", 
    "healthCenterNoLit", 
    "healthCenter", 
    "maternity", 
    "primaryhcc", 
    "healthCentre", 
    "UHC",
    # Ethiopia
    "healthCent",
    "highClinic",
    "medClinic",
    "lowClinic",
    # Bangladesh
    "uhfwc", "usc", "mcwc", "ngocln", "suncln"
  ), 1, FCdata$type_h)
  
  # Health posts and dispensaries (2)
  FCdata$type_h <- ifelse(is.na(FCdata$type_h) & FCdata$type_category %in% c(
    "disp", 
    "healthPost", 
    "subHealthPost", 
    "dispCCS", 
    "clinic", 
    "hp",
    "htc",
    "HTC",
    # Bangladesh
    "cc"
  ), 2, FCdata$type_h)
  
  # Convert type_h into a factor with levels 0-2 and appropriate labels
  FCdata$type_h <- factor(FCdata$type_h, 
                          levels = c(0, 1, 2),
                          labels = c("Hospital", "Health centre", "Health post/dispensary"))
  
  
  # managing authority (country specific)
  if (FCdata$v000[1] == "AF7") {
    
    # label define V008    
    # 1 "Government/public"
    # 2 "Private not-for-profit"
    # 3 "Private for-profit"
    
    FCdata$auth_govPublic <- ifelse(FCdata$v008 %in% 1, 1, 0)
    FCdata$auth_privateNonProfit <- ifelse(FCdata$v008 %in% 2, 1, 0)
    FCdata$auth_privateProfit <- ifelse(FCdata$v008 %in% 3, 1, 0)
    
    FCdata$auth_category <- NA
    FCdata$auth_category <- ifelse(FCdata$auth_govPublic == 1, "govPublic", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_privateNonProfit == 1, "privateNonProfit", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_privateProfit == 1, "privateProfit", FCdata$auth_category)
    
    
  } else if (FCdata$v000[1] == "HT7") {
    FCdata$auth_govPublic <- ifelse(FCdata$v008 %in% 1, 1, 0)
    FCdata$auth_privateNonProfit <- ifelse(FCdata$v008 %in% 2, 1, 0)
    FCdata$auth_privateProfit <- ifelse(FCdata$v008 %in% 3, 1, 0)
    FCdata$auth_mixed <- ifelse(FCdata$v008 %in% 4, 1, 0)
    
    
    FCdata$auth_category <- NA
    FCdata$auth_category <- ifelse(FCdata$auth_govPublic == 1, "govPublic", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_privateNonProfit == 1, "privateNonProfit", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_privateProfit == 1, "privateProfit", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_mixed == 1, "mixed", FCdata$auth_category)
    
    
  } else if (FCdata$v000[1] == "MW6") {
    
    # label define V008    
    # 1 "Government/public"
    # 2 "Christian Health Association of Malawi (CHAM)"
    # 3 "Private for profit"
    # 4 "Mission/ Faith-based (other than CHAM)"
    # 5 "NGO"
    # 6 "Company"
    
    FCdata$auth_govPublic <- ifelse(FCdata$v008 %in% 1, 1, 0)
    FCdata$auth_CHAM <- ifelse(FCdata$v008 %in% 2, 1, 0)
    FCdata$auth_privateProfit <- ifelse(FCdata$v008 %in% 3, 1, 0)
    FCdata$auth_faith <- ifelse(FCdata$v008 %in% 4, 1, 0)
    FCdata$auth_ngo <- ifelse(FCdata$v008 %in% 5, 1, 0)
    FCdata$auth_company <- ifelse(FCdata$v008 %in% 6, 1, 0)
    
    FCdata$auth_category <- NA
    FCdata$auth_category <- ifelse(FCdata$auth_govPublic == 1, "govPublic", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_CHAM == 1, "CHAM", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_privateProfit == 1, "privateProfit", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_faith == 1, "faith", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_ngo == 1, "ngo", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_company == 1, "company", FCdata$auth_category)
    
    
  } else if (FCdata$v000[1] == "NP7") {
    
    # label define V008    
    # 1 "Government/Public"
    # 2 "NGO/Private not-for-profit"
    # 3 "Private for profit"
    # 4 "Mission/ Faith-based"
    
    FCdata$auth_govPublic <- ifelse(FCdata$v008 %in% 1, 1, 0)
    FCdata$auth_privateNonProfit <- ifelse(FCdata$v008 %in% 2, 1, 0)
    FCdata$auth_privateProfit <- ifelse(FCdata$v008 %in% 3, 1, 0)
    FCdata$auth_faith <- ifelse(FCdata$v008 %in% 4, 1, 0)
    
    FCdata$auth_category <- NA
    FCdata$auth_category <- ifelse(FCdata$auth_govPublic == 1, "govPublic", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_privateNonProfit == 1, "privateNonProfit", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_privateProfit == 1, "privateProfit", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_faith == 1, "faith", FCdata$auth_category)
    
    
  } else if (FCdata$v000[1] == "TZ7") {
    
    # 1 "Government/public"
    # 2 "Private-for-profit"
    # 3 "Mission/ faith-based"
    # 4 "Parastatal (originally other)"
    
    FCdata$auth_govPublic <- ifelse(FCdata$v008 %in% 1, 1, 0)
    FCdata$auth_privateProfit <- ifelse(FCdata$v008 %in% 2, 1, 0)
    FCdata$auth_faith <- ifelse(FCdata$v008 %in% 3, 1, 0)
    FCdata$auth_parastatal <- ifelse(FCdata$v008 %in% 4, 1, 0)
    
    FCdata$auth_category <- NA
    FCdata$auth_category <- ifelse(FCdata$auth_govPublic == 1, "govPublic", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_privateProfit == 1, "privateProfit", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_faith == 1, "faith", FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_parastatal == 1, "parastatal", FCdata$auth_category)
    
  } else if (FCdata$v000[1] == "ET8") {  
    # label define V008    
    # 1 "Government/Public"
    # 2 "Other governmental (military, prison, federal police)"
    # 3 "Private for profit"
    # 4 "NGO (Mission/Faith-based, non profit)"
    
    
    FCdata$auth_govPublic     <- ifelse(FCdata$v008 %in% 1, 1, 0)
    FCdata$auth_otherGov      <- ifelse(FCdata$v008 %in% 2, 1, 0)
    FCdata$auth_privateProfit <- ifelse(FCdata$v008 %in% 3, 1, 0)
    FCdata$auth_ngoFaith         <- ifelse(FCdata$v008 %in% 4, 1, 0)   # recode ngoFaith → faith
    
    FCdata$auth_category <- NA
    FCdata$auth_category <- ifelse(FCdata$auth_govPublic     == 1, "govPublic",    FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_otherGov      == 1, "otherGov",     FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_privateProfit == 1, "privateProfit",FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_ngoFaith         == 1, "ngoFaith",        FCdata$auth_category)
    
  } else if (FCdata$v000[1] == "BD7") {  
    # label define V008    
    # 1 "Government/Public (MOHFW)"
    # 2 "Local Government"
    # 3 "NGO"
    # 4 "Private for Profit"
    
    FCdata$auth_govPublic   <- ifelse(FCdata$v008 %in% 1, 1, 0)
    FCdata$auth_localGov    <- ifelse(FCdata$v008 %in% 2, 1, 0)
    FCdata$auth_ngo         <- ifelse(FCdata$v008 %in% 3, 1, 0)
    FCdata$auth_privateProfit <- ifelse(FCdata$v008 %in% 4, 1, 0)
    
    FCdata$auth_category <- NA
    FCdata$auth_category <- ifelse(FCdata$auth_govPublic   == 1, "govPublic",     FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_localGov    == 1, "localGov",      FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_ngo         == 1, "ngo",           FCdata$auth_category)
    FCdata$auth_category <- ifelse(FCdata$auth_privateProfit == 1, "privateProfit", FCdata$auth_category)
    
    
  } else {
    stop(paste0("There is a problem with health facility type for survey ", FCdata$v000[1]))
  }
  
  # Harmonize managing authority categories into auth_h using auth_category
  FCdata$auth_h <- factor(NA, 
                          levels = c(0, 1, 2),
                          labels = c("Public", "Private not-for-profit", "Private for-profit"))
  
  # Public (0)
  FCdata$auth_h <- ifelse(is.na(FCdata$auth_h) & FCdata$auth_category %in% c(
    "govPublic", 
    "parastatal",
    "otherGov",
    "localGov"   # Bangladesh
  ), 0, FCdata$auth_h)
  
  # Private not-for-profit (1)
  FCdata$auth_h <- ifelse(is.na(FCdata$auth_h) & FCdata$auth_category %in% c(
    "privateNonProfit", 
    "CHAM", 
    "faith", 
    "ngo", 
    "mixed",
    "ngoFaith" # Ethiopia
  ), 1, FCdata$auth_h)
  
  # Private for-profit (2)
  FCdata$auth_h <- ifelse(is.na(FCdata$auth_h) & FCdata$auth_category %in% c(
    "privateProfit", 
    "company"
  ), 2, FCdata$auth_h)
  
  # Convert auth_h into a factor with levels 0-2 and appropriate labels
  FCdata$auth_h <- factor(FCdata$auth_h, 
                          levels = c(0, 1, 2),
                          labels = c("Public", "Private not-for-profit", "Private for-profit"))
  
  
  # Rural
  FCdata$rural <- NA
  FCdata$rural <- ifelse(FCdata$v003 == 2, 1, FCdata$rural)
  FCdata$rural <- ifelse(FCdata$v003 == 1, 0, FCdata$rural)
  
  # LEVEL A
  # Qualified medical providers
  FCdata$qualified2 <- NA
  FCdata$qualified2 <- ifelse((FCdata$nurse_bsn + FCdata$mdoctor) >= 2, 1, 0)
  FCdata$qualified2 <- ifelse(is.na(FCdata$qualified2), 0, FCdata$qualified2)
  
  # number of overnight beds
  FCdata$onBedsN <- NA
  FCdata$onBedsN <- FCdata$v143
  FCdata$onBedsN <- ifelse(FCdata$onBedsN == 9998, 0, FCdata$onBedsN) # !!! unknown counted as 0
  FCdata$onBedsN <- ifelse(is.na(FCdata$onBedsN), 0, FCdata$onBedsN) # !!! missing counted as 0
  # Maternity beds
  FCdata$mtBedsN <- NA
  FCdata$mtBedsN <- FCdata$v501
  FCdata$mtBedsN <- ifelse(FCdata$mtBedsN == 998, 0, FCdata$mtBedsN) # !!! unknown counted as 0
  FCdata$mtBedsN <- ifelse(is.na(FCdata$mtBedsN), 0, FCdata$mtBedsN) # !!! missing counted as 0
  # Maternity beds
  FCdata$dlBedsN <- NA
  FCdata$dlBedsN <- FCdata$v501a
  FCdata$dlBedsN <- ifelse(FCdata$dlBedsN == 998, 0, FCdata$dlBedsN) # !!! unknown counted as 0
  FCdata$dlBedsN <- ifelse(is.na(FCdata$dlBedsN), 0, FCdata$dlBedsN) # !!! missing counted as 0
  
  # beds 2
  FCdata$onBedsN2 <- FCdata$onBedsN + FCdata$mtBedsN
  # beds 4
  FCdata$onBedsN2 <- FCdata$onBedsN + FCdata$mtBedsN + FCdata$dlBedsN
  
  # 24 hour duty schedule
  FCdata$sched24 <- NA
  FCdata$sched24 <- ifelse(FCdata$v503 %in% c(1,3), 1, 0) # count if observed or reported
  FCdata$sched24 <- ifelse(is.na(FCdata$sched24), 0, FCdata$sched24)
  
  # water # Must be observed available
  
    # label define V264A1  
    # 0 "Not available"
    # 1 "Yes, observed"
    # 2 "Yes, reported"
  
  FCdata$water <- NA
  FCdata$water <- ifelse(FCdata$v124 %in% c(1) | 
                           FCdata$v168a1 %in% c(1) | 
                           FCdata$v231a1 %in% c(1) | 
                           FCdata$v264a1 %in% c(1) | 
                           FCdata$v331a1 %in% c(1) | 
                           FCdata$v431a1 %in% c(1) | 
                           FCdata$v531a1 %in% c(1) | 
                           FCdata$v631a1 %in% c(1) | 
                           FCdata$v1139a1 %in% c(1) | 
                           FCdata$v1277a1 %in% c(1) | 
                           FCdata$v1281a1 %in% c(1) | 
                           FCdata$v1329a1 %in% c(1) | 
                           FCdata$v1610a1 %in% c(1) | 
                           FCdata$v1705a1 %in% c(1), 
                         1, 0) # count if there is water onsite or w/in 500 meters
  
  # electricity
  #   Grid
  FCdata$elec_grid <- NA
  FCdata$elec_grid <- ifelse(FCdata$v120a %in% c(1,2,3), 1, 0) # count if connected & available, connected interrupted, dk if interupted
  #   generator
  FCdata$bkpGen <- NA
  FCdata$bkpGen <- ifelse(FCdata$v120 %in% c(5,6), 1, 0) # reported functional with fuel or don't know
  FCdata$bkpGen <- ifelse(is.na(FCdata$bkpGen), 0, FCdata$bkpGen) # missing coded as 0
  # Sufficient electricity
  FCdata$electricity <- NA
  FCdata$electricity <- ifelse(FCdata$elec_grid == 1 | FCdata$bkpGen == 1, 1, 0)
  
  # on-site latrines
  FCdata$latOnSite <- NA
  FCdata$latOnSite <- ifelse(!is.na(FCdata$v153), FCdata$v153, 0)
  
  # functioning telephone
  FCdata$comms <- NA
  FCdata$comms <- ifelse(FCdata$v127 %in% c(1,3), 1, 0) # count if observed or reported
  FCdata$comms <- ifelse(is.na(FCdata$v127), 0, FCdata$comms)
  
  # Level A
  FCdata$lvl_a <- NA
  FCdata$lvl_a <- ifelse(FCdata$qualified2 == 1 & 
                           FCdata$onBedsN >= 1 &
                           FCdata$sched24 == 1 &
                           FCdata$water == 1 &
                           FCdata$electricity == 1 &
                           FCdata$latOnSite == 1 &
                           FCdata$comms == 1,
                         1,
                         0
  )
  
  # LEVEL B
  # Availability of IV fluids # Double check this
  FCdata$iv_fluids <- NA
  FCdata$iv_fluids <- ifelse(FCdata$v534a %in% c(2,4) | 
                               FCdata$v165c %in% c(1,2) | FCdata$v165d %in% c(1,2) | # kits
                               FCdata$v924_01 %in% c(2,4) | FCdata$v924_02 %in% c(2,4) | FCdata$v924_03 %in% c(2,4) | FCdata$v924_05 %in% c(2,4), 1, 0)
  
  # availability of blood products
  FCdata$blood <- NA
  FCdata$blood <- FCdata$v839 # offers blood transfusions
  FCdata$blood <- ifelse(is.na(FCdata$v839), 0, FCdata$blood) # count missing as 0
  
  # basic surgical instruments
  #   Forceps #!!! Needs to be looked at
  FCdata$forceps <- NA
  FCdata$forceps <- ifelse(
    #FCdata$v1702g %in% c(1, 4) | 
    FCdata$v336g %in% c(1,2) | 
      FCdata$v531xj %in% c(1,2) | 
      FCdata$v531xk %in% c(1,2), 
    1, 0) # observed or reported functioning
  #   Needle driver
  FCdata$needleDriver <- NA
  FCdata$needleDriver <- ifelse(FCdata$v1702a %in% c(1, 4), 1, 0) # observed or reported functioning
  #   sterile scissors
  FCdata$scissors <- NA
  FCdata$scissors <- ifelse(FCdata$v1702d %in% c(1, 4), 1, 0)
  #   Basic Surgical Instruments
  FCdata$basicSurg <- NA
  FCdata$basicSurg <- ifelse(FCdata$forceps == 1 & FCdata$needleDriver == 1 & FCdata$scissors == 1, 1, 0)
  
  # Local anesthesia
  FCdata$local_anesth <- NA
  FCdata$local_anesth <- ifelse(FCdata$v1703d %in% c(2, 4) | FCdata$v336a %in% c(1,2) | FCdata$v557c %in% c(1,4), 1, 0) # observed valid or reported available
  
  # Sterile Dressings
  FCdata$sterileDress <- NA
  
  if (FCdata$v000[1] == "HT7") { # !!! calculated for haiti only!
    FCdata$sterileDress <- ifelse(FCdata$v1703g %in% c(1, 4), 1, 0) # observed or reported functioning
  }
  
  # !!! Stopped spot checking here
  
  # Supplemental Oxygen
  FCdata$suppOxy <- NA
  FCdata$suppOxy <- ifelse(FCdata$v166p %in% c(1, 4) | FCdata$v1608p %in% c(1,4) | # cylinders observed or reported functioning
                             FCdata$v166o %in% c(1,4) | FCdata$v1608o %in% c(1,4) | # concentrators observed or reported functioning
                             FCdata$v166q %in% c(1,4) | FCdata$v1608q %in% c(1,4),
                           1, 0)
  
  # level b
  FCdata$lvl_b <- NA
  FCdata$lvl_b <- ifelse(FCdata$lvl_a == 1 &
                           FCdata$iv_fluids == 1 &
                           FCdata$blood == 1 &
                           FCdata$basicSurg == 1 &
                           FCdata$local_anesth == 1 &
                           FCdata$sterileDress == 1 &
                           FCdata$suppOxy == 1,
                         1,
                         0
  )
  
  # Level C (Tertiary care facilities)
  #   >50 inpatient beds
  FCdata$beds50 <- NA
  FCdata$beds50 <- ifelse(FCdata$onBedsN2 >= 50, 1, 0)
  FCdata$beds50 <- ifelse(is.na(FCdata$onBedsN2), 0, FCdata$beds50)
  
  #   surgeon on staff
  FCdata$has_surgeon <- NA
  FCdata$has_surgeon <- ifelse(FCdata$surgeon >= 1, 1, 0)
  FCdata$has_surgeon <- ifelse(is.na(FCdata$surgeon), 0, FCdata$has_surgeon)
  
  # level c
  FCdata$lvl_c <- NA
  FCdata$lvl_c <- ifelse(FCdata$lvl_b == 1 &
                           # FCdata$beds50 == 1 &
                           FCdata$has_surgeon == 1,
                         1,
                         0
  )
  
  
  # Gage et al. crosswalk #### !!! coded 2017-18
  #   Basic Amenities (sri_basicamenities) ####
  #   * elec_st
  FCdata$elec_st <- NA
  FCdata$elec_st <- ifelse(FCdata$v120a %in% c(1) | FCdata$v120 %in% c(5), 1, 0)
  #   * water_st
  FCdata$water_st <- NA
  FCdata$water_st <- ifelse(FCdata$v124 %in% c(1,2), 1, 0)
  #   * privvis_all_st > 0 & privaud_all_st > 0
  #     - 0, no privacy
  #     - 1, private room, visual and auditory privacy
  #     - 2, other room, visual and auditory privacy
  #     - 4, visual privacy only
  FCdata$privvisaud_all_st <- ifelse(
    (FCdata$v167 %in% c(1,2) + 
       FCdata$v230 %in% c(1,2) + 
       FCdata$v264 %in% c(1,2) +
       FCdata$v330 %in% c(1,2) +
       FCdata$v430 %in% c(1,2) +
       FCdata$v530 %in% c(1,2) +
       FCdata$v630 %in% c(1,2) +
       FCdata$v1138 %in% c(1,2) +
       FCdata$v1276 %in% c(1,2) +
       FCdata$v1280 %in% c(1,2) +
       FCdata$v1328 %in% c(1,2) + 
       FCdata$v1609 %in% c(1,2) + 
       FCdata$v1704 %in% c(1,2)) ==
      apply(FCdata[c("v167",
                     "v230",
                     "v264",
                     "v330",
                     "v430",
                     "v530",
                     "v630",
                     "v1138",
                     "v1276",
                     "v1280",
                     "v1328",
                     "v1609",
                     "v1704")], 1, FUN = function(x) {
                       length(x[!is.na(x)])
                     }),
    1,
    0
  )
  #   * toilet_st
    # label define V153    
    # 0 "No functioning facility"
    # 1 "Yes"
  
  FCdata$toilet_st <- NA
  FCdata$toilet_st <- ifelse(FCdata$v153 %in% c(1), 1, 0)
  
  if (FCdata$v000[1] %in% "ET8") {
    
    # label define V153A   
    # 0 "No functioning facility"
    # 11 "Flush to piped sewer system"
    # 12 "Flush to septic tank"
    # 13 "Flush to pit latrine"
    # 14 "Flush to somewhere else"
    # 15 "Flush, don't know where"
    # 21 "Ventilated improved pit latrine"
    # 22 "Pit latrine with slab"
    # 23 "Pit latrine without slab/ open pit"
    # 31 "Composting toilet"
    # 41 "Bucket toilet"
    # 51 "Hanging toilet/ hanging latrine"
    
    FCdata$toilet_st <- NA
    FCdata$toilet_st <- ifelse(FCdata$v153a %in% c(11, 12, 13, 14, 15, 21, 22, 23, 31, 41, 51), 1, 0)
  }
  
  #   * comm_st Communication equipment (phone or SW radio): Reported availability accepted
    # label define V127    
    # 0 "No functioning landline, cell phone or shortwave at facility"
    # 1 "Observed functioning communication"
    # 3 "Reported functioning communication"
  
  
  FCdata$comm_st <- NA
  FCdata$comm_st <- ifelse(FCdata$v127 %in% c(1,3), 1, 0)
  #   * computer_st & internet_st # REPORTED IS ACCEPTED
  #     - computer_st
  
  # v128 functioning computer
    # label define V128    
    # 0 "No computer"
    # 1 "Yes, functioning"
    # 2 "Yes, but not functioning"
  # v128a computer observed
    # label define V128A   
    # 0 "No computer"
    # 1 "Yes, observed"
    # 2 "Yes, reported, not seen"
  
  FCdata$computer_st <- NA
  FCdata$computer_st <- ifelse(FCdata$v128 %in% c(1) & FCdata$v128a %in% c(1,2), 1, 0)
  #     - internet_st
    # label define V129    
    # 0 "No"
    # 1 "Yes at least 2 hours on client service days"
    # 2 "Yes, less than 2 hours on client service days"
  
  FCdata$internet_st <- NA
  FCdata$internet_st <- ifelse(FCdata$v129 %in% c(1,2), 1, 0) # includes facilities with less than 2 hours per day
  #     - compInt_st
  FCdata$compInt_st <- NA
  FCdata$compInt_st <- ifelse(FCdata$computer_st %in% c(1) & FCdata$internet_st %in% c(1), 1, 0)
  
  #   * ambulance_st: Reported availability accepted
    # label define V150    
    # 0 "No ambulance, no driver, no access to other vehicle"
    # 4 "Ambulance observed with fuel"
    # 5 "Ambulance reported with  fuel"
    # 6 "Ambulance obsd without/DK fuel"
    # 7 "Ambulance reptd without/DK fuel"
    # 8 "No ambulance, reptd access other vehicle"
  
  FCdata$ambulance_st <- NA
  FCdata$ambulance_st <- ifelse(FCdata$v150 %in% c(4,5,8), 1, 0)
  
  if (FCdata$v000[1] == "HT7") { # !!! calculated for Haiti only
    FCdata$ambulance_st <- ifelse(FCdata$sf452 %in% c(1), 1, FCdata$ambulance_st)
  }
  
  basicamenitiesVars <- c(
    "elec_st", 
    "water_st", 
    "privvisaud_all_st", 
    "toilet_st", 
    "comm_st", 
    "compInt_st", 
    "ambulance_st"
    )
  
  # Figure out how many columns are "eligible": not entirely NA or 0
  valid_basicamenities <- basicamenitiesVars[
    sapply(FCdata[basicamenitiesVars], function(x) any(!is.na(x) & x != 0))
  ]
  
  
  #   DOMAIN SCORE: sri_diagcapacity
  FCdata$sri_basicamenities_avail <- NA
  FCdata$sri_basicamenities_avail <- rowSums(FCdata[valid_basicamenities], na.rm = TRUE) / length(valid_basicamenities)
  FCdata$sri_basicamenities_valid <- length(valid_basicamenities)
  
  # DOMAIN SCORE sri_basicamenities
  FCdata$sri_basicamenities <- NA
  FCdata$sri_basicamenities <- rowSums(st_drop_geometry(FCdata[basicamenitiesVars]), na.rm = TRUE) / length(basicamenitiesVars)
  
  #   Basic Equipment (sri_basicequip) #### Observed and functioning
    # label define V166A   
    # 0 "Not available"
    # 1 "Observed, functioning"
    # 2 "Observed, missing functioning"
    # 3 "Observed, not/DK if functioning"
    # 4 "Reported functioning"
    # 5 "Reported, missing functioning"
    # 6 "Reported, not/DK  if functioning"
  
  #   * Adult scale	rowmax(eqanc_scale_st eqncd_ad_scale_st)
  #     - egncd_ad_scale_st, adult weighing scale in non-communicable disease room
  FCdata$eqanc_ad_scale_st <- NA
  FCdata$eqanc_ad_scale_st <- ifelse(FCdata$v166a %in% c(1), 1, 0) # observed or reported functioning
  #     - egncd_ad_scale_st, adult weighing scale in non-communicable disease room
  FCdata$eqncd_ad_scale_st <- NA
  FCdata$eqncd_ad_scale_st <- ifelse(FCdata$v433g %in% c(1), 1,0)
  #     - eqgopd_ad_scale_st, adult weighing scale in non-communicable disease room
  FCdata$eqgopd_ad_scale_st <- NA
  FCdata$eqgopd_ad_scale_st <- ifelse(FCdata$v1608a %in% c(1),1,0)
  #     - ad_scale_st: adult weighing scale anywhere in facility
  FCdata$eq_ad_scale_st <- NA
  FCdata$eq_ad_scale_st <- ifelse(FCdata$eqanc_ad_scale_st %in% c(1) | 
                                    FCdata$eqncd_ad_scale_st %in% c(1) | 
                                    FCdata$eqncd_ad_scale_st  %in% c(1),
                                  1, 0)
  #   * Child scale	rowmax(eqsc_pedscale_st eqncd_ch_scale_st)
  #     - eqsc_pedscale_st: pediatric scales in sick child room
  FCdata$eqsc_pedscale_st <- NA
  FCdata$eqsc_pedscale_st <- ifelse(FCdata$v265c %in% c(1), 1, 0)
  #     - eqncd_ch_scale_st: pediatric scales in non-communicable diseases room
  FCdata$eqncd_ch_scale_st <- NA
  FCdata$eqncd_ch_scale_st <- ifelse(FCdata$v1608b %in% c(1), 1, 0)
  #     - eqgopd_ch_scale_st: pediatric scales in general opd room
  FCdata$eqgopd_ch_scale_st <- NA
  FCdata$eqgopd_ch_scale_st <- ifelse(FCdata$v166b %in% c(1), 1, 0)
  #     - eq_ch_scale_st: pediatric scales anywhere
  FCdata$eq_ch_scale_st <- NA
  FCdata$eq_ch_scale_st <- ifelse(FCdata$eqsc_pedscale_st == 1 | 
                                    FCdata$eqncd_ch_scale_st == 1 | 
                                    FCdata$eqgopd_ch_scale_st == 1, 
                                  1, 0)
  #   * Infant scale
  # 0 "Not available"
  # 1 "Observed, functioning"
  # 2 "Observed, missing functioning"
  # 3 "Observed, not/DK if functioning"
  # 4 "Reported functioning"
  # 5 "Reported, missing functioning"
  # 6 "Reported, not/DK  if functioning"
  FCdata$eq_inf_scale_st <- NA
  FCdata$eq_inf_scale_st <- ifelse(
    FCdata$v536d %in% 1 | #     - v536d    "Newborn:Infant scale"
      FCdata$v265b %in% 1 | #     - v265b    "Sick child exam other equip:Infant scale"
      FCdata$v272a %in% 1, #     - v272a    "Growth monitoring equip: Infant scale"
    1, 0
  )
  #   * height length measuring
  # v272c    "Growth monitoring equip: Height or length board"
  FCdata$eq_heightLength_st <- NA
  FCdata$eq_heightLength_st <- ifelse(FCdata$v272c %in% 1, 1, 0)
  #   * Growth monitoring chart
  FCdata$eq_growthChart_st <- NA
  FCdata$eq_growthChart_st <- ifelse(
    FCdata$v273b %in% 1,  # v273b    "Growth monitoring equip: Growth charts"
    1,0
  )
  
  #   * Thermometer	rowmax(eqsc_therm_st eqncd_therm_st)
  #     - eqsc_therm_st: Thermometer in sick child room
  FCdata$eqsc_therm_st <- NA
  FCdata$eqsc_therm_st <- ifelse(FCdata$v265d %in% c(1,4), 1, 0)
  #     - eqnc_therm_st: Non-communicable diseases room thermometer
  FCdata$eqnc_therm_st <- NA
  FCdata$eqnc_therm_st <- ifelse(FCdata$v1608e %in% c(1,4), 1, 0)
  #     - eqgopd_therm_st: Thermometer in general opd area
  FCdata$eqgopd_therm_st <- NA
  FCdata$eqgopd_therm_st <- ifelse(FCdata$v166e %in% c(1,4), 1, 0)
  #     - eqoe_therm_st: Other equipment thermometer
  FCdata$eqoe_therm_st <- NA
  FCdata$eqoe_therm_st <- ifelse(FCdata$v533i %in% c(1,4), 1, 0)
  #     - eq_therm_st: thermometer
  FCdata$eq_therm_st <- NA
  FCdata$eq_therm_st <- ifelse(FCdata$eqsc_therm_st == 1 |
                                 FCdata$eqnc_therm_st == 1 |
                                 FCdata$eqgopd_therm_st == 1 |
                                 FCdata$eqoe_therm_st == 1, 
                               1, 0)
  
  #   * Stethoscope	rowmax(eqsc_stetho_st eqfp_stetho_st eqanc_stetho_st eqdel_stetho_st eqncd_stetho_st)
  #     - eqgopd_stetho_st: stethoscope in general opd area
  FCdata$eqgopd_stetho_st <- NA
  FCdata$eqgopd_stetho_st <- ifelse(FCdata$v166f %in% c(1,4), 1, 0)
  #     - eqsc_stetho_st: stethoscope in sick child exam room
  FCdata$eqsc_stetho_st <- NA
  FCdata$eqsc_stetho_st <- ifelse(FCdata$v265h %in% c(1,4), 1, 0)
  #     - eqanc_stetho_st: stethoscope in anc exam
  FCdata$eqanc_stetho_st <- NA
  FCdata$eqanc_stetho_st <- ifelse(FCdata$v433c %in% c(1,4), 1, 0)
  #     - eqoe_stetho_st: stethoscope in other equipment
  FCdata$eqoe_stetho_st <- NA
  FCdata$eqoe_stetho_st <- ifelse(FCdata$v533d %in% c(1,4) | FCdata$v333d %in% c(1,4), 1, 0)
  #     - eqnc_stetho_st: stethoscope in non-communicable disease
  FCdata$eqncd_stetho_st <- NA
  FCdata$eqncd_stetho_st <- ifelse(FCdata$v1608f %in% c(1,4), 1, 0)
  #     - eq_stetho_st: stethoscope in any room
  FCdata$eq_stetho_st <- NA
  FCdata$eq_stetho_st <- ifelse(
    FCdata$eqgopd_stetho_st == 1 |
      FCdata$eqsc_stetho_st == 1 |
      FCdata$eqanc_stetho_st == 1 |
      FCdata$eqoe_stetho_st == 1 |
      FCdata$eqncd_stetho_st == 1,
    1,0
  )
  #   * Blood pressure apparatus	rowmax(eqfp_bp_st eqanc_bp_st eqdel_bp_st eqncd_bp_st)
  #     - eqgopd_bp_st: blood pressure apparatus in general opd area
  FCdata$eqgopd_bp_st <- NA
  FCdata$eqgopd_bp_st <- ifelse(FCdata$v166g %in% c(1,4) | FCdata$v166h %in% c(1,4), 1, 0)
  #     - eqgoe_bp_st: blood pressure apparatus in other equipment
  FCdata$eqoe_bp_st <- NA
  FCdata$eqoe_bp_st <- ifelse(FCdata$v333b %in% c(1,4) | 
                                FCdata$v333d %in% c(1,4) | 
                                FCdata$v533c %in% c(1,4),# | 
                              # FCdata$v533e %in% c(1,4) | 
                              # FCdata$v533k %in% c(1,4),
                              1, 0)
  #     - eqganc_bp_st: blood pressure apparatus in anc area
  FCdata$eqanc_bp_st <- NA
  FCdata$eqanc_bp_st <- ifelse(FCdata$v433b %in% c(1,4) | FCdata$v433h %in% c(1,4), 1, 0)
  #     - eqncd_bp_st: blood pressure apparatus in non-communicable diseases
  FCdata$eqncd_bp_st <- NA
  FCdata$eqncd_bp_st <- ifelse(FCdata$v1608g %in% c(1,4) | FCdata$v1608h %in% c(1,4), 1, 0)
  #     - eq_bp_st: blood pressure apparatus anywhere
  FCdata$eq_bp_st <- NA
  FCdata$eq_bp_st <- ifelse(
    FCdata$eqgopd_bp_st == 1 |
      FCdata$eqoe_bp_st == 1 |
      FCdata$eqanc_bp_st == 1 |
      FCdata$eqncd_bp_st == 1,
    1,0
  )
  
  #   * Light source	rowmax(eqfp_exlight_st eqanc_exlight_st eqdel_light_st eqncd_light_st)
  #     - eqgopd_lightst: light source apparatus in general opd area
  FCdata$eqgopd_light_st <- NA
  FCdata$eqgopd_light_st <- ifelse(FCdata$v166i %in% c(1), 1, 0)
  #     - eqgoe_light_st: light source apparatus in other equipment
  FCdata$eqoe_light_st <- NA
  FCdata$eqoe_light_st <- ifelse(FCdata$v333e %in% c(1) | 
                                   FCdata$v533b %in% c(1),
                                 1, 0)
  #     - eqganc_light_st: light source apparatus in anc area
  FCdata$eqanc_light_st <- NA
  FCdata$eqanc_light_st <- ifelse(FCdata$v433i %in% c(1), 1, 0)
  #     - eqncd_light_st: light source apparatus in non-communicable diseases
  FCdata$eqncd_light_st <- NA
  FCdata$eqncd_light_st <- ifelse(FCdata$v1608i %in% c(1), 1, 0)
  #     - eq_light_st: light source apparatus anywhere
  FCdata$eq_light_st <- NA
  FCdata$eq_light_st <- ifelse(
    FCdata$eqgopd_light_st == 1 |
      FCdata$eqoe_light_st == 1 |
      FCdata$eqanc_light_st == 1 |
      FCdata$eqncd_light_st == 1,
    1,0
  )
  
  basicequipVars <- c("eq_ad_scale_st", 
                      "eq_ch_scale_st", 
                      "eq_therm_st", 
                      "eq_stetho_st", 
                      "eq_bp_st", 
                      "eq_light_st")
  
  
  # Figure out how many columns are "eligible": not entirely NA or 0
  valid_basicequip <- basicequipVars[
    sapply(FCdata[basicequipVars], function(x) any(!is.na(x) & x != 0))
  ]
  
  
  #   DOMAIN SCORE: sri_basicequip
  FCdata$sri_basicequip_avail <- NA
  FCdata$sri_basicequip_avail <- rowSums(FCdata[valid_basicequip], na.rm = TRUE) / length(valid_basicequip)
  FCdata$sri_basicequip_valid <- length(valid_basicequip)
  
  # DOMAIN SCORE: sri_basicequip
  FCdata$sri_basicequip <- NA
  FCdata$sri_basicequip <- rowSums(FCdata[basicequipVars], na.rm = TRUE) / length(basicequipVars)
  
  
  #   Infection Prevention (sri_infprev) ####
  #   * Safe final disposal of sharps	sharps_disp_st !!!
    # label define VT101   
    # 0 "No"
    # 1 "Yes"
  FCdata$sharps_disp_st <- NA
  FCdata$sharps_disp_st <- ifelse(FCdata$vt101 %in% c(1), 1, 0)
  #   * Safe final disposal of infectious wastes 	haz_disp_st
  FCdata$haz_disp_st <- NA
  FCdata$haz_disp_st <- ifelse(FCdata$vt102 %in% c(1), 1, 0)
  #   * Appropriate storage of sharps waste (sharps box/container) 	sharps_prop_st > 0
  FCdata$sharps_prop_st <- NA
  FCdata$sharps_prop_st <- ifelse(FCdata$vt103 %in% c(1), 1, 0)
  #   * Appropriate storage of infectious waste (waste receptacle with lid and plastic bin liner) 	wastebin_prop_st > 0
  FCdata$wastebin_prop_st <- NA
  FCdata$wastebin_prop_st <- ifelse(FCdata$vt104 %in% c(1), 1, 0)
  #   * Disinfectant 	surfdisinf_prop_st > 0: Observed
    # label define V168J   
    # 0 "Not available"
    # 1 "Yes, observed"
    # 2 "Yes, reported"
  
  FCdata$surfdisinf_prop_st <- NA
  FCdata$surfdisinf_prop_st <- ifelse(FCdata$v168j %in% c(1,2) | 
                                        FCdata$v231j %in% c(1,2) | 
                                        FCdata$v264j %in% c(1,2) | 
                                        FCdata$v331j %in% c(1,2) | 
                                        FCdata$v431j %in% c(1,2) | 
                                        FCdata$v531j %in% c(1,2) | 
                                        FCdata$v631j %in% c(1,2) | 
                                        FCdata$v866j %in% c(1,2) | 
                                        FCdata$v1139j %in% c(1,2) | 
                                        FCdata$v1277j %in% c(1,2) | 
                                        FCdata$v1281j %in% c(1,2) | 
                                        FCdata$v1329j %in% c(1,2) | 
                                        FCdata$v1610j %in% c(1,2) | 
                                        FCdata$v1705j %in% c(1,2),
                                      1,0
  )
  #   * Single-use, standard disposable or auto-disable syringes 	syringe_prop_st > 0
  #     - 1 is observed, 2 is reported
  #     - alternative coding for condition: rowSums(FCdata[c("v168l", "v231l", "v331l")]== 1 | FCdata[c("v168l", "v231l", "v331l")]== 2, na.rm=TRUE) > 1
  FCdata$syringe_prop_st <- NA
  FCdata$syringe_prop_st <- ifelse(FCdata$v168l %in% c(1) | 
                                     FCdata$v231l %in% c(1) | 
                                     FCdata$v264l %in% c(1) | 
                                     FCdata$v331l %in% c(1) | 
                                     FCdata$v431l %in% c(1) | 
                                     FCdata$v531l %in% c(1) | 
                                     FCdata$v631l %in% c(1) | 
                                     FCdata$v866l %in% c(1) | 
                                     FCdata$v1139l %in% c(1) | 
                                     FCdata$v1277l %in% c(1) | 
                                     FCdata$v1281l %in% c(1) | 
                                     FCdata$v1329l %in% c(1) | 
                                     FCdata$v1610l %in% c(1) | 
                                     FCdata$v1705l %in% c(1) |
                                     FCdata$v946_16 %in% c(1),
                                   1,0
  )
  #   * Soap and running water or alcohol-based hand rub 	soap_prop_st > 0 & water_prop_st > 0 | handdisinf_prop_st > 0 (if not missing)
  #     - soap_prop_st: All assessed rooms (out of a maximum of 7??, according to gage, but that's not what I've done)
  FCdata$soap_prop_st <- NA
  FCdata$soap_prop_st <- 
    rowSums(sapply(FCdata[c("v168c",
                            "v231c",
                            "v264c",
                            "v331c",
                            "v431c",
                            "v531c",
                            "v631c",
                            "v866c", 
                            "v1139c",
                            "v1277c",
                            "v1281c",
                            "v1329c",
                            "v1610c",
                            "v1705c")],
                   `%in%`,
                   c(1)
    )
    ) / rowSums(sapply(FCdata[c("v168c",
                                "v231c",
                                "v264c",
                                "v331c",
                                "v431c",
                                "v531c",
                                "v631c",
                                "v866c",
                                "v1139c",
                                "v1277c",
                                "v1281c",
                                "v1329c",
                                "v1610c",
                                "v1705c")], function(x) {is.na(x)}
    )
    )
  #     - water_prop_st: Proportion of assessed rooms that have piped water or water in a pitcher (not bucket) observed
  FCdata$water_prop_st <- NA
  FCdata$water_prop_st <- 
    rowSums(sapply(FCdata[c("v168a1",
                            "v231a1",
                            "v264a1",
                            "v331a1",
                            "v431a1",
                            "v531a1",
                            "v631a1",
                            "v866a1", 
                            "v1139a1",
                            "v1277a1",
                            "v1281a1",
                            "v1329a1",
                            "v1610a1",
                            "v1705a1")],
                   `%in%`,
                   c(1)
    )
    ) / rowSums(sapply(FCdata[c("v168a1",
                                "v231a1",
                                "v264a1",
                                "v331a1",
                                "v431a1",
                                "v531a1",
                                "v631a1",
                                "v866a1",
                                "v1139a1",
                                "v1277a1",
                                "v1281a1",
                                "v1329a1",
                                "v1610a1",
                                "v1705a1")], function(x) {is.na(x)}
    )
    )
  #     - handdisinf_prop_st: Proportion of assessed rooms that have hand disinfectant observed (not bucket) observed
  FCdata$handdisinf_prop_st <- NA
  FCdata$handdisinf_prop_st <- 
    rowSums(sapply(FCdata[c("v168q",
                            "v231q",
                            "v264q",
                            "v331q",
                            "v431q",
                            "v531q",
                            "v631q",
                            "v866q", 
                            "v1139q",
                            "v1277q",
                            "v1281q",
                            "v1329q",
                            "v1610q",
                            "v1705q")],
                   `%in%`,
                   c(1)
    )
    ) / rowSums(sapply(FCdata[c("v168q",
                                "v231q",
                                "v264q",
                                "v331q",
                                "v431q",
                                "v531q",
                                "v631q",
                                "v866q",
                                "v1139q",
                                "v1277q",
                                "v1281q",
                                "v1329q",
                                "v1610q",
                                "v1705q")], function(x) {is.na(x)}
    )
    )
  #     - handwash_any_st: soap_prop_st > 0 & water_prop_st > 0 | handdisinf_prop_st > 0
  FCdata$handwash_any_st <- NA
  FCdata$handwash_any_st <- ifelse((FCdata$soap_prop_st > 0 & FCdata$water_prop_st > 0) | FCdata$handdisinf_prop_st > 0,
                                   1,0)
  
  #   * Latex gloves 	gloves_prop_st > 0
  FCdata$gloves_prop_st <- NA
  FCdata$gloves_prop_st <- ifelse(
    rowSums(sapply(FCdata[c("v168g",
                            "v231g",
                            "v264g",
                            "v331g",
                            "v431g",
                            "v531g",
                            "v631g",
                            "v866g", 
                            "v1139g",
                            "v1277g",
                            "v1281g",
                            "v1329g",
                            "v1610g",
                            "v1705g")],
                   `%in%`,
                   c(1)
    )
    ) / rowSums(sapply(FCdata[c("v168g",
                                "v231g",
                                "v264g",
                                "v331g",
                                "v431g",
                                "v531g",
                                "v631g",
                                "v866g",
                                "v1139g",
                                "v1277g",
                                "v1281g",
                                "v1329g",
                                "v1610g",
                                "v1705g")], function(x) {is.na(x)}
    )
    ) >0,
    1,0
  )
  
  
  #   * Guidelines for standard precautions	guideprecaution #observed
  FCdata$guideprecaution <- NA
  FCdata$guideprecaution <- ifelse(
    rowSums(sapply(FCdata[c("v168u",
                            "v231u",
                            "v264u",
                            "v331u",
                            "v431u",
                            "v531u",
                            "v631u",
                            "v866u", 
                            "v1139u",
                            "v1277u",
                            "v1281u",
                            "v1329u",
                            "v1610u",
                            "v1705u")],
                   `%in%`,
                   c(1) # observed
    )
    ) >= 1, 1, 0)
  
  infprevVars <- c("sharps_disp_st",
                   "haz_disp_st",
                   "sharps_prop_st", 
                   "wastebin_prop_st", 
                   "surfdisinf_prop_st",
                   "syringe_prop_st",
                   "handwash_any_st", 
                   "gloves_prop_st",
                   "guideprecaution")
  
  # Figure out how many columns are "eligible": not entirely NA or 0
  valid_infprev <- infprevVars[
    sapply(FCdata[infprevVars], function(x) any(!is.na(x) & x != 0))
  ]
  
  
  #   DOMAIN SCORE: sri_infprev
  FCdata$sri_infprev_avail <- NA
  FCdata$sri_infprev_avail <- rowSums(FCdata[valid_infprev], na.rm = TRUE) / length(valid_infprev)
  FCdata$sri_infprev_valid <- length(valid_infprev)
  
  
  # DOMAIN SCORE:
  FCdata$sri_infprev <- NA
  FCdata$sri_infprev<- rowSums(FCdata[infprevVars], na.rm = TRUE) / length(infprevVars)
  
  #   Diagnostic Capacity ####
  #   Haemoglobin, Blood glucose, Malaria diagnostic capacity, Urine dipstick - protein, Urine dipstick - glucose, HIV diagnostic capacity, Syphilis RDT, Urine pregnancy test
  #   * Haemoglobin	diag_hemog	post-2012: HemoCue or Colorimeter/hemoglobinometer; pre-2012: Hemoglobinometer/HemoCuec (RW07:u346a, UG07:u346b1&u346b3) or Colorimeter/spectroscope
  FCdata$diag_hemog <- NA
  FCdata$diag_hemog <- ifelse(FCdata[["v845c"]] %in% c(1), 1, 0)
  #   * Blood glucose	diag_bloodgluc	
  FCdata$diag_bloodgluc <- NA
  FCdata$diag_bloodgluc <- ifelse(FCdata$v840f1 %in% c(1), 1, 0) # could also look at v828 - this is, apparently, more strict
  #   * Malaria diagnostic capacity	diag_malaria	
  # FCdata$diag_malaria <- NA
  # FCdata$diag_malaria <- ifelse(FCdata$vt824 %in% c(1), 1, 0)
  #   NEW CALCULATION
  
    # label define V852A   
    # 0 "Not used"
    # 1 "Equipment used and observed"
    # 2 "Equipment used reported, not seen"
    # 3 "Equipment used, not available today"
    # 4 "Equipment used, missing if items available"
  #   Malaria Giemsa stain
  FCdata$diagMalaria_giemsa <- NA
  FCdata$diagMalaria_giemsa <- ifelse(FCdata$v852a %in% c(1), 1, 0)
  #   Malaria Field Stain
  FCdata$diagMalaria_field <- NA
  FCdata$diagMalaria_field <- ifelse(FCdata$v852b %in% c(1), 1, 0)
  #   Malaria Acridine Orange (AO microscope and Acridine orange stain)
  FCdata$diagMalaria_AO <- NA
  FCdata$diagMalaria_AO <- ifelse(FCdata$v852d %in% c(1), 1, 0)
  #   Malaria RDT
    # label define V840G1  
    # 0 "Never available"
    # 2 "Yes, observed, at least 1 valid"
    # 3 "Yes, observed, none valid"
    # 4 "Yes, reported available, not seen"
    # 5 "Not available today"
  #   852c directs to v840g1
  FCdata$diagMalaria_RDT <- NA
  FCdata$diagMalaria_RDT <- ifelse(FCdata$v840g1 %in% 2, 1, 0)
  
  # Composite diagnostic capacity
  FCdata$diag_malaria <- NA
  FCdata$diag_malaria <- ifelse(
    FCdata$diagMalaria_giemsa == 1 |
      FCdata$diagMalaria_field == 1 |
      FCdata$diagMalaria_AO == 1 |
      FCdata$diagMalaria_RDT== 1,
    1, 0
    )
  
  
  #   * Urine dipstick - protein	diag_urineprot	!!! worth double checking the original questionnaire for this one.
  FCdata$diag_urineprot <- NA
  FCdata$diag_urineprot <- ifelse(FCdata$v851a %in% c(1), 1, 0)
  #   * Urine dipstick - glucose	diag_urinegluc	
  FCdata$diag_uringluc <- NA
  FCdata$diag_uringluc <- ifelse(FCdata$v851b %in% c(1), 1, 0)
  #   * HIV diagnostic capacity	diag_hiv	
  FCdata$diag_hiv <- NA
  FCdata$diag_hiv <- ifelse(FCdata$v840a %in% c(1), 1, 0)
  #   * Syphilis RDT	diag_syphilis	!!! using specifically the RDT
  FCdata$diag_syphilis <- NA
  FCdata$diag_syphilis <- ifelse(FCdata$v840e1 %in% c(2), 1, 0) # 4 = reported available, not seen
  #   * Urine pregnancy test	diag_urinepreg	
  FCdata$diag_urinepreg <- NA
  FCdata$diag_urinepreg <- ifelse(FCdata$v840f3 %in% c(1), 1, 0)
  
  diagcapacityVars <- c("diag_hemog",
                        "diag_bloodgluc",
                        "diag_malaria", 
                        "diag_urineprot", 
                        "diag_uringluc",
                        "diag_hiv",
                        "diag_syphilis", 
                        "diag_urinepreg")
  
  # Figure out how many columns are "eligible": not entirely NA or 0
  valid_diagcapacity <- diagcapacityVars[
    sapply(FCdata[diagcapacityVars], function(x) any(!is.na(x) & x != 0))
  ]
  
  
  #   DOMAIN SCORE: sri_diagcapacity
  FCdata$sri_diagcapacity_avail <- NA
  FCdata$sri_diagcapacity_avail <- rowSums(FCdata[valid_diagcapacity], na.rm = TRUE) / length(valid_diagcapacity)
  FCdata$sri_diagcapacity_valid <- length(valid_diagcapacity)
  
  #   DOMAIN SCORE: sri_diagcapacity
  FCdata$sri_diagcapacity <- NA
  FCdata$sri_diagcapacity<- rowSums(FCdata[diagcapacityVars], na.rm = TRUE) / length(diagcapacityVars)
  
  #   Medication ####
    # label define V903_16 
    # 0 "Never available"
    # 2 "Yes, observed, at least 1 valid"
    # 3 "Yes, observed, none valid"
    # 4 "Yes, reported available, not seen"
    # 5 "Not available today"
  
  #   Seems to be missing some! WHO has 24 (sans the acknowledged missing fluoxetine, this has 19)
  #   * Amitriptyline tablet	med_amitr
  FCdata$med_amitr <- NA
  FCdata$med_amitr <- ifelse(FCdata$v903_16 %in% c(2), 1, 0)
  #   * Amlodipine tablet or alternative calcium channel blocker	med_amlod
  FCdata$med_amlod <- NA
  FCdata$med_amlod <- ifelse(FCdata$v903_17 %in% c(2), 1, 0)
  #   * Amoxicillin syrup/suspension or dispersible tablet 	med_amoxicillinsyr
  FCdata$med_amoxicillinsyr <- NA
  FCdata$med_amoxicillinsyr <- ifelse(FCdata$v903_15 %in% c(2) | FCdata$v903_08 %in% c(2), 1, 0)
  #   * Amoxicillin tablet 	med_amoxicillintab
  FCdata$med_amoxicillintab <- NA
  FCdata$med_amoxicillintab <- ifelse(FCdata$v903_06 %in% c(2), 1, 0)
  #   * Ampicillin powder for injection 	med_ampicil
  FCdata$med_ampicil <- NA
  FCdata$med_ampicil <- ifelse(FCdata$v903_10 %in% c(2), 1, 0)
  #   * Beclometasone inhaler 	med_beclom
  FCdata$med_beclom <- NA
  FCdata$med_beclom <- ifelse(FCdata$v904_03 %in% c(2), 1, 0)
  #   * Ceftriaxone injection 	med_ceftria
  FCdata$med_ceftria <- NA
  FCdata$med_ceftria <- ifelse(FCdata$v905_03 %in% c(2), 1, 0)
  #   * Enalapril tablet or alternative ACE inhibitor e.g. lisinopril, ramipril, perindopril 	med_enal
  FCdata$med_enal <- NA
  FCdata$med_enal <- ifelse(FCdata$v903_19 %in% c(2) | FCdata$v903_05 %in% c(2), 1, 0)
  #   * Fluoxetine tablet NOT ASKED IN SPA
  #   * Gentamicin injection 	med_genta
  FCdata$med_genta <- NA
  FCdata$med_genta <- ifelse(FCdata$v909_02 %in% c(2), 1, 0)
  #   * Glibenclamide tablet 	med_glib
  FCdata$med_glib <- NA
  FCdata$med_glib <- ifelse(FCdata$v909_04 %in% c(2), 1, 0)
  #   * Ibuprofen tablet 	med_ibu
  FCdata$med_ibu <- NA
  FCdata$med_ibu <- ifelse(FCdata$v910_08 %in% c(2), 1, 0)
  #   * Insulin injection 	med_insul
  FCdata$med_insul <- NA
  FCdata$med_insul <- ifelse(FCdata$v910_06 %in% c(2), 1, 0)
  #   * Metformin tablet 	med_met
  FCdata$med_met <- NA
  FCdata$med_met <- ifelse(FCdata$v913_10 %in% c(2), 1, 0)
  #   * Omeprazole tablet or alternative such as pantoprazole, rabeprazole 	med_omep
  FCdata$med_omep <- NA
  FCdata$med_omep <- ifelse(FCdata$v915_03 %in% c(2), 1, 0)
  #   * Oral rehydration solution 	med_ors
  FCdata$med_ors <- NA
  FCdata$med_ors <- ifelse(FCdata$v915_01 %in% c(2), 1, 0)
  #   * Paracetamol tablet 	med_paracetamoltab_st
  FCdata$med_paracetamoltab_st <- NA
  FCdata$med_paracetamoltab_st <- ifelse(FCdata$v916_07 %in% c(2), 1, 0)
  #   * Salbutamol inhaler 	med_sal
  FCdata$med_sal <- NA
  FCdata$med_sal <- ifelse(FCdata$v917_04 %in% c(2), 1, 0)
  #   * Simvastatin tablet or other statin e.g. atorvastatin, pravastatin, fluvastatin 	med_simvas
  FCdata$med_simvas <- NA
  FCdata$med_simvas <- ifelse(FCdata$v917_05 %in% c(2), 1, 0)
  #   * Zinc sulphate tablet or syrup	med_zinc
  FCdata$med_zinc <- NA
  FCdata$med_zinc <- ifelse(FCdata$v920_02 %in% c(2), 1, 0)
  #   * Beta blocker: bisoprolol or atenolol ! ADDED BY ME
  FCdata$med_betab <- NA
  FCdata$med_betab <- ifelse(FCdata$v904_06 %in% c(2) | FCdata$v903_18 %in% c(2), 1, 0)
  #   * Magnesium sulphate injectable med_mag ! ADDED BY ME
  FCdata$med_mag <- NA
  FCdata$med_mag <- ifelse(FCdata$v913_01 %in% c(2) | FCdata$v534e %in% c(2), 1, 0)
  #   * Oxytocin (either in general med, or in injebtable uteronic) ! ADDED BY ME
  FCdata$med_oxyto <- NA
  FCdata$med_oxyto <- ifelse(FCdata$v915_02 %in% c(2) | FCdata$v534c %in% c(2), 1, 0)
  #   * Thiazide ! Added by me
  FCdata$med_thiaz <- NA
  FCdata$med_thiaz <- ifelse(FCdata$v918_04 %in% c(2), 1, 0)
  #   * diazepam injection
  FCdata$med_diaz <- NA
  FCdata$med_diaz <- ifelse(FCdata$v906_07 %in% c(2) | FCdata$v534d %in% c(2), 1, 0)
  #   * aspirin capsule tablet: !!! NOT IN MY ORIGINAL
  FCdata$med_aspirin <- NA
  FCdata$med_aspirin <- ifelse(FCdata$v903_02 %in% c(2), 1, 0)
  
  
  ## NON-INDEX INDICATORS
  # General med: folic acid tablets
  FCdata$med_folicAcid <- NA
  FCdata$med_folicAcid <- ifelse(
    FCdata$v908_03 %in% c(2) | # "General med:Folic acid tablets"
      FCdata$v432c %in% c(2) | # "ANC medications available:Folic acid tablets"
      FCdata$v432d %in% c(2) | # "ANC medications available:Combined iron and folic acid tablets"
      FCdata$v910_04 %in% c(2), # "General med:Iron + Folic acid tablets" 
    1, 0)
  # Iron tablets
  FCdata$med_ironTabs <- NA
  FCdata$med_ironTabs <- ifelse(
    FCdata$v432b %in% c(2) | # "ANC medications available:Iron tablets"
      FCdata$v432d %in% c(2) | # "ANC medications available:Combined iron and folic acid tablets"
      FCdata$v910_03 %in% c(2) | # "General med:Iron tablets"
      FCdata$v910_04 %in% c(2), # "General med:Iron + Folic acid tablets"
    1, 0
  )
  # Tetanus Toxoid vaccine
  FCdata$med_ttVax <- NA
  FCdata$med_ttVax <- ifelse(
    FCdata$v432e %in% 2 | # v432e "ANC medications available:Tetanus Toxoid vaccine"
      FCdata$lv203a %in% 2 | # lv203a "Avail/valid:Tetanus toxoid in pharmacy"
      FCdata$v918_05 %in% 2, # v918_05  "General med:Tetanus Toxoid vaccine"
    1, 0
  )
  # IPT medication
  FCdata$med_IPT <- NA
  FCdata$med_IPT <- ifelse(FCdata$v922_03 %in% 2, 1, 0)# v922_03  "Antimalarial:Fansidar (Sulfadoxine+Pyrimethamine (SP)"
  # ITN (not technically a medicine)
  FCdata$med_ITN <- NA
  FCdata$med_ITN <- ifelse(
    FCdata$v432f %in% 2 | # v432f    "ANC medications available:ITN mosquito bed net"
      FCdata$v946_18 %in% 1,
    1, 0# v946_18  "Other supply: Insecticide treated mosquito nets (ITNs)" 1 "Observed"
  )
  
  # NON SRI Meds
    # label variable v922_13  "Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 6 tablet"
    # label variable v922_14  "Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 12 table"
    # label variable v922_15  "Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 18 table"
    # label variable v922_16  "Antimalarial:combination therapy (ACT) Artemether Lumefrantrine(ALU-LA) 24 table"
    # label variable v922_17  "Antimalarial:Other anti-malarial medicine" (I'm not including this one)
    # label variable v922_18  "Antimalarial:combination therapy (ACT) CS Artemether Amodiaquine(ASAQ) 25mg/67.5"
    # label variable v922_19  "Antimalarial:combination therapy (ACT) CS Artemether Amodiaquine(ASAQ) 50mg/135m"
    # label variable v922_20  "Antimalarial:combination therapy (ACT) CS Artemether Amodiaquine(ASAQ) 100mg/270"
  
    # label define V922_13 
    # 0 "Never available"
    # 2 "Yes, observed, at least 1 valid"
    # 3 "Yes, observed, none valid"
    # 4 "Yes, reported available, not seen"
    # 5 "Not available today"
  
  FCdata$med_ACT <- NA
  FCdata$med_ACT <- ifelse(
    FCdata$v922_13 %in% c(2) |
      FCdata$v922_14 %in% c(2) |
      FCdata$v922_15 %in% c(2) |
      FCdata$v922_16 %in% c(2), 
      # FCdata$v922_17 %in% c(2) | # Not including because it's not specifically ACT
      # FCdata$v922_18 %in% c(2) |
      # FCdata$v922_19 %in% c(2) |
      # FCdata$v922_20 %in% c(2),
    1,0
  )
  
  
  if (FCdata$v000[1] == "MW6") {
    FCdata$med_ACT <- ifelse(FCdata$med_ACT %in% 1 | FCdata$v922_18 %in% 2, 1, 0)
    FCdata$med_ACT <- ifelse(FCdata$med_ACT %in% 1 | FCdata$v922_19 %in% 2, 1, 0)
    FCdata$med_ACT <- ifelse(FCdata$med_ACT %in% 1 | FCdata$v922_20 %in% 2, 1, 0)
  }
  
  if (FCdata$v000[1] == "HT7") {
    FCdata$med_ACT <- ifelse(
      FCdata$med_ACT %in% 1 | 
        FCdata$v922_18 %in% 2 | 
        FCdata$v922_19 %in% 2, 
      1, 0)
  }
  
 
  
  
  # DOMAIN SCORE: sri_med
  medVars <- c("med_amitr", # Not in original, but included in gage's work
               "med_amlod", #
               "med_amoxicillinsyr", #
               "med_amoxicillintab", #
               "med_ampicil", #
               "med_beclom", #
               "med_ceftria", #
               "med_enal", #
               "med_genta",#
               "med_glib",#
               "med_ibu",
               "med_insul",#
               "med_met",#
               "med_omep",#
               "med_ors",#
               "med_paracetamoltab_st",
               "med_sal",#
               "med_simvas",#
               "med_zinc",#
               "med_betab",#
               "med_mag", #
               "med_oxyto", #
               "med_thiaz",#
               "med_diaz", #
               "med_aspirin" #!!! Added
               )
  
  # Figure out how many columns are "eligible": not entirely NA or 0
  valid_meds <- medVars[
    sapply(FCdata[medVars], function(x) any(!is.na(x) & x != 0))
  ]
  
  
  #   DOMAIN SCORE: sri_med
  FCdata$sri_med_avail <- NA
  FCdata$sri_med_avail <- rowSums(FCdata[valid_meds], na.rm = TRUE) / length(valid_meds)
  FCdata$sri_med_valid <- length(valid_meds)
  
  #   DOMAIN SCORE: sri_med
  FCdata$sri_med <- NA
  FCdata$sri_med <- rowSums(FCdata[medVars], na.rm = TRUE) / length(medVars)
  
  FCdata$sri_score <- NA
  FCdata$sri_score <-rowSums(FCdata[c("sri_basicamenities",
                                      "sri_basicequip",
                                      "sri_infprev",
                                      "sri_diagcapacity",
                                      "sri_med")], na.rm = TRUE) / 5
  
  FCdata$sri_score_avail <- NA
  FCdata$sri_score_avail <-rowSums(FCdata[c(
    "sri_basicamenities_avail",
    "sri_basicequip_avail",
    "sri_infprev_avail",
    "sri_diagcapacity_avail",
    "sri_med_avail")], na.rm = TRUE) / 5
  
  
  sriVars <- c(medVars, 
                infprevVars, 
                basicamenitiesVars, 
                basicequipVars, 
                diagcapacityVars)
  
  # Get the sri
  mca_result <- ExPosition::epMCA(FCdata[sriVars], graphs = F)
  # create sri index
  FCdata$sri_index <- mca_result$ExPosition.Data$fi[,1]
  
  # make sure it runs in an interpretable direction
  if (cor(FCdata$sri_index, FCdata$sri_score) < 0) {
    FCdata$sri_index <- -FCdata$sri_index
  }
  
  FCdata$sri_index_pct <- Hmisc::cut2(FCdata$sri_index, g = 5) |> as.numeric() - 1
  
  
  
  
  #blank ####
  
  #   Family Planning Services ####
  #   * Guidelines on Family Planning	gl_fp_st
  #   * Staff trained in FP (at least 1 in the past 2 years)	factrainfp_any_r
  #   * Blood Pressure Apparatus	eqfp_bp_st
  #   * Combined estrogen progesterone oral contraceptive pills	med_ocontraceptives_st
  #   * Injectable contraceptives	med_injcontraceptives_st
  #   * Male condom	
  
  
  #   Antenatal Care ####
  #   Child Health Services: Routine child immunization ####
  #   Child health services: preventative and curative care ####
  #   Basic Obstetric Care ####
  #   Comprehensive Obstetric Care ####
  #   Adolescent Health ####
  
  
  # DUMMY & MODEL CODING
  
  # Days per month services offered
  
  # | Variable | Label |
  #   |---|---|
  #   | v200b | days/mo at facility: bcg immunization |
  #   | v200c | days/mo at facility: sick child consult |
  #   | v200d | days/mo at facility: growth monitoring |
  #   | v200e | days/mo at facility: routine pentavalent |
  #   | v200g | days/mo at facility: routine polio (vpo) |
  #   | v200h | days/mo at facility: routine measles |
  #   | v201b | days/mo at outreach: bcg immunization |
  #   | v201c | days/mo at outreach: sick child consult |
  #   | v201d | days/mo at outreach: growth monitoring |
  #   | v201e | days/mo at outreach: routine pentavalent |
  #   | v201g | days/mo at outreach: routine polio (vpo) |
  #   | v201h | days/mo at outreach: routine measles |
  #   | v301a | days/month fp services offered |
  #   | v401a | days/month for antenatal care |
  #   | v601a | number days/month offered |
  #   | v1140 | days/month tb services available |
  #   | v1307a | days/month hiv testing services available |
  
  
  # days/mo at facility: bcg immunization
  FCdata$daysMo_FCbcg <- FCdata$v200b
  # days/mo at facility: sick child consult
  FCdata$daysMo_FCsickChildConsult <- FCdata$v200c
  # days/mo at facility: growth monitoring
  FCdata$daysMo_FCgrowthMonitor <- FCdata$v200d
  # days/mo at facility: routine pentavalent
  FCdata$daysMo_FCpentavalent <- FCdata$v200e
  # days/mo at facility: routine polio (vpo)
  FCdata$daysMo_FCpolio <- FCdata$v200g
  # days/mo at facility: routine measles
  FCdata$daysMo_FCmeasles <- FCdata$v200h
  # days/mo at outreach: bcg immunization
  FCdata$daysMo_OUbcg <- FCdata$v201b
  # days/mo at outreach: sick child consult
  FCdata$daysMo_OUsickChildConsult <- FCdata$v201c
  # days/mo at outreach: growth monitoring
  FCdata$daysMo_OUgrowthMonitor <- FCdata$v201d
  # days/mo at outreach: routine pentavalent
  FCdata$daysMo_OUpentavalent <- FCdata$v201e
  # days/mo at outreach: routine polio (vpo)
  FCdata$daysMo_OUpolio <- FCdata$v201g
  # days/mo at outreach: routine measles
  FCdata$daysMo_OUmeasles <- FCdata$v201h
  # days/month fp services offered
  FCdata$daysMo_FPservices <- FCdata$v301a
  # days/month for antenatal care
  FCdata$daysMo_AntenatalCare <- FCdata$v401a
  # number days/month offered
  FCdata$daysMo_NumOffered <- FCdata$v601a
  # days/month tb services available
  FCdata$daysMo_TBservices <- FCdata$v1140
  # days/month hiv testing services available
  FCdata$daysMo_HIVtesting <- FCdata$v1307a
  
  
  # Services available: Child vaccination - See V200s
  FCdata$svc_childVax <- ifelse(FCdata$v012b %in% 1, 1, 0)
  # Services available: Stores vaccines - See LV200s
  FCdata$svc_storesVax <- ifelse(FCdata$v018 %in% 1, 1, 0)
  # Services available: Growth monitoring - See V200s
  FCdata$svc_growthMonitor <- ifelse(FCdata$v012d %in% 1, 1, 0)
  # Services available: Curative care for children < 5 years - See V200s
  FCdata$svc_curativeCareChild <- ifelse(FCdata$v012c %in% 1, 1, 0)
  # Services available: Family Planning - See V300s
  FCdata$svc_familyPlanning <- ifelse(FCdata$v013 %in% 1, 1, 0)
  # Services available: Sterilizations conducted(Q1302(12) or Q1302(13)=1) - See V300s
  FCdata$svc_sterilizations_male <- ifelse(FCdata$v013s %in% c(1, 3), 1, 0)
  # Services available: Sterilizations conducted(Q1302(12) or Q1302(13)=1) - See V300s
  FCdata$svc_sterilizations_female <- ifelse(FCdata$v013s %in% c(1, 2), 1, 0)
  # Services available: Antenatal(ANC) care - See V400s
  FCdata$svc_antenatalCare <- ifelse(FCdata$v014a %in% 1, 1, 0)
  # Services available: Prevention of mother-to-child transmission of HIV(PMTCT) - See V1200s
  FCdata$svc_pmtct <- ifelse(FCdata$v045 %in% 1, 1, 0)
  # Services available: PMTCT integrated in delivery services - See V1280s
  FCdata$svc_pmtctDelivery <- ifelse(FCdata$v045a %in% 1, 1, 0)
  # Services available: Normal delivery - See V500s
  FCdata$svc_normalDelivery <- ifelse(FCdata$v015a %in% 1, 1, 0)
  # Services available: Dx/Tx of malaria - See V1000s
  # treat malaria
  ##  vt825    "Facility offers malaria dx/tx"
  FCdata$svc_malariaDxTx <- NA
  FCdata$svc_malariaDxTx <- ifelse(FCdata$vt825 %in% 1, 1, 0)
  # rdt / microscopy of malaria
  FCdata$svc_malariaMicroRDT <- NA
  FCdata$svc_malariaMicroRDT <- ifelse(FCdata$vt824 %in% c(1), 1, 0)
  
  # Services available: Dx/Tx of STIs, excluding HIV - See V600s
  FCdata$svc_stiDxTx <- ifelse(FCdata$v016 %in% 1, 1, 0)
  # Services available: Dx/TX/Follow-up TX of TB - See V1100s
  FCdata$svc_tbDxTx <- ifelse(FCdata$v043 %in% 1, 1, 0)
  # Services available: HIV testing and counseling - See V1300s
  FCdata$svc_hivTesting <- ifelse(FCdata$v042 %in% 1, 1, 0)
  # Services available: HIV/AIDS antiretroviral(ART) - See V1400s
  FCdata$svc_art <- ifelse(FCdata$v044 %in% 1, 1, 0)
  # Services available: HIV/AIDS care and support services - See V1500s !!!
  FCdata$svc_hivCareSupport <- ifelse(FCdata$v046 %in% 1 | FCdata$v1515 %in% 1, 1, 0)
  # Services available: Diagnosis/management of non-communicable diseases - See V1600s
  FCdata$svc_nonCommDisease <- ifelse(FCdata$v048 %in% 1, 1, 0)
  # Services available: Minor surgery - See V1700s
  FCdata$svc_minorSurgery <- ifelse(FCdata$v049 %in% 1, 1, 0)
  # Services available: Caesarean delivery - See V500s
  FCdata$svc_caesarean <- ifelse(FCdata$v015c %in% 1, 1, 0)
  # Services available: Laboratory diagnostic services - See V800s
  FCdata$svc_labDiagnostic <- ifelse(FCdata$v034 %in% 1, 1, 0)
  # Services available: Blood typing - See V860s
  FCdata$svc_bloodTyping <- ifelse(FCdata$v034a %in% 1 | FCdata$v840l %in% 1, 1, 0)
  # Services available: Blood transfusion - See V830s, V860s, V890s
  FCdata$svc_bloodTransfusion <- ifelse(FCdata$v034b %in% 1 | FCdata$v839 %in% 1, 1, 0)
  # Child health services provided at facility:dx/tx child malnutrition
  FCdata$svc_childMalnutritionDxTx <- ifelse(FCdata$v267a %in% 1, 1, 0)
  # Child health services provided at facility:provide vitamin A supplementation to children
  FCdata$svc_vitaminASupp <- ifelse(FCdata$v267b %in% 1, 1, 0)
  # Child health services provided at facility:provide iron supplementation to children
  FCdata$svc_ironSupp <- ifelse(FCdata$v267c %in% 1, 1, 0)
  # Child health services provided at facility:provide zinc supplementation to children
  FCdata$svc_zincSupp <- ifelse(FCdata$v267d %in% 1, 1, 0)
  
  # As part of routine ANC
  ## Provision as part of routine ANC: Iron supplementation
  FCdata$svcANC_ironSupp <- NA
  FCdata$svcANC_ironSupp <- ifelse(FCdata$v461a %in% 1, 1, 0)
  ## Provision as part of routine ANC: Folic acid supplementation
  FCdata$svcANC_folicAcid <- NA
  FCdata$svcANC_folicAcid <- ifelse(FCdata$v461a %in% 1, 1, 0)
  ## Provision as part of routine ANC: Intermittent Preventive treatment (IPT) for ma
  FCdata$svcANC_IPTp <- NA
  FCdata$svcANC_IPTp <- ifelse(FCdata$v461c %in% 1, 1, 0)
  ## Provision as part of routine ANC: Tetanus toxoid vaccination
  FCdata$svcANC_TTvax <- NA
  FCdata$svcANC_TTvax <- ifelse(FCdata$v461d %in% 1, 1, 0)
  
  ## Routinely take blood pressure during ANC
  FCdata$svcANC_takeBP <- NA
  FCdata$svcANC_takeBP <- ifelse(FCdata$v407b %in% c(1,2), 1, 0)
  
  # Other guidelines
  #   IMCI
  FCdata$guideIMCI <- NA
  FCdata$guideIMCI <- ifelse(FCdata$v266a %in% c(1), 1, 0) # 1 is observed, 2 is reported UNFINISHED !!!
  #   ANC
  FCdata$guideANC <- NA
  FCdata$guideANC <- ifelse(FCdata$v434a %in% c(1), 1, 0) # observed (reported is 2)
  #   v271     "Growth monitoring: guidelines available"
  FCdata$guideGrowth <- NA
  FCdata$guideGrowth <- ifelse(FCdata$v271 %in% c(1), 1, 0)
  #   IPT
      # label define V434B   
      # 0 "Not available"
      # 1 "Yes, observed"
      # 2 "Yes, reported"
  FCdata$guideIPT <- NA
  FCdata$guideIPT <- ifelse(FCdata$v434b %in% c(1), 1, 0)
  #   Malaria dx tx
      # RDT guidelines
        # label variable v1006    "Malaria: Training manual, poster, job aid for Malaria RDT use"
        # label define V1006   
        # 0 "Not available"
        # 1 "Observed"
        # 2 "Reported, not seen"
  FCdata$guideMalariaRDT <- NA
  FCdata$guideMalariaRDT <- ifelse(FCdata$v1006 %in% 1, 1, 0)
      # National malaria dx/tx guidelines
  FCdata$guideMalariaNational <- NA
  FCdata$guideMalariaNational <- ifelse(FCdata$v1007a %in% 1, 1, 0)
      # Other malaria dx/tx guidelines
  FCdata$guideMalariaOther <- NA
  FCdata$guideMalariaOther <- ifelse(FCdata$v1007x %in% 1, 1, 0)
      # Composite
  FCdata$guide_malariaDxTx <- NA
  FCdata$guide_malariaDxTx <- ifelse(
    FCdata$guideMalariaNational == 1 |
      FCdata$guideMalariaOther == 1,
    1, 0
      )
  
  
  # any staff with training
  FCdata$trained2yrs_anc_anyStaff <- NA
  FCdata$trained2yrs_anc_anyStaff <- ifelse(FCdata$v2003b > 0 | FCdata$v2003c > 0, 1, 0)
  # v2001b   "Child health: n of providers in service with any IMCI, GM or EPI training last 2"
  FCdata$trained2yrs_imci_anyStaff <- NA
  FCdata$trained2yrs_imci_anyStaff <- ifelse(FCdata$v2001b > 0 | FCdata$v2001c > 0, 1, 0)
  # label variable v2012b   "Malaria: n of providers in service with training last 2 years"
  FCdata$trained2yrs_malaria_anyStaff <- NA
  FCdata$trained2yrs_malaria_anyStaff <- ifelse(FCdata$v2012b > 0 | FCdata$v2012c >0, 1, 0)
  
  
  # Antenatal care services
  ## Service availability 
  sa_ancIndicators <- c("svc_antenatalCare", "svc_ironSupp", "svcANC_folicAcid", "svcANC_TTvax", "svcANC_takeBP")
  ## Service Readiness
  sr_ancIndicators <- c(
    # Staff and guidelines (Missing: ANC check-lists and/or jobaids)
    "guideANC",
    "trained2yrs_anc_anyStaff",
    # Equipment
    "eq_bp_st",
    # Diagnostics
    "diag_hemog",
    "diag_urineprot",
    # Medicine and commodities - Observed in service area OR where routinely stored; in stock with at least one valid
    #   Iron Tablets
    "med_ironTabs",
    #   Folic acid tablets
    "med_folicAcid",
    #   Tetanus toxoid vaccine
    "med_ttVax",
    #   IPT drug Sulfadoxine + Pyrimethamine(SP)
    "med_IPT",
    #   ITNs: ITNs or vouchers available for distribution
    "med_ITN"
    )
  
  # Child health services: preventative and curative care
  ## Service availability
  sa_scIndicators <- c( # % of facilities offering:
    # Preventive and curative care for children under 5
    "svc_curativeCareChild",
    # Malnutrition diagnosis and treatment
    "svc_childMalnutritionDxTx",
    # Vitamin A supplementation
    "svc_vitaminASupp",
    # Iron supplementation
    "svc_ironSupp",
    # ORS and zinc supplementation
    "svc_zincSupp",
    # Growth monitoring
    "svc_growthMonitor",
    # Treatment of pneumonia APPEARS TO NOT BE INCLUDED !!!
    # Administration of amoxicillin for the treatment of pneumonia in children APPEARS TO NOT BE INCLUDED !!!
    # Treatment of malaria in children
    "svc_malariaDxTx"
  )
  
  # Child AND infant scale
  FCdata$eq_chInf_scale_st <- ifelse(FCdata$eq_inf_scale_st == 1 & FCdata$eq_ch_scale_st == 1, 1, 0)
  
  ## Service readiness
  sr_scIndicators <- c( # % of facilities providing child curative care services with tracer items on the day of the assessment
    # Staff and guidelines
    ## Guidelines for IMCI: Guidelines observed in service area.
    "guideIMCI",
    ## Guidelines for growth monitoring
    "guideGrowth",
    ## Staff trained in IMCI: 
    "trained2yrs_imci_anyStaff",
    ## Staff trained in growth monitoring (included in "trained2yrs_imci_anyStaff")
    # Equipment: Observed availability, reported functionality, and in service area or adjacent area.
    ## Child and infant scale
    "eq_chInf_scale_st",
    ## Length/height measuring equipment
    "eq_heightLength_st",
    ## Thermometer
    "eq_therm_st",
    ## Stethoscope
    "eq_stetho_st",
    ## Growth chart
    "eq_growthChart_st",
    # Diagnostics
    ## Haemoglobin (Hb)
    "diag_hemog"
    ## Test parasite in stool (general microscopy)
    ## Malaria diagnostic capacity
    # Medicines and commodities: Observed in service area OR where routinely stored; in stock with at least one valid.
    ## Oral rehydration solution packet
    ## Amoxicillin (dispersible tablet 250 or 500 mg OR syrup/suspension)
    ## Co-trimoxazole syrup/suspension
    ## Paracetamol syrup/suspension
    ## Vitamin A capsules
    ## Me-/albendazole cap/tab
    ## Zinc sulphate tablets, dispersible tablets or syrup
  )
  
  # Malaria service readiness (Lee, 2017 et al.)
  sr_malariaVars <- c(
    # staff and guidelines
    "guide_malariaDxTx",
    "guideIPT",
    "trained2yrs_malaria_anyStaff",
      # staff trained in IPT: NOT ACVAILABLE
    # Diagnostics
    "diag_malaria",
    # Medicines and commodities
    "med_ACT",
    "med_paracetamoltab_st",
    "med_IPT",
    "med_ITN"
  )
  
  # SRI Malaria (ignoring specific domains)
  FCdata$sr_malaria <- NA
  FCdata$sr_malaria <-rowSums(FCdata[sr_malariaVars], na.rm = TRUE) / length(sr_malariaVars)
  
  
  
  
  
  
  
  
  
  
  return(FCdata)
  
}




