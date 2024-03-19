#'
#' @name cleanBR
#' @rdname cleanBR
#' @title  cleanBR
#'
#' @description  This function cleans the birth recode data using code from the DHS repo
#'
#' @param BRdata This parameter takes a birth recode file from DHS. It should be in stata format, imported without transforming factors
#'
#' @author J.W. Rozelle
#' 
#'
#'
#' @examples
#' 
#' # Load data
#' br_lb7 <- foreign::read.dta(file.path(dataDir, "LBBR7AFL", "LBBR7AFL.DTA"), convert.factors = F)
#'
#' # Clean BR data 
#' br_lb7 <- cleanBR(br_lb7)
#' 
#' 


# # TESTING
# BRdata <- dhs_NeoPBR.list$ETBR81
# BRdata <- dhs_NeoPBR.list$ALBR71
# BRdata <- dhs_NeoPBR.list$GUBR71
# BRdata <- dfBR_list$MW7
# BRdata <- dhs_LBSL_BR.list$LBBR51


# BRdata <- BRdata.list.bkp$IA7
# BRdata <- BRdata.list$IA7
# BRdata <- BRdata.list$UG7
# BRdata <- BRdata.list$NG7


# # Make sure to pull full family lists
# BRdata$femaleID <- NA
# 
# BRdata$femaleID <- paste(BRdata$v001, BRdata$v002, BRdata$v003, sep="_")
# 
# femaleID.vec <- unique(BRdata$femaleID)
# 
# 
# BRdata <- dplyr::filter(BRdata, femaleID %in% femaleID.vec[1:5000])





# n.cores = 10

# BR Data recode

cleanBR <- function(BRdata, n.cores = NULL) {
  
  
  require(labelled)
  require(dplyr)
  require(iotools)
  if (!is.null(n.cores)) {
    require(parallel)
  }
  
  
  # Generate female ID from data
  BRdata$femaleID <- NA
  BRdata$femaleID <- paste(BRdata$v001, BRdata$v002, BRdata$v003, sep="_")
  
  # child ID
  BRdata$childID <- NA
  BRdata$childID <- paste(BRdata$femaleID, BRdata$bord, sep = "_")
  
  # child ID using bidx
  BRdata$child_bidx <- NA
  BRdata$child_bidx <- paste(BRdata$femaleID, BRdata$bidx, sep = "_")
  
  # birth order dummy variables
  #   1
  BRdata$bord1 <- NA
  BRdata$bord1 <- ifelse(BRdata$bord == 1, 1, 0)
  #   2
  BRdata$bord2 <- NA
  BRdata$bord2 <- ifelse(BRdata$bord == 2, 1, 0)
  #   3
  BRdata$bord3 <- NA
  BRdata$bord3 <- ifelse(BRdata$bord == 3, 1, 0)
  #   4
  BRdata$bord4 <- NA
  BRdata$bord4 <- ifelse(BRdata$bord == 4, 1, 0)
  #   5
  BRdata$bord5 <- NA
  BRdata$bord5 <- ifelse(BRdata$bord == 5, 1, 0)
  #   6 or more
  BRdata$bord6p <- NA
  BRdata$bord6p <- ifelse(BRdata$bord >= 6, 1, 0)
  
  

  
  # Normalized age
  BRdata$age_normalized <- NA
  BRdata$age_normalized <- (BRdata$v012 - mean(BRdata$v012)) / sd(BRdata$v012)
  
  # currently married
  BRdata$married <- NA
  BRdata$married <- ifelse(BRdata$v501 %in% c(1,2), 1, 0)
  BRdata$married <- ifelse(is.na(BRdata$v501), NA, BRdata$married)
  
  # respondent education level
  BRdata$edLevel <- NA
  BRdata$edLevel <- BRdata$v106
  
  BRdata$child_mob_cmc <- NA
  BRdata$child_mob_cmc <- BRdata$b3
  
  BRdata$child_dob_cdc <- NA
  BRdata$child_dob_cdc <- BRdata$b18
  
  BRdata$child_dob_yyyy <- NA
  BRdata$child_dob_yyyy <- floor((BRdata$child_mob_cmc - 1)/12) + 1900
  
  BRdata$child_dob_mm <- NA
  BRdata$child_dob_mm <- BRdata$child_mob_cmc - (BRdata$child_dob_yyyy-1900)*12
  
  # imputed year of birth
  BRdata$c_dob_yyyy <- NA
  BRdata$c_dob_yyyy <- BRdata$b2
  # imputed month of birth
  BRdata$c_dob_mm <- NA
  BRdata$c_dob_mm <- BRdata$b1
  # imputed dob
  BRdata$c_dob_dd <- NA

  # make accommodations for specific countries
  if (BRdata$v000[1] == "GU6") {
    BRdata$c_dob_dd <- 15
    BRdata$c_dob_dd <- ifelse(!is.na(BRdata$hw16) & BRdata$hw16 != 98, BRdata$hw16, BRdata$c_dob_dd)
  } else if (BRdata$v000[1] == "MM7") {
    BRdata$c_dob_dd <- 15
  } else if (!"b17" %in% names(BRdata)) {
    BRdata$c_dob_dd <- NA
    
    if ("hw16" %in% names(BRdata)) {
      BRdata$c_dob_dd <- ifelse(!is.na(BRdata$hw16) & !BRdata$hw16 %in% c(98, 99), BRdata$hw16, BRdata$c_dob_dd)
    }
    
  } else {
    BRdata$c_dob_dd <- BRdata$b17
    BRdata$c_dob_dd <- ifelse(BRdata$b17 == 98, NA, BRdata$c_dob_dd)
  }
  
  
  
  # date of birth in R format (if missing the day, change it to 15)
  BRdata$c_dobR <- NA
  BRdata$c_dobR <- as.Date(paste(BRdata$c_dob_yyyy, BRdata$c_dob_mm, ifelse(!is.na(BRdata$c_dob_dd), BRdata$c_dob_dd, 15), sep = "-"))
  
  # age in months
  BRdata$age_in_months <- NA
  BRdata$age_in_months <- BRdata$b19
  
  # Rename Wealth Index 
  BRdata$wealthIndex <- NA
  BRdata$wealthIndex <- BRdata$v190 - 1
  
  
  BRdata$rural <- NA
  BRdata$rural <- ifelse(BRdata$v025 %in% c(2), 1,0)
  
  BRdata$clusterID <- NA
  BRdata$clusterID <- BRdata$v001
  
  # rename child alive
  BRdata$alive <- NA
  BRdata$alive <- BRdata$b5
  
  
  # ******************************************************************************
  # Program: 			  FE_INT.R
  # Purpose: 		    Code fertility indicators from birth history reflecting birth intervals  
  # Data inputs: 		BR dataset
  # Data outputs:		coded variables, and output on screen and in excel tables
  # Author:				  Mahmoud Elkasabi
  # Date last modified: March 12 2021 by Mahmoud Elkasabi
  # ******************************************************************************
  #   
  # -----------------------------------------------------------------------------#
  # # Variables created in this file:
  #
  #//BIRTH INTERVALS
  #fe_int			"Birth interval of recent non-first births"
  #fe_age_int		"Age groups for birth interval table"
  #fe_bord			"Birth order"
  #fe_pre_sex		"Sex of preceding birth" [I ADJUSTED FILTER]
  #fe_pre_surv		"Survival of preceding birth" [I ADJUSTED FILTER]
  #fe_int_med		"Median number of months since preceding birth"
  #################################################################################
  
  BRdata = BRdata %>%
    mutate(wt = v005/1000000)
  
  # BIRTH INTERVALS
  
  #fe_int
  BRdata = BRdata %>%
    mutate(fe_int = case_when(
      b11 <= 17 ~ 1,
      b11 >= 18 & b11 <= 23 ~ 2,
      b11 >= 24 & b11 <= 35 ~ 3,
      b11 >= 36 & b11 <= 47 ~ 4,
      b11 >= 48 & b11 <= 59 ~ 5,
      b11 >= 60 ~ 6)) %>%
    mutate(fe_int = sjlabelled::set_label(fe_int, label = "Birth interval of recent non-first births")) %>%
    mutate(fe_int = factor(fe_int, levels = c(1,2,3,4,5,6), 
                           labels = c("less than 17 months", "18-23 months", "24-35 months", "36-47 months", "48-59 months", "60+ months" )))
  
  #fe_age_int
  BRdata = BRdata %>%
    mutate(fe_age_int = case_when(
      v013 ==1 ~ 1,
      v013 >=2 & v013 <=3 ~ 2,
      v013 >= 4 & v013 <= 5  ~ 3,
      v013 >= 6 & v013 <= 7  ~ 4)) %>%
    mutate(fe_age_int = sjlabelled::set_label(fe_age_int, label = "Age groups for birth interval table")) %>%
    mutate(fe_age_int = factor(fe_age_int, levels = c(1,2,3,4), labels = c("1", "2-3", "4-5", "6-7")))
  
  #fe_bord
  # b0 = child is twin, 0 = single birth, 1 = first of multiple, etc.
  # bord = birth order
  BRdata$fe_bord <- NA
  BRdata$fe_bord <- ifelse(BRdata$b0 <= 1, BRdata$bord, BRdata$fe_bord)
  BRdata$fe_bord <- ifelse(BRdata$b0 >= 2, BRdata$bord - BRdata$b0 + 1, BRdata$fe_bord)
  
  BRdata = BRdata %>%
    # mutate(fe_bord = case_when(
    #   b0 <= 1 ~ bord,
    #   b0 >= 2 ~ bord - b0 + 1)) %>%
    mutate(fe_bord_cat = case_when(
      fe_bord ==1 ~ 1,
      fe_bord ==2 | fe_bord ==3 ~ 2,
      fe_bord >= 4 & fe_bord <= 6  ~ 4,
      fe_bord >= 7 ~ 7)) %>%
    mutate(fe_bord_cat = sjlabelled::set_label(fe_bord_cat, label = "Birth order categories")) %>%
    mutate(fe_bord_cat = factor(fe_bord_cat, levels = c(1,2,4,7), labels = c("1", "2-3", "4-6", "7+")))
  
  
  #fe_pre_sex & fe_pre_surv
  BRdata = BRdata %>%
    arrange(caseid, bord) %>%
    mutate(ID = row_number()) %>%
    mutate(fe_pre_sex = 0) %>%
    mutate(fe_pre_surv = 0)
  
  # # loop
  # for (i in 2:(max(BRdata$ID))) {
  # 
  #   if (BRdata$caseid[i] == BRdata$caseid[i-1]) {
  #     BRdata$fe_pre_sex[i] <- BRdata$b4[i-1]
  #     BRdata$fe_pre_surv[i] <- BRdata$b5[i-1]
  #   }
  # }
  
  
  # OPTIMIZED 
  BRdata <- BRdata %>%
    arrange(ID) %>% # Ensure data is sorted by caseid and ID if it's not already
    group_by(caseid) %>%
    mutate(
      fe_pre_sex = lag(b4, default = first(b4)), # Shift b4 down by one within each caseid group
      fe_pre_surv = lag(b5, default = first(b5)) # Shift b5 down by one within each caseid group
    ) %>%
    ungroup() # Optionally remove the grouping structure
  
  BRdata$fe_pre_sex <- ifelse(BRdata$bord == 1, NA, BRdata$fe_pre_sex)
  BRdata$fe_pre_surv <- ifelse(BRdata$bord == 1, NA, BRdata$fe_pre_surv)
  
    
  
  gc()
  
  BRdata = BRdata %>%
    
    # !!! filter(b19 <= 59 & !is.na(b11)) %>% 
    mutate(fe_pre_sex = factor(fe_pre_sex, levels = c(1,2), labels = c("Male", "Female"))) %>%
    mutate(fe_pre_sex = sjlabelled::set_label(fe_pre_sex, label = "Sex of preceding birth")) %>%
    mutate(fe_pre_surv = factor(fe_pre_surv, levels = c(0,1), labels = c("Not alive", "Alive"))) %>%
    mutate(fe_pre_surv = sjlabelled::set_label(fe_pre_surv, label = "Survival of preceding birth")) %>%  
    mutate(time = b11)
  
  
  # /*****************************************************************************************************
  # Program: 			RH_DEL.R
  # Purpose: 			Code Delivery Care indicators
  # Data inputs: 	BR dataset
  # Data outputs:	coded variables
  # Author:			  Shireen Assaf 
  # Date last modified: Sept 10, 2021 by Shireen Assaf 		
  # *****************************************************************************************************/
  #
  # /*----------------------------------------------------------------------------//
  # Variables created in this file:
  # rh_del_place		"Live births by place of delivery"
  # rh_del_pltype		"Live births by type of place"
  # rh_del_pv			  "Person providing assistance during birth"
  # rh_del_pvskill	"Skilled provider providing assistance during birth"
  # rh_del_ces			"Live births delivered by cesarean"
  # rh_del_cestime	"Timing of decision to have Cesarean"
  # rh_del_stay			"Duration of stay following recent birth"
  # /----------------------------------------------------------------------------*/
  # 
  BRdata <- BRdata %>%
    mutate(wt = v005/1000000)
  
  # period and age of child
  # choose reference period, last 2 years (24 months) or last 5 years (60 months)
  # Using a period of the last 2 years will not match final report but would provide more recent information.
  BRdata <- BRdata %>%
    mutate(period = 60)
  
  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(BRdata))))
    BRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(BRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}
  
  if (b19_included==1) {
    BRdata <- BRdata %>%
      mutate(age = b19)
  } else {
    BRdata <- BRdata %>%
      mutate(age = v008 - b3)
  }
  
  
  # //Place of delivery
  # Note: please check the categories of m15 especially for older surveys. The category values may differ. 
  
  if (BRdata$v000[1] == "IA7") {
    
    BRdata <- BRdata %>%
      mutate(rh_del_place =
               case_when(
                 m15 >=20 & m15<40   ~ 1 ,
                 m15 >=10 & m15<20   ~ 2,
                 m15 >=40 & m15<99   ~ 3 ,
                 m15 == 99 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_place = c(99))) %>%
      set_value_labels(rh_del_place = c("Health facility" = 1, "Home"=2, "Other"=3, "Missing"=9  )) %>%
      set_variable_labels(rh_del_place = "Live births by place of delivery")
    # 
    
    # // Facility based delivery 
    BRdata$rh_del_fbd <- NA
    BRdata$rh_del_fbd <- ifelse(BRdata$rh_del_place %in% c(1), 1, 0)
    BRdata$rh_del_fbd <- ifelse(is.na(BRdata$rh_del_place) | BRdata$rh_del_place == 9, NA, BRdata$rh_del_fbd)
    
    # //Place of delivery - by place type
    BRdata <- BRdata %>%
      mutate(rh_del_pltype =
               case_when(
                 m15 >=20 & m15<30   ~ 1 ,
                 m15 >=30 & m15<40   ~ 2 ,
                 m15 >=10 & m15<20   ~ 3,
                 m15 >=40 & m15<99   ~ 4 ,
                 m15 == 99 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pltype = c(99))) %>%
      set_value_labels(rh_del_pltype = c("Health facility - public" = 1, "Health facility - private" = 2, "Home"=3, "Other"=4, "Missing"=9  )) %>%
      set_variable_labels(rh_del_pltype = "Live births by type of health facility")
    
    # //Assistance during delivery
    # **Note: Assistance during delivery and skilled provider indicators are both country specific indicators. 
    # **The table for these indicators in the final report would need to be checked to confirm the code below.
    
    # India  Births delivered with the assistance of doctors, auxiliary nurse midwives, nurses, midwives, and lady health visitors.
    
    # label variable m3a      "Assistance: doctor" *
    # label variable m3b      "Assistance: ANM/nurse/midwife/LHV" *
    # label variable m3c      "Assistance: other health professional" =
    # label variable m3d      "NA - Assistance: CS health professional"
    # label variable m3e      "NA - Assistance: CS health professional"
    # label variable m3f      "NA - Assistance: CS health professional"
    # label variable m3g      "Assistance: DAI/traditional birth attendant"
    # label variable m3h      "Assistance: friend / relative"
    # label variable m3i      "NA - Assistance: CS other person"
    # label variable m3j      "NA - Assistance: CS other person"
    # label variable m3k      "Assistance: other"
    # label variable m3l      "NA - Assistance: CS other"
    # label variable m3m      "NA - Assistance: CS other"
    # label variable m3n      "Assistance: no one"
    
    
    BRdata <- BRdata %>%
      mutate(rh_del_pv =
               case_when(
                 m3a == 1   ~ 1 ,
                 m3b == 1 ~ 2,
                 m3c == 1 # | 
                   # m3d == 1 | 
                   # m3e == 1 | 
                   # m3f == 1
                 ~ 3,
                 m3g == 1 ~ 4 ,
                 m3h == 1 |
                 # m3i == 1 | 
                 #   m3j == 1 | 
                   m3k == 1 #| 
                   # m3l == 1 | 
                   # m3m == 1 
                   ~ 5 ,
                 m3n ==1 ~ 6,
                 m3a ==8 | m3a==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pv = c(99))) %>%
      set_value_labels(rh_del_pv = c("Doctor" = 1, "ANM/nurse/midwife/LHV"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pv = "Person providing assistance during delivery")
    
    # //Skilled provider during delivery
    # ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
    BRdata <- BRdata %>%
      mutate(rh_del_pvskill =
               case_when(
                 rh_del_pv %in% c(1,2)   ~ 1 ,
                 rh_del_pv %in% c(3,4,5) ~ 2,
                 rh_del_pv ==6 ~ 3 ,
                 rh_del_pv==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
      set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")
    
  } else if (BRdata$v000[1] == "NG7") {
    
    BRdata <- BRdata %>%
      mutate(rh_del_place =
               case_when(
                 m15 >=20 & m15<40   ~ 1 ,
                 m15 >=10 & m15<20   ~ 2,
                 m15 >=40 & m15<99   ~ 3 ,
                 m15 == 99 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_place = c(99))) %>%
      set_value_labels(rh_del_place = c("Health facility" = 1, "Home"=2, "Other"=3, "Missing"=9  )) %>%
      set_variable_labels(rh_del_place = "Live births by place of delivery")
    # 
    
    # // Facility based delivery 
    BRdata$rh_del_fbd <- NA
    BRdata$rh_del_fbd <- ifelse(BRdata$rh_del_place %in% c(1), 1, 0)
    BRdata$rh_del_fbd <- ifelse(is.na(BRdata$rh_del_place) | BRdata$rh_del_place == 9, NA, BRdata$rh_del_fbd)
    
    # //Place of delivery - by place type
    BRdata <- BRdata %>%
      mutate(rh_del_pltype =
               case_when(
                 m15 >=20 & m15<30   ~ 1 ,
                 m15 >=30 & m15<40   ~ 2 ,
                 m15 >=10 & m15<20   ~ 3,
                 m15 >=40 & m15<99   ~ 4 ,
                 m15 == 99 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pltype = c(99))) %>%
      set_value_labels(rh_del_pltype = c("Health facility - public" = 1, "Health facility - private" = 2, "Home"=3, "Other"=4, "Missing"=9  )) %>%
      set_variable_labels(rh_del_pltype = "Live births by type of health facility")
    
    # //Assistance during delivery
    # **Note: Assistance during delivery and skilled provider indicators are both country specific indicators. 
    # **The table for these indicators in the final report would need to be checked to confirm the code below.
    
    # label variable m3a      "Assistance: doctor" *
    # label variable m3b      "Assistance: nurse/midwife" *
    # label variable m3c      "Assistance: auxiliary midwife" *
    # label variable m3d      "Assistance: community health extension worker"
    # label variable m3e      "NA - Assistance: CS health professional"
    # label variable m3f      "NA - Assistance: CS health professional"
    # label variable m3g      "Assistance: traditional birth attendant"
    # label variable m3h      "Assistance: relative/friend"
    # label variable m3i      "NA - Assistance: other"
    # label variable m3j      "NA - Assistance: CS other person"
    # label variable m3k      "Assistance: other"
    # label variable m3l      "NA - Assistance: CS other"
    # label variable m3m      "NA - Assistance: CS other"
    # label variable m3n      "Assistance: no one"
    
    # Nigeria Births delivered with the assistance of doctors, nurses/midwives, and auxiliary nurses/midwives.
    
    BRdata <- BRdata %>%
      mutate(rh_del_pv =
               case_when(
                 m3a == 1   ~ 1 ,
                 m3b == 1 | m3c == 1 ~ 2,
                 m3d == 1 # | 
                   # m3e == 1 | 
                   # m3f == 1 
                 ~ 3 ,
                 m3g == 1 ~ 4,
                 m3h == 1 |
                 # m3i == 1 | 
                 #   m3j == 1 | 
                 m3k == 1 #| 
                 # m3l == 1 | 
                 # m3m == 1 
                 ~ 5 ,
                 m3n ==1 ~ 6,
                 m3a ==8 | m3a==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pv = c(99))) %>%
      set_value_labels(rh_del_pv = c("Doctor" = 1, "Nurse/midwife/auxiliary nurse/midwife"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pv = "Person providing assistance during delivery")
    
    # //Skilled provider during delivery
    # ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
    BRdata <- BRdata %>%
      mutate(rh_del_pvskill =
               case_when(
                 rh_del_pv %in% c(1,2)   ~ 1 ,
                 rh_del_pv %in% c(3,4,5) ~ 2,
                 rh_del_pv ==6 ~ 3 ,
                 rh_del_pv==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
      set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")
    
  } else if (BRdata$v000[1] == "UG7") {
    
    BRdata <- BRdata %>%
      mutate(rh_del_place =
               case_when(
                 m15 >=20 & m15<40   ~ 1 ,
                 m15 >=10 & m15<20   ~ 2,
                 m15 >=40 & m15<99   ~ 3 ,
                 m15 == 99 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_place = c(99))) %>%
      set_value_labels(rh_del_place = c("Health facility" = 1, "Home"=2, "Other"=3, "Missing"=9  )) %>%
      set_variable_labels(rh_del_place = "Live births by place of delivery")
    # 
    
    # // Facility based delivery 
    BRdata$rh_del_fbd <- NA
    BRdata$rh_del_fbd <- ifelse(BRdata$rh_del_place %in% c(1), 1, 0)
    BRdata$rh_del_fbd <- ifelse(is.na(BRdata$rh_del_place) | BRdata$rh_del_place == 9, NA, BRdata$rh_del_fbd)
    
    # //Place of delivery - by place type
    BRdata <- BRdata %>%
      mutate(rh_del_pltype =
               case_when(
                 m15 >=20 & m15<30   ~ 1 ,
                 m15 >=30 & m15<40   ~ 2 ,
                 m15 >=10 & m15<20   ~ 3,
                 m15 >=40 & m15<99   ~ 4 ,
                 m15 == 99 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pltype = c(99))) %>%
      set_value_labels(rh_del_pltype = c("Health facility - public" = 1, "Health facility - private" = 2, "Home"=3, "Other"=4, "Missing"=9  )) %>%
      set_variable_labels(rh_del_pltype = "Live births by type of health facility")
    
    # //Assistance during delivery
    # **Note: Assistance during delivery and skilled provider indicators are both country specific indicators. 
    # **The table for these indicators in the final report would need to be checked to confirm the code below.
    
    # Uganda  Births delivered with the assistance of doctors, nurses/midwives, and/or medical assistants/clinical officers.
    
    # label variable m3a      "Assistance: doctor" *
    # label variable m3b      "Assistance: nurse/midwife" *
    # label variable m3c      "Assistance: medical assistant/clinical officer" *
    # label variable m3d      "Assistance: nursing aide/assistant"
    # label variable m3e      "NA - Assistance: CS health professional"
    # label variable m3f      "NA - Assistance: CS health professional"
    # label variable m3g      "Assistance: traditional birth attendant"
    # label variable m3h      "Assistance: relative/friend"
    # label variable m3i      "NA - Assistance: CS other person"
    # label variable m3j      "NA - Assistance: CS other person"
    # label variable m3k      "Assistance: other"
    # label variable m3l      "NA - Assistance: CS other"
    # label variable m3m      "NA - Assistance: CS other"
    # label variable m3n      "Assistance: no one"
    
    BRdata <- BRdata %>%
      mutate(rh_del_pv =
               case_when(
                 m3a == 1   ~ 1 ,
                 m3b == 1 | m3c == 1 ~ 2,
                 m3d == 1
                   # m3e == 1 | 
                   # m3f == 1 
                 ~ 3 ,
                 m3g == 1 ~ 4 ,
                 # m3i == 1 | 
                 #   m3j == 1 | 
                 m3h == 1 | m3k == 1 #| 
                 # m3l == 1 | 
                 # m3m == 1 
                 ~ 5 ,
                 m3n ==1 ~ 6,
                 m3a ==8 | m3a==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pv = c(99))) %>%
      set_value_labels(rh_del_pv = c("Doctor" = 1, "nurse/midwife/medical assistant/clinical officer"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pv = "Person providing assistance during delivery")
    
    # //Skilled provider during delivery
    # ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
    BRdata <- BRdata %>%
      mutate(rh_del_pvskill =
               case_when(
                 rh_del_pv %in% c(1,2)   ~ 1 ,
                 rh_del_pv %in% c(3,4,5) ~ 2,
                 rh_del_pv ==6 ~ 3 ,
                 rh_del_pv==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
      set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")
    
    
    
  } else {
    
    warning(paste0("Check ", BRdata$v000[1], " for place of delivery"))
    
    BRdata <- BRdata %>%
      mutate(rh_del_place =
               case_when(
                 m15 >=20 & m15<40   ~ 1 ,
                 m15 >=10 & m15<20   ~ 2,
                 m15 >=40 & m15<99   ~ 3 ,
                 m15 == 99 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_place = c(99))) %>%
      set_value_labels(rh_del_place = c("Health facility" = 1, "Home"=2, "Other"=3, "Missing"=9  )) %>%
      set_variable_labels(rh_del_place = "Live births by place of delivery")
    # 
    
    # // Facility based delivery 
    BRdata$rh_del_fbd <- NA
    BRdata$rh_del_fbd <- ifelse(BRdata$rh_del_place %in% c(1), 1, 0)
    BRdata$rh_del_fbd <- ifelse(is.na(BRdata$rh_del_place) | BRdata$rh_del_place == 9, NA, BRdata$rh_del_fbd)
    
    # //Place of delivery - by place type
    BRdata <- BRdata %>%
      mutate(rh_del_pltype =
               case_when(
                 m15 >=20 & m15<30   ~ 1 ,
                 m15 >=30 & m15<40   ~ 2 ,
                 m15 >=10 & m15<20   ~ 3,
                 m15 >=40 & m15<99   ~ 4 ,
                 m15 == 99 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pltype = c(99))) %>%
      set_value_labels(rh_del_pltype = c("Health facility - public" = 1, "Health facility - private" = 2, "Home"=3, "Other"=4, "Missing"=9  )) %>%
      set_variable_labels(rh_del_pltype = "Live births by type of health facility")
    
    # //Assistance during delivery
    # **Note: Assistance during delivery and skilled provider indicators are both country specific indicators. 
    # **The table for these indicators in the final report would need to be checked to confirm the code below.
    BRdata <- BRdata %>%
      mutate(rh_del_pv =
               case_when(
                 m3a == 1   ~ 1 ,
                 m3b == 1 ~ 2,
                 m3c == 1 | m3d == 1 | m3e == 1 | m3f == 1~ 3 ,
                 m3g == 1 ~ 4 ,
                 m3h == 1 | m3i == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ 5 ,
                 m3n ==1 ~ 6,
                 m3a ==8 | m3a==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pv = c(99))) %>%
      set_value_labels(rh_del_pv = c("Doctor" = 1, "Nurse/midwife"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pv = "Person providing assistance during delivery")
    
    # //Skilled provider during delivery
    # ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
    BRdata <- BRdata %>%
      mutate(rh_del_pvskill =
               case_when(
                 rh_del_pv %in% c(1,2)   ~ 1 ,
                 rh_del_pv %in% c(3,4,5) ~ 2,
                 rh_del_pv ==6 ~ 3 ,
                 rh_del_pv==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      naniar::replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
      set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")
  }
  
  # Skilled delivery binary
  BRdata$rh_del_skilledDelivery <- NA
  BRdata$rh_del_skilledDelivery <- ifelse(BRdata$rh_del_pvskill == 1, 1, 0)
  
  # //Caesarean delivery
  BRdata <- BRdata %>%
    mutate(rh_del_ces =
             case_when(
               m17==1   ~ 1 ,
               age>=period ~ 99,
               TRUE ~ 0 )) %>%
    naniar::replace_with_na(replace = list(rh_del_ces = c(99))) %>%
    set_value_labels(rh_del_ces = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(rh_del_ces = "Live births delivered by Caesarean")
  
  # //Timing of decision for caesarean
  #  some surveys did not ask this question, confirm m17a exists
  
  if ("m17a" %in% names(BRdata)) {
    BRdata <- BRdata %>%
      mutate(rh_del_cestime =
               case_when(
                 m17a==1 ~ 1 ,
                 m17a==2 ~ 2 , 
                 age>=period ~ 99,
                 TRUE ~ 0 )) %>%
      naniar::replace_with_na(replace = list(rh_del_cestime = c(99))) %>%
      set_value_labels(rh_del_cestime = c("Vaginal birth"=0, "before labor started"=1, "after labor started"=2  )) %>%
      set_variable_labels(rh_del_cestime = "Timing of decision to have Caesarean")
  }
  
  
  
  # //Duration of stay following recent birth
  BRdata <- BRdata %>%
    mutate(rh_del_stay =
             case_when(
               m61<106   ~ 1 ,
               m61>=106 & m61<112 ~ 2,
               m61>=112 & m61<124 ~ 3 ,
               (m61>=124 & m61<172) | m61==201 | m61==202 ~ 4 ,
               (m61>=172 & m61<200) | (m61>=203 & m61<400) ~ 5 ,
               m61==998 | m61==999 ~ 9 ,
               rh_del_place!=1 | bidx!=1 | age>=period ~ 99)) %>%
    naniar::replace_with_na(replace = list(rh_del_stay = c(99))) %>%
    set_value_labels(rh_del_stay = c("<6 hours" = 1, "6-11 hours"=2, "12-23 hours"=3, "1-2 days"=4, "3+ days"=5, "Don't know/missing"=9  )) %>%
    set_variable_labels(rh_del_stay = "Duration of stay following recent birth")
  
  
  # ******************************************************************************
  # Program: 			  CM_CHILD.R
  # Purpose: 		    Produce child mortality indicators   
  # Data inputs: 		BR dataset
  # Data outputs:		coded variables, and output on screen and in excel tables
  # Author:				  Mahmoud Elkasabi
  # Date last modified:  September 15 2021 by Mahmoud Elkasabi
  # ******************************************************************************
  #   
  # -----------------------------------------------------------------------------#
  # # Indicators created in this file:
  # NNMR		"Neonatal Mortality Rate"
  # PNNMR		"Post-neonatal Mortality Rate"
  # IMR			"Infant Mortality Rate"
  # CMR			"Child Mortality Rate"
  # U5MR		"Under-5 Mortality Rate"
  # u5_death "Under-5 death"
  # neonatal "
  # -----------------------------------------------------------------------------#
  #
  
  BRdata <- BRdata %>%
    mutate(child_sex = b4) %>%
    mutate(child_sex = sjlabelled::set_label(child_sex, label = "Sex of child"))  %>%
    mutate(months_age = b3-v011) %>%
    mutate(mo_age_at_birth =
             case_when(
               months_age < 20*12   ~ 1 ,
               months_age >= 20*12 & months_age < 30*12 ~ 2,
               months_age >= 30*12 & months_age < 40*12 ~ 3,
               months_age >= 40*12 & months_age < 50*12 ~ 4)) %>%
    mutate(mo_age_at_birth = factor(mo_age_at_birth, levels = c(1,2,3,4), labels = c("Mother's age at birth < 20", "Mother's age at birth 20-29", "Mother's age at birth 30-39","Mother's age at birth 40-49"))) %>%
    mutate(mo_age_at_birth = sjlabelled::set_label(mo_age_at_birth, label = "Mother's age at birth")) %>%
    mutate(birth_order =
             case_when(
               bord == 1  ~ 1,
               bord >= 2 & bord <= 3 ~ 2,
               bord >= 4 & bord <= 6 ~ 3,
               bord >= 7  ~ 4,
               bord == NA ~ 99)) %>%
    naniar::replace_with_na(replace = list(birth_order = c(99))) %>%
    mutate(birth_order = factor(birth_order, levels = c(1,2,3,4), labels = c("Birth order:1", "Birth order:2-3", "Birth order:4-6","Birth order:7+"))) %>%
    mutate(birth_order = sjlabelled::set_label(birth_order, label = "Birth order"))  %>%
    mutate(prev_bint =
             case_when(
               b11 <= 23 ~ 1,
               b11 >= 24 & b11 <= 35 ~ 2,
               b11 >= 36 & b11 <= 47 ~ 3,
               b11 >= 48 ~ 4)) %>%
    mutate(prev_bint = sjlabelled::set_label(prev_bint, label = "Preceding birth interval"))  %>%
    mutate(birth_size =
             case_when(
               m18 >= 4 & m18 <= 5 ~ 1,
               m18 <= 3 ~ 2,
               m18 > 5 ~ 99)) %>%
    mutate(birth_size = sjlabelled::set_label(birth_size, label = "Birth size")) 
  
  BRdata$mo_age_at_birth.months <- BRdata$months_age / 12 
  
  BRdata[["prev_bint"]] <- ifelse(is.na(BRdata[["prev_bint"]]), 999, BRdata[["prev_bint"]])
  BRdata[["birth_size"]] <- ifelse(is.na(BRdata[["birth_size"]]), 999, BRdata[["birth_size"]])
  
  BRdata <- BRdata %>%
    mutate(prev_bint = factor(prev_bint, levels = c(1,2,3,4,999), labels = c("Previous birth interval <2 years", "Previous birth interval 2 years", "Previous birth interval 3 years","Previous birth interval 4+ years", "missing"))) %>%
    mutate(birth_size = factor(birth_size, levels = c(1,2,99,999), labels = c("Birth size: Small/very small","Birth size: Average or larger", "Birth size: Don't know/missing", "missing" )))
  
  
  # age at death report units
  BRdata$aod_units <- NA
  BRdata$aod_units <- as.integer(stringr::str_trunc(BRdata$b6, 1, "right", ellipsis = ""))
  
  # age at death report number
  BRdata$aod_num <- NA
  BRdata$aod_num <- as.integer(stringr::str_trunc(BRdata$b6, 2, "left", ellipsis = ""))
  
  # death of child under five
  BRdata$U5_death <- NA
  BRdata$U5_death <- BRdata$b7 < 60
  
  # infant death
  BRdata$infant_death <- NA
  BRdata$infant_death <- BRdata$b7 < 12
  
  # neonatal death
  BRdata$neonatal_death <- NA
  BRdata$neonatal_death <- ifelse(BRdata$aod_units == 1 & BRdata$aod_num <= 28, TRUE, FALSE)
  
  # date of death
  BRdata$do_death <- as.Date(NA)
  BRdata$do_death <- dplyr::if_else(
    BRdata$aod_units == 1, 
    BRdata$c_dobR + lubridate::days(BRdata$aod_num), 
    BRdata$do_death
    )
  BRdata$do_death <- dplyr::if_else(
    BRdata$aod_units != 1, 
    as.integer(round(BRdata$b7 * 30.437 - 15.2185)) + BRdata$c_dobR, 
    BRdata$do_death
    )
  
  # twin 
  BRdata$multiBirthOrder <- NA
  BRdata$multiBirthOrder <- BRdata$b0
  
  
  
  # birth intervals
  #   Preceding birth interval in months
  BRdata$birthInt_succ_m <- NA
  BRdata$birthInt_succ_m <- BRdata$b11
  #   Succeeding birth interval in months
  BRdata$birthInt_succ_m <- NA
  BRdata$birthInt_succ_m <- BRdata$b12
  
  
  # ##################################################################################
  # # MORTALITY RATES ################################################################
  # ##################################################################################
  # 
  # BRdata_CMORT <- (BRdata[, c("v021", "v022","v024", "v025", "v005", "v008","v011", 
  #                             "b3", "b7", "v106", "v190", "child_sex", "mo_age_at_birth", "birth_order", "prev_bint","birth_size")])
  # 
  # # NNMR, PNNMR, IMR, CMR & U5MR
  # # TABLES 8.1, 8.2 and 8.3
  # 
  # # Different datasets for period ends: 5-9 and 10-14
  # BRdata_CMORT1 <- BRdata_CMORT
  # BRdata_CMORT2 <- BRdata_CMORT
  # BRdata_CMORT1$v008 <- BRdata_CMORT$v008 - 12 * (5)
  # BRdata_CMORT2$v008 <- BRdata_CMORT$v008 - 12 * (10)
  # 
  # resn1 <- as.data.frame(chmort(BRdata_CMORT))
  # resn1$period <- "0-4"
  # 
  # resn2 <- as.data.frame(chmort(BRdata_CMORT1))
  # resn2$period <- "5-9"
  # resn3 <- as.data.frame(chmort(BRdata_CMORT2))
  # resn3$period <- "10-14"
  # 
  # resc <- as.data.frame(rbind(chmort(BRdata_CMORT, Class="v024",Period = 120),
  #                             chmort(BRdata_CMORT, Class="v025",Period = 120),
  #                             chmort(BRdata_CMORT, Class="v106",Period = 120),
  #                             chmort(BRdata_CMORT, Class="v190",Period = 120),
  #                             chmort(BRdata_CMORT, Class="child_sex",Period = 120),
  #                             chmort(BRdata_CMORT, Class="mo_age_at_birth",Period = 120),
  #                             chmort(BRdata_CMORT, Class="birth_order",Period = 120),
  #                             chmort(BRdata_CMORT, Class="prev_bint",Period = 120),
  #                             chmort(BRdata_CMORT, Class="birth_size",Period = 120)))
  # 
  # resc$period <- "0-9"
  # 
  # CHMORT <- vector("list", 4)
  # CHMORT <- rbindlist(list(resn1,resn2,resn3,resc), fill = TRUE)
  # 
  # CHMORT[["Class"]] <- ifelse(is.na(CHMORT[["Class"]]), "National", CHMORT[["Class"]])
  # CHMORT <- CHMORT[CHMORT[["Class"]]!="missing",] 
  # 
  # rownames(CHMORT) <- paste(seq(1:230) , rep(row.names(resn1),46))
  # 
  # write.xlsx(CHMORT, "Tables_child_mort.xlsx", sheetName = "CMR",append=TRUE)
  # 
  # ########################################################################################
  
  # # female id that had a larger family, youngest birth is living and 
  # f_id <- "443_130_2"
  # birth <- 3
  
  # # female ID where first child was born in health facility
  # f_id <- "5_157_2"
  # # not born in health facility and died
  # f_id <- "2_236_1" 
  
  rownames(BRdata) <- BRdata$childID
  
  gc()
  
  
  return(BRdata)
  
}








