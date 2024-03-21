cleanIR <- function(dataset){
  
  require(future)
  require(future.apply)
  require(uuid)
  
  # Set up a plan to use available cores (multicore) or sessions (multisession)
  # Adjust according to your specific needs
  plan(multisession, workers = 10)
  
  # Parallelize the sapply function
  dataset$uuid <- future_sapply(1:nrow(dataset), function(x) {
    y <- uuid::UUIDgenerate()
    return(y)
  }, future.seed = TRUE) # Ensure reproducibility if needed
  
  
  # # date of interview
  dataset$interview_date <- NA
  dataset$interview_date <- as.Date(paste(dataset$v007, dataset$v006, dataset$v016, sep = "-"))
  
  # # renaming variables for use across datasets
  # dataset$uuid <- sapply(1:nrow(dataset), function(x) {
  #   y <- uuid::UUIDgenerate()
  #   return(y)
  # })
  
  # Generate female ID from data
  dataset$femaleID <- NA
  dataset$femaleID <- paste0(dataset$v001, "_", dataset$v002, "_", dataset$v003)
  
  ## consistent interview day
  dataset$intDay <- dataset$v008a
  ## consistent interview start time
  dataset$startTime <- dataset$v801
  ## consitent interview end time
  dataset$endTime <- dataset$v802
  ## consistent interview length
  dataset$intLength <- dataset$v803
  
  
  ## Create a consistent fieldworker ID variable between datasets
  dataset$fw_ID <- dataset$v028
  ## create a consistent cluster
  dataset$cluster_ID <- dataset$v021
  ## Age
  dataset$age_r <- dataset$v012
  ##    Age in 5 years
  dataset$age_5yrs <- NA
  dataset$age_5yrs <- ifelse(dataset$age_r %in% c(15:19), 1, dataset$age_5yrs)
  dataset$age_5yrs <- ifelse(dataset$age_r %in% c(20:24), 2, dataset$age_5yrs)
  dataset$age_5yrs <- ifelse(dataset$age_r %in% c(25:29), 3, dataset$age_5yrs)
  dataset$age_5yrs <- ifelse(dataset$age_r %in% c(30:34), 4, dataset$age_5yrs)
  dataset$age_5yrs <- ifelse(dataset$age_r %in% c(35:39), 5, dataset$age_5yrs)
  dataset$age_5yrs <- ifelse(dataset$age_r %in% c(40:44), 6, dataset$age_5yrs)
  dataset$age_5yrs <- ifelse(dataset$age_r %in% c(45:49), 7, dataset$age_5yrs)
  dataset$age_5yrs <- ifelse(dataset$age_r %in% c(50:54), 8, dataset$age_5yrs)
  dataset$age_5yrs <- ifelse(dataset$age_r %in% c(55:59), 9, dataset$age_5yrs)
  
  ## Fieldworker age
  try({dataset$age_fw <- dataset$fw104})
  ## Interviewer age
  dataset$interview_lang <- dataset$v045b
  ## Native language of respondent
  dataset$nlang_r <- dataset$v045c
  ## translator used
  dataset$translator <- dataset$v046
  ## type of place of residence
  dataset$rural <- NA
  dataset$rural <- ifelse(dataset$v102 == 2, 1, 0)
  ## education level
  dataset$edLevel_r <- dataset$v106
  ## fieldworker dhs experience
  try({
    dataset$dhs_experience <- NA
    dataset$dhs_experience <- ifelse(dataset$fw115 == 2, 0, dataset$fw115)
  })
  ## fieldworker survey experience
  try({
    dataset$survey_experience <- NA
    dataset$survey_experience <- ifelse(dataset$fw116 == 2, 0, dataset$fw116)
  })
  ## fieldworker education level
  try(dataset$edLevel_fw <- as.numeric(dataset$fw109))
  
  # fieldworker respondent differences
  ## fieldworker / respondent education differences
  try(dataset$edLevel_diff <- dataset$edLevel_fw - dataset$edLevel_r)
  ## any differences between fieldworker and respondent education level
  try(dataset$edLevel_anyDiff <- ifelse(dataset$edLevel_diff != 0, 1, 0))
  
  # Ever sex
  dataset$everSex <- NA
  dataset$everSex <- ifelse(dataset$v525 == 0, 0, 1)
  
  if(sum(is.na(dataset$everSex)) != sum(is.na(dataset$v525))) {
    warning(paste0(dataset$mv000[1], ": everSex missing does not match v501 missing"))
  }
  
  # Ever sex (imputed)
  dataset$everSex_imp <- NA
  dataset$everSex_imp <- ifelse(dataset$v531 == 0, 0, 1)
  
  if(sum(is.na(dataset$everSex_imp)) != sum(is.na(dataset$v531))) {
    warning(paste0(dataset$v000[1], ": everSex missing does not match v531 missing"))
  }
  
  if(sum(dataset$everSex_imp, na.rm = TRUE) != sum(dataset$everSex, na.rm = TRUE)) {
    warning(paste0(dataset$v000[1], ": The everSex derived from imputed and actual age at first sex do not match. (Women's questionnaire)"))
  }
  
  ### (everUnion)
  # Ever married or in union
  dataset$everUnion <- NA
  dataset$everUnion <- ifelse(dataset$v501 == 0, 0, dataset$everUnion)
  dataset$everUnion <- ifelse(dataset$v501 != 0, 1, dataset$everUnion)
  
  if(sum(is.na(dataset$everUnion)) != sum(is.na(dataset$v501))) {
    warning(paste0(dataset$mv000[1], ": everUnion missing does not match v501 missing"))
  }
  
  ### (everUnion_fw)
  dataset$everUnion_fw <- NA
  try({
    dataset$everUnion_fw <- ifelse(dataset$fw106 == "never married or lived with a man/woman", 0, 1)
  })
  
  if(sum(is.na(dataset$everUnion_fw)) != sum(is.na(dataset$fw106))) {
    warning(paste0(dataset$mv000[1], ": everUnion_fw missing does not match v501 missing"))
  }
  
  ### (ageDiff)
  # Age difference between interviewer and respondent\
  dataset$ageDiff <- NA
  try({
    dataset$ageDiff <- dataset$fw104 - dataset$v012
  })
  
  ### (ageDiff_gt_10)
  # Categorize age differences by greater than or less than 10 years
  dataset$ageDiff_gt_10 <- NA
  dataset$ageDiff_gt_10 <- ifelse(dataset$ageDiff >= 10, 1, 0)
  
  ### (ageDiff_gt_10)
  # Categorize age differences by greater than or less than 10 years
  dataset$ageDiff_in_10 <- NA
  dataset$ageDiff_in_10 <- ifelse(dataset$ageDiff >= 10, 0, 1)
  
  ### (ageDiff_gt_5)
  # Categorize age differences by greater than or less than 5 years
  dataset$ageDiff_gt_5 <- NA
  dataset$ageDiff_gt_5 <- ifelse(dataset$ageDiff >= 5, 1, 0)
  
  ### (ageDiff_in_5)
  # Categorize age differences by greater than or less than 5 years
  dataset$ageDiff_in_5 <- NA
  dataset$ageDiff_in_5 <- ifelse(dataset$ageDiff >= 5, 0, 1)
  
  ### (ageDiff_5_cat)
  # Categorize age difference by 5 year intervals
  dataset$ageDiff_5 <- dataset$ageDiff / 5
  dataset$ageDiff_5_cat <- ifelse(dataset$ageDiff_5 >= 0, floor(dataset$ageDiff_5), ceiling(dataset$ageDiff_5) + 1)
  
  ### (ageDiff_levels)
  dataset$ageDiff_5_2lvls <- NA
  dataset$ageDiff_5_2lvls <- ifelse(dataset$ageDiff >= 5, 1, 0)
  dataset$ageDiff_5_2lvls <- ifelse(dataset$ageDiff >= 10, 2, dataset$ageDiff_5_2lvls)
  
  ### ageDiff_levels dummy
  #### level 1 (5-9.9 years older)
  dataset$ageDiff_5_2lvls.1 <- NA
  dataset$ageDiff_5_2lvls.1 <- ifelse(dataset$ageDiff_5_2lvls == 1, 1, 0)
  #### level 2 (10 + years older)
  dataset$ageDiff_5_2lvls.2 <- NA
  dataset$ageDiff_5_2lvls.2 <- ifelse(dataset$ageDiff_5_2lvls == 2, 1, 0)
  
  # Rename Wealth Index 
  dataset$wealthIndex <- dataset$v190
  # Rename wealth index Score
  dataset$wealthIndexS <- dataset$v191
  
  # ever had a child death
  childAlive.vec <- c("b5_01", # Child alive
                      "b5_02",
                      "b5_03",
                      "b5_04",
                      "b5_05",
                      "b5_06",
                      "b5_07",
                      "b5_08",
                      "b5_09",
                      "b5_10",
                      "b5_11",
                      "b5_12",
                      "b5_13",
                      "b5_14",
                      "b5_15",
                      "b5_16",
                      "b5_17",
                      "b5_18",
                      "b5_19",
                      "b5_20"
  )
  
  dataset$anyCMortality <- apply(
    dataset[,childAlive.vec],
    1,function(i) {
      ifelse(sum(i, na.rm=TRUE) < sum(!is.na(i)), 1, ifelse(all(is.na(i)), NA, 0))
    })
  
  
  # Sibling death
  sibAlive.vec <- c("mm2_01", # Child alive
                    "mm2_02",
                    "mm2_03",
                    "mm2_04",
                    "mm2_05",
                    "mm2_06",
                    "mm2_07",
                    "mm2_08",
                    "mm2_09",
                    "mm2_10",
                    "mm2_11",
                    "mm2_12",
                    "mm2_13",
                    "mm2_14",
                    "mm2_15",
                    "mm2_16",
                    "mm2_17",
                    "mm2_18",
                    "mm2_19",
                    "mm2_20"
  )
  
  
  if ("mm1_01" %in% names(dataset)) {
    sibSex.vec <- c("mm1_01", # sibling alive
                    "mm1_02",
                    "mm1_03",
                    "mm1_04",
                    "mm1_05",
                    "mm1_06",
                    "mm1_07",
                    "mm1_08",
                    "mm1_09",
                    "mm1_10",
                    "mm1_11",
                    "mm1_12",
                    "mm1_13",
                    "mm1_14",
                    "mm1_15",
                    "mm1_16",
                    "mm1_17",
                    "mm1_18",
                    "mm1_19",
                    "mm1_20"
    )
    
    # any siblings died
    dataset$anySMortality <- NA
    try(
      dataset$anySMortality <- apply(
        dataset[,c(sibAlive.vec)],
        1,function(i) {
          ifelse(0 %in% i, 1, 0)
        })
    )
    #   add NA's back in
    dataset$anySMortality <- ifelse(is.na(dataset$mm1_01), NA, dataset$anySMortality)
    
    
    # sister death
    
    sibIndex <- c()
    for (i in 1:20) {if (i < 10) {sibIndex[i] <- paste0("0", i)} else {sibIndex[i] <- as.character(i)}}
    
    # count of sister deaths
    dataset$sisterDeath.count <- NA
    dataset$sisterDeath.count <- sapply(1:nrow(dataset), function(dfRow) {
      
      # check for a death in all sisters
      sisterDeaths <- sapply(sibIndex, function(x) {
        
        sex.sib <- paste0("mm1_", x)
        alive.sib <- paste0("mm2_", x)
        
        # 
        sisterDeath <- ifelse(dataset[dfRow,sex.sib] %in% c(2) & dataset[dfRow,alive.sib] %in% c(0), 1, 0)
        
        return(sisterDeath)
      })
      
      try(sisterDeath.count <- sum(sisterDeaths))
      if (!exists("sisterDeath.count")) {sisterDeath.count <- NA}
      
      return(sisterDeath.count)
      
    })
    dataset$sisterDeath.count <- ifelse(is.na(dataset$mm1_01), NA, dataset$sisterDeath.count)
    
    # any sister death
    dataset$sisterDeath <- NA
    dataset$sisterDeath <- ifelse(dataset$sisterDeath.count > 0, 1, 0)
    dataset$sisterDeath <- ifelse(is.na(dataset$sisterDeath.count), NA, dataset$sisterDeath)
  }
  # data warnings
  #   Warning for unmatched gender
  if(sum(dataset$fw105== "male", na.rm = TRUE) >= 1) {
    warning(paste0("\n\n", dataset$v000[1], ": WARNING! ", 
                   sum(dataset$fw105== "male", na.rm = TRUE), 
                   " of ", 
                   nrow(dataset),
                   " (",
                   round((sum(dataset$fw105== "male", na.rm = TRUE) / nrow(dataset)), 3)*100,
                   "%) women's questionnaires had male interviewers!"
    )
    )
  }
  
  
  # WArning about missing interviewer data  
  if(sum(is.na(dataset$fw104))>=1) {
    warning(paste0("\n", 
                   dataset$v000[1], 
                   ": Missing interviewer age (fw104) info from ", 
                   sum(is.na(dataset$fw104)), 
                   " of ", 
                   nrow(dataset), 
                   " (", round(sum(is.na(dataset$fw104))/nrow(dataset), 3)*100, 
                   "%) interviews (women's questionnaire)!")
    )
  }
  
  
  # Start DHS recode ####
  
  IRdata <- dataset
  rm(dataset)
  
  
  # period and age of child
  # choose reference period, last 2 years (24 months) or last 5 years (60 months)
  # Using a period of the last 2 years will not match final report but would provide more recent information.
  # period = 24
  IRdata <- IRdata %>%
    mutate(period = 60)
  
  # age of child. If b19_01 is not available in the data use v008 - b3_01
  if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
    IRdata [[paste("b19_01")]] <- NA
  if ("TRUE" %in% all(is.na(IRdata $b19_01)))
  { b19_included <- 0} else { b19_included <- 1}
  
  if (b19_included==1) {
    IRdata <- IRdata %>%
      mutate(age = b19_01)
  } else {
    IRdata <- IRdata %>%
      mutate(age = v008 - b3_01)
  }
  
  ### *** ANC visit indicators *** ###
  
  # //ANC by type of provider
  # ** Note: Please check the final report for this indicator to determine the categories and adjust the code and label accordingly. 
  IRdata <- IRdata %>%
    mutate(rh_anc_pv =
             case_when(
               m2a_1 == 1   ~ 1 ,
               m2b_1 == 1 ~ 2,
               m2c_1 == 1 | m2d_1 == 1 | m2e_1 == 1 ~ 3 ,
               m2f_1 == 1 | m2g_1 == 1 | m2h_1 == 1 | m2i_1 == 1 | m2j_1 == 1 | m2k_1 == 1 | m2l_1 == 1 | m2m_1 == 1 ~ 4 ,
               m2a_1 <2 ~ 5,
               m2a_1 == 9 ~ 9 ,
               age>=period ~ 99)) %>%
    naniar::replace_with_na(replace = list(rh_anc_pv = c(99))) %>%
    labelled::set_value_labels(rh_anc_pv = c("Doctor" = 1, "Nurse/midwife"=2, "Other health worker"=3, "TBA/other/relative"=4, "No ANC"=5, "don't know/missing"=9  )) %>%
    labelled::set_variable_labels(rh_anc_pv = "Person providing assistance during ANC")
  
  # //ANC by skilled provider
  # ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
  IRdata <- IRdata %>%
    mutate(rh_anc_pvskill =
             case_when(
               rh_anc_pv>=0 & rh_anc_pv<=2   ~ 1 ,  
               rh_anc_pv>2 & rh_anc_pv<=6 ~ 0)) %>%
    labelled::set_value_labels(rh_anc_pvskill = c("Yes" = 1, "No"=0)) %>%
    labelled::set_variable_labels(rh_anc_pvskill = "Skilled assistance during ANC")
  
  # //Number of ANC visits in 4 categories that match the table in the final report
  IRdata <- IRdata %>%
    mutate(rh_anc_numvs =
             case_when(
               m14_1 == 0 ~ 0 ,
               m14_1 == 1 ~ 1 ,
               m14_1  %in% c(2,3)   ~ 2 ,
               m14_1>=4 & m14_1<=90  ~ 3 ,
               m14_1>90  ~ 9 ,
               age>=period ~ 99 )) %>%
    naniar::replace_with_na(replace = list(rh_anc_numvs = c(99))) %>%
    labelled::set_value_labels(rh_anc_numvs = c(none = 0, "1" = 1, "2-3"=2, "4+"=3, "don't know/missing"=9  )) %>%
    labelled::set_variable_labels(rh_anc_numvs = "Number of ANC visits")
  
  # //4+ ANC visits  
  IRdata <- IRdata %>%
    mutate(rh_anc_4vs =
             case_when(
               rh_anc_numvs==3 ~ 1,
               rh_anc_numvs %in% c(0,1,2,9)   ~ 0 )) %>%
    labelled::set_value_labels(rh_anc_4vs = c("Yes" = 1, "No"=0)) %>%
    labelled::set_variable_labels(rh_anc_4vs = "Attended 4+ ANC visits")
  
  # //Number of months pregnant at time of first ANC visit
  IRdata <- IRdata %>%
    mutate(rh_anc_moprg =
             case_when(
               m14_1 == 0 ~ 0 ,
               m13_1  %in% c(0,1,2,3)   ~ 1 ,
               m13_1  %in% c(4,5)  ~ 2 ,
               m13_1  %in% c(6,7)~ 3,
               m13_1>=8 & m13_1<=90 ~ 4, 
               m13_1>90 & m13_1<100 ~ 9 ,
               age>=period ~ 99 )) %>%
    naniar::replace_with_na(replace = list(rh_anc_moprg = c(99))) %>%
    labelled::set_value_labels(rh_anc_moprg = c("No ANC" = 0, "<4" = 1, "4-5"=2, "6-7"=3, "8+"=4, "don't know/missing"=9  )) %>%
    labelled::set_variable_labels(rh_anc_moprg = "Number of months pregnant at time of first ANC visit")
  
  # //ANC before 4 months
  IRdata <- IRdata %>%
    mutate(rh_anc_4mo =
             case_when(
               rh_anc_moprg %in% c(0,2,3,4,9)   ~ 0 ,
               rh_anc_moprg==1 ~ 1)) %>%
    labelled::set_value_labels(rh_anc_4mo = c("Yes" = 1, "No"=0)) %>%
    labelled::set_variable_labels(rh_anc_4mo = "Attended ANC <4 months of pregnancy")
  
  # //Median number of months pregnant at time of 1st ANC
  # * Any ANC visits (for denominator)
  IRdata <- IRdata %>%
    mutate(ancany =
             case_when(
               m14_1 %in% c(0,99)   ~ 0 ,
               m14_1>=1 & m14_1<=60 | m14_1==98 ~ 1))
  # IRdata <- IRdata %>%
  #   mutate(anctiming=
  #            case_when(
  #              m13_1 != 98 ~ m13_1,
  #              m13_1 == 98 ~ 99)) %>%    
  #   naniar::replace_with_na(replace = list(anctiming = c(99))) 
  
  
  # #to obtain the 50% percentile
  # sp50 <- matrixStats::weightedMedian(IRdata$anctiming, IRdata$wt, idxs = NULL, na.rm = TRUE)
  # 
  # IRdata <- IRdata %>%
  #   mutate(dummy =
  #            case_when(
  #              anctiming<sp50 & ancany==1 ~ 1,
  #              ancany==1 ~ 0  ))
  # sL <- matrixStats::weightedMean(IRdata$dummy, IRdata$wt, idxs = NULL, na.rm = TRUE)
  # 
  # IRdata <- IRdata %>%
  #   mutate(dummy =
  #            case_when(
  #              anctiming<=sp50 & ancany==1 ~ 1,
  #              ancany==1 ~ 0  ))
  # sU <- matrixStats::weightedMean(IRdata$dummy, IRdata$wt, idxs = NULL, na.rm = TRUE)
  # 
  # IRdata <- IRdata %>%
  #   mutate(rh_anc_median=round(sp50+(0.5-sL)/(sU-sL),1)) %>%
  #   labelled::set_variable_labels(rh_anc_median = "Total- Median months pregnant at first visit")
  # 
  # #remove temporary values
  # rm(sL, sp50, sU)
  
  ### *** ANC components *** ###
  
  # //Took iron tablets or syrup
  IRdata <- IRdata %>%
    mutate(rh_anc_iron =
             case_when(
               m45_1 == 1 ~ 1 ,
               v208 ==0 | age>=period ~ 99,
               TRUE ~ 0)) %>%
    naniar::replace_with_na(replace = list(rh_anc_iron = c(99))) %>%
    labelled::set_value_labels(rh_anc_iron = c("No" = 0, "Yes" = 1 )) %>%
    labelled::set_variable_labels(rh_anc_iron = "Took iron tablet/syrup during pregnancy of last birth")
  
  # //Took intestinal parasite drugs 
  IRdata <- IRdata %>%
    mutate(rh_anc_parast =
             case_when(
               m60_1 == 1 ~ 1 ,
               v208 ==0 | age>=period ~ 99,
               TRUE ~ 0)) %>%
    naniar::replace_with_na(replace = list(rh_anc_parast = c(99))) %>%
    labelled::set_value_labels(rh_anc_parast = c("No" = 0, "Yes" = 1 )) %>%
    labelled::set_variable_labels(rh_anc_parast = "Took intestinal parasite drugs during pregnancy of last birth")
  
  # 	* for surveys that do not have this variable
  # 	cap gen rh_anc_parast=.
  # 	
  # * Among women who had ANC for their most recent birth	
  # 
  # //Informed of pregnancy complications
  # this variable is not always available in the data. Please check.
  IRdata <- IRdata %>%
    mutate(rh_anc_prgcomp =
             case_when(
               m43_1 == 1 & ancany==1 ~ 1  ,
               ancany==1 ~ 0 )) %>%
    labelled::set_value_labels(rh_anc_prgcomp = c("No" = 0, "Yes" = 1 )) %>%
    labelled::set_variable_labels(rh_anc_prgcomp = "Informed of pregnancy complications during ANC visit")
  
  # //Blood pressure measured
  IRdata <- IRdata %>%
    mutate(rh_anc_bldpres =
             case_when(
               m42c_1 == 1 & ancany==1 ~ 1  ,
               ancany==1 ~ 0 )) %>%
    labelled::set_value_labels(rh_anc_bldpres = c("No" = 0, "Yes" = 1 )) %>%
    labelled::set_variable_labels(rh_anc_bldpres = "Blood pressure was taken during ANC visit")
  
  # //Urine sample taken
  IRdata <- IRdata %>%
    mutate(rh_anc_urine =
             case_when(
               m42d_1 == 1 & ancany==1 ~ 1  ,
               ancany==1 ~ 0 )) %>%
    labelled::set_value_labels(rh_anc_urine = c("No" = 0, "Yes" = 1 )) %>%
    labelled::set_variable_labels(rh_anc_urine = "Urine sample was taken during ANC visit")
  
  # //Blood sample taken
  IRdata <- IRdata %>%
    mutate(rh_anc_bldsamp =
             case_when(
               m42e_1 == 1 & ancany==1 ~ 1  ,
               ancany==1 ~ 0 )) %>%
    labelled::set_value_labels(rh_anc_bldsamp = c("No" = 0, "Yes" = 1 )) %>%
    labelled::set_variable_labels(rh_anc_bldsamp = "Blood sample was taken during ANC visit")
  
  # //tetnaus toxoid injections
  IRdata <- IRdata %>%
    mutate(rh_anc_toxinj =
             case_when(
               m1_1 >1 & m1_1 <8 ~ 1 ,
               v208 ==0 | age>=period ~ 99,
               TRUE ~ 0)) %>%
    naniar::replace_with_na(replace = list(rh_anc_toxinj = c(99))) %>%
    labelled::set_value_labels(rh_anc_toxinj = c("No" = 0, "Yes" = 1 )) %>%
    labelled::set_variable_labels(rh_anc_toxinj = "Received 2+ tetanus injections during last pregnancy")
  
  
  # FAMILY PLANNING 
  # ******************************************************************************
  # Program: 			  FP_USE.do
  # Purpose: 		    Code contraceptive use indicators (ever and current use). Also source of method, brands, and information given. 
  # Data inputs: 		IR dataset
  # Data outputs:		coded variables
  # Author:				  Courtney Allen
  # Date last modified: March 29  2021 by Courtney Allen
  # ******************************************************************************
  
  # NOTE: this script is created to run from the FPmain.R file where the following libraries are loaded
  # -----------------------------------------------------------------------------#
  # Variables created in this file:
  # 
  # fp_evuse_any		"Ever used any contraceptive method"
  # fp_evuse_mod		"Ever used any modern method"
  # fp_evuse_fster	"Ever used female sterilization"
  # fp_evuse_mster	"Ever used male sterilization"
  # fp_evuse_pill		"Ever used pill"
  # fp_evuse_iud		"Ever used IUD"
  # fp_evuse_inj		"Ever used injectables"
  # fp_evuse_imp		"Ever used implants"
  # fp_evuse_mcond	"Ever used male condoms"
  # fp_evuse_fcond	"Ever used female condom"
  # fp_evuse_diaph	"Ever used diaphragm"
  # fp_evuse_lam		"Ever used LAM"
  # fp_evuse_ec			"Ever used emergency contraception"
  # fp_evuse_omod		"Ever used other modern method"
  # fp_evuse_trad		"Ever used any traditional method"
  # fp_evuse_rhy		"Ever used rhythm"
  # fp_evuse_wthd		"Ever used withdrawal"
  # fp_evuse_other	"Ever used other"
  # 
  # fp_cruse_any		"Currently use any contraceptive method"
  # fp_cruse_mod		"Currently use any modern method
  # fp_cruse_fster	"Currently use female sterilization"
  # fp_cruse_mster	"Currently use male sterilization"
  # fp_cruse_pill		"Currently use pill"
  # fp_cruse_iud		"Currently use IUD"
  # fp_cruse_inj		"Currently use injectables"
  # fp_cruse_imp		"Currently use implants"
  # fp_cruse_mcond	"Currently use male condoms"
  # fp_cruse_fcond	"Currently use female condom"
  # fp_cruse_diaph	"Currently use diaphragm"
  # fp_cruse_lam		"Currently use LAM"
  # fp_cruse_ec			"Currently use emergency contraception"
  # fp_cruse_omod		"Currently use other modern method"
  # fp_cruse_trad		"Currently use any traditional method"
  # fp_cruse_rhy		"Currently use rhythm"
  # fp_cruse_wthd		"Currently use withdrawal"
  # fp_cruse_other	"Currently use other"
  # 
  # fp_ster_age			"Age at time of sterilization for women"
  # fp_ster_median	"Median age at time of sterilization for women"
  # 
  # fp_source_tot		"Source of contraception - total"
  # fp_source_fster	"Source for female sterilization"
  # fp_source_pill	"Source for pill"
  # fp_source_iud		"Source for IUD"
  # fp_source_inj		"Source for injectables"
  # fp_source_imp		"Source for implants"
  # fp_source_mcond	"Source for male condom"
  # 
  # fp_brand_pill		"Pill users using a social marketing brand"
  # fp_brand_cond		"Male condom users using a social marketing brand"
  # 
  # fp_info_sideff		  "Informed about side effects or problems among female sterilization, pill, IUD, injectables, and implant users"
  # fp_info_what_to_do	"Informed of what to do if experienced side effects among female sterilization, pill, IUD, injectables, and implant users"
  # fp_info_other_meth	"Informed of other methods by health or FP worker among female sterilization, pill, IUD, injectables, and implant users"
  # fp_info_all 		    "Informed of all three (method information index) among female sterilization, pill, IUD, injectables, and implant users"
  
  #------------------------------------------------------------------------------
  
  
  ## Family planning messages
  
  
  # Family planning messages by radio 
  IRdata <- IRdata %>%
    mutate(fp_evuse_any = 
             ifelse(v302 > 0  & v302 < 8, 1, 0)) %>%  
    labelled::set_value_labels(fp_evuse_any = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_any = "Ever used any contraceptive method")
  
  
  # Ever use modern method
  IRdata <- IRdata %>%
    mutate(fp_evuse_mod = 
             ifelse(v302 == 3, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_mod = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_mod =  "Ever used any modern method")
  
  
  # Ever use female sterilization  
  IRdata <- IRdata %>%
    mutate(fp_evuse_fster = 
             ifelse(v305_06 > 0 & v305_06 < 8, 1, 0)) %>%
    labelled::set_value_labels(fp_evuse_fster = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_fster = "Ever used female sterilization")
  
  
  # Ever use male sterilization  
  IRdata <- IRdata %>%
    mutate(fp_evuse_mster = 
             ifelse(v305_07 > 0 & v305_07 < 8, 1, 0)) %>%  
    labelled::set_value_labels(fp_evuse_mster = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_mster = "Ever used male sterilization")
  
  
  # Ever use the contraceptive pill  
  IRdata <- IRdata %>%
    mutate(fp_evuse_pill = 
             ifelse(v305_01 > 0 & v305_01 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_pill = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_pill = "Ever used pill")
  
  
  # Ever use Interuterine contraceptive device (IUD)
  IRdata <- IRdata %>%
    mutate(fp_evuse_iud = 
             ifelse(v305_02 > 0 & v305_02 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_iud = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_iud = "Ever used IUD")
  
  
  # Ever use injectables (Depo-Provera) 
  IRdata <- IRdata %>%
    mutate(fp_evuse_inj = 
             ifelse(v305_03 > 0 & v305_03 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_inj = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_inj = "Ever used injectables")
  
  
  # Ever use implants (Norplant)  
  IRdata <- IRdata %>%
    mutate(fp_evuse_imp = 
             ifelse(v305_11 > 0 & v305_11 < 8, 1, 0)) %>%  
    labelled::set_value_labels(fp_evuse_imp = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_imp =  "Ever used implants")
  
  
  # Ever use male condoms  
  IRdata <- IRdata %>%
    mutate(fp_evuse_mcond = 
             ifelse(v305_05 > 0 & v305_05 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_mcond = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_mcond = "Ever used male condoms")
  
  
  # Ever use female condoms 
  IRdata <- IRdata %>%
    mutate(fp_evuse_fcond = 
             ifelse(v305_14 > 0 & v305_14 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_fcond = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_fcond =  "Ever used female condoms")
  
  
  # Ever use diaphragm  
  IRdata <- IRdata %>%
    mutate(fp_evuse_diaph = 
             ifelse(v305_04 > 0 & v305_04 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_diaph = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_diaph =  "Ever used diaphragm")
  
  
  # Ever use standard days method (SDM) 
  IRdata <- IRdata %>%
    mutate(fp_evuse_sdm = 
             ifelse(v305_18 > 0 & v305_18 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_sdm = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_sdm = "Ever used standard days method")
  
  
  # Ever use Lactational amenorrhea method (LAM) 
  IRdata <- IRdata %>%
    mutate(fp_evuse_lam = 
             ifelse(v305_13 > 0 & v305_13 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_lam = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_lam =  "Ever used LAM")
  
  
  ## Ever use emergency contraception  
  IRdata <- IRdata %>%
    mutate(fp_evuse_ec = 
             ifelse(v305_16 > 0 & v305_16 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_ec = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_ec = "Ever used emergency contraception")
  
  
  # Ever use country-specific modern methods and other modern contraceptive methods 
  IRdata <- IRdata %>%
    mutate(fp_evuse_omod = 
             ifelse(v305_17 > 0 & v305_17 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_omod = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_omod = "Ever used other modern method")
  
  
  # Ever use periodic abstinence (rhythm, calendar method) 
  IRdata <- IRdata %>%
    mutate(fp_evuse_rhy = 
             ifelse(v305_08 > 0 & v305_08 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_any = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_rhy = "Ever used rhythm method")
  
  
  # Ever use withdrawal  
  IRdata <- IRdata %>%
    mutate(fp_evuse_wthd = 
             ifelse(v305_09 > 0 & v305_09 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_wthd = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_wthd =  "Ever used withdrawal method")
  
  
  # Ever use country-specific traditional methods, and folk methods 
  IRdata <- IRdata %>%
    mutate(fp_evuse_other = 
             ifelse(v305_10 > 0 & v305_10 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_evuse_other = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_other =  "Ever used other method")
  
  
  # Ever use any traditional 
  IRdata <- IRdata %>%
    mutate(fp_evuse_trad = 
             ifelse(fp_evuse_rhy==1 | fp_evuse_wthd==1 | fp_evuse_other==1, 1, 0)) %>%
    labelled::set_value_labels(fp_evuse_rhy = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_evuse_trad = "Ever used any traditional method")
  #------------------------------------------------------------------------------#
  
  
  
  ### Current use of contraceptive methods
  
  
  
  # Currently use any method
  IRdata <- IRdata %>%
    mutate(fp_cruse_any = 
             ifelse(v313 > 0 & v313 < 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_any = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_any =  "Currently used any contraceptive method")
  
  # Currently use modern method
  IRdata <- IRdata %>%
    mutate(fp_cruse_mod = 
             ifelse(v313 == 3, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_mod = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_mod ="Currently used any modern method")
  
  # Currently use female sterilization  
  IRdata <- IRdata %>%
    mutate(fp_cruse_fster = 
             ifelse(v312 == 6, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_fster = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_fster = "Currently used female sterilization")
  
  # Currently use male sterilization  
  IRdata <- IRdata %>%
    mutate(fp_cruse_mster = 
             ifelse(v312 == 7, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_mster = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_mster = "Currently used male sterilization")
  
  # Currently use the contraceptive pill 
  IRdata <- IRdata %>%
    mutate(fp_cruse_pill = 
             ifelse(v312 == 1, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_pill = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_pill = "Currently used pill")
  
  # Currently use Interuterine contraceptive device 
  IRdata <- IRdata %>%
    mutate(fp_cruse_iud = 
             ifelse(v312 == 2, 1, 0)) %>%  
    labelled::set_value_labels(fp_cruse_iud = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_iud =  "Currently used IUD")
  
  # Currently use injectables (Depo-Provera) 
  IRdata <- IRdata %>%
    mutate(fp_cruse_inj = 
             ifelse(v312 == 3, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_inj = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_inj = "Currently used injectables")
  
  # Currently use implants (Norplant) 
  IRdata <- IRdata %>%
    mutate(fp_cruse_imp = 
             ifelse(v312 == 11, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_imp = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_imp = "Currently used implants")
  
  # Currently use male condom 
  IRdata <- IRdata %>%
    mutate(fp_cruse_mcond = 
             ifelse(v312 == 5, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_mcond = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_mcond = "Currently used male condoms")
  
  # Currently use female condom 
  IRdata <- IRdata %>%
    mutate(fp_cruse_fcond = 
             ifelse(v312 == 14, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_fcond = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_fcond = "Currently used female condom")
  
  # Currently use diaphragm
  IRdata <- IRdata %>%
    mutate(fp_cruse_diaph = 
             ifelse(v312 == 4, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_diaph = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_diaph = "Currently used diaphragm")
  
  # Currently use standard days method (SDM) 
  IRdata <- IRdata %>%
    mutate(fp_cruse_sdm = 
             ifelse(v312 == 18, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_sdm = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_sdm = "Currently used standard days method")
  
  # Currently use Lactational amenorrhea method (LAM) 
  IRdata <- IRdata %>%
    mutate(fp_cruse_lam = 
             ifelse(v312 == 13, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_lam = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_lam = "Currently used LAM")
  
  # Currently use emergency contraception 
  IRdata <- IRdata %>%
    mutate(fp_cruse_ec = 
             ifelse(v312 == 16, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_ec = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_ec = "Currently used emergency contraception")
  
  # Currently use country-specific modern methods and other modern contraceptive methods 
  IRdata <- IRdata %>%
    mutate(fp_cruse_omod = 
             ifelse(v312 == 17, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_omod = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_omod = "Currently used other modern method")
  
  # Currently use periodic abstinence (rhythm, calendar method) 
  IRdata <- IRdata %>%
    mutate(fp_cruse_rhy = 
             ifelse(v312 == 8, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_rhy = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_rhy = "Currently used rhythm method")
  
  # Currently use withdrawal (coitus interruptus) 
  IRdata <- IRdata %>%
    mutate(fp_cruse_wthd = 
             ifelse(v312 == 9, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_wthd = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_wthd = "Currently used withdrawal method")
  
  # Currently use country-specific traditional methods, and folk methods 
  IRdata <- IRdata %>%
    mutate(fp_cruse_other = 
             ifelse(v312==10 | v312==35, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_other = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_other = "Currently used other method")
  
  # Currently use any traditional 
  IRdata <- IRdata %>%
    mutate(fp_cruse_trad = 
             ifelse(v313 > 0 & v313 < 3, 1, 0)) %>%   
    labelled::set_value_labels(fp_cruse_trad = c(yes = 1, no = 0)) %>%
    labelled::set_variable_labels(fp_cruse_trad = "Currently used any traditional method")
  
  # /*****************************************************************************************************
  # Program: 			RH_PNC.R
  # Purpose: 			Code PNC indicators for women and newborns
  # Data inputs: 	IR dataset
  # Data outputs:	coded variables
  # Author:				Lindsay Mallick and Shireen Assaf
  # Date last modified: September 15 2021 by Mahmoud Elkasabi 
  # *****************************************************************************************************/
  # 
  # /*----------------------------------------------------------------------------//
  # Variables created in this file:
  # rh_pnc_wm_timing	"Timing after delivery for mother's PNC check"
  # rh_pnc_wm_2days 	"PNC check within two days for mother"
  # rh_pnc_wm_pv 		  "Provider for mother's PNC check"
  # 
  # rh_pnc_nb_timing	"Timing after delivery for newborn's PNC check"
  # rh_pnc_nb_2days 	"PNC check within two days for newborn"
  # rh_pnc_nb_pv 		  "Provider for newborn's PNC check"	
  # /----------------------------------------------------------------------------*/
  
  # age of child. If b19_01 is not available in the data use v008 - b3_01
  if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
    IRdata [[paste("b19_01")]] <- NA
  if ("TRUE" %in% all(is.na(IRdata $b19_01)))
  { b19_included <- 0} else { b19_included <- 1}
  
  if (b19_included==1) {
    IRdata <- IRdata %>%
      mutate(age = b19_01)
  } else {
    IRdata <- IRdata %>%
      mutate(age = v008 - b3_01)
  }
  
  
  # ** For surveys 2005 or after, postnatal care was asked for both institutional and non-institutional births. 
  # ** surveys before 2005 only ask PNC for non-institutional births but assumed women received PNC if they delivered at health facilities	 
  # ** This is checked using variable m51_1 which was used in older surveys
  # ** If the code does not run, perhaps it is because you need to use m51a_1. Uncomment the next line in that case.
  # 	*cap gen m51_1=m51a_1
  
  # ** To check if survey has m51_1, which was in the surveys before 2005. 
  if ("TRUE" %in% (!("m51_1" %in% names(IRdata))))
    IRdata [[paste("m51_1")]] <- NA
  if ("TRUE" %in% all(is.na(IRdata $m51_1)))
  { m51_included <- 0} else { m51_included <- 1}
  
  ### *** Mother's PNC *** ###		
  
  if (m51_included==1) {
    # //PNC timing for mother	
    IRdata <- IRdata %>%
      mutate(rh_pnc_wm_timing =
               case_when(
                 (m51_1 >=242 & m51_1<=297) | (m51_1>=306 & m51_1<=397) | (m50_1==0 | m50_1==9) | (m52_1>29 & m52_1<97) | is.na(m52_1)  ~ 0 ,
                 m51_1  %in% c(100, 101, 102, 103)  ~ 1 ,
                 (m51_1 >=104 & m51_1<=123) | m51_1 == 200  ~ 2 ,
                 (m51_1 >=124 & m51_1<=171) | m51_1 %in% c(201,202)~ 3,
                 (m51_1 >=172 & m51_1<=197) | m51_1 %in% c(203,204,205,206) ~ 4, 
                 (m51_1 >=207 & m51_1<=241) | (m51_1 >=301 & m51_1<=305)  ~ 5, 
                 m51_1  %in% c(198, 199, 298, 299, 398, 399, 998, 999)  ~ 9)) 
    
    IRdata[["rh_pnc_wm_timing"]] <- ifelse(IRdata[["bidx_01"]]!=1 | IRdata[["age"]]>=24, NA, IRdata[["rh_pnc_wm_timing"]])
    
    IRdata <- IRdata %>%
      set_value_labels(rh_pnc_wm_timing = c("No PNC" = 0, "<4hr" = 1, "4-23hrs"=2, "1-2days"=3, "3-6days"=4, "7-41days"=5, "don't know/missing"=9)) %>%
      set_variable_labels(rh_pnc_wm_timing = "Timing after delivery for mother's PNC check")
    
    
    # //PNC within 2days for mother	  
    IRdata <- IRdata %>%
      mutate(rh_pnc_wm_2days =
               case_when(
                 rh_pnc_wm_timing %in% c(1,2,3) ~ 1,
                 rh_pnc_wm_timing %in% c(0,4,5,9) ~ 0,
                 bidx_01!=1 | age>=24 ~ 99 )) %>%
      replace_with_na(replace = list(rh_pnc_wm_2days = c(99))) %>%
      set_value_labels(rh_pnc_wm_2days = c("No Visit w/in 2 days" = 0, "visit w/in 2 days" = 1 )) %>%
      set_variable_labels(rh_pnc_wm_2days = "PNC check within two days for mother")
    
    # //PNC provider for mother	
    # This is country specific and could be different for different surveys, please check footnote of the table for this indicator in the final report. 
    IRdata <- IRdata %>%
      mutate(rh_pnc_wm_pv =
               case_when(
                 ((age<24 & rh_pnc_wm_2days==1) & m52_1==0) | (age<24 & rh_pnc_wm_2days==0)  ~ 0,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1 ==11 ~ 1,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1 %in% c(12,13) ~ 2,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1 %in% c(14,15) ~ 3,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1>=16 & m52_1<=90 ~ 4,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1==96 ~ 5 ,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1>96 ~ 9)) %>%
      set_value_labels(rh_pnc_wm_pv = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_pnc_wm_pv = "Provider for mother's PNC check")
    
  }
  
  if (m51_included==0) {
    # //PNC timing for mother		
    # 	*did the mother have any check
    IRdata <- IRdata %>%
      mutate(momcheck =
               case_when(
                 (m62_1!=1 & m66_1!=1) & age<24 ~ 0,
                 (m62_1==1 | m66_1==1) & age<24 ~ 1,
                 age<24 ~ 0)) 
    
    IRdata <- IRdata %>% #!!!
      mutate(pnc_wm_time =
               case_when(
                 (m64_1 >= 11 & m64_1 <= 29) & age<24 ~ as.numeric(m63_1),
                 age<24 & momcheck == 1 ~ 999)) %>%
      mutate(pnc_wm_time1000 = 
               case_when(
                 pnc_wm_time<1000 ~ 1)) %>%
      mutate(pnc_wm_time =
               case_when(
                 pnc_wm_time1000==1 & (m64_1 > 30 & m64_1 < 100) & age<24 ~ 0,
                 TRUE ~ pnc_wm_time )) %>%
      mutate(pnc_wm_time999 = 
               case_when(
                 pnc_wm_time==999 ~ 1)) %>%
      mutate(pnc_wm_time = #!!!
               case_when(
                 pnc_wm_time999==1 & (m68_1 >= 11 & m68_1 <= 29) & age<24 ~ as.numeric(m67_1),
                 TRUE ~ pnc_wm_time ))  %>%
      mutate(pnc_wm_time0 =
               case_when(
                 m67_1 < 1000 & m68_1 > 30 & m68_1 < 100 & age<24 ~ 1,
                 TRUE ~ 0)) %>%
      mutate(pnc_wm_time =
               case_when(
                 pnc_wm_time0==1 ~ 0,
                 pnc_wm_time0==0 ~ as.numeric(pnc_wm_time))) %>%
      mutate(pnc_wm_time00 =
               case_when(
                 momcheck==0 & age<24 ~ 1)) %>%
      mutate(pnc_wm_time =
               case_when(
                 pnc_wm_time00==1 ~ 0,
                 TRUE ~ as.numeric(pnc_wm_time)))
    
    IRdata <- IRdata %>%
      mutate(rh_pnc_wm_timing =
               case_when(
                 (pnc_wm_time >=242 & pnc_wm_time<=299) | (pnc_wm_time>=306 & pnc_wm_time<900) | pnc_wm_time==0  ~ 0 ,
                 pnc_wm_time  %in% c(100, 101, 102, 103)  ~ 1 ,
                 (pnc_wm_time >=104 & pnc_wm_time<=123) | pnc_wm_time==200 ~ 2 ,
                 (pnc_wm_time >=124 & pnc_wm_time<=171) | pnc_wm_time %in% c(201,202)~ 3,
                 (pnc_wm_time >=172 & pnc_wm_time<=197) | pnc_wm_time %in% c(203,204,205,206) ~ 4, 
                 (pnc_wm_time >=207 & pnc_wm_time<=241) | (pnc_wm_time >=301 & pnc_wm_time<=305)  ~ 5, 
                 pnc_wm_time  %in% c(198, 199, 298, 299, 298, 399, 998, 999)  ~ 9 ,
                 bidx_01!=1 | age>=24 ~ 99 )) %>%
      replace_with_na(replace = list(rh_pnc_wm_timing = c(99))) %>%
      set_value_labels(rh_pnc_wm_timing = c("No check or past 41 days" = 0, "<4hr" = 1, "4-23hrs"=2, "1-2days"=3, "3-6days"=4, "7-41days"=5, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_pnc_wm_timing = "Timing after delivery for mother's PNC check")  
    
    
    # //PNC within 2days for mother	  
    IRdata <- IRdata %>%
      mutate(rh_pnc_wm_2days =
               case_when(
                 rh_pnc_wm_timing %in% c(1,2,3) ~ 1,
                 rh_pnc_wm_timing %in% c(0,4,5,9) ~ 0,
                 bidx_01!=1 | age>=24 ~ 99 )) %>%
      replace_with_na(replace = list(rh_pnc_wm_2days = c(99))) %>%
      set_value_labels(rh_pnc_wm_2days = c("Not in 2 days" = 0, "Within 2 days" = 1 )) %>%
      set_variable_labels(rh_pnc_wm_2days = "PNC check within two days for mother")
    
    # //PNC provider for mother	
    # This is country specific and could be different for different surveys, please check footnote of the table for this indicator in the final report. 
    #Providers of PNC for facility deliveries   
    IRdata <- IRdata %>%
      mutate(pnc_wm_pv_hf =
               case_when(
                 age<24 & rh_pnc_wm_2days==1 & m64_1==0 ~ 0,
                 age<24 & rh_pnc_wm_2days==0 ~ 0,
                 age<24 & rh_pnc_wm_2days==1 & m64_1 ==11 ~ 1,
                 age<24 & rh_pnc_wm_2days==1 & m64_1 %in% c(12,13) ~ 2,
                 age<24 & rh_pnc_wm_2days==1 & m64_1 %in% c(14,15) ~ 3,
                 age<24 & rh_pnc_wm_2days==1 & m64_1>=16 & m64_1<=90 ~ 4,
                 age<24 & rh_pnc_wm_2days==1 & m64_1==96 ~ 5,
                 age<24 & rh_pnc_wm_2days==1 & !(m64_1 %in% seq(11:96)) ~ 9 ,
                 bidx_01!=1 | age>=24 ~ 99 )) %>%
      replace_with_na(replace = list(pnc_wm_pv_hf = c(99))) %>%
      set_value_labels(pnc_wm_pv_hf = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know/missing"=9  ))
    
    #Providers of PNC for home deliveries or checks after discharge    
    IRdata <- IRdata %>%
      mutate(pnc_wm_pv_home =
               case_when(
                 age<24 & rh_pnc_wm_2days==1 & m68_1==0 ~ 0,
                 age<24 & rh_pnc_wm_2days==0 ~ 0,
                 age<24 & rh_pnc_wm_2days==1 & m68_1 ==11 ~ 1,
                 age<24 & rh_pnc_wm_2days==1 & m68_1 %in% c(12,13) ~ 2,
                 age<24 & rh_pnc_wm_2days==1 & m68_1 %in% c(14,15) ~ 3,
                 age<24 & rh_pnc_wm_2days==1 & m68_1>=16 & m68_1<=90 ~ 4,
                 age<24 & rh_pnc_wm_2days==1 & m68_1==96 ~ 5,
                 age<24 & rh_pnc_wm_2days==1 & !(m68_1 %in% seq(11:96)) ~ 9 ,
                 bidx_01!=1 | age>=24 ~ 99 )) %>%
      replace_with_na(replace = list(pnc_wm_pv_home = c(99)))  %>%
      set_value_labels(pnc_wm_pv_home = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know/missing"=9  ))
    
    #Combine two PNC provider variables 	
    IRdata <- IRdata %>%
      mutate(rh_pnc_wm_pv =
               ifelse(pnc_wm_pv_hf==9 & rh_pnc_wm_2days==1 & age<24,pnc_wm_pv_home,pnc_wm_pv_hf))  %>%
      set_variable_labels(rh_pnc_wm_pv = "Provider for mother's PNC check")
    
  }  
  
  ############################################
  ### *** Newborn's PNC *** ####
  # 
  # * some surveys (usually older surveys) do not have PNC indicators for newborns. For this you would need variables m70_1, m71_1, ..., m76_1
  # ** To check if survey has m51_1, which was in the surveys before 2005. 
  if ("TRUE" %in% (!("m70_1" %in% names(IRdata))))
    IRdata [[paste("m70_1")]] <- NA
  if ("TRUE" %in% all(is.na(IRdata $m70_1)))
  { m70_included <- 0} else { m70_included <- 1}
  
  # Survey has newborn PNC indicators
  if (m70_included==1) {
    
    if (m51_included==1) {
      
      #//PNC timing for newborn
      IRdata <- IRdata %>%
        mutate(rh_pnc_nb_timing =
                 case_when(
                   (m71_1 >=207 & m71_1<=297) | (m71_1>=301 & m71_1<397) | (m70_1==0 | m70_1==9) | (m72_1>29 & m72_1<97) | is.na(m72_1) ~ 0,
                   m71_1 ==100 ~ 1 ,
                   m71_1 %in% c(101,102,103) ~ 2 ,
                   (m71_1 >=104 & m71_1<=123) | m71_1 ==200 ~ 3,
                   (m71_1 >=124 & m71_1<=171) | m71_1 %in% c(201,202) ~ 4, 
                   (m71_1 >=172 & m71_1<=197) | (m71_1 >=203 & m71_1<=206)  ~ 5, 
                   m71_1  %in% c(198, 199, 298, 299, 398, 399, 998, 999)  ~ 9)) 
      
      IRdata[["rh_pnc_nb_timing"]] <- ifelse(IRdata[["bidx_01"]]!=1 | IRdata[["age"]]>=24, NA, IRdata[["rh_pnc_nb_timing"]]) 	  
      
      IRdata <- IRdata %>%
        set_value_labels(rh_pnc_nb_timing = c("No check or past 7 day" = 0, "<1hr" = 1, "1-3hrs"=2, "4-23hrs"=3, "1-2days"=4, "3-6days"=5, "Don't know/missing"=9  )) %>%
        set_variable_labels(rh_pnc_nb_timing = "Timing after delivery for mother's PNC check")  
      
      
      #//PNC within 2days for newborn	
      IRdata <- IRdata %>%
        mutate(rh_pnc_nb_2days =
                 case_when(
                   rh_pnc_nb_timing %in% c(1,2,3,4) ~ 1,
                   rh_pnc_nb_timing %in% c(0,5,9) ~ 0,
                   bidx_01!=1 | age>=24 ~ 99 )) %>%
        replace_with_na(replace = list(rh_pnc_nb_2days = c(99))) %>%
        set_value_labels(rh_pnc_nb_2days = c("No Visit within 2 days" = 0, "visit within 2 days" = 1 )) %>%
        set_variable_labels(rh_pnc_nb_2days = "PNC check within two days for newborn")
      
      # //PNC provider for newborn
      # This is country specific, please check table in final report
      IRdata <- IRdata %>%
        mutate(rh_pnc_nb_pv =
                 case_when(
                   ((age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) & (m72_1==0 | rh_pnc_nb_2days==0)) | (age<24 & rh_pnc_nb_2days==0) ~ 0,
                   (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1 ==11 ~ 1,
                   (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1 %in% c(12,13) ~ 2,
                   (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1 %in% c(14,15) ~ 3,
                   (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1>=16 & m72_1<=90 ~ 4,
                   (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1==96 ~ 5,
                   (age<24 & rh_pnc_nb_timing<9 & rh_pnc_nb_timing>0) &  m72_1>96 ~ 9)) %>%
        set_value_labels(rh_pnc_nb_pv = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know/missing"=9  )) %>%
        set_variable_labels(rh_pnc_nb_pv = "Provider for newborn's PNC check")
    }
    
    # 
    if (m51_included==0 & "m74_1" %in% names(IRdata)) {
      # 
      # //PNC timing for newborn	
      # *Newborn check
      IRdata <- IRdata %>%
        mutate(nbcheck =
                 case_when(
                   (m70_1==1 | m74_1==1) ~ 1,
                   (m70_1!=1 | m74_1!=1) ~ 0)) 
      
      IRdata <- IRdata %>%
        mutate(pnc_nb_timing_all =
                 case_when(
                   nbcheck!=1 & age<24 ~ 0,
                   (m76_1 >= 11 & m76_1 <= 29) & age<24 ~ as.numeric(m75_1),
                   age<24 & nbcheck == 1 ~ 999)) %>%
        mutate(pnc_nb_timing_all1000 = 
                 case_when(
                   pnc_nb_timing_all<1000 ~ 1)) %>%
        mutate(pnc_nb_timing_all =
                 case_when(
                   pnc_nb_timing_all1000==1 & (m76_1 > 30 & m76_1 < 100) & age<24 ~ 0,
                   TRUE ~ pnc_nb_timing_all )) %>%
        mutate(pnc_nb_timing_all999 = 
                 case_when(
                   pnc_nb_timing_all==999 ~ 1)) %>%
        mutate(pnc_nb_timing_all =
                 case_when(
                   pnc_nb_timing_all999==1 & (m72_1 >= 11 & m72_1 <= 29) & age<24 ~ as.numeric(m71_1), #!!!
                   TRUE ~ pnc_nb_timing_all ))  %>%
        mutate(pnc_nb_timing_all0 =
                 case_when(
                   m71_1 < 1000 & m72_1 > 30 & m72_1 < 100 & age<24 ~ 1,
                   TRUE ~ 0)) %>%
        mutate(pnc_nb_timing_all =
                 case_when(
                   pnc_nb_timing_all0==1 ~ 0,
                   pnc_nb_timing_all0==0 ~ as.numeric(pnc_nb_timing_all)))
      
      IRdata <- IRdata %>%
        mutate(rh_pnc_nb_timing =
                 case_when(
                   (pnc_nb_timing_all >=207 & pnc_nb_timing_all<=297) | (pnc_nb_timing_all>=301 & pnc_nb_timing_all<397) | pnc_nb_timing_all==0  ~ 0 ,
                   pnc_nb_timing_all  %in% c(100)  ~ 1 ,
                   pnc_nb_timing_all  %in% c(101,102,103)  ~ 2 ,
                   (pnc_nb_timing_all >=104 & pnc_nb_timing_all<=123) | pnc_nb_timing_all==200 ~ 3 ,
                   (pnc_nb_timing_all >=124 & pnc_nb_timing_all<=171) | pnc_nb_timing_all %in% c(201,202)~ 4,
                   (pnc_nb_timing_all >=172 & pnc_nb_timing_all<=197) | pnc_nb_timing_all %in% c(203,204,205,206) ~ 5, 
                   pnc_nb_timing_all  %in% c(198, 199, 298, 299, 298, 399, 998, 999)  ~ 9 ,
                   age>=24 ~ 99 )) %>%
        replace_with_na(replace = list(rh_pnc_nb_timing = c(99))) %>%
        set_value_labels(rh_pnc_nb_timing = c("No check or past 7 days" = 0, "less than 1 hour" = 1, "1-3 hours"=2, "4 to 23 hours"=3, "1-2 days"=4, "3-6 days new"=5, "Don't know/missing"=9  )) %>%
        set_variable_labels(rh_pnc_nb_timing = "Timing after delivery for mother's PNC check")  
      
      # //PNC within 2days for newborn	  
      IRdata <- IRdata %>%
        mutate(rh_pnc_nb_2days =
                 case_when(
                   rh_pnc_nb_timing %in% c(1,2,3,4) ~ 1,
                   rh_pnc_nb_timing %in% c(0,5,9) ~ 0,
                   age>=24 ~ 99 )) %>%
        replace_with_na(replace = list(rh_pnc_nb_2days = c(99))) %>%
        set_value_labels(rh_pnc_nb_2days = c("No Visit within 2 days" = 0, "visit within 2 days" = 1 )) %>%
        set_variable_labels(rh_pnc_nb_2days = "PNC check within two days for newborn")
      
      # //PNC provider for newborn	
      # This is country specific and could be different for different surveys, please check footnote of the table for this indicator in the final report. 
      #Providers of PNC for home deliveries or checks after discharge   
      IRdata <- IRdata %>%
        mutate(pnc_nb_pv_home =
                 case_when(
                   age<24 & rh_pnc_nb_2days==1 & m72_1==0 ~ 0,
                   age<24 & rh_pnc_nb_2days==0 ~ 0,
                   age<24 & rh_pnc_nb_2days==1 & m72_1 ==11 ~ 1,
                   age<24 & rh_pnc_nb_2days==1 & m72_1 %in% c(12,13) ~ 2,
                   age<24 & rh_pnc_nb_2days==1 & m72_1 %in% c(14,15) ~ 3,
                   age<24 & rh_pnc_nb_2days==1 & m72_1>=16 & m72_1<=90 ~ 4,
                   age<24 & rh_pnc_nb_2days==1 & m72_1==96 ~ 5,
                   age<24 & rh_pnc_nb_2days==1 & !(m72_1 %in% seq(11:96)) ~ 9 ,
                   age>=24 ~ 99 )) %>%
        replace_with_na(replace = list(pnc_nb_pv_home = c(99))) %>%
        set_value_labels(rh_pnc_nb_timing = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know or missing"=9))
      
      #Providers of PNC for facility deliveries   
      IRdata <- IRdata %>%
        mutate(pnc_nb_pv_hf =
                 case_when(
                   age<24 & rh_pnc_nb_2days==1 & m76_1==0 ~ 0,
                   age<24 & rh_pnc_nb_2days==0 ~ 0,
                   age<24 & rh_pnc_nb_2days==1 & m76_1 ==11 ~ 1,
                   age<24 & rh_pnc_nb_2days==1 & m76_1 %in% c(12,13) ~ 2,
                   age<24 & rh_pnc_nb_2days==1 & m76_1 %in% c(14,15) ~ 3,
                   age<24 & rh_pnc_nb_2days==1 & m76_1>=16 & m76_1<=90 ~ 4,
                   age<24 & rh_pnc_nb_2days==1 & m76_1==96 ~ 5,
                   age<24 & rh_pnc_nb_2days==1 & !(m76_1 %in% seq(11:96)) ~ 9 ,
                   age>=24 ~ 99 )) %>%
        replace_with_na(replace = list(pnc_nb_pv_hf = c(99))) %>%
        set_value_labels(pnc_nb_pv_hf = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know or missing"=9))
      
      #Combine two PNC provider variables 	
      IRdata <- IRdata %>%
        mutate(rh_pnc_nb_pv =
                 ifelse(pnc_nb_pv_hf==9 & rh_pnc_nb_2days==1 & age<24,pnc_nb_pv_home,pnc_nb_pv_hf))  %>%
        set_variable_labels(rh_pnc_nb_pv = "Provider for newborns's PNC check")
      
    }
    
  }
  
  # Survey does not have newborn PNC indicators
  if (m70_included==0) {
    # replace indicators as NA
    IRdata <- IRdata %>%
      mutate( rh_pnc_nb_timing =NA) %>%
      mutate( rh_pnc_nb_2days =NA) %>%
      mutate( rh_pnc_nb_pv =NA) 
  }
  
  #------------------------------------------------------------------------------#
  
  
  
  # # Age at female sterilization
  # 
  # v320labels <- val_labels(IRdata$v320)
  
  
  
  return(IRdata)
}

# ieClean_m <- function(dataset){
#   
#   # renaming variables for use across datasets
#   dataset$uuid <- apply(dataset, 1, UUIDgenerate)
#   
#   ## consistent interview day
#   dataset$intDay <- dataset$mv008a
#   ## consistent interview start time
#   dataset$startTime <- dataset$mv801
#   ## consitent interview end time
#   dataset$endTime <- dataset$mv802
#   ## consistent interview length
#   dataset$intLength <- dataset$mv803
#   
#   ## Create a consistent fieldworker ID variable between datasets
#   dataset$fw_ID <- dataset$mv028
#   ## create a consistent cluster
#   dataset$cluster_ID <- dataset$mv021
#   ## Age
#   dataset$age_r <- dataset$mv012
#   ##    Age in 5 years
#   dataset$age_5yrs <- NA
#   dataset$age_5yrs <- ifelse(dataset$age_r %in% c(15:19), 1, dataset$age_5yrs)
#   dataset$age_5yrs <- ifelse(dataset$age_r %in% c(20:24), 2, dataset$age_5yrs)
#   dataset$age_5yrs <- ifelse(dataset$age_r %in% c(25:29), 3, dataset$age_5yrs)
#   dataset$age_5yrs <- ifelse(dataset$age_r %in% c(30:34), 4, dataset$age_5yrs)
#   dataset$age_5yrs <- ifelse(dataset$age_r %in% c(35:39), 5, dataset$age_5yrs)
#   dataset$age_5yrs <- ifelse(dataset$age_r %in% c(40:44), 6, dataset$age_5yrs)
#   dataset$age_5yrs <- ifelse(dataset$age_r %in% c(45:49), 7, dataset$age_5yrs)
#   dataset$age_5yrs <- ifelse(dataset$age_r %in% c(50:54), 8, dataset$age_5yrs)
#   dataset$age_5yrs <- ifelse(dataset$age_r %in% c(55:59), 9, dataset$age_5yrs)
#   
#   # Sibling deaths
#   dataset$death_sib <- 
#     
#     
#     
#     
#     ## Fieldworker age
#     dataset$age_fw <- dataset$fw104
#   ## Interviewer age
#   dataset$interview_lang <- dataset$mv045b
#   ## Native language of respondent
#   dataset$nlang_r <- dataset$mv045c
#   ## translator used
#   dataset$translator <- dataset$mv046
#   ## type of place of residence
#   dataset$rural <- NA
#   dataset$rural <- ifelse(dataset$mv102 == 2, 1, 0)
#   ## education level
#   dataset$edLevel_r <- dataset$mv106
#   ## fieldworker dhs experience
#   dataset$dhs_experience <- NA
#   try({
#     dataset$dhs_experience <- ifelse(dataset$fw115 == 2, 0, dataset$fw115)
#   })
#   ## fieldworker survey experience
#   dataset$survey_experience <- NA
#   try({
#     dataset$survey_experience <- ifelse(dataset$fw116 == 2, 0, dataset$fw116)
#   })
#   ## fieldworker education level
#   try(dataset$edLevel_fw <- as.numeric(dataset$fw109))
#   
#   # fieldworker respondent differences
#   ## fieldworker / respondent education differences
#   try(dataset$edLevel_diff <- dataset$edLevel_fw - dataset$edLevel_r)
#   ## any differences between fieldworker and respondent education level
#   try(dataset$edLevel_anyDiff <- ifelse(dataset$edLevel_diff != 0, 1, 0))
#   
#   
#   # Ever sex
#   dataset$everSex <- NA
#   dataset$everSex <- ifelse(dataset$mv525 == 0, 0, 1)
#   
#   if(sum(is.na(dataset$everSex)) != sum(is.na(dataset$mv525))) {
#     warning(paste0(dataset$mv000[1], ": everSex missing does not match mv525 missing"))
#   }
#   
#   # Ever sex (imputed)
#   dataset$everSex_imp <- NA
#   dataset$everSex_imp <- ifelse(dataset$mv531 == 0, 0, 1)
#   
#   if(sum(is.na(dataset$everSex_imp)) != sum(is.na(dataset$mv531))) {
#     warning(paste0(dataset$mv000[1], ": everSex missing does not match mv531 missing"))
#   }
#   
#   if(sum(dataset$everSex_imp, na.rm = TRUE) != sum(dataset$everSex, na.rm = TRUE)) {
#     warning(paste0(dataset$mv000[1], ": The everSex derived from imputed and actual age at first sex do not match. (Men's questionnaire)"))
#   }
#   
#   ### (everUnion)
#   # Ever married or in union
#   dataset$everUnion <- NA
#   dataset$everUnion <- ifelse(dataset$mv501 == 0, 0, dataset$everUnion)
#   dataset$everUnion <- ifelse(dataset$mv501 != 0, 1, dataset$everUnion)
#   
#   if(sum(is.na(dataset$everUnion)) != sum(is.na(dataset$mv501))) {
#     warning(paste0(dataset$mv000[1], ": everUnion missing does not match mv501 missing"))
#   }
#   
#   ### (everUnion_fw)
#   dataset$everUnion_fw <- NA
#   dataset$everUnion_fw <- ifelse(dataset$fw106 == "never married or lived with a man/woman", 0, 1)
#   
#   if(sum(is.na(dataset$everUnion_fw)) != sum(is.na(dataset$fw106))) {
#     warning(paste0(dataset$mv000[1], ": everUnion_fw missing does not match mv501 missing"))
#   }
#   
#   ### (ageDiff)
#   # Age difference between interviewer and respondent
#   dataset$ageDiff <- NA
#   dataset$ageDiff <- dataset$fw104 - dataset$mv012
#   
#   ### (ageDiff_gt_10)
#   # Categorize age differences by greater than or less than 10 years
#   dataset$ageDiff_gt_10 <- NA
#   dataset$ageDiff_gt_10 <- ifelse(dataset$ageDiff >= 10, 1, 0)
#   
#   ### (ageDiff_gt_10)
#   # Categorize age differences by greater than or less than 10 years
#   dataset$ageDiff_in_10 <- NA
#   dataset$ageDiff_in_10 <- ifelse(dataset$ageDiff >= 10, 0, 1)
#   
#   ### (ageDiff_gt_5)
#   # Categorize age differences by greater than or less than 10 years
#   dataset$ageDiff_gt_5 <- NA
#   dataset$ageDiff_gt_5 <- ifelse(dataset$ageDiff >= 5, 1, 0)
#   
#   ### (ageDiff_in_5)
#   # Categorize age differences by greater than or less than 10 years
#   dataset$ageDiff_in_5 <- NA
#   dataset$ageDiff_in_5 <- ifelse(dataset$ageDiff >= 5, 0, 1)
#   
#   ### (ageDiff_5_cat)
#   # Categorize age difference by 5 year intervals
#   dataset$ageDiff_5 <- dataset$ageDiff / 5
#   dataset$ageDiff_5_cat <- ifelse(dataset$ageDiff_5 >= 0, floor(dataset$ageDiff_5), ceiling(dataset$ageDiff_5) + 1)
#   
#   ### (ageDiff_levels)
#   dataset$ageDiff_5_2lvls <- NA
#   dataset$ageDiff_5_2lvls <- ifelse(dataset$ageDiff >= 5, 1, 0)
#   dataset$ageDiff_5_2lvls <- ifelse(dataset$ageDiff >= 10, 2, dataset$ageDiff_5_2lvls)
#   
#   ### ageDiff_levels dummy
#   #### level 1 (5-9.9 years older)
#   dataset$ageDiff_5_2lvls.1 <- NA
#   dataset$ageDiff_5_2lvls.1 <- ifelse(dataset$ageDiff_5_2lvls == 1, 1, 0)
#   #### level 2 (10 + years older)
#   dataset$ageDiff_5_2lvls.2 <- NA
#   dataset$ageDiff_5_2lvls.2 <- ifelse(dataset$ageDiff_5_2lvls == 2, 1, 0)
#   
#   # Rename Wealth Index 
#   dataset$wealthIndex <- dataset$mv190
#   # Rename wealth index Score
#   dataset$wealthIndexS <- dataset$mv191
#   
#   # data warnings
#   #   Warning for unmatched gender
#   if(sum(dataset$fw105== "female", na.rm = TRUE) >= 1) {
#     warning(paste0("\n\n", dataset$mv000[1], ": WARNING! ", 
#                    sum(dataset$fw105== "female", na.rm = TRUE), 
#                    " of ", 
#                    nrow(dataset),
#                    " (",
#                    round((sum(dataset$fw105== "female", na.rm = TRUE) / nrow(dataset)), 3)*100,
#                    "%) men's questionnaires had female interviewers!"
#     )
#     )
#   }
#   
#   #   Warning about missing interviewer data
#   if(sum(is.na(dataset$fw104))>=1) {
#     warning(paste0("\n", 
#                    dataset$mv000[1], 
#                    ": Missing interviewer age (fw104) info from ", 
#                    sum(is.na(dataset$fw104)), 
#                    " of ", 
#                    nrow(dataset), 
#                    " (", 
#                    round(sum(is.na(dataset$fw104))/nrow(dataset), 3)*100, 
#                    "%) interviews (men's questionnaire)!")
#     )
#   }
#   
#   # Create a consistent fieldworker ID variable between datasets
#   dataset$fw_ID <- dataset$mv028
#   # create a consistent cluster
#   dataset$cluster_ID <- dataset$mv021
#   
#   
#   
#   return(dataset)
# }


