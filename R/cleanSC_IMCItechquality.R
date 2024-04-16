#' Clean and Process SC Data for IMCI Technical Quality Assessment
#'
#' This function processes a dataset (`SCdata`) to assess the Integrated Management of Childhood Illness (IMCI) technical quality. It validates the dataset against specific criteria, checks for the presence of required variables, calculates the IMCI technical quality scores for children aged less than and more than 2 months, and adds several new variables related to IMCI assessment criteria. It issues warnings for unsupported data types and stops execution if required counseling or physical exam variables are missing. This function is specifically validated for datasets with `c000` values of "MW6" or "HT7".
#'
#' @param SCdata A data frame containing the dataset to be processed. This dataset must include specific variables outlined in the function's internal documentation.
#' @return Returns the modified `SCdata` dataset with additional variables related to the IMCI technical quality assessment, including scores and counts for assessments specific to age groups (<2 months, >2 months).
#'
#' @details The function first checks for the presence of specific counseling and physical exam variables. If any are missing, the function stops with an error message prompting the user to first process the dataset with `cleanSC_couns()` or `cleanSC_visitActions()`. The function then processes various aspects of the IMCI assessment, such as main symptoms, general danger signs, HIV status inquiries, and feeding assessments, and calculates scores for technical quality based on these criteria. The function has been validated for datasets specifically labeled as "MW6" or "HT7" in the `c000` variable. If a dataset with a different `c000` value is processed, a warning is issued to indicate that the function's accuracy on such data has not been validated.
#'
#'
#' @references The source of the IMCI assessment criteria and the technical quality indicators is: DOI: 10.7189/jogh.14.04053.
#'
#' @examples
#' # Assuming SCdata is a dataset that has already been loaded and is available in the environment:
#' SCdata_processed <- cleanSC_IMCItechquality(SCdata)
#'
#' @export

cleanSC_IMCItechquality <- function(SCdata) {
  
  # spaList.path <- Sys.getenv("TESTING_SPA_DATA")
  # load(spaList.path)
  # SCdata <- spa.list$MW_SPA13$SC
  
  if(!SCdata$c000[1] %in% c("MW6", "HT7")) {
    warning("cleanSC_IMCItechquality() has not been validated on ", SCdata$c000[1], " data.")
  }
  
  # age in months c253
  
  # Note that the source of this list is from DOI: 10.7189/jogh.14.04053
  
  # check to make sure counseling has been added
  counsVars <- c(
    "counsSC_feedNotSick", 
    "counsSC_fluids",
    "counsSC_contFeed",
    "counsSC_nameIll", # *IMCI > 2m*
    "counsSC_descDanger", # *IMCI > 2m* # [IMCI <= 2m]
    "counsSC_visAid",
    "counsSC_anyCounseling",
    "oralMedSC_presc", # medicine prescribed
    "oralMedSC_explain", # *IMCI > 2m* # [IMCI <= 2m] medications explained
    "oralMedSC_repeatIns",
    "oralMedSC_firstDoseGiven",
    "oralMedSC_anyPresc",
    "counsSC_followUp", # *IMCI > 2m* # [IMCI <= 2m]
    "counsSC_compositeFluidFeed" # *IMCI > 2m* # [IMCI <= 2m]
  )
  
  
  if (sum(counsVars %in% names(SCdata)) != length(counsVars)) {
    stop("Error: SC dataset ", SCdata$c000[1], " must first be processed with cleanSC_couns()")
  }
  
  # check for physical exam variables
  peVars <- c(
    "pe_feverTherm", # *IMCI > 2m* # [IMCI <= 2m]
    "pe_feverFeel",
    "pe_respCount", # *IMCI > 2m* # [IMCI <= 2m]
    "pe_steth",
    "pe_turgor",
    "pe_pallorPalms",
    "pe_pallorMouth",
    "pe_lookInEar",
    "pe_feelBehindEar",
    "pe_undress",
    "pe_edema", # *IMCI > 2m*
    "pe_weigh", # *IMCI > 2m* # [IMCI <= 2m]
    "pe_plotWeight", # *IMCI > 2m* # [IMCI <= 2m]
    "pe_lookMouth", # [IMCI <= 2m]
    "pe_checkNeck",
    "pe_checkLymph"
  )
  
  if (sum(peVars %in% names(SCdata)) != length(peVars)) {
    stop("Error: SC dataset ", SCdata$c000[1], " missing physical exam variables, it must first be processed with cleanSC_visitActions()")
  }
  
  # PROVIDER ASKED / CARETAKER MENTIONED
  
  # c203 RECORD WHETHER A PROVIDER ASKED ABOUT OR WHETHER THE CARETAKER MENTIONED THAT THE CHILD HAD ANY OF THE FOLLOWING *MAIN SYMPTOMS*
  
  # *IMCI > 2m* label variable c203a    "Main symptoms:Cough or difficult breathing"
  SCdata$pask_cough <- NA
  SCdata$pask_cough <- ifelse(SCdata$c203a == 1, 1, 0)
  
  # *IMCI > 2m* # [IMCI <= 2m] c203b    "Main symptoms:Diarrhea"
  # label define C203B   
  # 0 "No"
  # 1 "Yes"
  SCdata$pask_diarrhea <- NA
  SCdata$pask_diarrhea <- ifelse(SCdata$c203b == 1, 1, 0)
  
  # *IMCI > 2m* c203c    "Main symptoms:Fever or body hotness" # not included!
  SCdata$pask_fever <- NA
  SCdata$pask_fever <- ifelse(SCdata$c203c == 1, 1, 0)
  
  # *IMCI > 2m* c203d    "Main symptoms:Ear problems"
  SCdata$pask_earPain <- NA
  SCdata$pask_earPain <- ifelse(SCdata$c203d == 1, 1, 0)
  
  mainSympVars <- c(
    "pask_cough",
    "pask_diarrhea",
    "pask_fever",
    "pask_earPain"
    )
  
  
  
  # c204 RECORD WHETHER A PROVIDER ASKED ABOUT OR WHETHER THE CARETAKER MENTIONED ANY OF THE FOLLOWING *GENERAL DANGER SIGNS*
  
  # *IMCI > 2m* # [IMCI <= 2m] c204a    "Other problems General:Unable to drink/breastfeed"
  # label define C204A   
  # 0 "No"
  # 1 "Yes, none of first 3 problems"
  # !!! Confusing, but based on the others - I think this is mislabelled, and should simply be 'Yes'
  SCdata$pask_drinkOrBf <- NA
  SCdata$pask_drinkOrBf <- ifelse(SCdata$c204a == 1, 1, 0)
  
  # *IMCI > 2m* # [IMCI <= 2m] c204b    "Other problems General:Vomiting everything"
  # label define C204B   
  # 0 "No"
  # 1 "Yes"
  SCdata$pask_vomit <- NA
  SCdata$pask_vomit <- ifelse(SCdata$c204b == 1, 1, 0)
  
  # *IMCI > 2m* # [IMCI <= 2m] c204c    "Other problems General:Had convulsions"
  # label define C204C   
  # 0 "No"
  # 1 "Yes"
  SCdata$pask_convulsions <- NA
  SCdata$pask_convulsions <- ifelse(SCdata$c204c == 1, 1, 0)
  
  dangerSignVars <- c(
    "pask_drinkOrBf",
    "pask_vomit",
    "pask_convulsions",
    "pask_motherHIV"
  )
  
  
  
  # RECORD WHETHER A PROVIDER CHECKED FOR SUSPECTED SYMPTOMATIC HIV INFECTION BY ASKING FOR ANY OF THE FOLLOWING:
  
  # *IMCI > 2m* # [IMCI <= 2m] c204d    "Other problems HIV symptomatic:Mother's HIV status"
  SCdata$pask_motherHIV <- NA
  SCdata$pask_motherHIV <- ifelse(SCdata$c204d == 1, 1, 0)
  
  
  
  # RECODE PALLOR TO SINGLE VARIABLE

  # *IMCI > 2m* label variable c205g    "Physical exam: Check for pallor by looking at conjunctiva or mouth"
  SCdata$pe_pallor <- NA
  SCdata$pe_pallor <- ifelse(SCdata$pe_pallorPalms == 1 | SCdata$pe_pallorMouth == 1, 1, 0)
  
  
  
  # RECORD WHETHER A PROVIDER ASKED ABOUT OR PERFORMED OTHER ASSESSMENTS OF THE CHILD’S HEALTH BY DOING ANY OF THE FOLLOWING:
  
  # c206a "Other assessments:Offered breast or drink"
  SCdata$assess_breast_drink <- NA
  SCdata$assess_breast_drink <- ifelse(SCdata$c206a == 1, 1, 0)
  # c206b "Other assessments:Normal feeding when not ill"
  SCdata$assess_feeding_notIll <- NA
  SCdata$assess_feeding_notIll <- ifelse(SCdata$c206b == 1, 1, 0)
  # c206c "Other assessments:Normal breastfeeding when not ill"
  SCdata$assess_bf_notIll <- NA
  SCdata$assess_bf_notIll <- ifelse(SCdata$c206c == 1, 1, 0)
  # [IMCI <= 2m] c206d "Other assessments:Feeding or breastfeeding during this illness"
  SCdata$assess_feeding_ill <- NA
  SCdata$assess_feeding_ill <- ifelse(SCdata$c206d == 1, 1, 0)
  # c206e "Other assessments:Discuss weight/growth/growth chart"
  SCdata$assess_growthChart <- NA
  SCdata$assess_growthChart <- ifelse(SCdata$c206e == 1, 1, 0)
  # *IMCI > 2m* # [IMCI <= 2m] c206f "Other assessments:Review immunization card or ask about vaccination history"
  SCdata$assess_vaxxCard <- NA
  SCdata$assess_vaxxCard <- ifelse(SCdata$c206f == 1, 1, 0)
  # *IMCI > 2m* # [IMCI <= 2m] c206g    "Other assessments:Ask if child received vitamin A within past 6 months"
  SCdata$assess_childVitA <- NA
  SCdata$assess_childVitA <- ifelse(SCdata$c206g == 1, 1, 0)
  # *IMCI > 2m* c206i    "Other assessments:Asked if child received de-worming medication in last 6 months"
  SCdata$assess_deworm <- NA
  SCdata$assess_deworm <- ifelse(SCdata$c206i == 1, 1, 0)
  # c206y "Other assessments:None of the above"
  SCdata$assess_none <- NA
  SCdata$assess_none <- ifelse(SCdata$c206y == 1, 1, 0)
  
  # [IMCI <= 2m] composite feeding or breastfeeding when not ill
  SCdata$assess_feedingBf_notIll <- NA
  SCdata$assess_feedingBf_notIll <- ifelse(SCdata$assess_feeding_notIll == 1 | SCdata$assess_bf_notIll == 1, 1, 0)
  
  
  IMCI_gt2m_Vars <- c(
    # ∙ Provider asked / caretaker mentioned
    "pask_drinkOrBf", #   - if child was unable to drink or breastfeed
    "pask_cough", #   - cough or difficult breathing
    "pask_diarrhea", #   - diarrhea
    "pask_fever", #   - fever
    "pask_vomit", #   - about vomiting
    "pask_convulsions", #   - convulsions
    "pask_earPain", #   - ear pain
    # ∙ Provider asked
    "pask_motherHIV", #   - about mother's HIV* status
    "assess_childVitA", #   - if child received vitamin A within past 6 months
    "assess_deworm", #   - if child received any deworming medication in last 6 mon.
    # ∙ Provider checked
    "pe_pallor", #   - palms / conjunctiva / mouth for pallor
    "pe_edema", #   - for oedema
    "assess_vaxxCard", #   - vaccination card or vaccinated
    # ∙ Provider counted respiration for 60 seconds
    "pe_weigh", # ∙ Provider weighed client
    "pe_plotWeight", # ∙ Provider plotted weight on growth chart
    "pe_feverTherm", # ∙ Provider took temperature
    "oralMedSC_explain", # ∙ Provider explained dosing if medication prescribed
    "counsSC_compositeFluidFeed", # ∙ Provider recommended food / liquid intake
    "counsSC_descDanger", # ∙ Provider described ≥ 1 danger sign requiring return to facility
    "counsSC_followUp", # ∙ Provider discussed follow-up appointment
    "counsSC_nameIll" # ∙ Provider stated diagnosis to caretaker
  )
  
  IMCI_lt2m_Vars <- c(
    # ∙ Provider asked / caretaker mentioned
    "pask_drinkOrBf", # - if child was unable to drink or breastfeed
    "pask_diarrhea", # - diarrhea
    "pask_convulsions", #   - convulsions
    # ∙ Provider asked
    "assess_feedingBf_notIll", # - about normal (breast)feeding pattern
    "assess_feeding_ill", # - about (breast)feeding pattern during this illness
    "pask_motherHIV", #   - about mother's HIV* status
    "assess_childVitA", #   - if child received vitamin A within past 6 months
    "pe_respCount", # ∙ Provider counted respiration for 60 seconds
    "pe_weigh", # ∙ Provider weighed client
    "pe_plotWeight", # ∙ Provider plotted weight on growth chart
    "pe_feverTherm", # ∙ Provider took temperature
    "pe_lookMouth", # ∙ Provider looked into childs mouth
    "assess_vaxxCard", # ∙ Provider checked vaccination card or vaccinated
    "oralMedSC_explain", # ∙ Provider explained dosing if medication prescribed
    "counsSC_compositeFluidFeed", # ∙ Provider recommended food / liquid intake
    "counsSC_descDanger", # ∙ Provider described ≥ 1 danger sign requiring return to facility
    "counsSC_followUp" # ∙ Provider discussed follow-up appointment
  )
  
  # IMCI Technical quality from DOI: 10.7189/jogh.14.04053
  #   Less than 2 months count
  SCdata$IMCI_techQual_lt2m_count <- NA
  SCdata$IMCI_techQual_lt2m_count <- ifelse(SCdata$c253 <= 2, rowSums(sf::st_drop_geometry(SCdata)[,IMCI_lt2m_Vars], na.rm = T), NA)
  # Score on scale of 0 to 100
  SCdata$IMCI_techQual_lt2m_score <- NA
  SCdata$IMCI_techQual_lt2m_score <- ifelse(SCdata$c253 <= 2, SCdata$IMCI_techQual_lt2m_count/ length(IMCI_lt2m_Vars), NA)
  
  #   Less than 2 months count
  SCdata$IMCI_techQual_gt2m_count <- NA
  SCdata$IMCI_techQual_gt2m_count <- ifelse(SCdata$c253 > 2, rowSums(sf::st_drop_geometry(SCdata)[,IMCI_gt2m_Vars], na.rm = T), NA)
  #   Score on scale of 0 to 100
  SCdata$IMCI_techQual_gt2m_score <- NA
  SCdata$IMCI_techQual_gt2m_score <- ifelse(SCdata$c253 > 2, SCdata$IMCI_techQual_gt2m_count/ length(IMCI_gt2m_Vars), NA)
  
  # Score on either
  SCdata$IMCI_techQual <- ifelse(SCdata$c253 > 2, SCdata$IMCI_techQual_gt2m_score, SCdata$IMCI_techQual_lt2m_score)
  
  return(SCdata)
  
}















