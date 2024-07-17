#' Clean and Enhance Kids' Recode (KR) Dataset
#'
#' This function cleans and adds several new variables to the Kids' Recode (KR) dataset, focusing on health indicators such as symptoms of ARI, fever, diarrhea, and the care-seeking behavior associated with these conditions. It generates female IDs, processes ARI indicators, fever indicators, diarrhea indicators, treatment sources, and calculates care-seeking behaviors. It assumes specific variable naming conventions aligned with the Demographic and Health Surveys (DHS).
#'
#' @param KRdata A dataframe representing the Kids' Recode (KR) dataset from DHS, containing variables related to children's health, particularly ARI, fever, and diarrhea indicators.
#'
#' @return The function returns the input dataframe augmented with new variables related to health indicators and care-seeking behaviors. These include indicators for ARI symptoms, fever symptoms, diarrhea symptoms, treatments sought for these conditions from various sources, and variables indicating whether any or all symptoms were treated.
#'
#' @details
#' This function is specifically designed for use with DHS Kids' Recode datasets. It expects certain variables to be present and formats them according to DHS standards. The function adds numerous health-related indicators and performs checks to ensure variable integrity, especially for key identifiers like childID and femaleID. The calculation of new indicators is based on DHS guidelines for ARI, fever, and diarrhea, including treatment seeking from different healthcare sources. Note that some variables and calculations may be country-specific and require the dataset to include relevant information.
#'
#' @examples
#' # Assuming KRdata is your KR dataset loaded into R
#' # KRdata <- read.csv("KRdata.csv")
#' KRdata_cleaned <- cleanKR(KRdata)
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_pad
#' @export

cleanKR <- function(KRdata) {
  
  
  # Generate female ID from data
  KRdata$femaleID <- NA
  KRdata$femaleID <- paste(KRdata$v001, KRdata$v002, KRdata$v003, sep="_")
  
  # label variable v001     "Cluster number"
  # label variable v002     "Household number"
  # label variable v003     "Respondent's line number"
  
  # linking to OB data
  KRdata$obLinkID <- NA
  KRdata$obLinkID <- paste(KRdata$v001, KRdata$v002, KRdata$b16, sep = "_")
  
  
  
  # /*****************************************************************************************************
  # Program: 			  CH_ARI_FV.R
  # Purpose: 			  Code ARI and fever variables.
  # Data inputs: 		KR dataset
  # Data outputs:		coded variables
  # Author:				  Shireen Assaf
  # Date last modified: July 29, 2022 by Shireen Assaf 
  # Notes:          Check notes for ARI/fever care and treatment variables which are country specific. 		
  # *****************************************************************************************************/
  # 
  # /*----------------------------------------------------------------------------
  # Variables created in this file:
  # ch_ari				      "ARI symptoms in the 2 weeks before the survey"
  # ch_ari_care			    "Advice or treatment sought for ARI symptoms"
  # ch_ari_care_day		  "Advice or treatment sought for ARI symptoms on the same or next day"
  # 
  # ch_ari_govh			    "ARI treatment sought from government hospital among children with ARI"
  # ch_ari_govh_trt		  "ARI treatment sought from government hospital among children with ARI that sought treatment"
  # ch_ari_govcent 		  "ARI treatment sought from government health center among children with ARI"
  # ch_ari_govcent_trt 	"ARI treatment sought from government health center among children with ARI that sought treatment"
  # ch_ari_pclinc 		  "ARI treatment sought from private hospital/clinic among children with ARI"
  # ch_ari_pclinc_trt 	"ARI treatment sought from private hospital/clinic  among children with ARI that sought treatment"
  # ch_ari_pdoc			    "ARI treatment sought from private doctor among children with ARI"
  # ch_ari_pdoc_trt		  "ARI treatment sought from private doctor among children with ARI that sought treatment"
  # ch_ari_pharm		    "ARI treatment sought from pharmacy among children with ARI"
  # ch_ari_pharm_trt	  "ARI treatment sought from pharmacy among children with ARI that sought treatment"
  # 
  # ch_fever			      "Fever symptoms in the 2 weeks before the survey"
  # ch_fev_care			    "Advice or treatment sought for fever symptoms"
  # ch_fev_care_day		  "Advice or treatment sought for ARI symptoms on the same or next day"
  # ch_fev_antib		    "Antibiotics taken for fever symptoms"
  # ----------------------------------------------------------------------------*/
  
  # weight variable 
  KRdata <- KRdata %>%
    mutate(wt = v005/1000000)
  
  # ** ARI indicators ***
  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(KRdata))))
    KRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}
  
  if (b19_included==1) {
    KRdata <- KRdata %>%
      mutate(age = b19)
  } else {
    KRdata <- KRdata %>%
      mutate(age = v008 - b3)
  }
  
  # //ARI symptoms
  # ARI definition differs by survey according to whether h31c is included or not
  if ("TRUE" %in% (!("h31c" %in% names(KRdata))))
    KRdata [[paste("h31c")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$h31c)))
  { h31c_included <- 0} else { h31c_included <- 1}
  
  if (h31c_included==1) {
    KRdata <- KRdata %>%
      mutate(ch_ari = 
               case_when(
                 h31b==1 & (h31c==1 | h31c==3) & b5==1 ~ 1,
                 b5==1 ~ 0  )) %>%
      set_value_labels(ch_ari = c("Yes" = 1, "No"=0)) %>%
      set_variable_labels(ch_ari = "ARI symptoms in the 2 weeks before the survey")
  } else {
    KRdata <- KRdata %>%
      mutate(ch_ari = 
               case_when(
                 h31b==1 & (h31==2) & b5==1 ~ 1 ,
                 b5==1 ~ 0 )) %>%
      set_value_labels(ch_ari = c("Yes" = 1, "No"=0)) %>%
      set_variable_labels(ch_ari = "ARI symptoms in the 2 weeks before the survey")
  }
  
  
  # survey specific changes
  # if survey is "IAKR23" or "PHKR31" 
  # KRdata <- KRdata %>%
  #   mutate(ch_ari = 
  #            case_when(
  #              h31b==1 & (h31==2|h31==1) ~ 1 ,
  #              b5==1 ~ 0 )) %>%
  #   set_value_labels(ch_ari = c("Yes" = 1, "No"=0)) %>%
  #   set_variable_labels(ch_ari = "ARI symptoms in the 2 weeks before the survey")
  # 
  
  # //ARI care-seeking
  # This is country specific and the footnote for the final table needs to be checked to see what sources are included. 
  # The code below only excludes traditional practitioner (usually h32t). 
  # The variable for traditional healer may be different for different surveys (you can check this by checking all the h32* variables). 
  # Some surveys also exclude pharmacies, shop, or other sources.
  # If you want to also remove pharmacy for example as a source of treatment (country specific condition) you can remove 
  # h32k from the code below.
  
  KRdata <- KRdata %>%
    mutate(ch_ari_care =
             case_when(
               (ch_ari==1 &  b5==1) & 
                 (h32a == 1 | h32b == 1 | h32c == 1 | h32d == 1 | h32e == 1 | h32f == 1 |
                    h32g == 1 | h32h == 1 | h32i == 1 | h32j == 1 | h32k == 1 | h32l == 1 |
                    h32m == 1 | h32n == 1 | h32o == 1 | h32p == 1 | h32q == 1 | h32r == 1 |
                    h32s == 1 |             h32u == 1 | h32v == 1 | h32w == 1 | h32x == 1 )  ~ 1 ,
               b5==1 & ch_ari==1 ~ 0)) %>%
    set_value_labels(ch_ari_care = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_care = "Advice or treatment sought for ARI symptoms")
  
  # //ARI care-seeking same or next day
  # some surveys do not have the variable needed to code this indicator which is h46b
  
  if ("TRUE" %in% (!("h46b" %in% names(KRdata))))
    KRdata [[paste("h46b")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$h46b)))
  {h46b_included <- 0} else {h46b_included <- 1}
  
  if (h46b_included==1) {
    KRdata <- KRdata %>%
      mutate(ch_ari_care_day = 
               case_when(
                 ch_ari==1 & h46b<2 & b5==1 ~ 1 ,
                 ch_ari==1 & b5==1 ~ 0 )) %>%
      set_value_labels(ch_ari_care_day = c("Yes" = 1, "No"=0)) %>%
      set_variable_labels(ch_ari_care_day = "Advice or treatment sought for ARI symptoms on the same or next day")
  } else {
    KRdata <- KRdata %>%
      mutate(ch_ari_care_day = NA)
  }
  
  # *** ARI treatment by source *** 
  # Two population bases: 1. among children with ARI symptoms, 2. among children with ARI symptoms that sought treatment
  # This is country specific and needs to be checked to produce the specific source of interest. 
  # Some sources are coded below and the same logic can be used to code other sources. h32a-z indicates the source.
  
  # //ARI treatment in government hospital
  KRdata <- KRdata %>%
    mutate(ch_ari_govh = 
             case_when(
               ch_ari==1 & h32a==1 & b5==1 ~ 1 ,
               ch_ari==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_govh = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_govh = "ARI treatment sought from government hospital among children with ARI")
  
  KRdata <- KRdata %>%
    mutate(ch_ari_govh_trt = 
             case_when(
               ch_ari_care==1 & h32a==1 & b5==1 ~ 1 ,
               ch_ari_care==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_govh_trt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_govh_trt = "ARI treatment sought from government hospital among children with ARI that sought treatment")
  
  # //ARI treamtment in government health center
  KRdata <- KRdata %>%
    mutate(ch_ari_govcent = 
             case_when(
               ch_ari==1 & h32b==1 & b5==1 ~ 1 ,
               ch_ari==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_govcent = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_govcent = "ARI treatment sought from government health center among children with ARI")
  
  KRdata <- KRdata %>%
    mutate(ch_ari_govcent_trt = 
             case_when(
               ch_ari_care==1 & h32b==1 & b5==1 ~ 1 ,
               ch_ari_care==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_govcent_trt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_govcent_trt = "ARI treatment sought from government health center among children with ARI that sought treatment")
  
  # //ARI treatment from a private hospital/clinic
  KRdata <- KRdata %>%
    mutate(ch_ari_pclinc = 
             case_when(
               ch_ari==1 & h32j==1 & b5==1 ~ 1 ,
               ch_ari==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_pclinc = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_pclinc = "ARI treatment sought from private hospital/clinic among children with ARI")
  
  KRdata <- KRdata %>%
    mutate(ch_ari_pclinc_trt = 
             case_when(
               ch_ari_care==1 & h32j==1 & b5==1 ~ 1 ,
               ch_ari_care==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_pclinc_trt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_pclinc_trt = "ARI treatment sought from private hospital/clinic among children with ARI that sought treatment")
  
  # //ARI treatment from a private doctor
  KRdata <- KRdata %>%
    mutate(ch_ari_pdoc = 
             case_when(
               ch_ari==1 & h32l==1 & b5==1 ~ 1 ,
               ch_ari==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_pdoc = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_pdoc = "ARI treatment sought from private doctor  among children with ARI")
  
  KRdata <- KRdata %>%
    mutate(ch_ari_pdoc_trt = 
             case_when(
               ch_ari_care==1 & h32l==1 & b5==1 ~ 1 ,
               ch_ari_care==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_pdoc_trt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_pdoc_trt = "ARI treatment sought from private doctor among children with ARI that sought treatment")
  
  # //ARI treatment from a pharmacy
  KRdata <- KRdata %>%
    mutate(ch_ari_pharm = 
             case_when(
               ch_ari==1 & h32k==1 & b5==1 ~ 1 ,
               ch_ari==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_pharm = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_pharm = "ARI treatment sought from a pharmacy among children with ARI")
  
  KRdata <- KRdata %>%
    mutate(ch_ari_pharm_trt = 
             case_when(
               ch_ari_care==1 & h32k==1 & b5==1 ~ 1 ,
               ch_ari_care==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari_pharm_trt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari_pharm_trt = "ARI treatment sought from a pharmacy among children with ARI that sought treatment")
  
  # *** Fever indicators ***
  
  # //Fever 
  KRdata <- KRdata %>%
    mutate(ch_fever = 
             case_when(
               h22==1 & b5==1 ~ 1,
               b5==1 ~ 0  )) %>%
    set_value_labels(ch_fever = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_fever = "Fever symptoms in the 2 weeks before the survey")
  
  # //Fever care-seeking
  # This is country specific and the footnote for the final table needs to be checked to see what sources are included. 
  # The code below only excludes traditional practitioner (usually h32t). 
  # The variable for traditional healer may be different for different surveys (you can check this by checking all the h32* variables). 
  # Some surveys also exclude pharmacies, shop, or other sources.
  # If you want to also remove pharmacy for example as a source of treatment (country specific condition) you can remove 
  # h32k from the code below.
  
  KRdata <- KRdata %>%
    mutate(ch_fev_care =
             case_when(
               (ch_fever==1 &  b5==1) & 
                 (h32a == 1 | h32b == 1 | h32c == 1 | h32d == 1 | h32e == 1 | h32f == 1 |
                    h32g == 1 | h32h == 1 | h32i == 1 | h32j == 1 | h32k == 1 | h32l == 1 |
                    h32m == 1 | h32n == 1 | h32o == 1 | h32p == 1 | h32q == 1 | h32r == 1 |
                    h32s == 1 |             h32u == 1 | h32v == 1 | h32w == 1 | h32x == 1 )  ~ 1 ,
               b5==1 & ch_fever==1 ~ 0)) %>%
    set_value_labels(ch_fev_care = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_fev_care = "Advice or treatment sought for fever symptoms")
  
  # //Fever care-seeking same or next day
  
  if (h46b_included==1) {
    KRdata <- KRdata %>%
      mutate(ch_fev_care_day = 
               case_when(
                 ch_fever==1 & h46b<2 & b5==1 ~ 1 ,
                 ch_fever==1 & b5==1 ~ 0 )) %>%
      set_value_labels(ch_fev_care_day = c("Yes" = 1, "No"=0)) %>%
      set_variable_labels(ch_fev_care_day = "Advice or treatment sought for fever symptoms on the same or next day")
  } else {
    KRdata <- KRdata %>%
      mutate(ch_fev_care_day = NA)
  }
  
  # # //Given antibiotics for fever 
  # KRdata <- KRdata %>%
  #   mutate(ch_fev_antib = 
  #            case_when(
  #              ch_fever==1 & (h37i==1 | h37j==1)  ~ 1,
  #              ch_fever==1 & (ml13i==1 | ml13j ==1)  ~ 1,
  #              ch_fever==1 & b5==1 ~ 0  )) %>%
  #   set_value_labels(ch_fev_antib = c("Yes" = 1, "No"=0)) %>%
  #   set_variable_labels(ch_fev_antib = "Antibiotics taken for fever symptoms")
  
  
  
  # /*****************************************************************************************************
  # Program: 			  CH_DIAR.R
  # Purpose: 			  Code diarrhea variables.
  # Data inputs: 		KR dataset
  # Data outputs:		coded variables
  # Author:				  Shireen Assaf
  # Date last modified: Aug 2 2022 by Shireen Assaf 
  # Notes:				      Check notes for diarrhea care and treatment variables which are country specific.
  # *****************************************************************************************************/
  # 
  # /*----------------------------------------------------------------------------
  # Variables created in this file:
  # ch_diar				    "Diarrhea in the 2 weeks before the survey"
  # ch_diar_care		  "Advice or treatment sought for diarrhea"
  # 
  # ch_diar_liq			  "Amount of liquids given for child with diarrhea"
  # ch_diar_food		  "Amount of food given for child with diarrhea"
  # 
  # ch_diar_ors			  "Given oral rehydration salts for diarrhea"
  # ch_diar_rhf			  "Given recommended homemade fluids for diarrhea"
  # ch_diar_ors_rhf		"Given either ORS or RHF for diarrhea"
  # ch_diar_zinc		  "Given zinc for diarrhea"
  # ch_diar_zinc_ors	"Given zinc and ORS for diarrhea"
  # ch_diar_ors_fluid	"Given ORS or increased fluids for diarrhea"
  # ch_diar_ort			  "Given oral rehydration treatment and increased liquids for diarrhea"
  # ch_diar_ort_feed	"Given ORT and continued feeding for diarrhea"
  # ch_diar_antib		  "Given antibiotic drugs for diarrhea"
  # ch_diar_antim		  "Given antimotility drugs for diarrhea"
  # ch_diar_intra		  "Given Intravenous solution for diarrhea"
  # ch_diar_other		  "Given home remedy or other treatment  for diarrhea"
  # ch_diar_notrt		  "No treatment for diarrhea"
  # 
  # ch_diar_govh 		    "Diarrhea treatment sought from government hospital among children with diarrhea"
  # ch_diar_govh_trt 	  "Diarrhea treatment sought from government hospital among children with diarrhea that sought treatment"
  # ch_diar_govh_ors 	  "Diarrhea treatment sought from government hospital among children with diarrhea that received ORS"
  # ch_diar_govcent 	  "Diarrhea treatment sought from government health center among children with diarrhea"
  # ch_diar_govcent_trt "Diarrhea treatment sought from government health center among children with diarrhea that sought treatment"
  # ch_diar_govcent_ors "Diarrhea treatment sought from government health center among children with diarrhea that received ORS"
  # ch_diar_pclinc 		  "Diarrhea treatment sought from private hospital/clinic among children with diarrhea"
  # ch_diar_pclinc_trt 	"Diarrhea treatment sought from private hospital/clinic among children with diarrhea that sought treatment"
  # ch_diar_pclinc_ors 	"Diarrhea treatment sought from private hospital/clinic among children with diarrhea that received ORS"
  # ch_diar_pdoc 		    "Diarrhea treatment sought from private doctor among children with diarrhea"
  # ch_diar_pdoc_trt 	  "Diarrhea treatment sought from private doctor among children with diarrhea that sought treatment"
  # ch_diar_pdoc_ors 	  "Diarrhea treatment sought from private doctor among children with diarrhea that received ORS"
  # ch_diar_pharm 		  "Diarrhea treatment sought from a pharmacy among children with diarrhea"
  # ch_diar_pharm_trt 	"Diarrhea treatment sought from a pharmacy among children with diarrhea that sought treatment"
  # ch_diar_pharm_ors 	"Diarrhea treatment sought from a pharmacy among children with diarrhea that received ORS"
  # ----------------------------------------------------------------------------*/
  
  # weight variable 
  KRdata <- KRdata %>%
    mutate(wt = v005/1000000)
  
  # //Diarrhea symptoms
  KRdata <- KRdata %>%
    mutate(ch_diar = 
             case_when(
               (h11==1 | h11==2) & b5==1 ~ 1,
               b5==1 ~ 0  )) %>%
    set_value_labels(ch_diar = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar = "Diarrhea in the 2 weeks before the survey")
  
  # //Diarrhea treatment	
  # This is country specific and the footnote for the final table needs to be checked to see what sources are included. 
  # The code below only excludes traditional practitioner (usually h12t). 
  # The variable for traditional healer may be different for different surveys (you can check this checking all the h12* variables). 
  # Some surveys also exclude pharmacies, shop, or other sources.
  # If you want to also remove pharmacy for example as a source of treatment (country specific condition) you can remove 
  # h12k from the code below.
  
  KRdata <- KRdata %>%
    mutate(ch_diar_care =
             case_when(
               ch_diar==1 & 
                 (h12a == 1 | h12b == 1 | h12c == 1 | h12d == 1 | h12e == 1 | h12f == 1 |
                    h12g == 1 | h12h == 1 | h12i == 1 | h12j == 1 | h12k == 1 | h12l == 1 |
                    h12m == 1 | h12n == 1 | h12o == 1 | h12p == 1 | h12q == 1 | h12r == 1 |
                    h12s == 1 |             h12u == 1 | h12v == 1 | h12w == 1 | h12x == 1 )  ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_care = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_care = "Advice or treatment sought for diarrhea")
  
  # //Liquid intake
  KRdata <- KRdata %>%
    mutate(ch_diar_liq =
             case_when(
               ch_diar==1 & h38==5  ~ 1 ,
               ch_diar==1 & h38==4  ~ 2 ,
               ch_diar==1 & h38==3  ~ 3 ,
               ch_diar==1 & h38==2  ~ 4 ,
               ch_diar==1 & h38==0  ~ 5 ,
               ch_diar==1 & (h38==8 | h38==9) ~ 9)) %>%
    set_value_labels(ch_diar_liq = c("More" = 1, "Same as usual"=2, "Somewhat less"=3, "Much less"=4, 
                                     "None"=5, "Don't know/missing"=9 )) %>%
    set_variable_labels(ch_diar_liq = "Amount of liquids given for child with diarrhea")
  
  # //Food intake
  KRdata <- KRdata %>%
    mutate(ch_diar_food =
             case_when(
               ch_diar==1 & h39==5  ~ 1 ,
               ch_diar==1 & h39==4  ~ 2 ,
               ch_diar==1 & h39==3  ~ 3 ,
               ch_diar==1 & h39==2  ~ 4 ,
               ch_diar==1 & h39==0  ~ 5 ,
               ch_diar==1 & h39==1  ~ 6 ,
               ch_diar==1 & (h39==8 | h39==9) ~ 9)) %>%
    set_value_labels(ch_diar_food = c("More" = 1, "Same as usual"=2, "Somewhat less"=3, "Much less"=4, 
                                      "None"=5, "Never gave food"=6, "Don't know/missing"=9 )) %>%
    set_variable_labels(ch_diar_food = "Amount of food given for child with diarrhea")
  
  
  # //ORS
  KRdata <- KRdata %>%
    mutate(ch_diar_ors =
             case_when(
               ch_diar==1 & (h13==1 | h13==2 | h13b==1)  ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_ors = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_ors = "Given oral rehydration salts for diarrhea")
  
  # //RHF
  KRdata <- KRdata %>%
    mutate(ch_diar_rhf =
             case_when(
               ch_diar==1 & (h14==1 | h14==2) ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_rhf = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_rhf = "Given recommended homemade fluids for diarrhea")
  
  # //ORS or RHF
  KRdata <- KRdata %>%
    mutate(ch_diar_ors_rhf =
             case_when(
               ch_diar==1 & (h13==1 | h13==2 | h13b==1 | h14==1 | h14==2) ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_ors_rhf = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_ors_rhf = "Given either ORS or RHF for diarrhea")
  
  # //Zinc
  KRdata <- KRdata %>%
    mutate(ch_diar_zinc =
             case_when(
               ch_diar==1 & h15e==1 ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_zinc = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_zinc = "Given zinc for diarrhea")
  
  # //Zinc and ORS
  KRdata <- KRdata %>%
    mutate(ch_diar_zinc_ors =
             case_when(
               ch_diar==1 & ((h13==1 | h13==2 | h13b==1) & h15e==1) ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_zinc_ors = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_zinc_ors = "Given zinc and ORS for diarrhea")
  
  # //ORS or increased liquids
  KRdata <- KRdata %>%
    mutate(ch_diar_ors_fluid =
             case_when(
               ch_diar==1 & (h13==1 | h13==2 | h13b==1 | h38==5) ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_ors_fluid = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_ors_fluid = "Given ORS or increased fluids for diarrhea")
  
  # //ORT or increased liquids
  KRdata <- KRdata %>%
    mutate(ch_diar_ort =
             case_when(
               ch_diar==1 & (h13==1 | h13==2 | h14==1 | h14==2 | h38==5) ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_ort = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_ort = "Given oral rehydration treatment or increased liquids for diarrhea")
  
  # //ORT and continued feeding
  KRdata <- KRdata %>%
    mutate(ch_diar_ort_feed =
             case_when(
               ch_diar==1 & ((h13==1 | h13==2 | h13b==1 | h14==1 | h14==2 | h38==5)&(h39>=3 & h39<=5)) ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_ort_feed = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_ort_feed = "Given ORT and continued feeding for diarrhea")
  
  # //Antibiotics
  KRdata <- KRdata %>%
    mutate(ch_diar_antib =
             case_when(
               ch_diar==1 & (h15==1 | h15b==1) ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_antib = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_antib = "Given antibiotic drugs for diarrhea")
  
  # //Antimotility drugs
  KRdata <- KRdata %>%
    mutate(ch_diar_antim =
             case_when(
               ch_diar==1 & h15a==1~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_antim = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_antim = "Given antimotility drugs for diarrhea")
  
  # //Intravenous solution
  KRdata <- KRdata %>%
    mutate(ch_diar_intra =
             case_when(
               ch_diar==1 & h15c==1~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_intra = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_intra = "Given Intravenous solution for diarrhea")
  
  # //Home remedy or other treatment
  KRdata <- KRdata %>%
    mutate(ch_diar_other =
             case_when(
               ch_diar==1 & h15d==1 | h15f==1 | h15g==1 | h15h==1 | h15i==1 | h15j==1 | h15k==1 | h15l==1 | h15m==1 | h20==1 ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_other = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_other = "Given home remedy or other treatment for diarrhea")
  
  # //No treatment
  KRdata <- KRdata %>%
    mutate(ch_diar_notrt =
             case_when(
               ch_diar==1 &  (ch_diar_ors==1 | ch_diar_rhf==1 | ch_diar_ors_rhf==1 | ch_diar_zinc==1 | ch_diar_zinc_ors==1 | ch_diar_ors_fluid==1 | 
                                ch_diar_ort==1 | ch_diar_ort_feed==1 | ch_diar_antib==1 | ch_diar_antim==1 | ch_diar_intra==1 | ch_diar_other==1) ~ 0 ,
               ch_diar==1 & h21a==1 ~ 1 )) %>%
    set_value_labels(ch_diar_notrt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_notrt = "No treatment for diarrhea")
  
  # Diarrhea treatment by source (among children with diarrhea symptoms)
  # Three population bases: 1. among children with diarrhea, 2. among children with diarrhea that sought treatment
  #                         3. among children with diarrhea that received ORS
  # This is country specific and needs to be checked to produce the specific source of interest. 
  # Some sources are coded below and the same logic can be used to code other sources. h12a-z indicates the source.
  # 
  # //Diarrhea treatment in government hospital
  KRdata <- KRdata %>%
    mutate(ch_diar_govh = 
             case_when(
               ch_diar==1 & h12a==1  & b5==1 ~ 1 ,
               ch_diar==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_govh = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_govh = "Diarrhea treatment sought from government hospital among children with diarrhea")
  
  KRdata <- KRdata %>%
    mutate(ch_diar_govh_trt = 
             case_when(
               ch_diar_care==1 & h12a==1 & b5==1 ~ 1 ,
               ch_diar_care==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_govh_trt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_govh_trt = "Diarrhea treatment sought from government hospital among children with diarrhea that sought treatment")
  
  KRdata <- KRdata %>%
    mutate(ch_diar_govh_ors = 
             case_when(
               ch_diar_ors==1 & h12a==1 & b5==1 ~ 1 ,
               ch_diar_ors==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_govh_ors = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_govh_ors = "Diarrhea treatment sought from government hospital among children with diarrhea that received ORS")
  
  # //Diarrhea treatment in government health center
  KRdata <- KRdata %>%
    mutate(ch_diar_govcent = 
             case_when(
               ch_diar==1 & h12b==1  & b5==1 ~ 1 ,
               ch_diar==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_govcent = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_govcent = "Diarrhea treatment sought from government health center among children with diarrhea")
  
  KRdata <- KRdata %>%
    mutate(ch_diar_govcent_trt = 
             case_when(
               ch_diar_care==1 & h12b==1 & b5==1 ~ 1 ,
               ch_diar_care==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_govcent_trt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_govcent_trt = "Diarrhea treatment sought from government health center among children with diarrhea that sought treatment")
  
  KRdata <- KRdata %>%
    mutate(ch_diar_govcent_ors = 
             case_when(
               ch_diar_ors==1 & h12b==1 & b5==1 ~ 1 ,
               ch_diar_ors==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_govcent_ors = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_govcent_ors = "Diarrhea treatment sought from government health center among children with diarrhea that received ORS")
  
  # //Diarrhea treatment from a private hospital/clinic
  KRdata <- KRdata %>%
    mutate(ch_diar_pclinc = 
             case_when(
               ch_diar==1 & h12j==1  & b5==1 ~ 1 ,
               ch_diar==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_pclinc = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_pclinc = "Diarrhea treatment sought from private hospital/clinic among children with diarrhea")
  
  KRdata <- KRdata %>%
    mutate(ch_diar_pclinc_trt = 
             case_when(
               ch_diar_care==1 & h12j==1 & b5==1 ~ 1 ,
               ch_diar_care==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_pclinc_trt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_pclinc_trt = "Diarrhea treatment sought from private hospital/clinic among children with diarrhea that sought treatment")
  
  KRdata <- KRdata %>%
    mutate(ch_diar_pclinc_ors = 
             case_when(
               ch_diar_ors==1 & h12j==1 & b5==1 ~ 1 ,
               ch_diar_ors==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_pclinc_ors = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_pclinc_ors = "Diarrhea treatment sought from private hospital/clinic among children with diarrhea that received ORS")
  
  # //Diarrhea treatment from a private doctor
  KRdata <- KRdata %>%
    mutate(ch_diar_pdoc = 
             case_when(
               ch_diar==1 & h12l==1  & b5==1 ~ 1 ,
               ch_diar==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_pdoc = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_pdoc = "Diarrhea treatment sought from private doctor among children with diarrhea")
  
  KRdata <- KRdata %>%
    mutate(ch_diar_pdoc_trt = 
             case_when(
               ch_diar_care==1 & h12l==1 & b5==1 ~ 1 ,
               ch_diar_care==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_pdoc_trt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_pdoc_trt = "Diarrhea treatment sought from private doctor among children with diarrhea that sought treatment")
  
  KRdata <- KRdata %>%
    mutate(ch_diar_pdoc_ors = 
             case_when(
               ch_diar_ors==1 & h12l==1 & b5==1 ~ 1 ,
               ch_diar_ors==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_pdoc_ors = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_pdoc_ors = "Diarrhea treatment sought from private doctor among children with diarrhea that received ORS")
  
  # //Diarrhea treatment from a pharmacy
  KRdata <- KRdata %>%
    mutate(ch_diar_pharm = 
             case_when(
               ch_diar==1 & h12k==1  & b5==1 ~ 1 ,
               ch_diar==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_pharm = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_pharm = "Diarrhea treatment sought from pharmacy among children with diarrhea")
  
  KRdata <- KRdata %>%
    mutate(ch_diar_pharm_trt = 
             case_when(
               ch_diar_care==1 & h12k==1 & b5==1 ~ 1 ,
               ch_diar_care==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_pharm_trt = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_pharm_trt = "Diarrhea treatment sought from pharmacy among children with diarrhea that sought treatment")
  
  KRdata <- KRdata %>%
    mutate(ch_diar_pharm_ors = 
             case_when(
               ch_diar_ors==1 & h12k==1 & b5==1 ~ 1 ,
               ch_diar_ors==1 & b5==1 ~ 0 )) %>%
    set_value_labels(ch_diar_pharm_ors = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_pharm_ors = "Diarrhea treatment sought from pharmacy among children with diarrhea that received ORS")
  
  
  # SYMPTOM COUNT ####
  KRdata$ill_count <- NA
  KRdata$ill_count <- KRdata$ch_diar + KRdata$ch_ari + KRdata$ch_fever
  
  # any illness in last two weeks
  KRdata$anyIllness <- NA
  KRdata$anyIllness <- ifelse(KRdata$ill_count > 0, 1, 0)
  
  # SYMTOM TREATED
  KRdata$care_count <- NA
  KRdata$care_count <- ifelse( KRdata$ch_ari %in% 1 | KRdata$ch_fever %in% 1 | KRdata$ch_diar %in% 1,
                          KRdata$ch_ari_care %in% 1 + KRdata$ch_fev_care %in% 1 + KRdata$ch_diar %in% 1,
                          KRdata$care_count
                          )
  
  # All symptoms treated
  KRdata$all_care <- NA
  KRdata$all_care <- ifelse(
    !is.na(KRdata$anyIllness) & KRdata$ill_count == KRdata$care_count,
    1,
    0)
  
  # Any care seeking
  KRdata$any_care <- NA 
  KRdata$any_care <- ifelse(KRdata$care_count > 1,
                            1, 0)
  
  # whether child with fever recieved any care for fever
  KRdata$anyCare_fever <- NA
  KRdata$anyCare_fever <- ifelse(KRdata$h32y == 0, 1, 0)
  
  # whether child was taken to a facility for fever
  KRdata$facCare_fever <- NA
  KRdata$facCare_fever <- ifelse(KRdata$h32z == 1, 1, 0)

  return(KRdata)
  
}


