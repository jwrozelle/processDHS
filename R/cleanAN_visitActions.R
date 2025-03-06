#' Process Visit Action Data from AN Form
#'
#' This function processes visit action data in the Service Provision Assessment (SPA) 
#' **Antenatal Care (AN) Form**, recoding selected physical examination procedures into binary indicators. 
#' It also calculates the count and proportion of completed procedures.
#'
#' @param ANdata A `data.frame` containing the AN form data. Must include columns for 
#' visit identification (`c000`) and individual procedures (`c106a` to `c106y`).
#' 
#' @return A modified version of `ANdata` with added columns:
#' \describe{
#'   \item{pe_bp}{Indicator for blood pressure check (1 = performed, 0 = not performed)}
#'   \item{pe_palpPresent}{Indicator for palpating abdomen for fetal presentation}
#'   \item{pe_palpHeight}{Indicator for palpating abdomen for fetal height}
#'   \item{pe_fetalHB}{Indicator for listening to the abdomen for fetal heartbeat}
#'   \item{pe_weight}{Indicator for weighing the client}
#'   \item{pe_anemia}{Indicator for examining conjunctiva/palms for anemia}
#'   \item{pe_edema}{Indicator for examining legs/feet/hands for edema}
#'   \item{pe_breasts}{Indicator for examining the client's breasts}
#'   \item{pe_vagExam}{Indicator for conducting a vaginal or perineal exam}
#'   \item{pe_swollenGlands}{Indicator for examining for swollen glands}
#'   \item{pe_ultrasound}{Indicator for conducting or referring for an ultrasound}
#'   \item{pe_fundalHeight}{Indicator for measuring fundal height with a tape measure (set to NA for certain countries)}
#'   \item{pe_none}{Indicator for no physical exam components being performed}
#'   \item{pe_visitActions_count}{Total number of performed visit actions}
#'   \item{pe_visitActions_pct}{Proportion of visit actions performed based on available data}
#' }
#'
#' @details
#' The function checks the country code (`c000`) and applies country-specific modifications:
#' - If the dataset is from **Haiti (`HT7`) or Nepal (`NP7`)**, `pe_fundalHeight` is set to `NA`.
#' - If the dataset is from an unsupported country, a warning is issued, and default processing is applied.
#'
#' The function also computes:
#' - `pe_visitActions_count`: Number of performed procedures.
#' - `pe_visitActions_pct`: Proportion of recorded visit actions relative to the number of available variables.
#'
#' @note
#' The function assumes the dataset contains variables `c106a` to `c106y`, corresponding to various physical 
#' examination actions performed during an antenatal care visit.
#'
#' @examples
#' \dontrun{
#' ANdata <- data.frame(c000 = c("AF7", "HT7"), c106a = c(1, NA), c106b = c(1, 0), c106c = c(NA, 1))
#' cleanAN_visitActions(ANdata)
#' }
#'
#' @export
cleanAN_visitActions <- function(ANdata) {
  # Function implementation remains unchanged
}




cleanAN_visitActions <- function(ANdata) {
  
  # Process ####
  # Actions Performed ####
  # [1] "AF7"
  # [1] "HT7"
  # [1] "MW6"
  # [1] "NP7"
  # [1] "TZ7"
  
  if (ANdata$c000[1] %in% c("AF7", "HT7", "MW6", "NP7", "TZ7")) {
    # label variable c106a    "Procedures performed:Blood pressure"
    ANdata$pe_bp <- ifelse(ANdata$c106a %in% 1, 1, 0)
    # label variable c106b    "Procedures performed:Palpate abdomen for fetal presentation"
    ANdata$pe_palpPresent <- ifelse(ANdata$c106b %in% 1, 1, 0)
    # label variable c106c    "Procedures performed:Palpate abdomen for fetal height"
    ANdata$pe_palpHeight <- ifelse(ANdata$c106c %in% 1, 1, 0)
    # label variable c106d    "Procedures performed:Listen to abdomen for fetal heartbeat"
    ANdata$pe_fetalHB <- ifelse(ANdata$c106d %in% 1, 1, 0)
    # label variable c106k    "Procedures performed:Weigh client"
    ANdata$pe_weight <- ifelse(ANdata$c106k %in% 1, 1, 0)
    # label variable c106l    "Procedures performed:Examine conjunctiva/palms for anemia"
    ANdata$pe_anemia <- ifelse(ANdata$c106l %in% 1, 1, 0)
    # label variable c106m    "Procedures performed:Examine legs/feet/hands for edema"
    ANdata$pe_edema <- ifelse(ANdata$c106m %in% 1, 1, 0)
    # label variable c106n    "Procedures performed:Examine client's breasts"
    ANdata$pe_breasts <- ifelse(ANdata$c106n %in% 1, 1, 0)
    # label variable c106o    "Procedures performed:Conduct vaginal exam/exam of perineal area"
    ANdata$pe_vagExam <- ifelse(ANdata$c106o %in% 1, 1, 0)
    # label variable c106p    "Procedures performed:Examine for swollen glands"
    ANdata$pe_swollenGlands <- ifelse(ANdata$c106p %in% 1, 1, 0)
    # label variable c106q    "Procedures performed:Conduct/refer ultrasound or look at ultrasound report"
    ANdata$pe_ultrasound <- ifelse(ANdata$c106q %in% 1, 1, 0)
    # label variable c106r    "Procedures performed:Measure fundal height with a tape measure"
    ANdata$pe_fundalHeight <- ifelse(ANdata$c106r %in% 1, 1, 0) # 
    ANdata$pe_fundalHeight <- ifelse(ANdata$c000[1] %in% c("HT7", "NP7"), NA, ANdata$pe_fundalHeight) # NA in haiti
    # label variable c106y    "Procedures performed:None of physical exam components"
    ANdata$pe_none <- ifelse(ANdata$c106y %in% 1, 1, 0)
  } else {
    
    warning(paste0("The visitActions function has not been written for ", ANdata$c000[1]))
    
    # label variable c106a    "Procedures performed:Blood pressure"
    ANdata$pe_bp <- ifelse(ANdata$c106a %in% 1, 1, 0)
    # label variable c106b    "Procedures performed:Palpate abdomen for fetal presentation"
    ANdata$pe_palpPresent <- ifelse(ANdata$c106b %in% 1, 1, 0)
    # label variable c106c    "Procedures performed:Palpate abdomen for fetal height"
    ANdata$pe_palpHeight <- ifelse(ANdata$c106c %in% 1, 1, 0)
    # label variable c106d    "Procedures performed:Listen to abdomen for fetal heartbeat"
    ANdata$pe_fetalHB <- ifelse(ANdata$c106d %in% 1, 1, 0)
    # label variable c106k    "Procedures performed:Weigh client"
    ANdata$pe_weight <- ifelse(ANdata$c106k %in% 1, 1, 0)
    # label variable c106l    "Procedures performed:Examine conjunctiva/palms for anemia"
    ANdata$pe_anemia <- ifelse(ANdata$c106l %in% 1, 1, 0)
    # label variable c106m    "Procedures performed:Examine legs/feet/hands for edema"
    ANdata$pe_edema <- ifelse(ANdata$c106m %in% 1, 1, 0)
    # label variable c106n    "Procedures performed:Examine client's breasts"
    ANdata$pe_breasts <- ifelse(ANdata$c106n %in% 1, 1, 0)
    # label variable c106o    "Procedures performed:Conduct vaginal exam/exam of perineal area"
    ANdata$pe_vagExam <- ifelse(ANdata$c106o %in% 1, 1, 0)
    # label variable c106p    "Procedures performed:Examine for swollen glands"
    ANdata$pe_swollenGlands <- ifelse(ANdata$c106p %in% 1, 1, 0)
    # label variable c106q    "Procedures performed:Conduct/refer ultrasound or look at ultrasound report"
    ANdata$pe_ultrasound <- ifelse(ANdata$c106q %in% 1, 1, 0)
    # label variable c106r    "Procedures performed:Measure fundal height with a tape measure"
    ANdata$pe_fundalHeight <- ifelse(ANdata$c106r %in% 1, 1, 0)
    # label variable c106y    "Procedures performed:None of physical exam components"
    ANdata$pe_none <- ifelse(ANdata$c106y %in% 1, 1, 0)
    
  }

  # label variable cf106a   "Routine action in facility: Blood pressure"
  # label variable cf106k   "Routine action in facility: Weigh client"
  # label variable cf106xa  "Routine action in facility: Conduct group health education"
  # label variable cf106xb  "Routine action in facility: Urine test for protein"
  # label variable cf106xc  "Routine action in facility: Blood test for anemia"
  # label variable cf106xd  "Routine action in facility: Malaria RDT"
  # label variable cf106xe  "Routine action in facility: HIV testing and counseling"
  # label variable cf106xf  "NA - CS Routine action in facility: Syphilis RDT"
  # label variable cf106xg  "NA - CS Routine action in facility: 
  
  
  # From Bergh et al.
  # Took blood pressure*, palpated abdomen for foetal presentation and height*, 
  # listened for foetal heartbeat*, weighed patient*, examined conjunctiva/palms* 
  # for anaemia, examined legs/feet/hands for oedema*, examined patient’s #
  # breasts*, conducted vaginal exam*, and examined for swollen glands*.
  
  # variable names
  peVars <- c(
    "pe_bp", # 1
    "pe_palpPresent", # 2
    "pe_palpHeight", # 3
    "pe_fetalHB", # 4
    "pe_weight", # 5
    "pe_anemia", # 6
    "pe_edema", # 7
    "pe_breasts", # 8
    "pe_vagExam", # 9
    "pe_swollenGlands", # 10
    # "pe_ultrasound",
    # "pe_fundalHeight"
  )
  
  ANdata$pe_visitActions_count <- NA
  ANdata$pe_visitActions_count <- rowSums(ANdata[,peVars], na.rm = T)
  
  # Get the count of peVars in the dataframe where values are not all missing
  visitActions_denominator <- sum(peVars %in% colnames(ANdata) & 
                                    sapply(ANdata[peVars[peVars %in% colnames(ANdata)]], function(col) 
                                      !all(is.na(col) | col == 0)))
  
  ANdata$pe_visitActions_pct <- ANdata$pe_visitActions_count / visitActions_denominator
  
  return(ANdata)
  
  
  
  
}