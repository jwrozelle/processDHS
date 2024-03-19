#' Summarize Sibling Data at Individual Respondent Level
#'
#' This function takes the cleaned sibling data produced by `mmExtractClean` and aggregates it at the 
#' individual respondent level. It generates summary statistics such as counts and indicators for 
#' maternal risk factors, sister mortality related to maternal events, and other derived variables.
#'
#' @param sibData A dataframe of cleaned and processed sibling data from `mmExtractClean`.
#' @import dplyr
#' @return A dataframe with individual-level summary statistics based on sibling information, 
#' including various risk and mortality indicators.
#' @examples
#' # Assuming 'sibData' is the cleaned sibling data
#' summarized_data <- sibatIRlvl(sibData)
#' @export


sibatIRlvl <- function(sibData) {
  
  IRsibData <- sibData %>% 
    group_by(femaleID) %>%
    summarise(
      # has a sister
      sister_count = sum(sister, na.rm=T),
      sisMatRisk_inclusive = ifelse(sum(sisMatRisk, na.rm = T)>0, 1, 0),
      sisMatRisk_known = ifelse(sum(sisMatRisk_known, na.rm = T)>0, 1, 0),
      # sister died
      sd_count=sum(sisDied, na.rm = T),
      sd_ever = ifelse(sum(sisDied, na.rm = T)>0, 1, 0),
      # While pregnant
      sd_whilePreg_count = sum(sisDied_whilePreg, na.rm = T),
      sd_whilePreg_ever = ifelse(sum(sisDied_whilePreg, na.rm = T)>0, 1, 0),
      sd_whilePreg_ever_2yrs = ifelse(sum(sisDied_whilePreg, na.rm = T)>0 & sum(mm6 <=2)>0, 1, 0),
      # during delivery
      sd_del_during_count = sum(sisDied_Del_during, na.rm = T),
      sd_del_during_ever = ifelse(sum(sisDied_Del_during, na.rm = T)>0, 1,0),
      sd_del_during_ever_2yrs = ifelse(sum(sisDied_Del_during, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0),
      # 6 weeks after delivery
      sd_del_6wks_count = sum(sisDied_Del_6wks, na.rm = T),
      sd_del_6wks_ever = ifelse(sum(sisDied_Del_6wks, na.rm = T)>0,1,0),
      sd_del_6wks_ever_2yrs = ifelse(sum(sisDied_Del_6wks, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0),
      # 2 months after delivery
      sd_del_2mnth_count = sum(sisDied_Del_2mnth, na.rm = T),
      sd_del_2mnth_ever = ifelse(sum(sisDied_Del_2mnth, na.rm = T)>0,1,0),
      sd_del_2mnth_ever_2yrs = ifelse(sum(sisDied_Del_2mnth, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0),
      # before or during delivery
      sd_del_b4Dur_count = sum(sisDied_Del_b4Dur, na.rm = T),
      sd_del_b4Dur_ever = ifelse(sum(sisDied_Del_b4Dur, na.rm = T)>0,1,0),
      sd_del_b4Dur_ever_2yrs = ifelse(sum(sisDied_Del_b4Dur, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0),
      # sibling died after delivery
      sd_del_in2mnths_count = sum(sisDied_Del_in2mnths, na.rm = T),
      sd_del_in2mnths_ever = ifelse(sum(sisDied_Del_in2mnths, na.rm = T)>0,1,0),
      sd_del_in2mnths_ever_2yrs = ifelse(sum(sisDied_Del_in2mnths, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0),
      # sibling died during pregnancy or within two months after
      sd_nearBirth_count = sum(sisDied_nearBirth, na.rm = T),
      sd_nearBirth_ever = ifelse(sum(sisDied_nearBirth, na.rm = T)>0,1,0),
      sd_nearBirth_ever_2yrs = ifelse(sum(sisDied_nearBirth, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0)
    )
  
  # add country level info
  IRsibData$v000 <- sibData$v000[1]
  
  return(IRsibData)
  
}