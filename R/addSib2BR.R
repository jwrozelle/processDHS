#' Merge Sibling Summary Data with Birth Records
#'
#' This function merges individual-level summary data from `sibatIRlvl` with birth records data. 
#' It calculates additional variables based on the timing of siblings' deaths relative to birth events 
#' and aggregates these at the child level. The function aims to enrich birth records with maternal 
#' and sibling mortality risk factors.
#'
#' @param BRdata A dataframe containing birth records.
#' @param sibData A dataframe of cleaned and processed sibling data from `mmExtractClean`.
#' @param by The matching key column name, defaults to "femaleID".
#' @import dplyr
#' @return A dataframe of birth records enriched with sibling mortality and risk factor information.
#' @examples
#' # Assuming 'BRdata' is your birth records dataset and 'sibData' is cleaned sibling data
#' enriched_BRdata <- addSib2BR(BRdata, sibData)
#' @export


addSib2BR <- function(BRdata, sibData, by = "femaleID") {
  
  # the long duplicate merge
  mergedLong.df <- merge(BRdata, sibData, by = by, all.x = T, all.y = F)
  
  # sibling died at all
  mergedLong.df$prev_sisDied <- NA
  mergedLong.df$prev_sisDied <- ifelse(mergedLong.df$sisDied == 1 &  # this will code 
                                         !is.na(mergedLong.df$sib_dodR) &
                                         mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                       1, 0)
  
  # sibling died, unrelated to pregnancy
  mergedLong.df$prev_sisDied_nonMat <- NA
  mergedLong.df$prev_sisDied_nonMat <- ifelse(mergedLong.df$sisDied == 1 &  # this will code 
                                                mergedLong.df$sisDied_Del_2mnth != 1 &
                                                mergedLong.df$sisDied_Del_6wks != 1 & 
                                                mergedLong.df$sisDied_Del_b4Dur != 1 &
                                                !is.na(mergedLong.df$sib_dodR) &
                                                mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                              1, 0)
  
  # sibling died while pregnant
  mergedLong.df$prev_sisDied_whilePreg <- NA
  mergedLong.df$prev_sisDied_whilePreg <- ifelse(mergedLong.df$sisDied_whilePreg == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  # sibling died while pregnant
  mergedLong.df$prev_sisDied_whilePreg <- NA
  mergedLong.df$prev_sisDied_whilePreg <- ifelse(mergedLong.df$sisDied_whilePreg == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  # sibling died during delivery
  mergedLong.df$prev_sisDied_Del_during <- NA
  mergedLong.df$prev_sisDied_Del_during <- ifelse(mergedLong.df$sisDied_Del_during == 1 &  # this will code 
                                                    !is.na(mergedLong.df$sib_dodR) &
                                                    mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                  1, 0)
  
  # sibling died "since delivery"
  mergedLong.df$prev_sisDied_Del_since <- NA
  mergedLong.df$prev_sisDied_Del_since <- ifelse(mergedLong.df$sisDied_Del_since == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  # sibling died 6 weeks after delivery
  mergedLong.df$prev_sisDied_Del_6wks <- NA
  mergedLong.df$prev_sisDied_Del_6wks <- ifelse(mergedLong.df$sisDied_Del_6wks == 1 &  # this will code 
                                                  !is.na(mergedLong.df$sib_dodR) &
                                                  mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                1, 0)
  
  # sibling died 2 months after delivery
  mergedLong.df$prev_sisDied_Del_2mnth <- NA
  mergedLong.df$prev_sisDied_Del_2mnth <- ifelse(mergedLong.df$prev_sisDied_Del_2mnth == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  # before or during delivery
  mergedLong.df$prev_sisDied_Del_b4Dur <- NA
  mergedLong.df$prev_sisDied_Del_b4Dur <- ifelse(mergedLong.df$sisDied_Del_b4Dur == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  # sibling died after delivery
  mergedLong.df$prev_sisDied_Del_in2mnths <- NA
  mergedLong.df$prev_sisDied_Del_in2mnths <- ifelse(mergedLong.df$sisDied_Del_in2mnths == 1 &  # this will code 
                                                      !is.na(mergedLong.df$sib_dodR) &
                                                      mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                    1, 0)
  # sibling died during pregnancy or within two months after
  mergedLong.df$prev_sisDied_nearBirth <- NA
  mergedLong.df$prev_sisDied_nearBirth <- ifelse(mergedLong.df$sisDied_nearBirth == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  
  # group back to child level
  BRsibData.df <- mergedLong.df %>%
    group_by(childID) %>%
    summarise(
      
      motherHasSister = ifelse(sum(!is.na(mm1), na.rm = T) > 0, 1, 0),
      
      # While pregnant
      prev_sd_count = sum(prev_sisDied, na.rm = T),
      prev_sd_ever = ifelse(sum(prev_sisDied, na.rm = T)>0, 1, 0),
      
      # nonMaternal death
      prev_sd_nonMat_count = sum(prev_sisDied_nonMat, na.rm = T),
      prev_sd_nonMat_ever = ifelse(sum(prev_sisDied_nonMat, na.rm = T)>0,1,0),
      
      # While pregnant
      prev_sd_whilePreg_count = sum(prev_sisDied_whilePreg, na.rm = T),
      prev_sd_whilePreg_ever = ifelse(sum(prev_sisDied_whilePreg, na.rm = T)>0, 1, 0),
      # during delivery
      prev_sd_del_during_count = sum(prev_sisDied_Del_during, na.rm = T),
      prev_sd_del_during_ever = ifelse(sum(prev_sisDied_Del_during, na.rm = T)>0, 1,0),
      # 6 weeks after delivery
      prev_sd_del_6wks_count = sum(prev_sisDied_Del_6wks, na.rm = T),
      prev_sd_del_6wks_ever = ifelse(sum(prev_sisDied_Del_6wks, na.rm = T)>0,1,0),
      # 2 months after delivery
      prev_sd_del_2mnth_count = sum(prev_sisDied_Del_2mnth, na.rm = T),
      prev_sd_del_2mnth_ever = ifelse(sum(prev_sisDied_Del_2mnth, na.rm = T)>0,1,0),
      # before or during delivery
      prev_sd_del_b4Dur_count = sum(prev_sisDied_Del_b4Dur, na.rm = T),
      prev_sd_del_b4Dur_ever = ifelse(sum(prev_sisDied_Del_b4Dur, na.rm = T)>0,1,0),
      # sibling died after delivery
      prev_sd_del_in2mnths_count = sum(prev_sisDied_Del_in2mnths, na.rm = T),
      prev_sd_del_in2mnths_ever = ifelse(sum(prev_sisDied_Del_in2mnths, na.rm = T)>0,1,0),
      # sibling died during pregnancy or within two months after
      prev_sd_nearBirth_count = sum(prev_sisDied_nearBirth, na.rm = T),
      prev_sd_nearBirth_ever = ifelse(sum(prev_sisDied_nearBirth, na.rm = T)>0,1,0)
    )
  
  
  BRsibData.df <- merge(BRdata, BRsibData.df, by = "childID", all.x = T, all.y = F)
  
  
  
  return(BRsibData.df)
  
}