

cleanBR_c1c2_interimMM <- function(BRdata, sibData, by = "femaleID", child_ID = "childID") {
  
  if(length(unique(BRdata[[child_ID]])) != nrow(BRdata)) {
    stop("Error in child_ID: ", child_ID, " must be a unique identifier.")
  }
  
  # Filter to only second birth
  BRdata_bord2.df <- dplyr::filter(BRdata, bord == 2)
  
  # subset to only necessary variables for faster data
  BRdata_bord2_subset.df <- BRdata_bord2.df[, c("childID", "femaleID", "c_dobR", "first_c_dobR")]
  
  # filter only sibling deaths
  sibData_mortality <- dplyr::filter(sibData, sisDied == 1)
  
  # merge (this will duplicate some children)
  BRMM_long.df <- merge(BRdata_bord2_subset.df, sibData_mortality, by = by, all.x = T, all.y = F)
  
  # calculate variables
  
  BRMM_long.df$interimB1B2_sd_b4Dur <- ifelse(
    BRMM_long.df$sisDied_Del_b4Dur == 1 &  # this will code 
      !is.na(BRMM_long.df$sisDied_Del_b4Dur) &
      BRMM_long.df$sib_dodR < BRMM_long.df$c_dobR & 
      BRMM_long.df$sib_dodR > BRMM_long.df$first_c_dobR,
    1, 0)
  
  # Group back to child level
  BRdata_level.df <- BRMM_long.df %>% 
    dplyr::group_by(.data[[child_ID]]) %>% 
    summarise(
      interimB1B2_sd_b4Dur_count = sum(interimB1B2_sd_b4Dur, na.rm = T),
      interimB1B2_sd_b4Dur_ever = ifelse(sum(interimB1B2_sd_b4Dur, na.rm = T)>0, 1, 0)
    )
  
  BRdata.df <- merge(BRdata, BRdata_level.df, by = child_ID, all.x = T, all.y = F)
  
  return(BRdata.df)
  
}

