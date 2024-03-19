#' Summarize Birth Records at the Individual Level
#'
#' This function processes birth records data to compute summaries of neonatal, infant, and under-5 mortality by female ID. It calculates the total counts of deaths in each category and indicates the occurrence of any deaths.
#'
#' @param BRdata A dataframe containing birth records data, including columns for neonatal death (`neonatal_death`), infant death (`infant_death`), and under-5 deaths (`U5_death`), along with a unique identifier for females (`femaleID`).
#'
#' @return A dataframe grouped by `femaleID` with summary statistics for neonatal, infant, and under-5 mortality. Each group includes the count of deaths and a binary indicator for the occurrence of any deaths in these categories. Additionally, a survey identifier (`v000`) is included to identify the dataset.
#'
#' @details The function groups the data by `femaleID` and then calculates the following for each group:
#' - `neo_death_count`: Total number of neonatal deaths.
#' - `neo_death_any`: Binary indicator if there is at least one neonatal death.
#' - `inf_death_count`: Total number of infant deaths.
#' - `inf_death_any`: Binary indicator if there is at least one infant death.
#' - `u5_death_count`: Total number of under-5 deaths.
#' - `u5_death_any`: Binary indicator if there is at least one under-5 death.
#' The function assumes that each record in `BRdata` corresponds to a unique birth event.
#'
#' @examples
#' # Assuming BRdata is your dataset containing birth records:
#' # BR_summary <- BRatIRlevel(BRdata)
#'
#' @import dplyr
#' @export

BRatIRlevel <- function(BRdata) {
  
  require(dplyr)
  
  if (!"neonatal_death" %in% BRdata) {
    stop("Error: This data must first be processed by `cleanBR()`")
  }
  
  BR_grouped <- BRdata %>% 
    dplyr::group_by(femaleID) %>%
    dplyr::summarise(
      neo_death_count = sum(neonatal_death, na.rm = T),
      neo_death_any = ifelse(sum(neonatal_death, na.rm = T) %in% 1:30, 1, 0),
      inf_death_count = sum(infant_death, na.rm = T),
      inf_death_any = ifelse(sum(infant_death, na.rm = T) %in% 1:30, 1, 0),
      u5_death_count = sum(U5_death, na.rm = T),
      u5_death_any = ifelse(sum(U5_death, na.rm = T) %in% 1:30, 1, 0)#,
      # v000 = v000
      )
  
  
  BR_grouped$v000 <- BRdata$v000[1]
  
  return(BR_grouped)
  
}













