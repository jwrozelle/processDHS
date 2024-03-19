

cleanAN_toFCLevel <- function(ANdata) {
  
  require(dplyr)
  
  AN_FClevel.df <- ANdata %>%
    group_by(facID) %>%
    summarise(AN_num = n(),
              bypassAN_pct = mean(bypassAN, na.rm = T),
              feeAN_pct = mean(feeAN, na.rm = T),
              feeAN_avg = mean(feeAmount_AN, na.rm = T),
              waitAN_avg = mean(waitAN, na.rm = T)
    )
  
  return(AN_FClevel.df)
  
}







