

cleanSC_toFCLevel <- function(SCdata) {
  
  require(dplyr)
  
  SC_FClevel.df <- SCdata %>%
    group_by(facID) %>%
    summarise(SC_num = n(),
              bypassSC_pct = mean(bypassSC, na.rm = T),
              feeSC_pct = mean(feeSC, na.rm = T),
              feeSC_avg = mean(feeAmount_SC, na.rm = T),
              waitSC_avg = mean(waitSC, na.rm = T)
              )
  
  return(SC_FClevel.df)
  
}







