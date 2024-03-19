

cleanFP_toFCLevel <- function(FPdata) {
  
  require(dplyr)
  
  FP_FClevel.df <- FPdata %>%
    dplyr::group_by(facID) %>%
    dplyr::summarise(FP_num = n(),
              bypassFP_pct = mean(bypassFP, na.rm = T),
              feeFP_pct = mean(feeFP, na.rm = T),
              feeFP_avg = mean(feeAmount_FP, na.rm = T),
              waitFP_avg = mean(waitFP, na.rm = T)
              )
  
  return(FP_FClevel.df)
  
}























