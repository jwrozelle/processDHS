#' Summarize Sick Child Visit Data at Facility Level
#'
#' This function aggregates sick child (SC) visit data to the facility level, computing summary statistics 
#' including the number of SC visits, the proportion of bypass SC visits, the proportion of SC visits with a fee, 
#' the average fee amount, and the average wait time per facility.
#'
#' @param SCdata A data frame containing sick child visit records. Must include at least the columns \code{inv_id}, 
#' \code{bypassSC}, \code{feeSC}, \code{feeAmount_SC}, and \code{waitSC}. If \code{facID} is not present, 
#' it will be created from \code{inv_id}.
#'
#' @return A data frame summarizing the SC visit data at the facility level, with one row per unique facility ID (\code{facID}). 
#' The output includes:
#' \itemize{
#'   \item \code{facID}: Facility identifier.
#'   \item \code{SC_num}: Number of SC observations at the facility.
#'   \item \code{bypassSC_pct}: Mean proportion of SC bypass visits (ignoring \code{NA} values).
#'   \item \code{feeSC_pct}: Mean proportion of SC visits with a fee (ignoring \code{NA} values).
#'   \item \code{feeSC_avg}: Average SC fee amount (ignoring \code{NA} values).
#'   \item \code{waitSC_avg}: Average wait time for SC visits (ignoring \code{NA} values).
#' }
#'
#' @import dplyr
#' @export
#'
#' @examples
#' # Assuming SCdata is your input data frame:
#' result <- cleanSC_toFCLevel(SCdata)
cleanSC_toFCLevel <- function(SCdata) {
  
  require(dplyr)
  
  if (!"facID" %in% names(SCdata)) {
    SCdata$facID <- SCdata$inv_id
  }
  
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













