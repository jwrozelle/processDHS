#' Clean and Score Facility Capability Data
#'
#' This function processes facility capability data by computing scores based
#' on several health facility aspects including basic amenities, basic equipment,
#' infection prevention, essential medicines, and diagnostic capacity.
#' It also applies multiple correspondence analysis (MCA) to create a standardized
#' index score.
#'
#' @param FCdata A data frame containing facility capability data with columns
#'               corresponding to various health facility aspects. It must be
#'               compatible with the `sf` package structure for geometric operations.
#'
#' @return A modified version of `FCdata` which includes several new columns:
#'         - `sriJDK_basicamenities`: the score for basic amenities,
#'         - `sriJDK_basicequip`: the score for basic equipment,
#'         - `sriJDK_infprev`: the score for infection prevention,
#'         - `sriJDK_med`: the score for essential medicines,
#'         - `sriJDK_diagcapacity`: the score for diagnostic capacity,
#'         - `sriJDK_score`: an aggregated score from the above categories,
#'         - `sriJDK_index`: a standardized index derived from MCA,
#'         - `sriJDK_index_pct`: percentile ranking of the `sriJDK_index`.
#'
#' @details The function requires the `sf`, `ExPosition`, and `Hmisc` packages.
#'          It assumes that the necessary variable vectors are stored in the `varVecs`
#'          dataset from the `processDHS` package. If `varVecs` does not exist in
#'          the global environment, it attempts to load it. Scores for each category
#'          are calculated as the mean of selected variables after omitting NA values.
#'          The overall `sriJDK_score` is computed as the average of these category
#'          scores. The function finally applies MCA on selected variables, derives
#'          an index (`sriJDK_index`), and adjusts it if necessary to ensure that
#'          the correlation with `sriJDK_score` is positive. The index is then
#'          converted into a percentile ranking.
#'
#' @examples
#' # Assuming `FCdata` is your dataset and `varVecs` is loaded:
#' cleaned_data <- cleanFC_jdkSRI(FCdata)
#'
#' @export


cleanFC_jdkSRI <- function(FCdata) {
  
  require(sf)
  require(Hmisc)
  require(ExPosition)
  
  # Load variable vectors
  
  if (!exists("varVecs")) {
    data("varVecs", package = "processDHS")
  }
  
  # sriJDK_diagcapacity_Vars <- varVecs$sri_diagcapacity_Vars[!varVecs$sri_diagcapacity_Vars %in% c("diag_urineprot", "diag_uringluc")]
  # sriJDK_infprev_Vars <- varVecs$sri_infprev_Vars[!varVecs$sri_infprev_Vars %in% c("syringe_prop_st")]
  # sriJDK_basicamenities_Vars <- varVecs$sri_basicamenities_Vars[!varVecs$sri_basicamenities_Vars %in% c("privvisaud_all_st")]
  # sriJDK_med_Vars <- varVecs$sri_med_Vars
  # sriJDK_basicequip_vars <- varVecs$sri_basicequip_Vars
  
  # Basic amenities
  FCdata$sriJDK_basicamenities <- NA
  FCdata$sriJDK_basicamenities <- rowSums(sf::st_drop_geometry(FCdata[varVecs$sriJDK_basicamenities_Vars]), na.rm = TRUE) / length(varVecs$sriJDK_basicamenities_Vars)
  # Basic equipment
  FCdata$sriJDK_basicequip <- NA
  FCdata$sriJDK_basicequip <- rowSums(sf::st_drop_geometry(FCdata[varVecs$sriJDK_basicequip_vars]), na.rm = TRUE) / length(varVecs$sriJDK_basicequip_vars)  
  # Infection prevention
  FCdata$sriJDK_infprev <- NA
  FCdata$sriJDK_infprev <- rowSums(sf::st_drop_geometry(FCdata[varVecs$sriJDK_infprev_Vars]), na.rm = TRUE) / length(varVecs$sriJDK_infprev_Vars)  
  # Essential medicines
  FCdata$sriJDK_med <- NA
  FCdata$sriJDK_med <- rowSums(sf::st_drop_geometry(FCdata[varVecs$sriJDK_med_Vars]), na.rm = TRUE) / length(varVecs$sriJDK_med_Vars)  
  # Diagnostic Capacity
  FCdata$sriJDK_diagcapacity <- NA
  FCdata$sriJDK_diagcapacity <- rowSums(sf::st_drop_geometry(FCdata[varVecs$sriJDK_diagcapacity_Vars]), na.rm = TRUE) / length(varVecs$sriJDK_diagcapacity_Vars)    
  
  
  # score
  
  sriJDK_Vars <- c(
    varVecs$sriJDK_basicamenities_Vars,
    varVecs$sriJDK_basicequip_vars,
    varVecs$sriJDK_diagcapacity_Vars,
    varVecs$sriJDK_infprev_Vars,
    varVecs$sriJDK_med_Vars
  )
  
  FCdata$sriJDK_score <-rowSums(FCdata[c("sriJDK_basicamenities",
                                      "sriJDK_basicequip",
                                      "sriJDK_infprev",
                                      "sriJDK_diagcapacity",
                                      "sriJDK_med")], na.rm = TRUE) / 5
  
  
  
  # Get the sri
  mca_result <- ExPosition::epMCA(FCdata[sriJDK_Vars], graphs = F)
  # create sri index
  FCdata$sriJDK_index <- mca_result$ExPosition.Data$fi[,1]
  
  # make sure it runs in an interpretable direction
  if (cor(FCdata$sriJDK_index, FCdata$sriJDK_score) < 0) {
    FCdata$sriJDK_index <- -FCdata$sriJDK_index
  }
  
  FCdata$sriJDK_index_pct <- Hmisc::cut2(FCdata$sriJDK_index, g = 5) |> as.numeric() - 1

    
  return(FCdata)
}


