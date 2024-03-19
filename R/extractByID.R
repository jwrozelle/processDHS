#' Extract Variable Value by ID and Index or Lag
#'
#' This function extracts the value of a specified variable based on either a direct index number or a lag from the current birth order. It is designed to work with data frames where each row corresponds to a child and rows are grouped by a parent identifier. This function is particularly useful for longitudinal or panel datasets where one needs to reference values from different rows based on a unique ID scheme.
#'
#' @param df A dataframe containing the data to be processed.
#' @param varName The name of the variable from which to extract values. If `NULL`, the function will return an error.
#' @param parentID The name of the column serving as the parent identifier. Default is "femaleID".
#' @param childID The name of the column serving as the child identifier. Must be unique for each row.
#' @param index The name of the column containing the index or order of children for each parent. Default is "bord".
#' @param sep The separator used in constructing reference IDs by concatenating `parentID` and `index`. Default is "_".
#' @param lag An optional integer indicating the lag from the current index from which to extract the value. If specified, `indexNumber` should be `NULL`.
#' @param indexNumber An optional integer specifying the exact index from which to extract the value. If specified, `lag` should be `NULL`.
#'
#' @return A vector containing the extracted values for the specified variable, corresponding to the given index or lag for each row in the input dataframe. If a match is not found for a constructed reference ID, the function will return `NA` for that entry.
#'
#' @details The function first checks for the proper specification of parameters, ensuring either `indexNumber` or `lag` is provided, but not both. It then validates the uniqueness of `childID` values. Subsequent operations include subsetting the data for processing efficiency, verifying ID format, constructing reference IDs based on the given criteria (`indexNumber` or `lag`), and merging the extracted values back into the dataset. The final output is sorted by the original row order to maintain dataset integrity.
#'
#'
#' @export

extractByID <- function(df, varName = NULL, parentID = "femaleID", childID = "childID", index = "bord", sep = "_", lag = NULL, indexNumber = NULL) {
  
  
  
  # stop if one of orderNumber or lag is not filled out
  if (is.null(lag) & is.null(indexNumber)) {
    stop(paste0("Either the indexNumber number or the lag from current birth order must be specified"))
  } else if (!is.null(lag) & !is.null(indexNumber)) {
    stop(paste0("Only specify one of lag or indexNumber, not both."))
  }
  
  # stop if childID is not uniquely valid
  if (nrow(df) != length(unique(df[[childID]]))) {
    stop(paste0("Error with "), childID, ": childID must be a unique identifier.")
  }
  
  # subset the data to speed processing
  df$rowNumber <- 1:nrow(df) # paranoid sorting insurance
  dataSubset <- df[, c(varName, parentID, childID, index, "rowNumber")]
  dataSubset <- dataSubset %>% arrange(rowNumber)
  
  
  
  if (sum(dataSubset[[childID]] == paste0(dataSubset[[parentID]], sep, dataSubset[[index]])) != nrow(dataSubset)) {
    stop("It appears your IDs are in the wrong format. IDs must be constructed with paste0(df$<parentID>, <sep>, df$<index>)")
  }
  
  # get the new ID
  if (!is.null(indexNumber)) {
    dataSubset$reference_childID <- paste0(dataSubset[[parentID]], sep, indexNumber)
  } else if (!is.null(lag)) {
    dataSubset$reference_childID <- paste0(dataSubset[[parentID]], sep, dataSubset[[index]] + lag)
  }
  
  # change ids that are not in df to missing
  dataSubset$reference_childID <- ifelse(dataSubset$reference_childID %in% dataSubset[[childID]], dataSubset$reference_childID, NA)
  dataSubset$temp_e9f820ceb7074f1d8ed0f666a38ab5c4 <- dataSubset[,varName]
  # make a copy for merging
  dataSubCopy <- dataSubset[,c(childID, "temp_e9f820ceb7074f1d8ed0f666a38ab5c4")]
  # remove this from the new variable
  dataSubset <- dataSubset[,!names(dataSubset) %in% "temp_e9f820ceb7074f1d8ed0f666a38ab5c4"]
  
  # merge with new data
  dataSubset <- merge(dataSubset, dataSubCopy, by.x = "reference_childID", by.y = childID, all.x = T, all.y)
  
  dataSubset <- dataSubset %>% arrange(rowNumber)
  
  output.vec <- dataSubset$temp_e9f820ceb7074f1d8ed0f666a38ab5c4
  
  return(output.vec)
  
}


