#' Extract Values by Dynamically Constructed Column Names
#'
#' Dynamically constructs column names from a base name and an index, then extracts values from these columns for each row in a dataframe. It allows for optional zero padding on the indices and lag adjustment.
#'
#' @param df A dataframe from which to extract values.
#' @param idx A string specifying the name of the column in `df` that contains the indices used to construct the column names for extraction.
#' @param varName The base name of the variables/columns from which to extract values.
#' @param varNameIdx_sep Separator to be used between `varName` and the index. Defaults to "_".
#' @param lag An integer value to add to the index values found in `idx`. Useful for time-shifted or birth-shifted data extraction. Defaults to 0.
#' @param zeroPad_numDigits An optional integer specifying the number of digits for zero padding the index values. If not NULL, indices are zero-padded to this length.
#' @param output.class The desired class of the output vector. If specified, can be "numeric" or "integer" to convert the output values to the specified class. If NULL, the class is not altered.
#'
#' @return A vector containing the extracted values from `df`, according to the dynamically constructed column names. If `output.class` is specified, the vector is converted to the specified class.
#'
#' @details The function constructs column names by combining `varName`, `varNameIdx_sep`, and the zero-padded (if applicable) indices from the `idx` column, optionally adjusted by `lag`. It then extracts values from these columns for each row in `df`. This is particularly useful for extracting time-series or panel data where the variable of interest changes over time or conditions.
#'
#' @examples
#' # Assuming df is your dataframe, idx is the column "timeIndex",
#' # and you want to extract values from columns named "var_01", "var_02", ..., "var_12"
#' # with zero padding of 2 digits and a lag of 1:
#' # extracted_values <- extractByRC(df, "timeIndex", "var", zeroPad_numDigits = 2, lag = 1, output.class = "numeric")
#'
#' @import stringr
#' @export

extractByRC <- function(df, idx, varName, varNameIdx_sep = "_", lag = 0, zeroPad_numDigits = NULL, output.class = NULL) {
  
  require(stringr)
  
  output.vec <- rep(NA, nrow(df))
  
  if (!"integer" %in% class(df[[idx]])) {
    stop(paste0(idx, " is not an vector of class integer. This function requires an integer format for the idx argument."))
  }
  
  
  # get vector of variable/column names
  if (!is.null(zeroPad_numDigits)) {
    
    # stop if the zero pad length is shorter than the maximum number of characters in idx.
    if (max(stringr::str_length(as.character(df[[idx]])), na.rm = T) > zeroPad_numDigits) {
      stop("Variable ", idx, " has a maximum length of ", max(stringr::str_length(as.character(df[[idx]]))), ". zeroPad_numDigits must be at least as long as the longest ", idx, " value.")
    }
    
    # pad the zeros
    zeroPadding.vec <- sapply(df[[idx]], function(idx) {
      zeroPaddingCount <- zeroPad_numDigits - stringr::str_length(idx)
      zeroPad <- ifelse(!is.na(idx), paste0(rep("0", zeroPaddingCount), collapse = ""), NA)
      
      return(zeroPad)
    })
    
    idxNumChar.vec <- ifelse(!is.na(df[[idx]]), paste0(zeroPadding.vec, df[[idx]] + lag), NA)
  } else {
    idxNumChar.vec <- as.character(df[[idx]] + lag)
  }
  

  
  varNames_idx <- ifelse(!is.na(idxNumChar.vec), paste0(varName, varNameIdx_sep, idxNumChar.vec), NA)
  
  # check for unique, non-missing names that are not in df
  if (sum(!unique(varNames_idx[!is.na(varNames_idx)]) %in% names(df)) > 0) {
    warning("Some variable names were created that do not exist in dataframe")
  }
  
  # Convert column names to column indices
  col_indices <- match(varNames_idx, names(df))
  # Create a matrix of indices
  index_matrix <- cbind(1:nrow(df), col_indices)
  # Extract values using matrix indexing
  output.vec <- df[index_matrix]
  
  
  # convert output if requested
  if(!is.null(output.class)) {
    if (output.class == "numeric") {
      output.vec <- as.numeric(output.vec)
    } else if (output.class == "integer") {
      output.vec <- as.integer(output.vec)
    }
  } 
  
  return(output.vec)
  
}




