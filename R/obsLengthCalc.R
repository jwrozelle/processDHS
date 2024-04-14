#' Calculate Observation Length in Minutes
#'
#' This function calculates the difference in minutes between two sets of time points. 
#' Times are expected to be in "HHMM" format. The function checks for consistency in vector lengths,
#' and optionally corrects cases where the start time is later than the end time by swapping them.
#'
#' @param startVec A character vector representing the start times in "HHMM" format.
#' @param endVec A character vector representing the end times in "HHMM" format.
#' @param correct A logical value indicating whether to correct instances where the start time
#'        is later than the end time. If TRUE, times are swapped. If FALSE, the function
#'        proceeds without swapping and issues a warning.
#'
#' @return An integer vector of observation lengths in minutes.
#'
#'
#' @examples
#' startVec <- c("0900", "1430", "1200")
#' endVec <- c("0930", "1500", "1230")
#' obsLengthCalc(startVec, endVec)
#' obsLengthCalc(startVec, c("0915", "1445", "1300"), correct = FALSE)

obsLengthCalc <- function(startVec, endVec, correct = T) {
  
  # startVec <- SCdata$c517a
  # endVec <- SCdata$c517b
  
  if (length(startVec) != length(endVec)) {
    stop("startVec and endVec must be the same length")
  }
  
  if (sum(ifelse((endVec - startVec) < 0, T, F), na.rm = T) > 0) {
    if (correct) {
      warning("There are ", sum(ifelse((endVec - startVec) < 0, T, F), na.rm = T), " observations where the start time is after the end time. \nStart and end times have been swapped in these cases.")
      
      tmpStart <- ifelse((endVec - startVec) >= 0, startVec, endVec)
      endVec <- ifelse((endVec - startVec) >= 0, endVec, startVec)
      startVec <- tmpStart
      rm(tmpStart)
      
    } else {
      warning("There are ", sum(ifelse((endVec - startVec) < 0, T, F), na.rm = T), " observations where the start time is after the end time. \nNegative lengths produced.")
    }
  }
  
  
  
  # start time hour
  start_hr <- stringr::str_sub(startVec, 1, -3) |> as.numeric()
  # start time minutes
  start_min <- stringr::str_sub(startVec, -2, -1) |> as.numeric()
  # end time hour
  end_hr <- stringr::str_sub(endVec, 1, -3) |> as.numeric()
  # end time minutes
  end_min <- stringr::str_sub(endVec, -2, -1) |> as.numeric()
  
  
  length_hr <- end_hr - start_hr
  length_min <- end_min - start_min
  
  lengthMinutes <- length_hr*60 + length_min
  
  return(lengthMinutes)
  
}