#' Find Indices of the Smallest n Values
#'
#' This function returns the indices of the smallest `n` values in a numeric vector.
#' It sorts the indices of the vector by the values in ascending order and then selects
#' the first `n` indices. This is similar to `which.min()` but for multiple values.
#'
#' @param x A numeric vector. This vector is the one from which the smallest values
#'   are sought.
#' @param n An integer value specifying the number of smallest values to find. This
#'   number should not exceed the length of `x`.
#'
#' @return Returns a vector of indices corresponding to the smallest `n` values
#'   in the vector `x`.
#'
#' @examples
#' vec <- c(10, 20, 3, 5, 8)
#' which_kmin(vec, 3)
#'
#' @export
which_kmin <- function(x, n) {
  
  # x <- row
  # 
  # x <- c(10, 20, 3, 5, 8)
  # x <- c(NA, NA, 10, NA, NA, Inf)
  # names(x) <- c("a", "b", "c", "d", "e", "f")
  # n <- 6
  
  NoCalc.flag <- F
  
  # deal with situations where there are missing / nonsense numbers
  if (sum(x %in% c(NA, Inf, NULL, NaN)) == length(x)) {
    smallest_indices <- rep(NA, n)
    indices.names <- rep(NA, n)
    NoCalc.flag <- T
  } else if (sum(!x %in% c(NA, Inf, NULL, NaN)) >= n) { # set all the na, null and NaN to inf
    x.names <- names(x)
    x <- ifelse(x %in% c(NA, NULL, NaN), Inf, x)
    names(x) <- x.names
  } 
  
  if (!NoCalc.flag) {
    sorted_indices <- order(x)
    smallest_indices <- head(sorted_indices, n)
    
    
    if(length(smallest_indices) < n) {
      smallest_indices[(length(smallest_indices)+1):n] <- rep(NA, n - length(smallest_indices))
      if (!is.null(names(x))) {
        indices.names <- x[sorted_indices[1:n]] |> names()
        indices.names[(length(indices.names)+1):n] <- NA
      }
    } else if (!is.null(names(x))) {
      indices.names <- x[sorted_indices[1:n]] |> names()
      
    }
    
    
    
    # Truncate and fill with NAs if there aren't enough non missing values
    if (length(x) >= n & sum(!x %in% c(NA, Inf, NULL, NaN)) < n & !NoCalc.flag) {
      truncate.number <- sum(!x %in% c(NA, Inf, NULL, NaN))
      smallest_indices <- c(smallest_indices[1:truncate.number], rep(NA, n - truncate.number))
      indices.names <- c(indices.names[1:truncate.number], rep(NA, n - truncate.number))
    }
  }
  
  if (!is.null(names(x))) {
    names(smallest_indices) <- indices.names
  }
  
  
  return(smallest_indices)
}