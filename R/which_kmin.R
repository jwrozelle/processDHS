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
  
  sorted_indices <- order(x)
  smallest_indices <- head(sorted_indices, n)
  names(smallest_indices) <- x[sorted_indices[1:n]] |> names()
  
  return(smallest_indices)
}