

# cut out the duplicated column names

preMergeTrim <- function(x, y, by) {
  
  yTrim.df <- y[!names(y) %in% names(x)]
  
  yTrim.df[[by]] <- y[[by]]
  
  return(yTrim.df)
  
}






