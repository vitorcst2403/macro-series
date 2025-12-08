# aglutina valores de um vetor
aglut <- function(x) {
  x <- as.numeric(x)  
  non_na <- x[!is.na(x)]
  if (length(non_na) > 0) {
    return(non_na[1])
  } else {
    return(NA)
  }
}
