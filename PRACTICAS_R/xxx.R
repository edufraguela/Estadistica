#' 
#' PRACTICA 1
#'

#' 1.9
#' Media
x <- 1:11
n <- length(x)
sum(x)/n
mean(x)
#'
media <- function(x){
  n <- length(x)
  m <- sum(x)/n
  return(m)
}
