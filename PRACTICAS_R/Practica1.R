#' 
#' PRACTICA 1
#'

#' 1.9
#' 
#' 
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
#'
#'
#'
#' Varianza
x <- 1:11
n <- length(x)
sum((x-mean(x))^2)/n
mean((x-mean(x))^2)

varianza <- function(x){
  n <- length(x)
  v <- sum((x-mean(x))^2)/n
  return(v)
}
varianza(x)
#'
#'
#'
#'Cuasi-varianza
cuasi_var <- function(x){
  n <- length(x)
  v <- (sum(x^2)-n*mean(x)^2)/(n-1)
    return(v)
}
x <- 1:11
var(x); cuasi_var(x)
y <- x + 1.e10
var(y); cuasi_var(y)
#'
#'
#'
#' Media truncada
media_truncada <- function(x, p=0.2) {
  n <- length(x)
  k <- round(p*n)
  if ((k<0)|(k>=(n/2))) stop('Valor de p incorrecto')
  x <- sort(x)
  y <- x[(k+1):(n-k)]
  return(mean(y))
}
x <- (1:11)^2
media_truncada(x)

media_truncada(x,0.1)

media_truncada(x,10) ('Da error porque el 10 no vale como párametro')

media_truncada(x,0)
mean(x)




