#' logistic
#' 
#' logistic growth with harvesting
#' @export
logistic <- function(x, h, r = 1, K = 100){
  S <- max(x - h, 0)
  max( r * S * (1 - S / K) + S, 0)
}


#' Beverton-Holt
#' @export
bevertonholt <- function(x, h, r = 1, K = 100){
  S <- max(x - h, 0)
  max( (1 + r) * S / (1 + S / K), 0)
}


#' Ricker
#' @export
ricker <- function(x, h, r = 1, K = 100){
  S <- max(x - h, 0)
  S * exp(r * (1 - S / K))
}

#' Gompertz
#' @export
gompertz <- function(x, h, r = 1, K = 100){
  S <- max(x - h, 0)
  S * exp(r - S / K)
}
