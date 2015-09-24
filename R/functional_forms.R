#' logistic
#' 
#' logistic growth with harvesting
#' @export
#' 
logistic <- function(x, h, r = 1, K = 100){
  S <- max(x-h, 0)
  max( r * S * (1 - S / K) + S, 0)
}


# Beverton-Holt
#' @export
bevertonholt <- function(x, h, A = 1, B = 50){
  S <- max(x-h, 0)
  max( A * S / (B + S) + S, 0)
}


# Ricker
#' @export
logistic <- function(x, h, r = 1, K = 100){
  S <- max(x-h, 0)
  S * exp(r * (1 - S / K)) + S
}

# Gomperitz