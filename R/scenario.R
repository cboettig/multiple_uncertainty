#' @export
scenario <- function(S,
                     x0 = 100,
                     f = logistic,
                     x_grid,
                     Tmax = 50,
                     sigma_g = 0.2,
                     sigma_m = 0.0,
                     sigma_i = 0.0,
                     noise_dist = "uniform",
                     y_grid = x_grid) {
  
  
  if(is.character(f))
    f <- get(f)
  
  if(noise_dist == "uniform")
    shock <- function(x, sigma) runif(1, x * (1 - sigma), x * 1 + sigma)
  else if(noise_dist == "lognormal")
    shock <- function(x, sigma) rlnorm(1, log(x), sigma)
  else 
    stop(paste("noise_dist", noise_dist, "not recognized"))

  
  measure <- function(x) shock(x, sigma_m)
  implement <- function(q) shock(q, sigma_i)
  set_quota <- function(y){
    i <- max(which(y_grid <= y))
    ## quota is observed minus target escapement
    y - S[i]
  } 
  
  ## Initialize
  x <- numeric(Tmax+1)
  y <- numeric(Tmax)
  h <- numeric(Tmax)
  q <- numeric(Tmax)
  x[1] <- x0
  
  ## Simulate
  for(t in 1:Tmax){
    y[t] <- measure(x[t])
    q[t] <- set_quota(y[t])
    h[t] <- min(implement(q[t]), x[t]) ## True harvest cannot exceed x[t]
    x[t + 1] <- shock(f(x[t], h[t]), sigma_g)
    
  }
  
  data.frame(t = 1:Tmax, x = x[1:Tmax], y = y, q = q, h = h)
}