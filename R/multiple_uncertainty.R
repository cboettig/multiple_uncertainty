#' multiple uncertainty
#' 
#' Stochastic dynamic programming solution under multiple uncertainty
#' @export
#' @importFrom MDPtoolbox mdp_policy_iteration
multiple_uncertainty <- function(f = logistic, 
                                 x_grid = seq(0,150,length=151), 
                                 h_grid = x_grid, 
                                 sigma_g = 0.2, 
                                 sigma_m = 0.0, 
                                 sigma_i = 0.0, 
                                 delta = 0.05, 
                                 noise_dist = c("uniform", "lognormal"), 
                                 y_grid = x_grid, 
                                 q_grid = x_grid,
                                 profit_fn = function(x,h) pmin(x, h),
                                 assume = "Bayes"){
  ## 'assume' determines the rule on how we compute the inverse
  
  ## allow f to be a function or a character string
  if(is.character(f))
    f <- get(f)
  
  ## Handle choice of noise distribution. Needs to define pdf, cdf, invpdf, invcdf
  if(noise_dist == "uniform"){
    pdf = function(p,mu,s) dunif(p, mu * (1 - s), mu * (1 + s)) 
    cdf = function(p,mu,s) punif(p, mu * (1 - s), mu * (1 + s))
    invpdf = function(p,mu,s) iunifpdf(p, mu, s, assume = assume) 
    invcdf = function(p,mu,s) iunifcdf(p, mu, s, assume = assume)
  } else if (noise_dist == "lognormal"){
    pdf = function(p,mu,s) dlnorm(p, log(mu), s)
    cdf = function(p,mu,s) plnorm(p, log(mu), s)
    invpdf = function(p,mu,s) ilognpdf(p, log(mu), s, assume = assume)
    invcdf = function(p,mu,s) ilogncdf(p, log(mu), s, assume = assume)
    ## Confirm cdf is integral pdf
  } else {
    stop("Noise distribution not recognized")
  }

  ## Store constants 
  n_x <- length(x_grid)
  n_h <- length(h_grid)
  n_y <- length(y_grid)
  n_q <- length(q_grid)

  ## Define a profit function.    
  
  P <- outer(x_grid, h_grid, profit_fn)
  Minv <- pdf_matrix(y_grid, x_grid, sigma_m, invpdf, invcdf) 
  M <- pdf_matrix(x_grid, y_grid, sigma_m, pdf, cdf) 
  I <- pdf_matrix(q_grid, h_grid, sigma_i, pdf, cdf)

  F <- array(0, c(n_x, n_x, n_h))
  for(h in 1:n_h){
    for(x in 1:n_x){
      mu <- f(x_grid[x], h_grid[h])
      F[x, , h] <- pdf_matrix(mu, x_grid, sigma_g, pdf, cdf)
    }
  }

  Phi <- array(0, c(n_y, n_y, n_h))
  for(k in 1:n_h){
    Phi[,,k] <- Minv %*% F[, , k] %*% M
  }

  T <- array(0, dim = c(n_y, n_y, n_q))
  for(i in 1:n_y){  
    T[i, , ] <- Phi[i, , ] %*% t(I)
  }

  Ep <- Minv %*% P %*% t(I) ## expected profit (from space x,h -> y,q)
  
  ## Much faster than writing recursion by hand
  out <- MDPtoolbox::mdp_policy_iteration(P = T, R = Ep, discount = 1/(1+delta) )
  S <- y_grid - q_grid[out$policy]
  
}

