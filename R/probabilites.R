## Internal functions

iunifpdf <- function(x, y, sigma, assume = "Bayes"){
  a <- (1 - sigma)
  b <- (1 + sigma)
  ## Bayes Law inversion of uniform, a la Sethi
  if(assume == "Bayes")
    out <- (x * log(b / a)) ^ -1
  ## Inverse Uniform Distribution, a la Springborn
  else
   out <- y * (x ^ -2) / (b - a)
  
  out[x < y / b] <- 0
  out[x > y / a] <- 0
  out
}

iunifcdf <- function(x, y, sigma, assume = "Bayes"){
  a <-  (1 - sigma)
  b <-  (1 + sigma)
  ## Bayes Law inversion of uniform, a la Sethi:
  if(assume == "Bayes")
    out <- log(x * b / y) / log(b / a)
  ## Inverse uniform distribution CDF, a la Springborn
  else
    out <-  (b - y / x ) / (b - a)
  
  out[x < y / b] = 0
  out[x > y / a] = 1
  out
}

ilognpdf <- function(x, y, sigma){
    out <- (y / (x * sigma * sqrt(2 * pi))) * exp(-(log(x) - y)^2 / (2 * sigma^2))
    out[is.nan(out)] <- 0
    out
}

ilogncdf <- function(x, y, sigma){ # FIXME rewrite this to catch conditions resulting in NAs first (throws WARNINGS)
  suppressWarnings(N <- qlnorm(x, y, sigma))
  N[is.na(N)] <- 1
  N
}



snap_to_grid <- function(x, grid) sapply(x, function(x) grid[which.min(abs(grid - x))])   

pdfn <- function(p, mu, s, grid, pdf){
  if(mu <= 0){
    out <- as.integer(p == 0)
  } else if(s == 0){  ## delta spike if s = 0  
    out <- as.numeric(as.integer(snap_to_grid(p, grid) == snap_to_grid(mu, grid)))
  } else if(s > 0){ ## Evaluate pdf only for mu, s > 0
    out <- pdf(p, mu, s)
  } else { # all other cases. perhaps should be warning/error instead
    out <- 0
  }
  out
}

pdf_matrix <- function(i_grid, j_grid, sigma, pdf, cdf){
  n_i <- length(i_grid)
  n_j <- length(j_grid)
  out <- array(0, c(n_i, n_j))
  A <- array(0, c(n_i, n_j))
  for(i in 1:n_i){
    ## compute pdf on extended grid as loop
    A[i, ] <- pdfn(j_grid, i_grid[i], sigma, j_grid, pdf)
    ## handle any all-zero rows 
    if(sum(A[i,]) == 0 || i_grid[i] == 0){
      A[i, ] <- c(1, numeric(dim(A)[2]-1))
    } else if(sigma > 0){
      ## normalize row
      N <- cdf(j_grid[n_j], i_grid[i], sigma) 
      A[i, ] <- A[i, ] %*% t(N) / sum(A[i,])
      ## pile on boundary any density from extended grid
      A[i,n_j] <- 1 - N + A[i, n_j]
    }
  }
  out = A
}



