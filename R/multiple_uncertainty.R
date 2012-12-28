#' SDP under multiple uncertainty
#'
#' Computes the SDP solution under the case of growth noise, 
#' implementation errors in harvesting, and meaurement errors in 
#' the stock assessment.  
#'
#' @param f the growth function of the escapement population (x-h)
#'   should be a function of f(t, y, p), with parameters p
#' @param p the parameters of the growth function
#' @param x_grid the discrete values allowed for the population size, x
#' @param h_grid the discrete values of harvest levels to optimize over
#' @param sigma is the shape parameters for noise distribution (sigma_g, sigma_m, sigma_i) (default is no noise)
#' @param pdfn is the probability density function (same functional form is used for growth, measure, implement).  (Default is uniform)
#' @param profit is the profit function (defaults to the realized harvest)
#' @return The D matrix giving the optimal harvest for each possible state in each timestep.  
#' Hence the optimal policy at time t is given by the policy function D[,t].  
#' Values of D[,t] correspond to the index of h_grid.  Indices of of D[,t] correspond to states in y_grid.  
#' @export
SDP_multiple_uncertainty <- function(f, p, x_grid, h_grid, Tmax = 25,
                                     sigmas =c(sigma_g=0.3, sigma_m=0, sigma_i=0), 
                                     pdfn = pdfn, profit = function(x,h) pmin(x, h)){
  
    sigma_g <- sigmas[1]      
    sigma_m <- sigmas[2]
    sigma_i <- sigmas[3]
    n_x <- length(x_grid)
    n_h <- length(h_grid)
    D <- matrix(NA, nrow=n_x, ncol=Tmax)  
            
    ## Compute the transition matrices 
    P <-  outer(x_grid, h_grid, profit) 
    M <- rownorm( outer(x_grid, x_grid, pdfn, sigma_m) )
    I <- rownorm( outer(h_grid, h_grid, pdfn, sigma_i) )  
    

    
    # Much faster to fill out f calls as a matrix ahead of time.  
    f_matrix <- outer(x_grid, h_grid, f, p)
    # Fill out uncertainty in transitions first
    G <- outer(x_grid, x_grid, pdfn, sigma_g)
    
    F <- lapply(1:n_h, function(q){  
      t(sapply(1:n_x, function(y){
        out <- numeric(n_x)
        mu <- M[y,] %*% f_matrix %*% I[q,] 
        # Handle special cases
        if(snap_to_grid(mu,x_grid)==0){ # 
          out[1] <- 1   
        } else {
        #out <- pdfn(x_grid, mu, sigma_g)  ## All computational time spent here.  could be done by look-up:
         mu_i <- which.min(abs(x_grid - mu)) ## snap mu to grid first
         out <- G[,mu_i]   ## then do this by table look-up
         ## Not identical but very close.  
        }
        out/sum(out)
      }))
    })
    
    
    # n_y by n_x, given y %*% n_x by n_h %*% n_h by n_q, given q
    Ep <- M %*% P %*% t(I)
    V <- Ep
    for(t in 1:Tmax){
      D[,(Tmax-t+1)] <- apply(V, 1, which.max) 
      ## q can often exceed y: if fishing is free, there might be more x than you think.  In such cases it is worth attempting to fish extra, and we shouldn't exert q < y.  
      #if(any(pmin(D[,(Tmax-t+1)], 1:n_h) !=  D[,(Tmax-t+1)])) stop()
      #v_t <- sapply(1:n_h, function(i) V[i,D[i,(Tmax-t+1)]]) # Look-up value given by which.max
      v_t <- apply(V, 1, max) # vector of current values 
      V <- sapply(1:n_h, function(j){ # updated value matrix
        Ep[,j] + (1-delta) * M %*% F[[j]] %*% v_t
      })
    }
    list(D=D, M=M, I=I, P=P, Ep=Ep, V=V, F=F)
}
  
# row-normalize the probability distribution
rownorm <- function(M) t(apply(M, 1, function(x) x/sum(x)))

# binning routine (vectorized)
snap_to_grid <- function(x, grid) sapply(x, function(x) grid[which.min(abs(grid - x))])   

 

# Uniform pdf where width scales with mean (probably better way to do that)
# And handles degenerate/delta fn cases such as no width and zero mean
FUN <- function(P, mu, s){
  if(mu == 0){
    as.integer(P == 0)
  } else if(s > 0){
    if(mu > 0){
      #dlnorm(P/mu, 0, s)
      dunif(P, mu * (1 - s), mu * (1 + s))
    }
  }
  else { # delta spike
    P <- snap_to_grid(P, x_grid)
    mu <- snap_to_grid(mu, x_grid)
    as.numeric(as.integer(P == mu))
  }
}


#' 
#' @export
pdfn <- Vectorize(FUN)

