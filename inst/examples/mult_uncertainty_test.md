Compare multiple uncertainties

  








```r
f <- function(x, h, p) {
    A <- p[1]
    B <- p[2]
    s <- pmax(x - h, 0)
    A * s/(1 + B * s)
}
profit = function(x, h) pmin(x, h)
```



```r
pars <- c(1.5, 0.05)
K <- (pars[1] - 1)/pars[2]
xmin <- 0
xmax <- 1.5 * K
n_x <- 500
n_h <- n_x
x_grid <- seq(xmin, xmax, length = n_x)
h_grid <- seq(xmin, xmax, length = n_h)
delta <- 0.05
xT <- 0
OptTime <- 25
sigma_g = 0.3
```





```r
g <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = c(sigma_g = sigma_g, 
    sigma_m = 0, sigma_i = 0), pdfn = pdfn)
```



```r
m <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = c(sigma_g = 0.03, 
    sigma_m = 0.3, sigma_i = 0), pdfn = pdfn)
```



```r
i <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = c(sigma_g = 0.03, 
    sigma_m = 0, sigma_i = 0.3), pdfn = pdfn)
```


Plot the policy function (in terms of escapement, `x-h`, rather than harvest `h`) at equilibrium (first time-step):


```r
require(reshape2)
policies <- melt(data.frame(stock = x_grid, g = x_grid[g$D[, 1]], 
    meas = x_grid[m$D[, 1]], imp = x_grid[i$D[, 1]]), id = "stock")
```



