






Define our functions 



```r
f <- function(x, h, p) {
    sapply(x, function(x) {
        S = max(x - h, 0)
        p[1] * S * (1 - S/p[2]) + S
    })
}
pars = c(0.2, 100)
K <- pars[2]
```







```r
xmin <- 0
xmax <- 1.5 * K
n_x <- 300
n_h <- n_x
x_grid <- seq(xmin, xmax, length = n_x)
h_grid <- seq(xmin, xmax, length = n_h)
delta <- 0.05
xT <- 0
OptTime <- 15
profit <- function(x, h) pmin(x, h)
sigma <- 0.5
```




# Scenarios: 

We use Monte Carlo integration over the noise processes to determine the transition matrix.  




```r
compute_policy <- function(sigmas, stationary = TRUE) {
    z_g <- function() 1 + (2 * runif(1, 0, 1) - 1) * sigmas[1]
    z_m <- function() 1 + (2 * runif(1, 0, 1) - 1) * sigmas[2]
    z_i <- function() 1 + (2 * runif(1, 0, 1) - 1) * sigmas[3]
    
    out <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = sigmas, 
        pdfn = pdfn)
    
    if (stationary) 
        out$D <- sapply(1:OptTime, function(i) out$D[, 1])
    
    list(D = out$D, V = out$V)
}
```



```r
require(snowfall)
sfInit(parallel = TRUE, cpu = 4)
sfLibrary(multipleuncertainty)
sfExportAll()
```


Determine the policies for each of the scenarios (noise combinations).


```r
set <- list(det = c(0, 0, 0), g = c(sigma, 0, 0), m = c(0, sigma, 0), all = c(sigma, 
    sigma, sigma))
scenarios <- lapply(set, compute_policy)
policies <- sapply(scenarios, function(out) out$D[, 1])
values <- sapply(scenarios, function(out) out$V)
```



### plots



```r
policy <- melt(data.frame(cbind(stock = x_grid, policies)), id = "stock")
```



```r
ggplot(policy) + geom_point(aes(stock, stock - x_grid[value], color = variable), 
    shape = "+") + stat_smooth(aes(stock, stock - x_grid[value], color = variable), 
    degree = 1, se = FALSE, span = 0.3) + ylab("escapement")
```

![plot of chunk sethiplots-escapement](http://farm9.staticflickr.com/8109/8555192399_3ba80d6fb8_o.png) 



```r
ggplot(policy) + geom_point(aes(stock, x_grid[value], color = variable), shape = "+") + 
    stat_smooth(aes(stock, x_grid[value], color = variable), degree = 1, se = FALSE, 
        span = 0.3) + ylab("harvest")
```

![plot of chunk sethiplots-harvest](http://farm9.staticflickr.com/8225/8555192575_03df7c08b6_o.png) 




```r
# value <- melt(data.frame(cbind(stock = x_grid,values)), id = 'stock')
# ggplot(value) + geom_point(aes(stock, value, color=variable), shape='+')
# + stat_smooth(aes(stock, value, color=variable), degree=0, se=FALSE,
# span=0.15) + ylab('Net Present Value')
```




## Simulations


```r
simulatereps <- function(opt, sigmas) {
    
    z_g <- function() 1 + (2 * runif(1, 0, 1) - 1) * sigmas[1]
    z_m <- function() 1 + (2 * runif(1, 0, 1) - 1) * sigmas[2]
    z_i <- function() 1 + (2 * runif(1, 0, 1) - 1) * sigmas[3]
    
    sims <- lapply(1:100, function(i) {
        ForwardSimulate(f, pars, x_grid, h_grid, x0 = K, opt$D, z_g, z_m, z_i, 
            profit)
    })
    
    sims
}
```



All cases


```r
allcases <- lapply(scenarios, function(policyfn_i) {
    lapply(set, function(sigmas) {
        simulatereps(policyfn_i, sigmas)
    })
})
```



```r
sims <- unlist(allcases, recursive = FALSE)
dat <- melt(sims, id = names(sims[[1]][[1]]))
dt <- data.table(dat)
setnames(dt, c("L2", "L1"), c("reps", "uncertainty"))  # names are nice
```



### Plots 



```r
ggplot(subset(dt, reps == 1)) + geom_line(aes(time, fishstock)) + geom_line(aes(time, 
    harvest), col = "darkgreen") + facet_wrap(~uncertainty)
```

![plot of chunk onerep](http://farm9.staticflickr.com/8381/8556302574_deed16d2c9_o.png) 


Summary statistics 


```r
profits <- dt[, sum(profit), by = c("reps", "uncertainty")]
ggplot(profits) + geom_histogram(aes(V1)) + facet_wrap(~uncertainty)
```

![the distribution of profits by scenario](http://farm9.staticflickr.com/8226/8555193335_afd82bd9b0_o.png) 




```r
means <- profits[, mean(V1), by = uncertainty]
sds <- profits[, sd(V1), by = uncertainty]
```


## Tables of results

Note that columns represent the decision-maker's beliefs about uncertainty and rows represent the true uncertainty present in the simulation.  


```r
require(xtable)
uncertainties <- names(set)
print(xtable(matrix(means$V1, nrow = length(set), dimnames = list(uncertainties, 
    uncertainties))), type = "html")
```

<!-- html table generated in R 2.15.3 by xtable 1.7-0 package -->
<!-- Wed Mar 13 17:26:48 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> det </TH> <TH> g </TH> <TH> m </TH> <TH> all </TH>  </TR>
  <TR> <TD align="right"> det </TD> <TD align="right"> 100.12 </TD> <TD align="right"> 100.12 </TD> <TD align="right"> 100.00 </TD> <TD align="right"> 100.00 </TD> </TR>
  <TR> <TD align="right"> g </TD> <TD align="right"> 100.13 </TD> <TD align="right"> 100.10 </TD> <TD align="right"> 100.00 </TD> <TD align="right"> 100.00 </TD> </TR>
  <TR> <TD align="right"> m </TD> <TD align="right"> 102.10 </TD> <TD align="right"> 102.53 </TD> <TD align="right"> 100.00 </TD> <TD align="right"> 100.00 </TD> </TR>
  <TR> <TD align="right"> all </TD> <TD align="right"> 103.73 </TD> <TD align="right"> 103.89 </TD> <TD align="right"> 100.73 </TD> <TD align="right"> 99.87 </TD> </TR>
   </TABLE>

```r
print(xtable(matrix(sds$V1, nrow = length(set), dimnames = list(uncertainties, 
    uncertainties))), type = "html")
```

<!-- html table generated in R 2.15.3 by xtable 1.7-0 package -->
<!-- Wed Mar 13 17:26:48 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> det </TH> <TH> g </TH> <TH> m </TH> <TH> all </TH>  </TR>
  <TR> <TD align="right"> det </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> g </TD> <TD align="right"> 0.06 </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> m </TD> <TD align="right"> 2.78 </TD> <TD align="right"> 2.91 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> all </TD> <TD align="right"> 11.20 </TD> <TD align="right"> 11.16 </TD> <TD align="right"> 2.55 </TD> <TD align="right"> 1.49 </TD> </TR>
   </TABLE>



