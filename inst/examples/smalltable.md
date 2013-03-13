






Define our functions 



```r
f <- function(x, h, p) {
    sapply(x, function(x) {
        S = max(x - h, 0)
        p[1] * S * (1 - S/p[2]) + S
    })
}
pars = c(1.5, 100)
K <- pars[2]
```



```r
f <- function(x, h, p) {
    A <- p[1]
    B <- p[2]
    s <- pmax(x - h, 0)
    A * s/(1 + B * s)
}
pars <- c(1.5, 0.05)
K <- (pars[1] - 1)/pars[2]
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
set <- list(det = c(0, 0, 0), g = c(0.3, 0, 0), m = c(0, 0.3, 0), all = c(0.3, 
    0.3, 0.3))
scenarios <- lapply(set, compute_policy)
```




```r
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

![plot of chunk sethiplots-escapement](http://farm9.staticflickr.com/8389/8555730040_fc8eddf024_o.png) 



```r
ggplot(policy) + geom_point(aes(stock, x_grid[value], color = variable), shape = "+") + 
    stat_smooth(aes(stock, x_grid[value], color = variable), degree = 1, se = FALSE, 
        span = 0.3) + ylab("harvest")
```

![plot of chunk sethiplots-harvest](http://farm9.staticflickr.com/8088/8554620361_cfc107f3e7_o.png) 




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

![plot of chunk onerep](http://farm9.staticflickr.com/8093/8555730606_bd2de36772_o.png) 


Summary statistics 


```r
profits <- dt[, sum(profit), by = c("reps", "uncertainty")]
ggplot(profits) + geom_histogram(aes(V1)) + facet_wrap(~uncertainty)
```

![the distribution of profits by scenario](http://farm9.staticflickr.com/8521/8555730786_9c52920022_o.png) 




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
<!-- Wed Mar 13 13:01:08 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> det </TH> <TH> g </TH> <TH> m </TH> <TH> all </TH>  </TR>
  <TR> <TD align="right"> det </TD> <TD align="right"> 19.11 </TD> <TD align="right"> 19.11 </TD> <TD align="right"> 18.71 </TD> <TD align="right"> 18.86 </TD> </TR>
  <TR> <TD align="right"> g </TD> <TD align="right"> 19.06 </TD> <TD align="right"> 19.07 </TD> <TD align="right"> 18.77 </TD> <TD align="right"> 18.86 </TD> </TR>
  <TR> <TD align="right"> m </TD> <TD align="right"> 18.72 </TD> <TD align="right"> 18.69 </TD> <TD align="right"> 18.55 </TD> <TD align="right"> 18.75 </TD> </TR>
  <TR> <TD align="right"> all </TD> <TD align="right"> 18.07 </TD> <TD align="right"> 17.93 </TD> <TD align="right"> 18.34 </TD> <TD align="right"> 18.55 </TD> </TR>
   </TABLE>

```r
print(xtable(matrix(sds$V1, nrow = length(set), dimnames = list(uncertainties, 
    uncertainties))), type = "html")
```

<!-- html table generated in R 2.15.3 by xtable 1.7-0 package -->
<!-- Wed Mar 13 13:01:08 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> det </TH> <TH> g </TH> <TH> m </TH> <TH> all </TH>  </TR>
  <TR> <TD align="right"> det </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> g </TD> <TD align="right"> 3.03 </TD> <TD align="right"> 3.05 </TD> <TD align="right"> 3.26 </TD> <TD align="right"> 3.52 </TD> </TR>
  <TR> <TD align="right"> m </TD> <TD align="right"> 0.76 </TD> <TD align="right"> 0.85 </TD> <TD align="right"> 0.76 </TD> <TD align="right"> 0.79 </TD> </TR>
  <TR> <TD align="right"> all </TD> <TD align="right"> 3.23 </TD> <TD align="right"> 3.01 </TD> <TD align="right"> 3.25 </TD> <TD align="right"> 3.23 </TD> </TR>
   </TABLE>



