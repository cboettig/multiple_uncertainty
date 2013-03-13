






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
OptTime <- 5
profit <- function(x, h) pmin(x, h)
```




# Scenarios: 

We use Monte Carlo integration over the noise processes to determine the transition matrix.  




```r
compute_policy <- function(sigmas) {
    z_g <- function() 1 + (2 * runif(1, 0, 1) - 1) * sigmas[1]
    z_m <- function() 1 + (2 * runif(1, 0, 1) - 1) * sigmas[2]
    z_i <- function() 1 + (2 * runif(1, 0, 1) - 1) * sigmas[3]
    
    out <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = sigmas, 
        pdfn = pdfn)
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

![plot of chunk sethiplots-escapement](http://farm9.staticflickr.com/8507/8555086582_7294098926_o.png) 



```r
ggplot(policy) + geom_point(aes(stock, x_grid[value], color = variable), shape = "+") + 
    stat_smooth(aes(stock, x_grid[value], color = variable), degree = 1, se = FALSE, 
        span = 0.3) + ylab("harvest")
```

![plot of chunk sethiplots-harvest](http://farm9.staticflickr.com/8247/8553980011_94ca80e9bf_o.png) 




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

![plot of chunk onerep](http://farm9.staticflickr.com/8516/8553980455_460e73256b_o.png) 


Summary statistics 


```r
profits <- dt[, sum(profit), by = c("reps", "uncertainty")]
ggplot(profits) + geom_histogram(aes(V1)) + facet_wrap(~uncertainty)
```

![the distribution of profits by scenario](http://farm9.staticflickr.com/8249/8555087568_580de3a032_o.png) 




```r
means <- profits[, mean(V1), by = uncertainty]
sds <- profits[, sd(V1), by = uncertainty]
```



```r
require(xtable)
uncertainties <- names(set)
print(xtable(matrix(means$V1, nrow = length(set), dimnames = list(uncertainties, 
    uncertainties))), type = "html")
```

<!-- html table generated in R 2.15.3 by xtable 1.7-0 package -->
<!-- Wed Mar 13 08:56:54 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> det </TH> <TH> g </TH> <TH> m </TH> <TH> all </TH>  </TR>
  <TR> <TD align="right"> det </TD> <TD align="right"> 12.94 </TD> <TD align="right"> 12.94 </TD> <TD align="right"> 13.01 </TD> <TD align="right"> 13.03 </TD> </TR>
  <TR> <TD align="right"> g </TD> <TD align="right"> 13.06 </TD> <TD align="right"> 13.09 </TD> <TD align="right"> 13.09 </TD> <TD align="right"> 12.85 </TD> </TR>
  <TR> <TD align="right"> m </TD> <TD align="right"> 12.25 </TD> <TD align="right"> 12.36 </TD> <TD align="right"> 12.87 </TD> <TD align="right"> 12.89 </TD> </TR>
  <TR> <TD align="right"> all </TD> <TD align="right"> 11.88 </TD> <TD align="right"> 11.80 </TD> <TD align="right"> 12.37 </TD> <TD align="right"> 12.82 </TD> </TR>
   </TABLE>

```r
print(xtable(matrix(sds$V1, nrow = length(set), dimnames = list(uncertainties, 
    uncertainties))), type = "html")
```

<!-- html table generated in R 2.15.3 by xtable 1.7-0 package -->
<!-- Wed Mar 13 08:56:54 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> det </TH> <TH> g </TH> <TH> m </TH> <TH> all </TH>  </TR>
  <TR> <TD align="right"> det </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> g </TD> <TD align="right"> 1.41 </TD> <TD align="right"> 1.55 </TD> <TD align="right"> 1.57 </TD> <TD align="right"> 1.50 </TD> </TR>
  <TR> <TD align="right"> m </TD> <TD align="right"> 0.58 </TD> <TD align="right"> 0.56 </TD> <TD align="right"> 0.12 </TD> <TD align="right"> 0.12 </TD> </TR>
  <TR> <TD align="right"> all </TD> <TD align="right"> 1.44 </TD> <TD align="right"> 1.47 </TD> <TD align="right"> 1.50 </TD> <TD align="right"> 1.85 </TD> </TR>
   </TABLE>



