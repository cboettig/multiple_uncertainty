


```r
f <- function(x, h, p) {
    A <- p[1]
    B <- p[2]
    s <- pmax(x - h, 0)
    A * s/(1 + B * s)
}
```



```r
pars <- c(1.5, 0.05)
K <- (pars[1] - 1)/pars[2]
xmin <- 0
xmax <- 1.5 * K
n_x <- 300
n_h <- n_x
x_grid <- seq(xmin, xmax, length = n_x)
h_grid <- seq(xmin, xmax, length = n_h)
delta <- 0.05
xT <- 0
OptTime <- 5
sigma_g <- 0.3
profit <- function(x, h) pmin(x, h)
```




# Scenarios: 

We use Monte Carlo integration over the noise processes to determine the transition matrix.  


```r
require(snowfall)
```

```
## Loading required package: snowfall
```

```
## Loading required package: snow
```

```r
sfInit(parallel = TRUE, cpu = 16)
```

```
## R Version:  R version 2.15.2 (2012-10-26)
```

```
## snowfall 1.84 initialized (using snow 0.3-10): parallel execution on 16
## CPUs.
```




```r
scenario <- function(policy_g, policy_m, policy_i) {
    
    z_g <- function() 1 + (2 * runif(1, 0, 1) - 1) * policy_g
    z_m <- function() 1 + (2 * runif(1, 0, 1) - 1) * policy_m
    z_i <- function() 1 + (2 * runif(1, 0, 1) - 1) * policy_i
    
    out <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = c(sigma_g = sigma_g, 
        sigma_m = 0, sigma_i = 0), pdfn = pdfn)
    out
}
```


Determine the policies for each of the scenarios (noise combinations).


```r
lvl <- 0.5
```



```r
det <- scenario(0.01, 0, 0)
```



```r
all_low <- scenario(0.1, 0.1, 0.1)
```



```r
g <- scenario(lvl, 0, 0)
```



```r
m <- scenario(0, lvl, 0)
```



```r
i <- scenario(0, 0, lvl)
```



```r
gm <- scenario(lvl, lvl, 0)
```



```r
gi <- scenario(lvl, 0, lvl)
```



```r
mi <- scenario(0, lvl, lvl)
```



```r
gmi <- scenario(lvl, lvl, lvl)
```




```r
low <- all_low
```



### plots



```r
require(reshape2)
policy <- melt(data.frame(stock = x_grid, det = det$D[, 1], low = low$D[, 1], 
    g = g$D[, 1], m = m$D[, 1], i = m$D[, 1], gm = gm$D[, 1], gi = gi$D[, 1], 
    mi = mi$D[, 1], gmi = gmi$D[, 1]), id = "stock")

ggplot(policy) + geom_point(aes(stock, stock - x_grid[value], color = variable), 
    shape = "+") + stat_smooth(aes(stock, stock - x_grid[value], color = variable), 
    degree = 1, se = FALSE, span = 0.3) + ylab("escapement")
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using
## loess. Use 'method = x' to change the smoothing method.
```

![plot of chunk sethiplots](http://carlboettiger.info/assets/figures/2013-01-08-15-48-22-b1a266500e-sethiplots1.png) 

```r

ggplot(policy) + geom_point(aes(stock, x_grid[value], color = variable), shape = "+") + 
    stat_smooth(aes(stock, x_grid[value], color = variable), degree = 1, se = FALSE, 
        span = 0.3) + ylab("harvest")
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using
## loess. Use 'method = x' to change the smoothing method.
```

![plot of chunk sethiplots](http://carlboettiger.info/assets/figures/2013-01-08-15-49-06-b1a266500e-sethiplots2.png) 

```r


value <- melt(data.frame(stock = x_grid, det = det$V, low = low$V, g = g$V, 
    m = m$V, gm = gm$V, gi = gi$V, mi = mi$V, gmi = gmi$V), id = "stock")

ggplot(value) + geom_point(aes(stock, value, color = variable), shape = "+") + 
    # stat_smooth(aes(stock, value, color=variable), degree=0, se=FALSE,
# span=0.15) +
ylab("Net Present Value")
```

![plot of chunk sethiplots](http://carlboettiger.info/assets/figures/2013-01-08-15-50-19-b1a266500e-sethiplots3.png) 




## Simulations


```r
simulatereps <- function(opt, true_g, true_m, true_i) {
    
    z_g <- function() 1 + (2 * runif(1, 0, 1) - 1) * true_g
    z_m <- function() 1 + (2 * runif(1, 0, 1) - 1) * true_m
    z_i <- function() 1 + (2 * runif(1, 0, 1) - 1) * true_i
    
    sims <- lapply(1:100, function(i) {
        ForwardSimulate(f, pars, x_grid, h_grid, x0 = K, opt$D, z_g, z_m, z_i, 
            profit)
    })
    
    sims
}
```



All cases


```r
policyfn <- list(det = det, low = low, g = g, m = m, i = i, gm = gm, gi = gi, 
    mi = mi, gmi = gmi)
noise <- list(det = c(0.01, 0, 0), low = c(0.1, 0.1, 0.1), growth = c(lvl, 0, 
    0), measure = c(0, lvl, 0), implement = c(0, 0, lvl), growth_measure = c(lvl, 
    lvl, 0), growth_implement = c(lvl, 0, lvl), measure_implement = c(0, lvl, 
    lvl), all = c(lvl, lvl, lvl))
allcases <- lapply(policyfn, function(policyfn_i) {
    lapply(noise, function(noise_i) {
        simulatereps(policyfn_i, noise_i[1], noise_i[2], noise_i[3])
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

![plot of chunk onerep](http://carlboettiger.info/assets/figures/2013-01-08-16-31-02-a06af9b44f-onerep.png) 


This plot summarizes the stock dynamics by visualizing the replicates.


```r
p1 <- ggplot(subset(dt, fishstock > 0))
p1 + geom_line(aes(time, fishstock, group = reps), alpha = 0.1) + facet_wrap(~uncertainty)
```

![the induced dynamics in the stock size over time, for all replicates, by scenario](http://carlboettiger.info/assets/figures/2013-01-08-16-31-30-a06af9b44f-stock.png) 




```r
profits <- dt[, sum(profit), by = c("reps", "uncertainty")]
ggplot(profits) + geom_histogram(aes(V1)) + facet_wrap(~uncertainty)
```

![the distribution of profits by scenario](http://carlboettiger.info/assets/figures/2013-01-08-16-31-55-a06af9b44f-profits.png) 


Summary statistics 


```r
means <- profits[, mean(V1), by = uncertainty]
sds <- profits[, sd(V1), by = uncertainty]
```



```r
require(xtable)
```

```
## Loading required package: xtable
```

```r
uncertainties <- names(noise)
print(xtable(matrix(means$V1, nrow = length(noise), dimnames = list(uncertainties, 
    uncertainties))), type = "html")
```

<!-- html table generated in R 2.15.2 by xtable 1.7-0 package -->
<!-- Tue Jan  8 16:32:10 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> det </TH> <TH> low </TH> <TH> growth </TH> <TH> measure </TH> <TH> implement </TH> <TH> growth_measure </TH> <TH> growth_implement </TH> <TH> measure_implement </TH> <TH> all </TH>  </TR>
  <TR> <TD align="right"> det </TD> <TD align="right"> 12.94 </TD> <TD align="right"> 12.93 </TD> <TD align="right"> 12.94 </TD> <TD align="right"> 12.94 </TD> <TD align="right"> 12.93 </TD> <TD align="right"> 12.94 </TD> <TD align="right"> 12.94 </TD> <TD align="right"> 12.93 </TD> <TD align="right"> 12.93 </TD> </TR>
  <TR> <TD align="right"> low </TD> <TD align="right"> 12.72 </TD> <TD align="right"> 12.86 </TD> <TD align="right"> 12.71 </TD> <TD align="right"> 12.77 </TD> <TD align="right"> 12.72 </TD> <TD align="right"> 12.72 </TD> <TD align="right"> 12.79 </TD> <TD align="right"> 12.81 </TD> <TD align="right"> 12.68 </TD> </TR>
  <TR> <TD align="right"> growth </TD> <TD align="right"> 13.14 </TD> <TD align="right"> 12.81 </TD> <TD align="right"> 12.52 </TD> <TD align="right"> 12.85 </TD> <TD align="right"> 13.04 </TD> <TD align="right"> 12.59 </TD> <TD align="right"> 12.63 </TD> <TD align="right"> 12.63 </TD> <TD align="right"> 13.14 </TD> </TR>
  <TR> <TD align="right"> measure </TD> <TD align="right"> 11.48 </TD> <TD align="right"> 11.64 </TD> <TD align="right"> 11.58 </TD> <TD align="right"> 11.57 </TD> <TD align="right"> 11.58 </TD> <TD align="right"> 11.41 </TD> <TD align="right"> 11.65 </TD> <TD align="right"> 11.50 </TD> <TD align="right"> 11.58 </TD> </TR>
  <TR> <TD align="right"> implement </TD> <TD align="right"> 12.01 </TD> <TD align="right"> 12.14 </TD> <TD align="right"> 12.10 </TD> <TD align="right"> 12.05 </TD> <TD align="right"> 12.13 </TD> <TD align="right"> 12.08 </TD> <TD align="right"> 12.09 </TD> <TD align="right"> 12.11 </TD> <TD align="right"> 12.15 </TD> </TR>
  <TR> <TD align="right"> growth_measure </TD> <TD align="right"> 11.61 </TD> <TD align="right"> 11.41 </TD> <TD align="right"> 11.71 </TD> <TD align="right"> 12.04 </TD> <TD align="right"> 12.04 </TD> <TD align="right"> 11.42 </TD> <TD align="right"> 11.66 </TD> <TD align="right"> 11.67 </TD> <TD align="right"> 11.51 </TD> </TR>
  <TR> <TD align="right"> growth_implement </TD> <TD align="right"> 12.16 </TD> <TD align="right"> 11.97 </TD> <TD align="right"> 12.18 </TD> <TD align="right"> 11.75 </TD> <TD align="right"> 12.17 </TD> <TD align="right"> 12.01 </TD> <TD align="right"> 12.20 </TD> <TD align="right"> 12.02 </TD> <TD align="right"> 12.12 </TD> </TR>
  <TR> <TD align="right"> measure_implement </TD> <TD align="right"> 11.36 </TD> <TD align="right"> 11.33 </TD> <TD align="right"> 11.43 </TD> <TD align="right"> 11.46 </TD> <TD align="right"> 11.48 </TD> <TD align="right"> 11.38 </TD> <TD align="right"> 11.45 </TD> <TD align="right"> 11.49 </TD> <TD align="right"> 11.27 </TD> </TR>
  <TR> <TD align="right"> all </TD> <TD align="right"> 11.26 </TD> <TD align="right"> 11.47 </TD> <TD align="right"> 11.58 </TD> <TD align="right"> 11.30 </TD> <TD align="right"> 11.59 </TD> <TD align="right"> 11.15 </TD> <TD align="right"> 11.60 </TD> <TD align="right"> 11.78 </TD> <TD align="right"> 11.33 </TD> </TR>
   </TABLE>

```r
print(xtable(matrix(sds$V1, nrow = length(noise), dimnames = list(uncertainties, 
    uncertainties))), type = "html")
```

<!-- html table generated in R 2.15.2 by xtable 1.7-0 package -->
<!-- Tue Jan  8 16:32:10 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> det </TH> <TH> low </TH> <TH> growth </TH> <TH> measure </TH> <TH> implement </TH> <TH> growth_measure </TH> <TH> growth_implement </TH> <TH> measure_implement </TH> <TH> all </TH>  </TR>
  <TR> <TD align="right"> det </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.05 </TD> </TR>
  <TR> <TD align="right"> low </TD> <TD align="right"> 0.52 </TD> <TD align="right"> 0.54 </TD> <TD align="right"> 0.48 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 0.54 </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 0.51 </TD> <TD align="right"> 0.50 </TD> </TR>
  <TR> <TD align="right"> growth </TD> <TD align="right"> 2.29 </TD> <TD align="right"> 2.35 </TD> <TD align="right"> 1.99 </TD> <TD align="right"> 2.31 </TD> <TD align="right"> 2.29 </TD> <TD align="right"> 2.35 </TD> <TD align="right"> 2.06 </TD> <TD align="right"> 2.25 </TD> <TD align="right"> 2.27 </TD> </TR>
  <TR> <TD align="right"> measure </TD> <TD align="right"> 1.03 </TD> <TD align="right"> 0.95 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.96 </TD> <TD align="right"> 1.04 </TD> <TD align="right"> 1.01 </TD> <TD align="right"> 0.97 </TD> <TD align="right"> 0.97 </TD> <TD align="right"> 0.93 </TD> </TR>
  <TR> <TD align="right"> implement </TD> <TD align="right"> 0.79 </TD> <TD align="right"> 0.75 </TD> <TD align="right"> 0.77 </TD> <TD align="right"> 0.82 </TD> <TD align="right"> 0.80 </TD> <TD align="right"> 0.79 </TD> <TD align="right"> 0.83 </TD> <TD align="right"> 0.75 </TD> <TD align="right"> 0.82 </TD> </TR>
  <TR> <TD align="right"> growth_measure </TD> <TD align="right"> 2.54 </TD> <TD align="right"> 2.10 </TD> <TD align="right"> 2.51 </TD> <TD align="right"> 2.45 </TD> <TD align="right"> 2.50 </TD> <TD align="right"> 2.26 </TD> <TD align="right"> 2.43 </TD> <TD align="right"> 2.41 </TD> <TD align="right"> 2.03 </TD> </TR>
  <TR> <TD align="right"> growth_implement </TD> <TD align="right"> 2.16 </TD> <TD align="right"> 2.65 </TD> <TD align="right"> 2.15 </TD> <TD align="right"> 2.26 </TD> <TD align="right"> 2.23 </TD> <TD align="right"> 2.52 </TD> <TD align="right"> 2.13 </TD> <TD align="right"> 2.33 </TD> <TD align="right"> 2.37 </TD> </TR>
  <TR> <TD align="right"> measure_implement </TD> <TD align="right"> 1.12 </TD> <TD align="right"> 1.09 </TD> <TD align="right"> 1.02 </TD> <TD align="right"> 1.14 </TD> <TD align="right"> 1.10 </TD> <TD align="right"> 1.12 </TD> <TD align="right"> 1.03 </TD> <TD align="right"> 1.08 </TD> <TD align="right"> 1.20 </TD> </TR>
  <TR> <TD align="right"> all </TD> <TD align="right"> 2.45 </TD> <TD align="right"> 2.28 </TD> <TD align="right"> 2.43 </TD> <TD align="right"> 2.49 </TD> <TD align="right"> 2.17 </TD> <TD align="right"> 2.15 </TD> <TD align="right"> 2.40 </TD> <TD align="right"> 2.25 </TD> <TD align="right"> 2.15 </TD> </TR>
   </TABLE>



