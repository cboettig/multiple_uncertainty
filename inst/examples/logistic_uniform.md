  

```
## Loading required package: multipleuncertainty
```

```
## Loading required package: reshape2
```

```
## Loading required package: ggplot2
```

```
## Loading required package: data.table
```

```
## Loading required package: pdgControl
```

```
## Attaching package: 'pdgControl'
```

```
## The following object(s) are masked from 'package:multipleuncertainty':
## 
## SDP_multiple_uncertainty, active_adaptive_simulate, dp_optim,
## model_uncertainty, setmodel, update_belief
```









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

```
## Error: could not find function "ForwardSimulate"
```



```r
sims <- unlist(allcases, recursive = FALSE)
```

```
## Error: object 'allcases' not found
```

```r
dat <- melt(sims, id = names(sims[[1]][[1]]))
```

```
## Error: object 'sims' not found
```

```r
dt <- data.table(dat)
```

```
## Error: object 'dat' not found
```

```r
setnames(dt, c("L2", "L1"), c("reps", "uncertainty"))  # names are nice
```

```
## Error: x is not a data.table or data.frame
```



### Plots 



```r
ggplot(subset(dt, reps == 1)) + geom_line(aes(time, fishstock)) + geom_line(aes(time, 
    harvest), col = "darkgreen") + facet_wrap(~uncertainty)
```

```
## Error: object 'reps' not found
```


This plot summarizes the stock dynamics by visualizing the replicates.


```r
p1 <- ggplot(subset(dt, fishstock > 0))
```

```
## Error: object 'fishstock' not found
```

```r
p1 + geom_line(aes(time, fishstock, group = reps), alpha = 0.1) + facet_wrap(~uncertainty)
```

```
## Error: object 'p1' not found
```




```r
profits <- dt[, sum(profit), by = c("reps", "uncertainty")]
```

```
## Error: invalid 'type' (closure) of argument
```

```r
ggplot(profits) + geom_histogram(aes(V1)) + facet_wrap(~uncertainty)
```

```
## Error: object 'profits' not found
```


Summary statistics 


```r
means <- profits[, mean(V1), by = uncertainty]
```

```
## Error: object 'profits' not found
```

```r
sds <- profits[, sd(V1), by = uncertainty]
```

```
## Error: object 'profits' not found
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

```
## Error: object 'means' not found
```

```r
print(xtable(matrix(sds$V1, nrow = length(noise), dimnames = list(uncertainties, 
    uncertainties))), type = "html")
```

```
## Error: object 'sds' not found
```



