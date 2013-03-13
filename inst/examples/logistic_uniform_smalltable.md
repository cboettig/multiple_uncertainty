






Define our functions 



```r
f <- function(x, h, p) {
    sapply(x, function(x) {
        S = max(x - h, 0)
        p[1] * S * (1 - S/p[2]) + S
    })
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
set <- list(det = c(0, 0, 0), g = c(0.5, 0, 0), m = c(0, 0.5, 0), all = c(0.5, 
    0.5, 0.5))
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

![plot of chunk sethiplots-escapement](http://farm9.staticflickr.com/8233/8554878428_39ac57a7ed_o.png) 



```r
ggplot(policy) + geom_point(aes(stock, x_grid[value], color = variable), shape = "+") + 
    stat_smooth(aes(stock, x_grid[value], color = variable), degree = 1, se = FALSE, 
        span = 0.3) + ylab("harvest")
```

![plot of chunk sethiplots-harvest](http://farm9.staticflickr.com/8389/8553773407_6d5e92f064_o.png) 




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
policyfn <- sapply(scenarios, function(out) out)  # consider driving with stationary policy ...
allcases <- lapply(policyfn, function(policyfn_i) {
    lapply(set, function(sigmas) {
        simulatereps(policyfn_i, sigmas)
    })
})
```

```
Error: could not find function "ForwardSimulate"
```



```r
sims <- unlist(allcases, recursive = FALSE)
```

```
Error: object 'allcases' not found
```

```r
dat <- melt(sims, id = names(sims[[1]][[1]]))
```

```
Error: object 'sims' not found
```

```r
dt <- data.table(dat)
```

```
Error: object 'dat' not found
```

```r
setnames(dt, c("L2", "L1"), c("reps", "uncertainty"))  # names are nice
```

```
Error: x is not a data.table or data.frame
```



### Plots 



```r
ggplot(subset(dt, reps == 1)) + geom_line(aes(time, fishstock)) + geom_line(aes(time, 
    harvest), col = "darkgreen") + facet_wrap(~uncertainty)
```

```
Error: object 'reps' not found
```


Summary statistics 


```r
means <- profits[, mean(V1), by = uncertainty]
```

```
Error: object 'profits' not found
```

```r
sds <- profits[, sd(V1), by = uncertainty]
```

```
Error: object 'profits' not found
```



```r
require(xtable)
uncertainties <- names(noise)
print(xtable(matrix(means$V1, nrow = length(noise), dimnames = list(uncertainties, 
    uncertainties))), type = "html")
```

```
Error: object 'means' not found
```

```r
print(xtable(matrix(sds$V1, nrow = length(noise), dimnames = list(uncertainties, 
    uncertainties))), type = "html")
```

```
Error: object 'sds' not found
```



