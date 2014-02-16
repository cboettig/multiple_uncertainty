Compare multiple uncertainties

  

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






```r
f <- function(x, h, p) {
    sapply(x, function(x) {
        S = max(x - h, 0)
        p[1] * S * (1 - S/p[2]) + S
    })
}
```



```r
# pars <- c(1.5, 0.05) K <- (pars[1] - 1)/pars[2]

pars <- c(1, 100)
K <- 100
xmin <- 0
xmax <- 1.5 * K
n_x <- 300
n_h <- n_x
x_grid <- seq(xmin, xmax, length = n_x)
h_grid <- seq(xmin, xmax, length = n_h)
delta <- 0.05
xT <- 0
OptTime <- 20
sigma_g <- 0.3
sigma_m <- 0.3
sigma_i <- 0.3
small_noise <- 0.05
profit <- function(x, h) pmin(x, h)
pdf = pdfn_lnorm
```


The default pdfn is uniform











