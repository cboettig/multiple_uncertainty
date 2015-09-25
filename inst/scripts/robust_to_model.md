``` r
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("tidyr")
library("ggplot2")
library("multipleuncertainty")
knitr::opts_chunk$set(cache = TRUE)
```

``` r
f <- gompertz
x <- numeric(50)
x[1] <- 60
for(t in 1:length(x))
  x[t+1] = f(x[t], 0)
qplot(seq_along(x), x)
```

![](robust_to_model_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
fig3 <- function(model){  
  grid <- seq(0, 200, length = 401)
  model <- switch(model,
         logistic = logistic,
         bevertonholt = bevertonholt,
         ricker = ricker,
         gompertz = gompertz)
  small     <- multiple_uncertainty(f = model, x_grid = grid, sigma_g = 0.1, sigma_m = 0.1, sigma_i = 0.1)
  growth    <- multiple_uncertainty(f = model, x_grid = grid, sigma_g = 0.5, sigma_m = 0.1, sigma_i = 0.1)
  measure   <- multiple_uncertainty(f = model, x_grid = grid, sigma_g = 0.1, sigma_m = 0.5, sigma_i = 0.1)
  implement <- multiple_uncertainty(f = model, x_grid = grid, sigma_g = 0.1, sigma_m = 0.1, sigma_i = 0.5)
  df <- data.frame(y_grid = grid, small = small, growth = growth, 
                   measure = measure, implement = implement) %>%
    tidyr::gather(scenario, value, -y_grid)
}

df <- 
data.frame(model = c("logistic", "bevertonholt", "ricker", "gompertz"), stringsAsFactors = FALSE) %>%
  dplyr::group_by(model) %>%
  dplyr::do(fig3(.$model))
```

``` r
df %>%
  ggplot(aes(x = y_grid, y = value, col = scenario)) + 
    geom_point()  + facet_wrap(~model, ncol = 2) + 
    xlab("Stock") + ylab("Escapement") + 
    coord_cartesian(xlim = c(0, 150), ylim = c(0,100)) + 
    theme_bw() 
```

![](robust_to_model_files/figure-markdown_github/unnamed-chunk-4-1.png)
