``` r
library("dplyr")
library("tidyr")
library("ggplot2")
library("multipleuncertainty")
```

``` r
grid <- seq(0, 150, length = 201)
f <- "allen"
noise <- "lognormal"
small     <- multiple_uncertainty(f = f, x_grid = grid, sigma_g = 0, sigma_m = 0, sigma_i = 0, noise_dist = noise)
growth    <- multiple_uncertainty(f = f, x_grid = grid, sigma_g = 0.5, sigma_m = 0, sigma_i = 0, noise_dist = noise)
measure   <- multiple_uncertainty(f = f, x_grid = grid, sigma_g = 0, sigma_m = 0.5, sigma_i = 0, noise_dist = noise)
implement <- multiple_uncertainty(f = f, x_grid = grid, sigma_g = 0, sigma_m = 0, sigma_i = 0.5, noise_dist = noise)
df <- data.frame(y_grid = grid, small = small, growth = growth, 
                   measure = measure, implement = implement) %>%
    tidyr::gather(scenario, value, -y_grid)
```

``` r
df %>% 
  ggplot(aes(x = y_grid, y = value, col = scenario)) + 
    geom_point()  + 
    xlab("Stock") + ylab("Escapement") + 
    coord_cartesian(xlim = c(0, 100)) + 
    theme_bw()
```

![](allee_files/figure-markdown_github/unnamed-chunk-3-1.png)
