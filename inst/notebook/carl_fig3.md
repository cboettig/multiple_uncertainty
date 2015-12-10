``` r
library("dplyr")
library("tidyr")
library("ggplot2")
library("multipleuncertainty")
```

``` r
fig3 <- function(noise){  
  grid <- seq(0, 200, by=0.5)
  small     <- multiple_uncertainty(f = logistic, x_grid = grid, sigma_g = 0.1, sigma_m = 0.1, sigma_i = 0.1, noise_dist = noise)
  growth    <- multiple_uncertainty(f = logistic, x_grid = grid, sigma_g = 0.5, sigma_m = 0.1, sigma_i = 0.1, noise_dist = noise)
  measure   <- multiple_uncertainty(f = logistic, x_grid = grid, sigma_g = 0.1, sigma_m = 0.5, sigma_i = 0.1, noise_dist = noise)
  implement <- multiple_uncertainty(f = logistic, x_grid = grid, sigma_g = 0.1, sigma_m = 0.1, sigma_i = 0.5, noise_dist = noise)
  df <- data.frame(y_grid = grid, small = small, growth = growth, 
                   measure = measure, implement = implement) %>%
    tidyr::gather(scenario, value, -y_grid)
}

df <- 
data.frame(noise = c("uniform", "lognormal")) %>%
  dplyr::group_by(noise) %>%
  dplyr::do(fig3(.$noise))
```

``` r
df %>% ggplot(aes(x = y_grid, y = value, col = scenario)) + 
    geom_line()  + 
    facet_wrap(~ noise) + 
    xlab("Stock") + 
    ylab("Escapement") + 
    coord_cartesian(xlim = c(0, 150), ylim = c(0,100)) + 
    theme_bw()
```

![](carl_fig3_files/figure-markdown_github/unnamed-chunk-3-1.png)

Reed Result
-----------

``` r
reed <- multiple_uncertainty(noise_dist = "lognormal")

qplot(seq_along(reed), reed) + 
    coord_cartesian(xlim = c(0, 150), ylim = c(0, 150)) + 
    xlab("Stock") + ylab("Escapement") +
    theme_bw()
```

![](carl_fig3_files/figure-markdown_github/unnamed-chunk-4-1.png)
