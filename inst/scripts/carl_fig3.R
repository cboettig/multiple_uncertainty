library("tidyr")
library("ggplot2")
library("multipleuncertainty")

f <- logistic
grid = seq(0, 200, length = 401)
Tmax = 50
delta = 0.05
noise_dist = "uniform"

small     <- multiple_uncertainty(f = f, x_grid = grid, sigma_g = 0.1, sigma_m = 0.1, sigma_i = 0.1)
growth    <- multiple_uncertainty(f = f, x_grid = grid, sigma_g = 0.5, sigma_m = 0.1, sigma_i = 0.1)
measure   <- multiple_uncertainty(f = f, x_grid = grid, sigma_g = 0.1, sigma_m = 0.5, sigma_i = 0.1)
implement <- multiple_uncertainty(f = f, x_grid = grid, sigma_g = 0.1, sigma_m = 0.1, sigma_i = 0.5)

data.frame(y_grid = y_grid, small = small, growth = growth, 
           measure = measure, implement = implement) %>%
  tidyr::gather(scenario, value, -y_grid) %>%  
  ggplot(aes(x = y_grid, y = value, col = scenario)) + 
    geom_point() + 
    coord_cartesian(xlim = c(0, 150)) + 
    theme_bw()


if(0){
  reed =  multiple_uncertainty()
  qplot(seq_along(reed), reed)
  }
