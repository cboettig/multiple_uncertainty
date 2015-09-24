library("tidyr")
library("ggplot2")
library("multipleuncertainty")

f <- logistic
grid = seq(0, 200, length = 601)
x_grid =  grid 
h_grid =  grid
y_grid  = grid
q_grid  = grid

Tmax = 50
delta = 0.05
noise_dist = "uniform"

sigma_g = 0.1
sigma_m = 0.1
sigma_i = 0.1
small =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, noise_dist, y_grid, q_grid)

sigma_g = 0.5
sigma_m = 0.1
sigma_i = 0.1
growth =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, noise_dist, y_grid, q_grid)

sigma_g = 0.1
sigma_m = 0.5
sigma_i = 0.1
measure =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, noise_dist, y_grid, q_grid)


sigma_g = 0.1
sigma_m = 0.1
sigma_i = 0.5
implement =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, noise_dist, y_grid, q_grid)

##############


data.frame(y_grid = y_grid, small = small, growth = growth, 
           measure = measure, implement = implement) %>%
  tidyr::gather(scenario, value, -y_grid) %>%  
  ggplot(aes(x = y_grid, y = value, col = scenario)) + 
    geom_point() + 
    coord_cartesian(xlim = c(0, 150)) + 
    theme_bw()


if(0){
  reed =  multiple_uncertainty(f, x_grid, sigma_g = 0.2, sigma_m = 0, sigma_i = 0)
  qplot(y_grid, reed)
  }
