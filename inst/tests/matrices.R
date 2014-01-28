rm(list=ls())
library(multipleuncertainty)
f = function(x, h, p) 0.2 * (x-h) * (1 - (x-h) / 100) + (x-h)
x_grid = 1:150
h_grid = x_grid
Tmax = 5

out <- SDP_multiple_uncertainty(f, p, x_grid, h_grid, Tmax = 5, delta= 0.01, 
                                sigmas =c(sigma_g=0.3, sigma_m=0.3, sigma_i=0.3),
                                pdfn = pdfn, profit = function(x,h) pmin(x, h))

attach(out)


plot(x_grid, x_grid - x_grid[D[,1]], ylab="escapement")
