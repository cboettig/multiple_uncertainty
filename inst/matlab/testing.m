
f=@(x, h) (0.2 * (x-h) * (1 - (x-h) ./ 100) + (x-h));
x_grid = [1:150];
h_grid = x_grid;
Tmax = 15;
sigma_g = 0.3;
sigma_m = 0.0;
sigma_i = 0.0;
delta = 0.01;
[D, V, M, I, P, Ep, F] =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta);

D(:,1)

