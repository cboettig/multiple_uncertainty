
f=@(x, h) (0.2 * (x-h) * (1 - (x-h) ./ 100) + (x-h));
x_grid = [1:150];
h_grid = x_grid;
Tmax = 5;
sigma_g = 0.3;
sigma_m = 0.3;
sigma_i = 0.3;
delta = 0.01;
[D, V, M, I, P, Ep, F, G, f_matrix] =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta);

plot(x_grid, x_grid - x_grid(D(:,1)))
