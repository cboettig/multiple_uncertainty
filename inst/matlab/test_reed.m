
f = @(x, h) max( max((x - h),0) * (1 - max((x - h),0) ./ 100) + max((x - h),0), 0);
%f = @(x, h) max( max((x - h),0) * exp(1.1 * (1 - max((x - h),0) ./ 100)), 0) ;
%f = @(x, h) max( 2 * max((x - h),0) / (1 + 4 * max((x - h),0)), 0); % K = 0.25
x_grid = [0:5:150];
h_grid = x_grid;
%h_grid = [0:1:30]; % Must be same dimensions as x_grid, or L91 errors...  
Tmax = 10;
sigma_g = 0.0;
delta = 0.0;
[D, V, P, G, f_matrix] =  reed(f, x_grid, h_grid, Tmax, sigma_g, delta);

plot(x_grid, x_grid - x_grid(D(:,1)), '.-')
ylim([0 100])
xlim([0 150])
