
f=@(x, h) max( (x-h) * (1 - (x-h) ./ 100) + (x-h), 0);
x_grid = [0:5:100];
h_grid = x_grid; % Must be same dimensions as x_grid, or L91 errors...  
Tmax = 5;
sigma_g = 0.1;
sigma_m = 0.;
sigma_i = 0.0;
delta = 0.05;
%% Noise type
%pdf = @(p,mu,s) lognpdf(p ./ mu, 0, s);
pdf = @(p,mu,s) unifpdf(p, mu .* (1 - s), mu .* (1 + s)); 

[D, V, M, I, P, Ep, F, f_matrix] =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);

plot(x_grid, x_grid - x_grid(D(:,1)), '.-')
ylim([0 100])
xlim([0 100])

saveas(1, "policyfn.png")
