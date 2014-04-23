clear;

f=@(x, h) max( (x-h) * (1 - (x-h) ./ 100) + (x-h), 0);

x_grid =  linspace(0,150,151); 
h_grid =  linspace(0,150,151);
y_grid  = linspace(0,150,151);
q_grid  = linspace(0,150,151);

Tmax = 10;
delta = 0.05;

%pdf = @(p,mu,s) lognpdf(p ./ mu, 0, s);
pdf = @(p,mu,s) unifpdf(p, mu .* (1 - s), mu .* (1 + s)); 

sigma_g = 0.1;
sigma_m = 0.1;
sigma_i = 0.1;
[D, V, M, I, P, Ep, F, f_matrix] =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf, y_grid, q_grid);
small = y_grid - q_grid(D(:,1));

sigma_g = 0.5;
sigma_m = 0.1;
sigma_i = 0.1;
[D, V, M, I, P, Ep, F, f_matrix] =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf, y_grid, q_grid);
growth = y_grid - q_grid(D(:,1));


sigma_g = 0.1;
sigma_m = 0.5;
sigma_i = 0.1;
[D, V, M, I, P, Ep, F, f_matrix] =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf, y_grid, q_grid);
measure = y_grid - q_grid(D(:,1));

sigma_g = 0.1;
sigma_m = 0.1;
sigma_i = 0.5;
[D, V, M, I, P, Ep, F, f_matrix] =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf, y_grid, q_grid);
implement = y_grid - q_grid(D(:,1));

colorlines={'b','k--','g.-','r.'};
figure
plot(y_grid,small,colorlines{1},...
     y_grid, growth, colorlines{2},...
     y_grid, measure, colorlines{3},...
     y_grid, implement, colorlines{4})
axis([0 100 0 100])
xlabel('Fish Stock')
ylabel('Escapement policy')
title('Figure 3 in Sethi')
legend('Deterministic','Large Growth','Large Measurement','Large  Implementation')
legend('boxoff')
plot2svg('multiple_uncertainty.svg')

