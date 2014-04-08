clear;

f = @(x, h) max( max((x - h),0) * (1 - max((x - h),0) ./ 100) + max((x - h),0), 0);
%f=@(x, h) max( (x-h) * (1 - (x-h) ./ 100) + (x-h), 0);

x_grid = [0:5:150];
h_grid = x_grid; % Must be same dimensions as x_grid, or L91 errors...  

Tmax = 10;
delta = 0.05;

pdf = @(p,mu,s) lognpdf(p ./ mu, 0, s);
%pdf = @(p,mu,s) unifpdf(p, mu .* (1 - s), mu .* (1 + s)); 

sigma_g = 0.2;
sigma_m = 0.0;
sigma_i = 0.0;
[D, V, M, I, P, Ep, F, f_matrix] =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);
small = x_grid - x_grid(D(:,1));


XL='Fish Stock';
YL = 'Policy Function, H';
YL2 = 'Value Funciton';
colorlines={'b','k--','g.-','r.'};
figure
plot(x_grid,small,colorlines{1})
axis([0 100 0 100])
xlabel(XL)
ylabel(YL)
%title('Figure 3 in Sethi')
%legend('All Low','Large Growth','Large Measurement','Large  Implementation')

