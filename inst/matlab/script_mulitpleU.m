% Carl's script for multiple uncertainty
clear all


% Sethi parameters in the base case
% r=1, K=100;
% Grid is 200 measurements over 1-150 
% Sethi assumptions on shocks: .5 = high, .1=low, .9 = very large
tic

%f=@(x, h)(1*(x-h)*(1 - (x-h)./100)+(x-h));
f = @(x, h) max( max((x - h),0) * (1 - max((x - h),0) ./ 100)+ max((x - h),0), 0);
pdf = @(p,mu,s) unifpdf(p, mu .* (1 - s), mu .* (1 + s)); 
%pdf = @(p,mu,s) lognpdf(p ./ mu, 0, s);

% Building a grid based on problem
%x_grid=[1:.5:40 40.5:.1:60 61:2:150];
x_grid = linspace(1,100,75); 
h_grid = linspace(1,100,75); %x_grid; 

Tmax = 5;
XL='Fish Stock';
YL = 'Escapement, X-H';
YL2 = 'Value Funciton';
colorlines={'b','k--','g.-','r.'};
delta = 0.05; % Discount rate (converted to discount factor in code)


%% Run the following code to do just one run of the program
test=1;

if test==1 % Set to run with small uncertainty
    tic
    sigma_g = 0.0; % Stock shock
    sigma_m = 0.0; % Measurement shock
    sigma_i = 0.0; % harvest implementation shock
    
    %[D, V, M, I, P, Ep, F, f_matrix,iter,ESC]
    [D, V, M, I, P, Ep, F, G, iter, ESC] =  multiple_uncertainty_rev(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);
    % Figures
    % Policy Function752
    figure
    subplot(311)
    plot(x_grid, x_grid - x_grid(D(:,1)),'.-');
    axis([0 100 0 100])
    xlabel(XL)
    ylabel(YL)
    title('All uncertainties low: Fig 2 Sethi')
    subplot(312)
    plot(x_grid,V','.-')
    xlabel(XL)
    ylabel(YL2)
    subplot(313)
    plot(x_grid,x_grid(D(:,1)),'.-')
    xlabel(XL)
    ylabel('Harvest')
    toc
    dbstop
end

%% This runs the deterministic case and saves the results
    sigma_g = 0.0; % Stock shock
    sigma_m = 0.0; % Measurement shock
    sigma_i = 0.0; % harvest implementation shock
    % Not calling back F or f_matrix to save memory space
    [det.D, det.V, ~, ~, ~, ~,~, ~, iter] =  multiple_uncertainty_rev(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);
    % Figures
    % Policy Function
    figure
    subplot(211)
    plot(x_grid, x_grid - x_grid(det.D(:,1)));
    axis([0 100 0 100])
    xlabel(XL)
    ylabel(YL)
    title('Deterministic Case')
    subplot(212)
    plot(x_grid,det.V(80:100, :)','.-')
    xlabel(XL)
    ylabel(YL2)


%% This loop generates Figure 1 in Sethi

for i=1:3
    
    if i==1
        sigma_g = 0.5; % Stock shock .5 = high, .1=low, .9 = very large
    elseif  i==2
        sigma_m = 0.1; % Measurement shock
    elseif i==3
        sigma_g = 0.9;
    end
    
    % Not calling back F or f_matrix to save memory space
    [D, V, ~, ~, ~, ~,~, ~, iter] =  multiple_uncertainty_rev(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);
    Y{i}=x_grid - x_grid(D(:,1));
    Val{i}=V;
    %Epp{i}=Ep;
    %FF{i}=F;
    Iter(i)=iter;
    
end

%This generates Figure 1 in Sethi
figure
plot(x_grid, x_grid - x_grid(det.D(:,1)),colorlines{1},...
     x_grid,Y{1},colorlines{2},x_grid,Y{2},colorlines{3},...
     x_grid,Y{3},colorlines{4})
axis([0 100 0 100])
xlabel(XL)
ylabel(YL)
title('Figure 1 in Sethi')
legend('Deterministic','Large Growth Shock (LGS), Stock known','LGS, Stock unknown','Very LGS, Stock unknown')

figure
subplot(221)
plot(x_grid,det.V(80:100, :)','.-')
ylabel(YL2)
title('Deterministic')
subplot(222)
plot(x_grid,Val{1}(80:100, :)','.-')
title('Large Growth Shock (LGS), Stock known')
subplot(223)
plot(x_grid,Val{2}(80:100, :)','.-')
ylabel(YL2)
xlabel(XL)
title('LGS, Stock unknown')
subplot(224)
plot(x_grid,Val{3}(80:100, :)','.-')
xlabel(XL)
title('Very LGS, Stock unknown')


%% Generating Figure 3 in Sethi
sigma_g = 0.1; % Stock shock
sigma_m = 0.1; % Measurement shock
sigma_i = 0.1; % harvest implementation shock
[Fig3.D, Fig3.V, ~, ~, ~, ~,~, ~,  iter] =  multiple_uncertainty_rev(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);
sigma_g = 0.5; % Stock shock
sigma_m = 0.1; % Measurement shock
sigma_i = 0.1; % harvest implementation shock
[Fig3.D1, Fig3.V1, ~, ~, ~, ~,~, ~,  iter] =  multiple_uncertainty_rev(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);
sigma_g = 0.1; % Stock shock
sigma_m = 0.5; % Measurement shock
sigma_i = 0.1; % harvest implementation shock
[Fig3.D2, Fig3.V2, ~, ~, ~, ~,~, ~,  iter] =  multiple_uncertainty_rev(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);
sigma_g = 0.1; % Stock shock
sigma_m = 0.1; % Measurement shock
sigma_i = 0.5; % harvest implementation shock
[Fig3.D3, Fig3.V3, ~, ~, ~, ~,~, ~,  iter] =  multiple_uncertainty_rev(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);

figure
plot(x_grid,x_grid - x_grid(Fig3.D(:,1)),colorlines{1},...
     x_grid,x_grid - x_grid(Fig3.D1(:,1)),colorlines{2},...
     x_grid,x_grid - x_grid(Fig3.D2(:,1)),colorlines{3},...
     x_grid,x_grid - x_grid(Fig3.D3(:,1)),colorlines{4})
axis([0 100 0 100])
xlabel(XL)
ylabel(YL)
title('Figure 3 in Sethi')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')



%% Generating Figure 4 in Sethi
sigma_g = 0.5; % Stock shock
sigma_m = 0.1; % Measurement shock
sigma_i = 0.5; % harvest implementation shock
[Fig4.D1, Fig4.V1, ~, ~, ~, ~,~, ~,  iter] =  multiple_uncertainty_rev(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);

sigma_g = 0.5; % Stock shock
sigma_m = 0.5; % Measurement shock
sigma_i = 0.5; % harvest implementation shock
[Fig4.D2, Fig4.V2, ~, ~, ~, ~,~, ~, ~, iter] =  multiple_uncertainty_rev(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf);

figure
plot(x_grid,x_grid - x_grid(Fig4.D1(:,1)),'b--',...
     x_grid,x_grid - x_grid(Fig4.D2(:,1)),'r.-')
axis([0 100 0 100])
xlabel(XL)
ylabel(YL)
title('Figure 4 in Sethi')
legend('Large Growth & Implementation','All Large')
grid on



toc/60


