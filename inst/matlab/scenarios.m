clear;

% Fixed parameters and grids: 
x_grid =  linspace(0,200,201); 
y_grid  = linspace(0,200,201);
h_grid =  linspace(0,120,121);
q_grid  = linspace(0,120,121);
Tmax = 10;
delta = 0.05;
grids = struct('x', x_grid, 'y', y_grid, 'h', h_grid, ...
               'q', q_grid, 'Tmax', Tmax, 'delta', delta);


global_data = []; % initialize variable

%%% Run the scenarios %%%%%%%%%%
%   y_grid, escapement, sigma_g, sigma_m, sigma_i, r, K, recruitment, noise, id
% 
% Where:
%   recruitment: 1 = logistic, 2 = ricker, 3 = beverton-holt
%   noise: 1 = uniform, 2 = lognormal
% 
%  escapement is calculated escapement. All other columns are parameters just 
%  for reference.  id is a unique id for each scenario. 


% logistic uniform, r=1, K=100
global_data = [global_data; scenario([0.1, 0.1, 0.1], 1, 100, 1, 1, 1, grids)];
global_data = [global_data; scenario([0.5, 0.1, 0.1], 1, 100, 1, 1, 2, grids)];
global_data = [global_data; scenario([0.1, 0.5, 0.1], 1, 100, 1, 1, 3, grids)];
global_data = [global_data; scenario([0.1, 0.1, 0.5], 1, 100, 1, 1, 4, grids)];

% logistic lognormal, r=1, K=100
global_data = [global_data; scenario([0.1, 0.1, 0.1], 1, 100, 1, 2, 5, grids)];
global_data = [global_data; scenario([0.5, 0.1, 0.1], 1, 100, 1, 2, 6, grids)];
global_data = [global_data; scenario([0.1, 0.5, 0.1], 1, 100, 1, 2, 7, grids)];
global_data = [global_data; scenario([0.1, 0.1, 0.5], 1, 100, 1, 2, 8, grids)];





% logistic uniform, r=.1, K=100
global_data = [global_data; scenario([0.1, 0.1, 0.1], 1, 100, 1, 1, 9, grids)];
global_data = [global_data; scenario([0.5, 0.1, 0.1], 1, 100, 1, 1, 10, grids)];
global_data = [global_data; scenario([0.1, 0.5, 0.1], 1, 100, 1, 1, 11, grids)];
global_data = [global_data; scenario([0.1, 0.1, 0.5], 1, 100, 1, 1, 12, grids)];

% logistic lognormal, r=.1, K=100
global_data = [global_data; scenario([0.1, 0.1, 0.1], 1, 100, 1, 2, 13, grids)];
global_data = [global_data; scenario([0.5, 0.1, 0.1], 1, 100, 1, 2, 14, grids)];
global_data = [global_data; scenario([0.1, 0.5, 0.1], 1, 100, 1, 2, 15, grids)];
global_data = [global_data; scenario([0.1, 0.1, 0.5], 1, 100, 1, 2, 16, grids)];

% bevholt uniform, r=1, K=100
global_data = [global_data; scenario([0.1, 0.1, 0.1], 1, 100, 1, 1, 17, grids)];
global_data = [global_data; scenario([0.5, 0.1, 0.1], 1, 100, 1, 1, 18, grids)];
global_data = [global_data; scenario([0.1, 0.5, 0.1], 1, 100, 1, 1, 19, grids)];
global_data = [global_data; scenario([0.1, 0.1, 0.5], 1, 100, 1, 1, 20, grids)];

% bevholt lognormal, r=1, K=100
global_data = [global_data; scenario([0.1, 0.1, 0.1], 1, 100, 1, 2, 21, grids)];
global_data = [global_data; scenario([0.5, 0.1, 0.1], 1, 100, 1, 2, 22, grids)];
global_data = [global_data; scenario([0.1, 0.5, 0.1], 1, 100, 1, 2, 23, grids)];
global_data = [global_data; scenario([0.1, 0.1, 0.5], 1, 100, 1, 2, 24, grids)];



%% write the output data
csvwrite('scenarios.csv', global_data)






%% Plot from file -- can be run from here without rerunning the above 
global_data = csvread('scenarios.csv');

colorlines={'b','k--','g.-','r.'};
figure
hold on;
for i = 1:4
  y_grid = global_data(global_data(:,end)==i,1);
  escapement = smooth(y_grid, global_data(global_data(:,end)==i,2));
  plot(y_grid, escapement, colorlines{i})
end
axis([0 120 0 120])
xlabel('Fish Stock')
ylabel('Escapement')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')
legend('boxoff')
plot2svg('scenarios.svg')



%%% Lognormal set 
colorlines={'b','k--','g.-','r.'};
figure
hold on;
for i = 1:4
  j = i+4;
  y_grid = global_data(global_data(:,end)==j,1);
  escapement = smooth(y_grid, global_data(global_data(:,end)==j,2));
  plot(y_grid, escapement, colorlines{i})
end
axis([0 120 0 120])
xlabel('Fish Stock')
ylabel('Escapement')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')
legend('boxoff')
plot2svg('lognormal.svg')




