% Generate a csv file whose columns are:
%
%   escapement, sigma_g, sigma_m, sigma_i, recruitment, noise, id
% 
% Where recruitment: 1 = logistic, 2 = ricker, 3 = beverton-holt
% and   noise: 1 = uniform, 2 = lognormal
% and   id: indicates a unique run
% 
% Other parameters are fixed 
clear;


% Fixed parameters and grids: 
x_grid =  linspace(0,200,151); 
y_grid  = linspace(0,200,151);
h_grid =  linspace(0,100,51);
q_grid  = linspace(0,100,51);
Tmax = 10;
delta = 0.05;
grids = struct('x', x_grid, 'y', y_grid, 'h', h_grid, ...
               'q', q_grid, 'Tmax', Tmax, 'delta', delta);


global_data = []; % initialize variable

%%% Run the scenarios %%%%%%%%%%


% logistic uniform
global_data = [global_data; scenario([0.1, 0.1, 0.1], 1, 1, 1, grids)];
global_data = [global_data; scenario([0.5, 0.1, 0.1], 1, 1, 2, grids)];
global_data = [global_data; scenario([0.1, 0.5, 0.1], 1, 1, 3, grids)];
global_data = [global_data; scenario([0.1, 0.1, 0.5], 1, 1, 4, grids)];

% logistic lognormal
global_data = [global_data; scenario([0.1, 0.1, 0.1], 1, 2, 5, grids)];
global_data = [global_data; scenario([0.5, 0.1, 0.1], 1, 2, 6, grids)];
global_data = [global_data; scenario([0.1, 0.5, 0.1], 1, 2, 7, grids)];
global_data = [global_data; scenario([0.1, 0.1, 0.5], 1, 2, 8, grids)];


%% write the output data
csvwrite('scenarios.csv', global_data)



%% Plot from file -- can be run from here without rerunning the above 
global_data = csvread('scenarios.csv');

colorlines={'b','k--','g.-','r.'};
figure
hold on;
for i = 1:4
  y_grid = global_data(global_data(:,8)==i,1);
  escapement = smooth(y_grid, global_data(global_data(:,8)==i,2));
  plot(y_grid, escapement, colorlines{i})
end
axis([0 120 0 120])
xlabel('Fish Stock')
ylabel('Escapement')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')
%legend('boxoff')
plot2svg('scenarios.svg')



%%% Lognormal set 
colorlines={'b','k--','g.-','r.'};
figure
hold on;
for i = 5:8
  y_grid = global_data(global_data(:,8)==i,1);
  escapement = smooth(y_grid, global_data(global_data(:,8)==i,2));
  plot(y_grid, escapement, colorlines{i})
end
axis([0 120 0 120])
xlabel('Fish Stock')
ylabel('Escapement')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')
%legend('boxoff')
plot2svg('lognormal.svg')




