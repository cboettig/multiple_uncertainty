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
legend('boxoff')
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
legend('boxoff')
plot2svg('lognormal.svg')



