%% Plot from file -- can be run from here without rerunning the above 
global_data = csvread('scenarios.csv');

colorlines={'b','k--','g.-','r.'};
figure
hold on;
for i = 1:4
  y_grid = global_data(global_data(:,end)==i,1);
  escapement = smooth(global_data(global_data(:,end)==i,2), 15);
  plot(y_grid, escapement, colorlines{i})
end
axis([0 120 0 120])
xlabel('Fish Stock')
ylabel('Escapement')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')
legend('boxoff')
plot2svg('scenarios.svg')


%%% Lognormal set 
figure
hold on;
for i = 1:4
  j = i+4;
  y_grid = global_data(global_data(:,end)==j,1);
  escapement = global_data(global_data(:,end)==j,2);
  plot(y_grid, escapement, colorlines{i})
end
axis([0 120 0 120])
xlabel('Fish Stock')
ylabel('Escapement')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')
legend('boxoff')
plot2svg('lognormal.svg')


%% logistic uniform small r
figure
hold on;
for i = 1:4
  j = i+8;
  y_grid = global_data(global_data(:,end)==j,1);
  escapement = smooth(global_data(global_data(:,end)==j,2), 15);
  plot(y_grid, escapement, colorlines{i})
end
axis([0 120 0 120])
xlabel('Fish Stock')
ylabel('Escapement')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')
legend('boxoff')
plot2svg('logistic_unif_small_r.svg')

%% logistic lognormal small r
figure
hold on;
for i = 1:4
  j = i+12;
  y_grid = global_data(global_data(:,end)==j,1);
  escapement = global_data(global_data(:,end)==j,2);
  plot(y_grid, escapement, colorlines{i})
end
axis([0 120 0 120])
xlabel('Fish Stock')
ylabel('Escapement')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')
legend('boxoff')
plot2svg('logistic_lognormal_small_r.svg')




%%% Bevholt  
figure
hold on;
for i = 1:4
  j = i+16;
  y_grid = global_data(global_data(:,end)==j,1);
  escapement = smooth(global_data(global_data(:,end)==j,2), 15);
  plot(y_grid, escapement, colorlines{i})
end
axis([0 120 0 120])
xlabel('Fish Stock')
ylabel('Escapement')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')
legend('boxoff')
plot2svg('bevholt_uniform.svg')

%%% Bevholt Lognormal 
figure
hold on;
for i = 1:4
  j = i+20;
  y_grid = global_data(global_data(:,end)==j,1);
  escapement = global_data(global_data(:,end)==j,2);
  plot(y_grid, escapement, colorlines{i})
end
axis([0 120 0 120])
xlabel('Fish Stock')
ylabel('Escapement')
legend('All Low','Large Growth','Large Measurement','Large  Implementation')
legend('boxoff')
plot2svg('bevholt_lognormal.svg')


