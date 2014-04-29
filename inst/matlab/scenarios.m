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
x_grid =  linspace(0,150,31); 
h_grid =  linspace(0,150,31);
y_grid  = linspace(0,150,31);
q_grid  = linspace(0,150,31);
Tmax = 10;
delta = 0.05;
grids = struct('x', x_grid, 'y', y_grid, 'h', h_grid, 'q', q_grid, 'Tmax', Tmax, 'delta', delta)



global_data = []; % initialize variable
%% Define the thing that we'll want to loop over 
function data = scenario(id, sigma, noise, recruitment, grids)

  %% FIXME consider varying growth rate "r" as well 

  %% Select the recruitment function
  if recruitment == 1
    f=@(x, h) max( (x-h) * (1 - (x-h) ./ 100) + (x-h), 0);
  elseif recruitment == 2 

  elseif recruitment == 3

  end 
  
  %% Select the noise function
  if noise == 1
    pdf = @(p,mu,s) unifpdf(p, mu .* (1 - s), mu .* (1 + s)); 
  elseif noise == 2
    pdf = @(p,mu,s) lognpdf(p ./ mu, 0, s);
  end

  %% do the analysis
  [D, V, M, I, P, Ep, F, f_matrix] =  multiple_uncertainty(f, grids.x, grids.h, grids.Tmax, sigma(1), sigma(2), sigma(3), grids.delta, pdf, grids.y, grids.q);

  %% Summarize the results in a table (matrix)
  escapement = grids.y - grids.q(D(:,1));
  data = [escapement', bsxfun(@times,  ones(length(escapement),1), [sigma(1), sigma(2), sigma(3), recruitment,  noise, id])];
end




%%% Run the scenarios

global_data = [global_data; scenario(1, [0.5, 0.1, 0.1], 1, 1, grids)]
global_data = [global_data; scenario(2, [0.1, 0.5, 0.1], 1, 1, grids)]
global_data = [global_data; scenario(3, [0.1, 0.1, 0.5], 1, 1, grids)]



%% write the output data
csvwrite("escapement.csv", global_data)




