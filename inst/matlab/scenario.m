%% Define the thing that we'll want to loop over 
function data = scenario(sigma, recruitment, noise, id, grids)

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
  data = [grids.y', escapement', ... 
         bsxfun(@times,  ones(length(escapement),1), ... 
                [sigma(1), sigma(2), sigma(3), recruitment,  noise, id])];
end

