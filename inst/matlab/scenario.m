%% Define the thing that we'll want to loop over 
function data = scenario(sigma, r, K, recruitment, noise, id, grids)

  %% Select the recruitment function
  if recruitment == 1
    recruit_f=@(x, r, K)  x+r*x.*(1-x/K);
  elseif recruitment == 2 
    recruit_f=@(x, r, K)  (1+r)*x.*exp(-(log(1+r)/K)*x);
  elseif recruitment == 3
    recruit_f=@(x, r, K)  (1+r)*x./(1+r/K*x);

  end 
 
  f = @(x,h) max(recruit_f(x-h, r, K), 0)
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
                [sigma(1), sigma(2), sigma(3), r, K,...
                 recruitment,  noise, id])];
end

