function [D, V, M, I, P, Ep, F, f_matrix]  =  multiple_uncertainty(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf, y_grid, q_grid)
%' SDP under multiple uncertainty
%'
%' Computes the SDP solution under the case of growth noise, 
%' implementation errors in harvesting, and meaurement errors in 
%' the stock assessment.  
%'
%' @param f the growth function of the escapement population (x-h)
%'   should be a function of f(y, h)
%' @param p the parameters of the growth function
%' @param x_grid the discrete values allowed for the population size, x
%' @param h_grid the discrete values of harvest levels to optimize over
%' @param sigma is the shape parameters for noise distribution (sigma_g, sigma_m, sigma_i) (default is no noise)
%' @return The D matrix giving the optimal harvest for each possible state in each timestep.  
%' Hence the optimal policy at time t is given by the policy function D(,t).  
%' Values of D(,t) correspond to the index of h_grid.  Indices of of D(,t) correspond to states in y_grid.  
%' @export

    %% Store constants 
    n_x = length(x_grid);
    n_h = length(h_grid);
    n_y = length(y_grid);
    n_q = length(q_grid);

    %% D is the decision matrix we will generate in this function.
    %% Rows correspond to states, columns to times
    D = zeros(n_y, Tmax);

    %% Define a profit function.  
    %% For a trivial example, we assume no cost to harvesting, each fish sells for 1 unit.  So can sell as many fish as you harvested: 
    function out = profit(x,y)
      out = bsxfun(@min,x,y); 
    end  



    %% generate various sources of noise, 
    %% or delta fns if noise is zero
    function out = pdfn(p, mu, s, grid, pdf)
      out = 0; % avoid cases in which out is undefined (e.g. mu < 0, s > 0)
      if mu == 0
        out = +(p == 0); % + coerces logical to double
      elseif s > 0
        if mu > 0
           out = pdf(p, mu, s);
%          out = unifpdf(p, mu .* (1 - s), mu .* (1 + s)); 
%          out = lognpdf(p ./ mu, 0, s);
        end
      else  % delta spike if s = 0
        out = +isequal(histc(mu,grid), histc(p, grid)); % + coerces logical to double
      end
    end



    %% Should this pile density on the boundary instead? 
    function out = norm1r(M)
       % Find all-zero rows and normalize to 1
       ind = find(sum(M, 2) == 0);
       M(ind,1) = 1;
       % row normalize
       if(all(sum(M,2) > 0))
         out = bsxfun(@rdivide, M, sum(M, 2));
       else 
         out;
       end 
    end
    
%% GENERATE THE PROBABILITY MATRICES DEFINED IN %%%%%  
%% http://www.carlboettiger.info/2012/11/01/multiple-uncertainty-corrections.html 


    % P profit expected from (true) stock x given (true harvest) y
    [X, H] = meshgrid(x_grid, h_grid); 
    P = profit(X, H)';
   
    % M is a matrix of the probability of being in observed state Y given the true 
    % state X.  We represent both as discrete values in x_grid
    % Normalized such that y = M*x, the probability of observing over the possibly y's given a normalized probability in being state x is still normalized.  
    [Y,X] = meshgrid(y_grid, x_grid); 
    M = norm1r(arrayfun(@(x,y) pdfn(x, y, sigma_m, x_grid, pdf), Y, X))';

    % I is a matrix of the probability of implementing a harvest H given 
    % a quota set to Q.  We represent both as discrete values in h_grid
    % Normalized such that a quota q is implemented as a normalized probability density
    % of harvests h; I*q = h, sum(h) == 1
    [H,Q] = meshgrid(h_grid, q_grid); 
    I = norm1r(arrayfun(@(x,y) pdfn(x, y, sigma_i, h_grid, pdf), Q, H))';

    % f_matrix is a matrix whose element i,j tells us the expected 
    % stock size next year given current stock size of x_grid[i] and harvest of h_grid[j] 
    [X,H] = meshgrid(x_grid, h_grid);
    f_matrix = arrayfun(f, X, H)';
  
    %% Calculate F   
    F = zeros(n_y, n_x, n_q);
    for q = 1:n_q
      for y = 1:n_y
        mu = M(y,:) * f_matrix * I(:,q); % mean transition rate 
        F(y,:,q) = norm1r(arrayfun( @(x) pdfn(x, mu, sigma_g, x_grid, pdf), x_grid));
      end 
    end

    % The profit expected for a given action and state reflect 
    % the uncertainty in implementation of the action and 
    % measurement of the state

    Ep = M * P * I;   % expected profit (from space x,h -> y,q)   
    V = Ep;           % Initialize  

    for t = 1:Tmax

      %% Enforce no harvest greater than assessed stock size
      V = tril(V); % Use only lower triangle, upper set to 0.

      %% Maximize the value-to-go
      [v_t, v_index] = max(V, [], 2);  
      % For multiple matches, gives smallest index to match

      D(:, (Tmax - t + 1)) = v_index;

      for j = 1:n_q
        if sigma_g == 0 %% Then f_matrix takes us off-grid to where we don't know the value
          v_t_interp = interp1(y_grid, v_t, f_matrix(:,j));
          V(:,j) = Ep(:,j) + 1 / (1 + delta) * M' * v_t_interp;
        else 
          V(:,j) = Ep(:,j) + 1 / (1 + delta) * F(:, :, j) * M' * v_t;
        end
      end
    end
end


