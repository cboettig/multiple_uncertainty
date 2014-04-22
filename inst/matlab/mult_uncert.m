function [D, V, M, I, P, Ep, F, f_matrix]  =  mult_uncert(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, pdf, y_grid, q_grid)
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


    % Store grid sizes as constants (for looping)
    n_x = length(x_grid);
    n_h = length(h_grid);
    n_y = length(y_grid);
    n_q = length(q_grid);

    %% D is the decision matrix we will generate in this function. 
    %% Rows correspond to states, columns to times.  
    %% D is defined in the measured stock space, y.  
    D = zeros(n_y, Tmax);

    %% Define a profit function.  
    %% For a trivial example, we assume no cost to harvesting, each fish 
    %% sells for 1 unit.  So can sell as many fish as you harvested: 
    function out = profit(x,y)
      out = bsxfun(@min,x,y); 
    end  


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

    %% generate various sources of noise, or delta fns if noise is zero
    %% More generally this function need not be the same for each uncertainty source
    %% grid given to place delta spike
    function out = pdfn(p, mu, s, grid, pdf)
      out = 0; % avoid cases in which out is undefined (e.g. mu < 0, s > 0)
      if mu == 0
        out = +(p == 0); % + coerces logical to double
      elseif s > 0
        if mu > 0
           out = pdf(p, mu, s);
        end
      else  % delta spike if s = 0
        out = +isequal(histc(mu,grid), histc(p, grid)); % + coerces logical to double
      end
    end


% GENERATE THE PROBABILITY MATRICES DEFINED IN % 
% http://www.carlboettiger.info/2012/11/01/multiple-uncertainty-corrections.html %

    % P is the profit expected from (true) stock x given (true harvest) y
    [X, H] = meshgrid(x_grid, h_grid); 
    P = profit(X, H)';

    % M is a matrix of the probability of being in observed state Y given the true 
    % state X.  We represent both as discrete values in x_grid
    [Y,X] = meshgrid(y_grid, x_grid); 
    M = norm1r(arrayfun(@(x,y) pdfn(x, y, sigma_m, x_grid, pdf), Y, X))';

    % I is a matrix of the probability of implementing a harvest H given 
    % a quota set to Q.  We represent both as discrete values in h_grid
    [H,Q] = meshgrid(h_grid, q_grid); 
    I = norm1r(arrayfun(@(x,y) pdfn(x, y, sigma_i, h_grid, pdf), Q, H))';

    % G is a matrix of the probability of having stock x_{t+1} given x_t
    [X,X] = meshgrid(x_grid, x_grid);
    G = norm1r(arrayfun(@(x,y) pdfn(x, y, sigma_g, x_grid, pdf), X, X))';


    % f_matrix is a matrix whose element i,j tells us the expected 
    % stock size next year given current stock size of x_grid[i] and harvest of h_grid[j] 
    [X,H] = meshgrid(x_grid, h_grid);
    f_matrix = arrayfun(f, X, H)';
  
    %% Calculate F: defined in transitions in the measured space y and implemented quota q
    F = zeros(n_y, n_x, n_q);
    for q = 1:n_q
      for y = 1:n_y
        mu = M(y,:) * f_matrix * I(:,q); % mean transition rate 
        out = arrayfun( @(x) pdfn(x, mu, sigma_g, x_grid, pdf), x_grid);
        if(sum(out) > 0)
          F(y,:,q) = out / sum(out); % as rows 
        else 
          F(y,:,q) = [1, zeros(1, length(out)-1)]; 
        end
      end 
    end

    % The profit expected for a given action and state reflect 
    % the uncertainty in implementation of the action and measurement of the state
    Ep = M * P * I;   
    V = Ep; % Initialize.  Ep & V in y_grid by q_grid space
    for t = 1:Tmax
      [v_t, v_index] = max(V, [], 2);  % how does this handle multiple matches?  Gives smallest index to match (just like R)
      % Note that matlab calls this dimension 2, whereas in R, `apply` calls it dimension 1
      D(:, (Tmax - t + 1)) = v_index;
      for j = 1:n_q
        if sigma_g == 0 %% Then X_t+1 is off-grid where we don't know the value
          v_t_interp = interp1(x_grid, v_t, f_matrix(:,j));
          V(:,j) = Ep(:,j) + 1 / (1 + delta) * M * v_t_interp;
        else 
          V(:,j) = Ep(:,j) + 1 / (1 + delta) * M * F(:, :, j)' * v_t;
        end
      end
    end
end


