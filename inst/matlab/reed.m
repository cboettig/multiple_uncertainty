function [D, V, P, G, f_matrix]  =  reed(f, x_grid, h_grid, Tmax, sigma_g, delta)
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
%' @param sigma_g is the shape parameters for noise distribution 
%' @return The D matrix giving the optimal harvest for each possible state in each timestep.  
%' Hence the optimal policy at time t is given by the policy function D(,t).  
%' Values of D(,t) correspond to the index of h_grid.  Indices of of D(,t) correspond to states in y_grid.  
%' @export

    %%%%%%%%%%%%% Helper functions and subroutines %%%%%%%%%%%%%%%%%%%%%%%%%    

    %% Define a profit function.  
    %% For a trivial example, we assume no cost to harvesting, each fish sells for 1 unit.  
    %% So can sell as many fish as you harvested: 
    function out = profit(x,y)
      out = bsxfun(@min,x,y); 
    end  

    %% Compute the probability density on a discrete grid %%
    %%   (There must be a cleaner way to handle this.)
    function out = snap_to_grid(x, grid)
      [v,i] = min((grid - x).^2);
      out = grid(i);  
    end


    %% generate various sources of noise, or delta fns if noise is zero
    function out = pdfn(p, mu, s, grid)
      if mu == 0
        out = (p == 0);
      elseif s > 0
        if mu > 0
          out = unifpdf(p, mu .* (1 - s), mu .* (1 + s)); % Could use lognpdf for lognormal
        end
      else  % delta spike if s = 0
        out = isequal(histc(mu,grid), histc(p, grid));
      end
    end


    % Define a row normalize function for matrices 
    function out = norm1r(M)
       out = bsxfun(@rdivide, M, sum(M, 2));
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %%%%%%%%%%% Main Routine %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
    % Just rename some stuff
    n_x = length(x_grid);
    n_h = length(h_grid);

    % D is the decision matrix we will generate in this function. 
    % Rows correspond to states, columns to times
    D = zeros(length(x_grid), Tmax);

    % P is the profit expected from (true) stock x given (true harvest) y
    [X, H] = meshgrid(x_grid, h_grid); 
    P = profit(X, H);

    % f_matrix is a matrix whose element i,j tells us the expected 
    % stock size next year given current stock size of x_grid[i] and harvest of h_grid[j] 
    [X,H] = meshgrid(x_grid, h_grid);
    f_matrix = arrayfun(f, H, X);
   
    % G is a matrix of growth noise, the probability of being at state y given
    [X,Y] = meshgrid(x_grid, x_grid);
    G = norm1r(arrayfun(@(x,y) pdfn(x, y, sigma_g, x_grid), Y, X));
    V = P; % Initialize the value

    %% Perform the dynamic programming algorithm / Bellman iteration loop
    for t = 1:Tmax
      [v_t, v_index] = max(V, [], 2);  % how does this handle multiple matches?  Gives smallest index to match (just like R)
      % Note that matlab calls this dimension 2, whereas in R, `apply` calls it dimension 1
      D(:, (Tmax - t + 1)) = v_index;
      for k = 1:n_h
        v_t_interp = interp1(x_grid, v_t, f_matrix(:,k));
        V(:,k) = P(:,k) + (1-delta) * G * v_t_interp;
      end
    end
end

