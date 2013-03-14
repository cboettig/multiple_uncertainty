%' SDP under multiple uncertainty
%'
%' Computes the SDP solution under the case of growth noise, 
%' implementation errors in harvesting, and meaurement errors in 
%' the stock assessment.  
%'
%' @param f the growth function of the escapement population (x-h)
%'   should be a function of f(t, y, p), with parameters p
%' @param p the parameters of the growth function
%' @param x_grid the discrete values allowed for the population size, x
%' @param h_grid the discrete values of harvest levels to optimize over
%' @param sigma is the shape parameters for noise distribution (sigma_g, sigma_m, sigma_i) (default is no noise)
%' @return The D matrix giving the optimal harvest for each possible state in each timestep.  
%' Hence the optimal policy at time t is given by the policy function D[,t].  
%' Values of D[,t] correspond to the index of h_grid.  Indices of of D[,t] correspond to states in y_grid.  
%' @export

function [D, V] =  SDP_multiple_uncertainty(f, p, x_grid, h_grid, Tmax, sigmas)

    % Just rename some stuff
    sigma_g = sigmas(1);     
    sigma_m = sigmas(2);
    sigma_i = sigmas(3);
    n_x = length(x_grid);
    n_h = length(h_grid);

    % D is the decision matrix we will generate in this function. Rows correspond to states, columns to times
    D = zeros(length(x_grid), Tmax);

    %% Define a profit function.  
    %% For a trivial example, we assume no cost to harvesting, each fish sells for 1 unit.  So can sell as many fish as you harvested: 
    function out = profit(x,y)
      out = min(x,y); 
    end  


    %% binning routine (vectorized)
    function out = snap_to_grid(x, grid)
      [v,i] = min(abs(grid-x));
      out = grid(i);  
    end
     
    %% generate various sources of noise, or delta fns if noise is zero 
    function out = pdfn(P, mu, s)
      if mu == 0
        out = (P == 0);
      else if s > 0
        if mu > 0
          %dlnorm(P/mu, 0, s)
          out = unifrnd(P, mu * (1 - s), mu * (1 + s));
        end
      else  % delta spike
        P = snap_to_grid(P, x_grid);
        mu = snap_to_grid(mu, x_grid);
        out = (P == mu);
      end
    end

    %%% GENERATE THE PROBABILITY MATRICES DEFINED IN http://www.carlboettiger.info/2012/11/01/multiple-uncertainty-corrections.html %%%%%

    % P is the profit expected from (true) stock x given (true harvest) y
    [X, H] = meshgrid(x_grid, h_grid); 
    P = arrayfun(@profit, X, H);

    % M is a matrix of the probability of being in observed state Y given the true state X.  We represent both as discrete values in x_grid
    % More generally, observed and true states may be discritized to different grids, 
    % more gemerally the pdfn could take different forms in growth, measurement, and implementation noise G, M, I matrices.  
    [X,Y] = meshgrid(x_grid, x_grid); 
    M = arrayfun(@pdfn, X, Y, sigma_m);

    % I is a matrix of the probability of implementing a harvest H given 
    % a quota set to Q.  We represent both as discrete values in h_grid
    [H,Q] = meshgrid(h_grid, h_grid); 
    I = arrayfun(@pdfn, H, Q, sigma_i);

    % f_matrix is a matrix whose element i,j tells us the expected 
    % stock size next year given current stock size of x_grid[i] and harvest of h_grid[j] 
    [X,H] = meshgrid(x_grid, h_grid);
    f_matrix = arrayfun(@f, X, H);
   
    % G is a matrix of growth noise, the probability of being at state y given
    [X,Y] = meshgrid(x_grid, x_grid);
    G = arrayfun(@pdfn, X, Y, sigma_g);

    %% Calculate F   
    F = zeros(n_x, n_x, n_h);
    for q = 1:n_h
      for y = 1:n_x
        out = zeros(n_x,1);
        mu = M(y,:) * f_matrix * I(q,:); % mean transition rate 
        %%  Handle special cases
        if snap_to_grid(mu,x_grid) == 0;  % if we transition to zero, 
          out(1) = 1;  % probability of going to the zeroth state, (x_grid[1]) is unity
       else 
         [val, index] = min(abs(x_grid - mu)); %% snap mu to grid first
         out = G(:, index);   %% then do this by table look-up
        end
      F(:,y,q) = out / sum(out);
      end 
    end
    

    % The profit expected for a given action and state reflect 
    % the uncertainty in implementation of the action and measurement of the state
    Ep <- M * P * I';          % matrix multiplications
    V = Ep % Initialize  
    for t = 1:Tmax
      [v_t, v_index] = max(V, [], 1); 
      D(:, (Tmax - t + 1)) = v_index;
      for j = 1:n_h
        V(:,j) = Ep(:,j) + (1-delta) * M * F(:, :, j) * v_t;
      end
    end
    %% Returns the policy decision matrix D.  Sometimes the value V associated with the optimal decision is also of interest
    D;
end


  %% Sidenote: q can often exceed y: if fishing is free, there might be more x than you think.  In such cases it is worth attempting to fish extra, and we shouldn't exert q < y. 



