function [D, V, M, I, P, Ep, F, f_matrix,iter,ESC]  =  multiple_uncertainty_rev(f, x_grid, h_grid, Tmax, sigma_g, sigma_m, sigma_i, delta, Pdf)
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

% Holding for when I run value function iteration
    iter=1;
    ESC = 1; 
    
    % Just rename some stuff
    n_x = length(x_grid);
    n_h = length(h_grid);

    % D is the decision matrix we will generate in this function. Rows correspond to states, columns to times
    D = zeros(length(x_grid), Tmax);

    %% Define a profit function.  
    %% For a trivial example, we assume no cost to harvesting, each fish sells for 1 unit.  So can sell as many fish as you harvested: 
    function out = profit(x,y)
        out = bsxfun(@min,x,y); % this picks out the min of X or H because H can never be > x and assumes p=1, c=0.
%       % This does a more complicated profit function
        %h = bsxfun(@min,x,y); p=1; c=.5;  % Sethi used c=5 in paper
        %out = p*h - c./x;
    end  


    %% generate various sources of noise, or delta fns if noise is zero
    function out = pdfn(p, mu, s, grid, Pdf)
      if mu == 0
        out = +(p == 0);
      elseif s > 0
        if mu > 0
           out = Pdf(p, mu, s);
%          out = unifpdf(p, mu .* (1 - s), mu .* (1 + s)); 
%          out = lognpdf(p ./ mu, 0, s);
        end
      else  % delta spike if s = 0
        out = +isequal(histc(mu,grid), histc(p, grid));
      end
    end


    %%% GENERATE THE PROBABILITY MATRICES DEFINED IN                                    %%%%%  
    %%%% http://www.carlboettiger.info/2012/11/01/multiple-uncertainty-corrections.html %%%%%

    % More generally: Note that the  observed and true states may be discritized to different grids, 
    % and the pdfn could take different forms in growth, measurement, and implementation noise G, M, I matrices.  

    % P is the profit expected from (true) stock x given (true harvest) y
    [X, H] = meshgrid(x_grid, h_grid); 
    P = profit(X, H);

    function out = norm1r(M)
       out = bsxfun(@rdivide, M, sum(M, 2));
%      out = M ./ sum(M,2); %% this notation works on OCTAVE but not MATLAB
    end
    
    % M is a matrix of the probability of being in observed state Y given the true 
    % state X.  We represent both as discrete values in x_grid
    [X,Y] = meshgrid(x_grid, x_grid); 
    M = norm1r(arrayfun(@(x,y) pdfn(x, y, sigma_m, x_grid, Pdf), Y, X));

    % I is a matrix of the probability of implementing a harvest H given 
    % a quota set to Q.  We represent both as discrete values in h_grid
    [H,Q] = meshgrid(h_grid, h_grid); 
    I = norm1r(arrayfun(@(x,y) pdfn(x, y, sigma_i, h_grid, Pdf), Q, H));

    % f_matrix is a matrix whose element i,j tells us the expected 
    % stock size next year given current stock size of x_grid[i] and harvest of h_grid[j] 
    [X,H] = meshgrid(x_grid, h_grid);
    f_matrix = arrayfun(f, H, X);
  
    %% Calculate F   
    F = zeros(n_x, n_x, n_h);
    for q = 1:n_h
      for y = 1:n_x
        mu = M(y,:) * f_matrix * I(q,:)'; % mean transition rate %% FIXME Seems to require h_grid dimension is same as x_grid dimension??  
        out = arrayfun( @(x) pdfn(x, mu, sigma_g, x_grid, Pdf), x_grid); 
        F(y,:,q) = out / sum(out); % as rows 
      end 
    end

    % The profit expected for a given action and state reflect 
    % the uncertainty in implementation of the action and measurement of the state
    Ep = M * P * I';          % matrix multiplications
    V = Ep; % Initialize  
    for t = 1:Tmax
      [v_t, v_index] = max(V, [], 2);  % how does this handle multiple matches?  Gives smallest index to match (just like R)
      % Note that matlab calls this dimension 2, whereas in R, `apply` calls it dimension 1
      D(:, (Tmax - t + 1)) = v_index;
      for j = 1:n_h
        if sigma_g == 0 %% Then f_matrix takes us off-grid to where we don't know the value
          v_t_interp = interp1(x_grid, v_t, f_matrix(:,j));
          V(:,j) = Ep(:,j) + (1 / (1 + delta)) * M * v_t_interp;
        else
          %v_t_interp = interp1(x_grid, v_t, f_matrix(:,j));
          V(:,j) = Ep(:,j) + (1 / (1 + delta))* M * F(:, :, j) * v_t;
        end
      end
    end
end

% Changes to run value function convergence
% Ep = M * P * I';          % matrix multiplications
%     V = Ep; % Initialize 
%     % v_t=Ep(:,1);
%      VError=10; 
%      TOL=1e-3; % Same  as Sethi
%      vv=zeros(1,n_h)';
%      iter=1;
%     for t = 1:Tmax
%     %while VError>TOL  
%       [v_t, v_index] = max(V,[], 2);  % how does this handle multiple matches?  Gives smallest index to match (just like R)
%       
%       %[v_t, v_index] = max(Ep + (1/(1+delta)) * M * F(:, :, 5)' * repmat(v_t, 1,n_h));   
%       % Note that matlab calls this dimension 2, whereas in R, `apply` calls it dimension 1
%       D(:, (Tmax - t + 1)) = v_index;
%       ESC(:,iter)=x_grid-x_grid(v_index); 
%       %D=v_index;
% %       plot(x_grid,x_grid-x_grid(D))
% %       hold on;
%       
%       %keyboard
%       for j = 1:n_h
%           
%          v_t_interp = interp1(x_grid, v_t, f_matrix(:,j)); %,'V5cubic');
% 
%         %% Interpolation looks up the value of the corresponding
%         %% f(x_t,h) transition starting at each x_t. 
%         V(:,j) = Ep(:,j) + (1/(1+delta)) * v_t_interp;
%       %end 
%        %  V(:,j) = Ep(:,j) + (1/(1+delta)) * M * F(:, :, j)'* v_t;        
%        end
%          %V=interp1(x_grid,V,x_grid,'pchip');
%       %keyboardsize
%       VError=max(abs((v_t-vv)./vv));
%       vv=v_t; %v_t=v_t';
%       iter=iter+1;
%         if iter>500, break; end
%     end
%     %% Returns the policy decision matrix D.  Sometimes the value V associated with the optimal decision is also of interest
%     D; % V=v_t;
