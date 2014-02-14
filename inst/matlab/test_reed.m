

% Define the mean dynamics of the state equation as function of the state x and action we take h
% The actual transition will be a uniform probability interval of size x*sigma centered on this value.  
f = @(x, h) max( max((x - h), 0) * (1 - max((x - h),0) ./ 100) + max((x - h),0), 0);

% (The max conditions to avoid negative values probably aren't necessary).  
% Alternative state equations.  Eventually we will want to choose between a few 
% different equations for f. %f = @(x, h) max( max((x - h),0) * exp(1.1 * (1 - max((x - h),0) ./ K)), 0) ;
%f = @(x, h) max( 2 * max((x - h),0) / (1 + 4 * max((x - h),0)), 0); % K = 0.25


% Define the state space.  Here we a really coarse grid so things run quickly  
x_grid = [0:5:150];

% Define the action space.  For simplicity, we just make it the same as the state space 
h_grid = x_grid;

% 
Tmax = 10;

% Define the noise parameter
sigma_g = 0.2;

% Define the discount rate.  
delta = 0.05;

% We'll be using a trivial profit function hardwired into the code for the
% moment: the amount of fish harvested, min(h,x) since we can't harvest
% more fish than there are.


%% Here we go, solve the simple SDP 
[D, V, P, f_matrix] =  reed(f, x_grid, h_grid, Tmax, sigma_g, delta);

% Plot the resulting policy at T=1 in terms of escapement (x - h)
plot(x_grid, x_grid - x_grid(D(:,1)), '.-')
ylim([0 100])
xlim([0 150])


