
f=@(x, h) (0.2 * (x-h) * (1 - (x-h) ./ 100) + (x-h));
x_grid = [1:5] * 10;
h_grid = x_grid;
Tmax = 5;
[D, V, M, I, P, Ep, F] =  multiple_uncertainty(f, x_grid, h_grid, Tmax, 0.3, 0., 0., 0.01);



function out = ssum(x,y, a)
  out = (x + y);
end

[X, Y] = meshgrid([1:5], [1:5])
% Works
b = arrayfun(@ssum, X, Y, 1)

% errors
b = arrayfun(@ssum, X, Y, [1:6])
