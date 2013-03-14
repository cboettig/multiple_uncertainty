function [D, V] = example()

  f=@(x, h) (0.2 * (x-h) * (1 - (x-h) ./ 100) + (x-h));

  x_grid = [1:15] * 10;
  h_grid = x_grid;
  Tmax = 5;
  [D, V] =  multiple_uncertainty(f, x_grid, h_grid, Tmax, 0.3, 0.3, 0.3);
end
