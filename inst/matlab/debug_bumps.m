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


sigma_m = .5
pdf = @(p,mu,s) unifpdf(p, mu .* (1 - s), mu .* (1 + s)); 
x_grid = linspace(0,200,401);
y_grid  = linspace(0,200,401);
[Y,X] = meshgrid(y_grid, x_grid); 
array = arrayfun(@(x,y) pdfn(x, y, sigma_m, x_grid, pdf), Y, X);


pile = norm_pile_bdry(array(1:100, 1:100))';
sum(pile,1)
sum(pile,2)



M = norm1r(array(1:100, 1:100))';
m = M(1:50, 1:50); % boundary a real problem still...
sum(m,2)
sum(m,1)
