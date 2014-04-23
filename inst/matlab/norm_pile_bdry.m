function out = norm_pile_bdry(M)
  % Find all-zero rows and normalize to 1
  ind = find(sum(M, 2) == 0);
  M(ind,1) = 1;

  N = max(sum(M,2)); % Normalization factor
  M = M/N;

  errors = sum(M,2) > 1 || sum(M,2) < 0;
  if(any(errors))
    warning("matrix may not be probabilities")
  end

  missing = (sum(M,2) < 1);
  missing_weight = 1 - sum(M,2);
  Lhand = M(missing,1) > 0;
  Rhand = M(missing, end) > 0;

  if(any(Lhand & Rhand))
    warning("renormalizing both boundaries, consider finer grid")
  % FIXME do something for this case. Split evenly? 
  % (very rare case, but in reality would need pdfn) 
  end


  M(Lhand, 1) = M(Lhand, 1) + missing_weight(Lhand);
  M(Rhand, end) = M(Rhand, end) + missing_weight(Rhand);

  if(any(sum(M,2) != 1))
    warning("matrix may not be normalized")
  end

  out = M;
end

