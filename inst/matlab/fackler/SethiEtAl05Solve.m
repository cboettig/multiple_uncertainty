% SethEtAl05Solve 
% Author: Paul Fackler
% April 2014 



% SethiEtAl05Solve Solves a fisheries management problem
% USAGE
%   [model,results]=SethiEtAl05Solve(GF,r,K,sigg,sigi,sigm,dr,ytype, ...
%                                    gtype,itype,mtype,na,nq,ng,ni,nm,Smax);
% INPUTS
%   GF    : growth function: 
%             'l' logistic      - x+r*x.*(1-x/K)
%             'r' Ricker        - (1+r)*x.*exp(-(log(1+r)/K)*x)
%             'b' Beverton-Holt - (1+r)*x./(1+r/K*x)
%   r     : intrinsic growth rate
%   K     : carrying capacity
%   sigg  : stock noise standard deviation
%   sigi  : harvest noise standard deviation
%   sigm  : assessment noise standard deviation
%   dr    : discount rate   
%   ytype : stock/assessment relationship: 0) S=Y*e  1) Y=S*e
%   gtype : growth noise distribution 1) uniform 2) Gamma 3) lognormal
%   itype : harvest noise distribution 1) uniform 2) Gamma 3) lognormal
%   mtype : measurement noise distribution 1) uniform 2) Gamma 3) lognormal
%   na    : number of assessment values
%   nq    : number of quota values
%   ng    : number of growth noise values
%   ni    : number of harvest noise values
%   nm    : number of measurement noise values
%   Smax  : maximum value of the assessed stock
% OUTPUTS
%   model   : an MDPSolve model structure
%   results : an MDPSolve results structure
%
% Based on:
% Fishery management under multiple uncertainty
% Gautam Sethi, Christopher Costello, Anthony Fisher, Michael Hanemann, Larry Karp
% Journal of Environmental Economics and Management 50 (2005) 300-318

function [model,results]=SethiEtAl05Solve(GF,r,K,sigg,sigi,sigm,dr,ytype, ...
                           gtype,itype,mtype,na,nq,ng,ni,nm,Smax)

switch lower(GF(1))
  case 'l'
    G = @(x) x+r*x.*(1-x/K);                 % logistic growth function
  case 'b'
    G = @(x) (1+r)*x./(1+r/K*x);             % Beverton-Holt growth function
  case 'r'
    G = @(x) (1+r)*x.*exp(-(log(1+r)/K)*x);  % Ricker growth function
end

  
harv=@(S,Q,v) min(S,v*Q);  % harvest function

% Y=Se or S=Ye
if ytype==1
  g=@(X,e) G(X(:,1)./e(:,3)-harv(X(:,1)./e(:,3),X(:,2),e(:,2))).*(e(:,1).*e(:,4));
else
  g=@(X,e) G(X(:,1).*e(:,3)-harv(X(:,1).*e(:,3),X(:,2),e(:,2))).*(e(:,1)./e(:,4));
end
delta=1/(1+dr);

[zg,pg]=unitshocks(ng,sigg,gtype);
[zi,pi]=unitshocks(ni,sigi,itype);
[zm,pm]=unitshocks(nm,sigm,mtype);

ee=rectgrid(zg,zi,zm,zm);
pp=rectgrid(pg,pi,pm,pm); pp=prod(pp,2); pp=pp/sum(pp);

A=linspace(0,Smax,na)';   % possible assessment values
Q=linspace(0,Smax,nq)';   % possible quota values                               %% No need to assume quota between 0 & Smax
X=rectgrid(A,Q);          % Single grid of assessment values, X(:,1) and quota values X(:,2)

X=X(X(:,2)<=X(:,1),:);    % eliminate quotas that are greater than assessments from the search space  %% A-HA! 
Ix=getI(X,1);             % Huh?  

% transition matrix
options=struct('cleanup',2,'rectinterp',1,'expande',0);
P=g2P(g,A,X,ee,pp,options);
P=mxv(P,1./sum(P));  % improve normalization

% A vector of rewards for all possible state-action pairs 
R=0;
for i=1:length(zi)      % Grid for implementation error
  for j=1:length(zm)    % Grid for measurment error 
    R=R + harv(X(:,1) / zm(j), X(:,2), zi(i)) * pm(j) * pi(i);
  end
end

% \sum_i \sum_j min(Assessed_stock / measurement_grid_j,  Quota * implementation_grid_i)   * measurement_shock_j * implementation_shock_i



model=struct('P',P,'R',R,'d',delta,'X',X,'Ix',Ix);
options=struct('colstoch',1);
results=mdpsolve(model,options);


% computes discrete shock distributions (nodes and weights)
% n    : number of shocks
% sig  : shock standard deviation
% type : 1: uniform, 2: Gamma, 3: lognormal
function [x,w]=unitshocks(n,sig,type)
if sig==0, x=1; w=1; return; end
switch type
  case 1  % uniform
    mu=sqrt(3)*sig;
    %[x,w]=qnwlege(n,1-mu,1+mu);  w=w/sum(w);
    [x,w]=qnwbeta(n,1,1); x=(1-mu)+2*mu*x;
  case 2  % Gamma
    [x,w]=qnwgamma(n,1/sig^2,sig^2); 
  case 3  % lognormal
    sigma2=log(1+sig)^2;
    [x,w]=qnwnorm(n,-sigma2/2,sigma2);
    x=exp(x);
end
