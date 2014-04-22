% Fishery management under multiple uncertainty
% Gautam Sethi, Christopher Costello, Anthony Fisher, Michael Hanemann, Larry Karp
% Journal of Environmental Economics and Management 50 (2005) 300–318
clc

lo=0.0577;
hi=0.2887;

GF='l';
r=1;       % intrinsic growth rate
K=100;     % carrying capacity
sigg=lo;   % stock noise standard deviation
sigi=lo;   % harvest noise standard deviation
sigm=lo;   % assessment noise standard deviation
dr=0.05;   % discount rate   
ytype=1;   % 0) S=Y*e  1) Y=S*e
% noise distributions 1: uniform 2: Gamma 3: lognormal
gtype=1;   % growth noise
itype=1;   % harvest noise
mtype=1;   % assessment noise

% discretization parameters
na=89;      % number of assessment values
nq=177;      % number of quota values
ng=7;       % number of growth noise values
ni=7;       % number of harvest noise values
nm=11;       % number of measurement noise values
Smax=K*1.76; % maximum value of the assessed stock

% all low uncertainty
[model,results]=SethiEtAl05Solve(GF,r,K,sigg,sigi,sigm,dr,ytype, ...
                                 gtype,itype,mtype,na,nq,ng,ni,nm,Smax);
XX=model.X(results.Ixopt,:);

figure(1); 
plot(XX(:,1),XX(:,1)-XX(:,2),'k-','LineWidth',2)
hold on

% high growth uncertainty
[model,results]=SethiEtAl05Solve(GF,r,K,hi,sigi,sigm,dr,ytype, ...
                                 gtype,itype,mtype,na,nq,ng,ni,nm,Smax);
XX=model.X(results.Ixopt,:);
plot(XX(:,1),XX(:,1)-XX(:,2),'k--','LineWidth',2)

% high measurement uncertainty
[model,results]=SethiEtAl05Solve(GF,r,K,sigg,sigi,hi,dr,ytype, ...
                                 gtype,itype,mtype,na,nq,ng,ni,nm,Smax);
XX=model.X(results.Ixopt,:);
plot(XX(:,1),XX(:,1)-XX(:,2),'k-.','LineWidth',2)

% high harvest uncertainty
[model,results]=SethiEtAl05Solve(GF,r,K,sigg,hi,sigm,dr,ytype, ...
                                 gtype,itype,mtype,na,nq,ng,ni,nm,Smax);
XX=model.X(results.Ixopt,:);

%%
plot(XX(:,1),XX(:,1)-XX(:,2),'k:','LineWidth',2)
hold off

xlabel('Measured Stock')
ylabel('Optimal Expected Escapement')
title('Fig. 3. Optimal fishing policy under multiple small/single large uncertainty')

xlim([0 100])
ylim([0 100])
legend('All Uncertainties Low','Large Growth Uncertainty','Large Measurement Uncertainty',...
       'Large Implementation Uncertainty','location','northwest')
     switch lower(GF)
       case 'l'
         text(50,10,['Logistic, r=' num2str(r)])
       case 'r'
         text(50,10,['Ricker, r=' num2str(r)])
       case 'b'
         text(50,10,['Beverton-Holt, r=' num2str(r)])
     end

plot2svg('SethiEtAl05_Figure3.svg')
