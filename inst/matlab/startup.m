% Adds the MDPSOLVE path so MATLAB can find the toolbox and demos.
% Other things can be added to this file to set the defaults you like.
% Putting this file in the right place allows it to be run automatically
%   when you start MATLAB. See MATLAB documentation on startup file.


currentdir = cd
cd /home/cboettig/.matlab/MDPSOLVE
addpath(genpath(cd))

cd /home/cboettig/.matlab/plot2svg
addpath(genpath(cd))

cd(currentdir)

%format compact 
%set(0,'DefaultFigureColor',[1 1 1])
%set(0,'DefaultSurfaceFaceColor','interp','DefaultSurfaceEdgeColor','interp')
%nameConflict=warning('off','MATLAB:dispatcher:nameConflict');
