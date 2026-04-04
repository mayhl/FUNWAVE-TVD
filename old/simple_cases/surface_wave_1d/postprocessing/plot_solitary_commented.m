% Clearing Matlab workspace
clear all

% -----------------------
% ----- User Input ------
% -----------------------

% Directory of output data files
fdir='/Users/fengyanshi15/tmp1/';

% Time series file to load
files=[1 6 11 17];


% -----------------------
% -- End of user input --
% -----------------------

% Plot time intervals
dt=10

% Setting up partition
m=1024;
dx=1.0;
x=[0:m-1]*dx

% Loading bathy file
dep = load([fdir 'dep.out']);

% Plot window dimensions
wid=6;
len=8;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf

for num=1:length(files)
    
% Padding integer values with zeros
% to be 5 letters long e.g. 1 -> 00001
fnum=sprintf('%.5d',files(num));

% Loading data from file
eta=load([fdir 'eta_' fnum]);

% Plotting data on subplot region 
subplot(length(files), 1, num);

% Plotting wave displacement
plot(x,-dep,'k',x,eta,'b','LineWidth',2)
axis([0 1024 -1.5 1.5])
grid
xlabel('x(m)')
ylabel('eta(m)')
title([' Time = ' num2str(files(num)*dt) ' sec '])

end
