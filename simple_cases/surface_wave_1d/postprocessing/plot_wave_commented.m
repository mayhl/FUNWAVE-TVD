% Clearing Matlab workspace
clear all

% -----------------------
% ----- User Input ------
% -----------------------

% Directory of output data files
fdir='/Users/fengyanshi15/tmp1/';


% Time series file to load
file_num = 14

% -----------------------
% -- End of user input --
% -----------------------

% Setting up partition
m=1024;
dx=1.0;
x=[0:m-1]*dx;

% Loading bathy file
dep = load([fdir 'dep.out']);

% Location of wavemaker
wd=10;
x_wm=[250-wd 250+wd 250+wd 250-wd 250-wd];

% Location of sponge layers
x_sponge=[0 180 180 0 0]; 
yy=[-10 -10 10 10 -10];

% Plot window dimensions
wid=8;
len=4;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf

% Padding integer values with zeros
% to be 5 letters long e.g. 1 -> 00001
fnum=sprintf('%.5d',file_num);

% Loading data from file
eta=load([fdir 'eta_' , fnum ]);

% Plotting wave displcaement
plot(x,-dep,'k',x,eta,'b','LineWidth',2)

% Do not overwrite old plot data when
% plotting bathy, sponge layers, wavemaker
hold on
plot(x_wm,yy,'r')
text(x_wm(2),0.6,'wavemaker','Color','r','LineWidth',2)
plot(x_sponge,yy,'k')
text(x_sponge(1)+20,0.6,'sponge layer','Color','k','LineWidth',2)
axis([0 1024 -1 1])
hold off

grid
xlabel('x(m)')
ylabel('eta(m)')

