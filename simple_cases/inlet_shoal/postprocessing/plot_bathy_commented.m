% Clearing Matlab workspace
clear all

% -----------------------
% ----- User Input ------
% -----------------------

% Directory of output data files
fdir = '../bathy/'

% -----------------------
% -- End of user input --
% -----------------------

% Getting depth file and determining domain dimensions
dep=load([ fdir 'dep.out']);
[n,m]=size(dep);

% Setting up partition
dx=2.0;
dy=2.0;
x=[0:m-1]*dx;
y=[0:n-1]*dy;

% Location of sponge layers
x_sponge=[0 180 180 0 0];
y_sponge=[0 0 y(end) y(end) 0];

% Location of wavemaker
x_wavemaker=[240 260 260 240 240];
y_wavemaker=[0 0 y(end) y(end) 0];

% Plot window dimensions
wid=5;
len=6;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf

% Plotting depth data
pcolor(x,y,-dep),shading flat

cbar=colorbar;
set(get(cbar,'ylabel'),'String',' -dep (m) ')

% Do not overwrite old plot data when
% plotting sponge layers and wavemaker
hold on
plot(x_sponge,y_sponge,'g--','LineWidth',2)
text(10,1000,'Sponge','Color','g')
plot(x_wavemaker,y_wavemaker,'w-','LineWidth',2)
text(270,1200,'Wavemaker','Color','w')
hold off
    
caxis([-10 3])
xlabel('x (m)')
ylabel('y (m)')

%outputing plot as image is Matlab script directory
print -djpeg inlet_shoal.jpg