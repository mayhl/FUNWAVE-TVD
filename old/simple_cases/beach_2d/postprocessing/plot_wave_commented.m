% Clearing Matlab workspace
clear all

% -----------------------
% ----- User Input ------
% -----------------------

% Directory of output data files
fdir='../input_files/output/';
 
% Time series files to plot
nfile=[ 10 , 50];

% Time values for series in minutes
times = {'20','100'};

% Change to plot color bars
plot_color_bars = true;

% -----------------------
% -- End of user input --
% -----------------------

if (length(nfile) ~= length(times) )
    error('Number of files to load does not equal number of time series values.')
end

% Getting domain dimensions from file
eta=load([fdir 'eta_00001']);
[n,m]=size(eta);

% Setting up partition
dx=2.0;
dy=2.0;
x=[0:m-1]*dx;
y=[0:n-1]*dy;

% Location of sponge layers
x_sponge=[0 100 100 0 0];
y_sponge=[0 0 y(end) y(end) 0];

% Location of wavemaker
x_wavemaker=[150 160 160 150 150];
y_wavemaker=[0 0 y(end) y(end) 0];

% Plot window dimensions
wid=8;
len=5;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf


for num=1:length(nfile)

% Padding integer values with zeros
% to be 5 letters long e.g. 1 -> 00001
fnum=sprintf('%.5d',nfile(num));

% Loading data from files
eta=load([fdir 'eta_' fnum]);
mask=load([fdir 'mask_' fnum]);

% Removing masked regions from plot
eta(mask==0)=NaN;

% Plotting data on subplot region 
subplot(1,length(nfile), num)

% Plotting wave displacement
pcolor(x,y,eta),shading flat

caxis([-0.5 2])
title([' Time = ' times{num} ' sec '])

% Do not overwrite old plot data when
% plotting sponge layers and wavemaker
hold on 

plot(x_sponge,y_sponge,'g--','LineWidth',2)
h1=text(50,500,'Sponge','Color','w');
set(h1, 'rotation', 90) 

plot(x_wavemaker,y_wavemaker,'r-','LineWidth',2)
h2=text(180,700,'Wavemaker','Color','w');
set(h2, 'rotation', 90)

hold off

% Only adding y label for leftmost subplot
if num==1
ylabel(' y (m) ')
end

xlabel(' x (m) ')

% Ploting color bars is toggled
if (plot_color_bars)
    cbar=colorbar;
    set(get(cbar,'ylabel'),'String','\eta (m) ')
end

set(gcf,'Renderer','zbuffer')

end
%print -djpeg eta_inlet_shoal_irr.jpg