% Clearing Matlab workspace
clear all

% -----------------------
% ----- User Input ------
% -----------------------

fdir='../input_files/output/';
fdir='C:\Users\Michael Lam\Desktop\simple_cases_input\beach_2D\'

% Time series to plot
nfile=[1];

% Time values for series in minutes
times = {'200'};

% Plot resolution for vector field (sk=1 for full resolution) 
sk=8;

% -----------------------
% -- End of user input --
% -----------------------

if (length(nfile) ~= length(times) )
    error('Number of files to load does not equal number of time series values.')
end

% Getting domain dimensions
eta=load([fdir 'eta_00001']);
[n,m]=size(eta);

% Setting up partition
dx=2.0;
dy=2.0;
x=[0:m-1]*dx;
y=[0:n-1]*dy;

% Sponge layer location
x_sponge=[0 100 100 0 0];
y_sponge=[0 0 y(end) y(end) 0];

% Wavemaker location
x_wavemaker=[150 160 160 150 150];
y_wavemaker=[0 0 y(end) y(end) 0];

% Dimensions of plot window 
wid=4;
len=5;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf


for num=1:length(nfile)

% Padding integer number with zereo to
% be 5 characters long e.g. 1 -> 00001
fnum=sprintf('%.5d',nfile(num));

% Loading data from files
u=load([fdir 'umean_' fnum]);
v=load([fdir 'vmean_' fnum]);
ht=load([fdir 'Hsig_' fnum]);
mask=load([fdir 'mask_' fnum]);

% Removing masked points from plot
u(mask==0)=NaN;
v(mask==0)=NaN;
ht(mask==0)=NaN;

pcolor(x,y,ht),shading flat
hold on
caxis([-0. 1])
colorbar

% Plot flow field
quiver(x(1:sk:end,1:sk:end),y(1:sk:end,1:sk:end),u(1:sk:end,1:sk:end),v(1:sk:end,1:sk:end),'w')

% Do not overwrite old plot data when
% plotting sponge layers and wavemaker   
hold on
plot(x_sponge,y_sponge,'g--','LineWidth',2)
h1=text(50,500,'Sponge','Color','w');
set(h1, 'rotation', 90)

plot(x_wavemaker,y_wavemaker,'w-','LineWidth',2)
h2=text(180,700,'Wavemaker','Color','w');
set(h2, 'rotation', 90)
hold off

if num==1
    ylabel(' y (m) ')
    xlabel(' x (m) ')
end

cbar=colorbar;
set(get(cbar,'ylabel'),'String','\eta (m) ')


end
%print -djpeg eta_inlet_shoal_irr.jpg