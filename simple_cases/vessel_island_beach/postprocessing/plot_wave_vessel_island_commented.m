% Clearing Matlab workspace
clear all

% -----------------------
% ----- User Input ------
% -----------------------

% Directory of output data files
fdir='output/';

% Directory of depth file
fdep=fdir;

% Toggle to not plot animation in Matlab 
% and just output as video
do_not_show_figure = false;

% Toggle to plot simulation time
show_time = true;

% -----------------------
% -- End of user input --
% -----------------------

% Command window inputs for plot interval
ns=input('input plot start number: ns=');
ne=input('input plot end number:ne=');

% Loading data from file
mask=load([fdir 'eta_00001']);
dep1=load([fdep 'depth.txt']);

% Getting grid dimensions
[n m]=size(dep1);

% Initializing parition from file
x0=0.0;
y0=0.0;
dx=2.0;
dy=dx;
x=x0+[0:m-1]*dx;
y=y0+[0:n-1]*dy;


% Set up file and options for creating the movie
vidObj = VideoWriter('movie.avi');  % Set filename to write video file
vidObj.FrameRate=10;  % Define the playback framerate [frames/sec]
open(vidObj);

h=figure(1);

if (do_not_show_figure)
set(h, 'Visible', 'off');
end

% Dimensions of plot window 
wid=6;
len=6;
set(h,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);

for num=ns:ne

% Padding integer values with zeros
% to be 5 letters long e.g. 1 -> 00001
fnum=sprintf('%.5d',num);

% Loading wave displacement from file
eta=load([fdir 'eta_' fnum]);

% Removing masked regions from plot
eta(eta>10.0)=NaN;

% Clearing figure
clf

% Plotting wave displacement
pcolor(x,y,eta),shading interp;
caxis([-0.5 0.5])

xlabel('x(m)');
ylabel('y(m)');

% Add simulation to title if toggled
if (show_time)
time =sprintf('%5.1f',(num-1)*2.5);
title(['Time = ', time, ' sec']);
end

pause(0.1)

% Addding plot to video 
currframe=getframe(gcf);
writeVideo(vidObj,currframe);  % Get each recorded frame and write it to filename defined above


end

close(vidObj)

