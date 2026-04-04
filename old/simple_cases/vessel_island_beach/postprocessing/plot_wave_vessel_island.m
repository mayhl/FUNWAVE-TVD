clear all

fdir='output/';
fdep='';

mask=load([fdir 'eta_00001']);

ns=input('input plot start number: ns=');
ne=input('input plot end number:ne=');

dep1=load([fdep 'depth.txt']);

%n=3600;
%m=3200;
[n m]=size(dep1);
x0=0.0;
dx=2.0;
y0=0.0;

x=x0+[0:m-1]*dx;
y=y0+[0:n-1]*dx;



% VESSEL


%ns=100;
%ne=ns;
% Set up file and options for creating the movie
vidObj = VideoWriter('movie.avi');  % Set filename to write video file
vidObj.FrameRate=10;  % Define the playback framerate [frames/sec]
open(vidObj);

h=figure(1);
%set(h, 'Visible', 'off');

wid=6;
len=6;
set(h,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);



for num=ns:ne
    
fnum=sprintf('%.5d',num);
eta=load([fdir 'eta_' fnum]);


eta(eta>10.0)=NaN;

clf



pcolor(x,y,eta),shading interp;
caxis([-0.5 0.5])

xlabel('x(m)');
ylabel('y(m)');

%hour=sprintf('%.4d',(num-1)*2.5);
%title(['Time =', hour, ' sec']);

pause(0.1)

currframe=getframe(gcf);
    writeVideo(vidObj,currframe);  % Get each recorded frame and write it to filename defined above


end
close(vidObj)
