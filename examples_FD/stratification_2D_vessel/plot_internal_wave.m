fdir = '/Users/fengyanshi/TMP/tmp3/';

m=128;
n=32;
l=30;

dx=5.0;
dy=5.0;


x=[0:m-1]*dx;
y=[0:n-1]*dy;


ns=input('ns=');
ne=input('ne=');

% Set up file and options for creating the movie
vidObj = VideoWriter('movie.avi');  % Set filename to write video file
vidObj.FrameRate=10;  % Define the playback framerate [frames/sec]
open(vidObj);

wid=8;
len=10;
%set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);

colormap jet

myVideo = VideoWriter('videoOut.mp4','MPEG-4');
myVideo.FrameRate = 25;  
myVideo.Quality = 100;
vidHeight = 576; %this is the value in which it should reproduce
vidWidth = 1024; %this is the value in which it should reproduce
open(myVideo);

icount=0;
for num=ns:1:ne

icount=icount+1;

fnum=sprintf('%.4d',num);
sali=load([fdir 'sali_' fnum]);
eta=load([fdir 'eta_' fnum]);

nshow=floor(n/2);

z2D=(10+eta);
dz1D=z2D(nshow,:)/l;

z=-10.0+dz1D.*[0:l-1]';

[Vx,Vz]=meshgrid(x,[0:l-1]);

sali3d1=reshape(sali,[n,l,m]);
sali3d=permute(sali3d1,[1 3 2]);
sali2d=squeeze(sali3d(nshow,:,:))';

clf
%contourf(Vx,Vz,sali2d,10)
contourf(Vx,z,sali2d,[22:0.1:25])
caxis([22 24.5])
axis([0 640 -10 1])
grid

xlabel('x(m)')
ylabel('z(m)')

pause(1)

F = print('-RGBImage','-r300');
J = imresize(F,[vidHeight vidWidth]);
mov(icount).cdata = J;


writeVideo(myVideo,mov(icount).cdata);

end
close(myVideo)


