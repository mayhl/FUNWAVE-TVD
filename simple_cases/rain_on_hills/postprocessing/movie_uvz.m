clear all
fdir='../work/output/';

dep=load([fdir 'dep.out']);
dx=10.0;
dy=10.0;
[n,m]=size(dep);
x=[0:m-1]*dx;
y=[0:n-1]*dy;
[xx,yy]=meshgrid(x,y);

myVideo = VideoWriter('videoOut.mp4','MPEG-4');
myVideo.FrameRate = 25;  
myVideo.Quality = 100;
%vidHeight = 576; %this is the value in which it should reproduce
%vidWidth = 1024; %this is the value in which it should reproduce
open(myVideo);


fig=figure(1);
wid=8;
len=12;
set(fig,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[1 1 wid len]);

ax=[0 6550 0 1990];

files=[10:10];

for num=1:length(files)

fnum=sprintf('%.5d',files(num));

u=load([fdir 'u_' fnum]);
v=load([fdir 'v_' fnum]);
eta=load([fdir 'eta_' fnum]);
mask=load([fdir 'mask_' fnum]);
dep1=dep;
eta(mask<1)=NaN;
dep1(mask<1)=NaN;

u(mask<1)=NaN;
v(mask<1)=NaN;

uu=sqrt(u.^2+v.^2);
[vort vort1]=curl(xx,yy,u,v);

time=num2str(files(num)*20,'%0.1f');

clf

colormap(jet)

subplot(311)
% plot total depth
hp=pcolor(xx,yy,eta+dep1);shading interp
caxis([0.0 0.5])
hold on
contour(xx,yy,dep,[-5:0.2:0],'Color','k')

h_bar=colorbar('location','EastOutside');
set(get(h_bar,'xlabel'),'string','total depth (m)' )

xlabel('x (m)')
ylabel('y (m)')

title(['time = ' time ' s'])

axis(ax)

subplot(312)
% plot total depth
hp=pcolor(xx,yy,uu);shading interp
caxis([0.0 3.0])
hold on
contour(xx,yy,dep,[-5:0.2:0],'Color','k')

h_bar=colorbar('location','EastOutside');
set(get(h_bar,'xlabel'),'string','flow speed (m/s)' )

xlabel('x (m)')
ylabel('y (m)')

%title(['time = ' time ' s'])

axis(ax)

subplot(313)
% plot total depth
hp=pcolor(xx,yy,vort);shading interp
caxis([-0.1 0.1])
hold on
%contour(xx,yy,dep,[-5:0.2:0],'Color','k')
sc=50;
sk=5;
quiver(xx(1:sk:end,1:sk:end),yy(1:sk:end,1:sk:end),u(1:sk:end,1:sk:end)*sc,v(1:sk:end,1:sk:end)*sc,0)

h_bar=colorbar('location','EastOutside');
set(get(h_bar,'xlabel'),'string','vorticity (1/s^2)' )

xlabel('x (m)')
ylabel('y (m)')

%title(['time = ' time ' s'])

axis(ax)

pause(0.1)

F = print('-RGBImage','-r300');
%J = imresize(F,[vidHeight vidWidth]);
mov(num).cdata = F;


writeVideo(myVideo,mov(num).cdata);

end
close(myVideo)






