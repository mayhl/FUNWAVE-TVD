clear all

fdir='/Users/fengyanshi/TMP/tmp2/';

dep=load('depth_a15.txt');

[n,m]=size(dep);
x=[0:m-1]*1;
y=[0:n-1]*2;
[xx,yy]=meshgrid(x,y);

% define movie file and parameters
myVideo = VideoWriter('videoOut.mp4','MPEG-4');
myVideo.FrameRate = 2;  
myVideo.Quality = 100;
%vidHeight = 576; %this is the value in which it should reproduce
%vidWidth = 1024; %this is the value in which it should reproduce
open(myVideo);

wid=12;
len=8;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[0.5 2.5 wid len],'paperposition',[0 0 wid len]);

% previous version (2nd revision) nstart=280

dt=30;
for k=3:32
time=(k-1)*dt;
num=(k-1);
num_avg=floor(time/50);

fnum=sprintf('%.5d',num);
eta=load([fdir 'eta_' fnum]);
%mask=load([fdir 'mask_' fnum]);
FoamEta=load([fdir 'FoamEta_' fnum]);
%eta(mask<1)=NaN;
%FoamEta(mask<1)=NaN;

% averaged properties
fnum=sprintf('%.5d',num_avg);
u=load([fdir 'umean_' fnum]);
v=load([fdir 'vmean_' fnum]);

%dep(mask<1)=NaN;
%u(mask<1)=NaN;
%v(mask<1)=NaN;

ax=[0 250 0 500];
clf

subplot(131)
% plot eta
hp=pcolor(xx,yy,eta);shading interp
caxis([-0.6 1.2])
colormap(jet)
h_bar=colorbar('location','SouthOutside');
set(get(h_bar,'xlabel'),'string','\eta (m)' )

xlabel('x (m)')
ylabel('y (m)')

%axis image, 
axis(ax)

% -------------------
subplot(132)

[w w_ang]=curl(xx,yy,u,v);

pcolor(xx,yy,w),shading interp;
caxis([-.055 .06])
h_bar=colorbar('location','SouthOutside');
set(get(h_bar,'xlabel'),'string','vorticity (s^{-1})' )

xlabel('x (m)')
%ylabel('y (m)')
hold on
plot([140 140],[0 2000],'k--','LineWidth',2)

%axis image, 
axis(ax)

subplot(133)

pcolor(xx,yy,log(FoamEta)),shading interp;
h_bar=colorbar('location','SouthOutside');
set(get(h_bar,'xlabel'),'string','Foam Thickness (log in meter)' )
%axis image, 
axis(ax)
caxis([-10 10])
hold on
s=20;
sx=6;
sy=6;
quiver(xx(1:sy:end,1:sx:end),yy(1:sy:end,1:sx:end),s*u(1:sy:end,1:sx:end),s*v(1:sy:end,1:sx:end),0,'w')

xlabel('x (m)')
%ylabel('y (m)')

pause(0.1)
set(gcf,'PaperPositionMode','auto')

% save image
F = print('-RGBImage','-r300');
%J = imresize(F,[vidHeight vidWidth]);
%mov(k).cdata = J;
mov(k).cdata = F;

writeVideo(myVideo,mov(k).cdata);

end
close(myVideo)


print -djpeg100 eta_vor_foam.jpg
