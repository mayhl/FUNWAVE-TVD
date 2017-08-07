clear all

fdir='../results/'

dep=load('../bathy/depth_a15.txt');

[n,m]=size(dep);
x=[0:m-1]*1;
y=[0:n-1]*2;
[xx,yy]=meshgrid(x,y);

wid=8;
len=5;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[0 0 wid len],'paperposition',[0 0 wid len]);

nstart=input('nstart');
nend=input('nend');

% previous version (2nd revision) nstart=280

icount=0;
for num=nstart:nend

icount=icount+1;
fnum=sprintf('%.5d',num);

u=load([fdir 'umean_' fnum]);
v=load([fdir 'vmean_' fnum]);
eta=load([fdir 'eta_' fnum]);
mask=load([fdir 'mask_' fnum]);
eta(mask<1)=NaN;

dep(mask<1)=NaN;
u(mask<1)=NaN;
v(mask<1)=NaN;

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

contourf(xx,yy,-dep,10);
caxis([-12 0])
h_bar=colorbar('location','SouthOutside');
set(get(h_bar,'xlabel'),'string','depth (m)' )
%axis image, 
axis(ax)
caxis([-12 2.5])
hold on
s=20;
sx=6;
sy=6;
quiver(xx(1:sy:end,1:sx:end),yy(1:sy:end,1:sx:end),s*u(1:sy:end,1:sx:end),s*v(1:sy:end,1:sx:end),0,'k')

xlabel('x (m)')
%ylabel('y (m)')

pause(0.1)
end