clear all
fdir='../work/output/';

dep=load([fdir 'dep.out']);
dx=10.0;
dy=10.0;
[n,m]=size(dep);
x=[0:m-1]*dx;
y=[0:n-1]*dy;
[xx,yy]=meshgrid(x,y);


fig=figure(1);
wid=10;
len=8;
set(fig,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[1 1 wid len]);

ax=[0 6550 0 1990];

files=[90:90];

for num=1:length(files)

fnum=sprintf('%.5d',files(num));

u=load([fdir 'u_' fnum]);
v=load([fdir 'v_' fnum]);
eta=load([fdir 'eta_' fnum]);
mask=load([fdir 'mask_' fnum]);
dep1=dep;
eta(mask<1)=NaN;
dep1(mask<1)=NaN;

%u(mask<1)=NaN;
%v(mask<1)=NaN;

uu=sqrt(u.^2+v.^2);
[vort vort1]=curl(xx,yy,u,v);

time=num2str(files(num)*20,'%0.1f');

clf

colormap(jet)

subplot(211)
contourf(xx,yy,-dep,[-12:0 0.1:0.1:8])
caxis([-12 12])
hold on
contour(xx,yy,-dep,[0 0],'Color','k','LineWidth',2)
h_bar=colorbar('location','EastOutside');
set(get(h_bar,'xlabel'),'string','depth (m)' )

xlabel('x (m)')
ylabel('y (m)')

subplot(212)
% 
hp=pcolor(xx,yy,uu);shading interp
caxis([0.0 3.0])
hold on
%contour(xx,yy,dep,[-5:0.2:0],'Color','k')
sc=50;
sk=5;
quiver(xx(1:sk:end,1:sk:end),yy(1:sk:end,1:sk:end),u(1:sk:end,1:sk:end)*sc,v(1:sk:end,1:sk:end)*sc,0)

h_bar=colorbar('location','EastOutside');
set(get(h_bar,'xlabel'),'string','flow speed (m/s)' )

xlabel('x (m)')
ylabel('y (m)')

%title(['time = ' time ' s'])

axis(ax)


end
print -djpeg snap.jpg






