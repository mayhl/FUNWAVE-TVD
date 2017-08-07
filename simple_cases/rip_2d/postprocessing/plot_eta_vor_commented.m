% Clearing Matlab workspace
clear all

% -----------------------
% ----- User Input ------
% -----------------------

fdir='C:\Users\Michael Lam\Desktop\simple_cases_input\rip_2D/'


% -----------------------
% -- End of user input --
% -----------------------

% Getting depth file and determining domain dimensions

dep=load([fdir 'dep.out']);
[n,m]=size(dep);
dx = 1;
dy = 2;
x=[0:m-1]*dx;
y=[0:n-1]*dy;

% Generating 2D grid with x and y points
[xx,yy]=meshgrid(x,y);


% Plot window dimensions
set(gcf,'units','inches','paperunits','inches','papersize', [7 17],'position',[1 1 8 12],'paperposition',[0 0 6 12]);


nstart=input('nstart');
nend=input('nend');

% previous version (2nd revision) nstart=280

icount=0;
for num=nstart:nend
    
% Padding integer values with zeros
% to be 5 letters long e.g. 1 -> 00001
icount=icount+1;
fnum=sprintf('%.5d',num);
% Loading data from files

u=load([fdir 'umean_' fnum]);
v=load([fdir 'vmean_' fnum]);
eta=load([fdir 'eta_' fnum]);
mask=load([fdir 'mask_' fnum]);


% Removing masked regions from plot
eta(mask<1)=NaN;
dep(mask<1)=NaN;
u(mask<1)=NaN;
v(mask<1)=NaN;

ax=[0 250 0 2000];
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