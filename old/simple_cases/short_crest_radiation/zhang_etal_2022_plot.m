clear all

foldname='st_360_av_240';
fdir_results=['/Volumes/DISK_2020_5/OceanBeach_onyx/PlaneBeach_',foldname,'/'];
fdir_curr='/Users/fengyanshi/WORK/papers/zhang_etal_2022/idealized_beach/';

% st means start averaging, av means averaging interval
% in /PlaneBeach_st_360_av_240/ use output number 2,3, looks great to represent small sxx along node

m=960;
n=480;
DimsX={[m n]};

dx=2.0;
x=[0:m-1]*dx;
y=[0:n-1]*dx;
[X,Y]=meshgrid(x,y);

xshift=-m*2;
yshift=-n+20.0-318;

X=X+xshift;
Y=Y+yshift;
x=x+xshift;
y=y+yshift;

icount=0;

SHsig=zeros(n,m);
Setamean=zeros(n,m);
Sum=zeros(n,m);
Svm=zeros(n,m);
SDxSxx=zeros(n,m);
SDySxy=zeros(n,m);
SPgrdX=zeros(n,m);
SFRCX=zeros(n,m);
SDxUUH=zeros(n,m);
SDyUVH=zeros(n,m);
SDySyy=zeros(n,m);
SDxSxy=zeros(n,m);
SPgrdY=zeros(n,m);
SFRCY=zeros(n,m);
SDxUVH=zeros(n,m);
SDyVVH=zeros(n,m);

ncount=0;
for numb=2:3
ncount=ncount+1;

eval(['cd ' fdir_results]);
fnum=sprintf('%.5d',numb);

fname=['eta_' fnum];
fileID=fopen(fname);
eta=fread(fileID,DimsX{1},'*single');
fclose(fileID);
eta=eta';

fname=['Hsig_' fnum];
fileID=fopen(fname);
Hsig=fread(fileID,DimsX{1},'*single');
fclose(fileID);
Hsig=Hsig';
SHsig=SHsig+Hsig;

fname=['etamean_' fnum];
fileID=fopen(fname);
etamean=fread(fileID,DimsX{1},'*single');
fclose(fileID);
etamean=etamean';
Setamean=Setamean+etamean;

fname=['umean_' fnum];
fileID=fopen(fname);
um=fread(fileID,DimsX{1},'*single');
fclose(fileID);
um=um';
Sum=Sum+um;

fname=['vmean_' fnum];
fileID=fopen(fname);
vm=fread(fileID,DimsX{1},'*single');
fclose(fileID);
vm=vm';
Svm=Svm+vm;


% x -direction ----------

fname=['DxSxx_' fnum];
fileID=fopen(fname);
DxSxx=fread(fileID,DimsX{1},'*single');
fclose(fileID);
DxSxx=DxSxx';
SDxSxx=SDxSxx+DxSxx;

fname=['DySxy_' fnum];
fileID=fopen(fname);
DySxy=fread(fileID,DimsX{1},'*single');
fclose(fileID);
DySxy=DySxy';
SDySxy=SDySxy+DySxy;

fname=['PgrdX_' fnum];
fileID=fopen(fname);
PgrdX=fread(fileID,DimsX{1},'*single');
fclose(fileID);
PgrdX=PgrdX';
SPgrdX=SPgrdX+PgrdX;


fname=['FRCX_' fnum];
fileID=fopen(fname);
FRCX=fread(fileID,DimsX{1},'*single');
fclose(fileID);
FRCX=FRCX';
SFRCX=SFRCX+FRCX;

fname=['DxUUH_' fnum];
fileID=fopen(fname);
DxUUH=fread(fileID,DimsX{1},'*single');
fclose(fileID);
DxUUH=DxUUH';
SDxUUH=SDxUUH+DxUUH;

fname=['DyUVH_' fnum];
fileID=fopen(fname);
DyUVH=fread(fileID,DimsX{1},'*single');
fclose(fileID);
DyUVH=DyUVH';
SDyUVH=SDyUVH+DyUVH;

% y -direction ----------

fname=['DySyy_' fnum];
fileID=fopen(fname);
DySyy=fread(fileID,DimsX{1},'*single');
fclose(fileID);
DySyy=DySyy';
SDySyy=SDySyy+DySyy;

fname=['DxSxy_' fnum];
fileID=fopen(fname);
DxSxy=fread(fileID,DimsX{1},'*single');
fclose(fileID);
DxSxy=DxSxy';
SDxSxy=SDxSxy+DxSxy;

fname=['PgrdY_' fnum];
fileID=fopen(fname);
PgrdY=fread(fileID,DimsX{1},'*single');
fclose(fileID);
PgrdY=PgrdY';
SPgrdY=SPgrdY+PgrdY;

fname=['FRCY_' fnum];
fileID=fopen(fname);
FRCY=fread(fileID,DimsX{1},'*single');
fclose(fileID);
FRCY=FRCY';
SFRCY=SFRCY+FRCY;

fname=['DyVVH_' fnum];
fileID=fopen(fname);
DyVVH=fread(fileID,DimsX{1},'*single');
fclose(fileID);
DyVVH=DyVVH';
SDyVVH=SDyVVH+DyVVH;

fname=['DxUVH_' fnum];
fileID=fopen(fname);
DxUVH=fread(fileID,DimsX{1},'*single');
fclose(fileID);
DxUVH=DxUVH';
SDxUVH=SDxUVH+DxUVH;

end

% avg
Hsig=SHsig/ncount;
etam=Setamean/ncount;
um=Sum/ncount;
vm=Svm/ncount;
DxSxx=SDxSxx/ncount;
DySxy=SDySxy/ncount;
PgrdX=SPgrdX/ncount;
FRCX=SFRCX/ncount;
DxUUH=SDxUUH/ncount;
DyUVH=SDyUVH/ncount;
DySyy=SDySyy/ncount;
DxSxy=SDxSxy/ncount;
PgrdY=SPgrdY/ncount;
FRCY=SFRCY/ncount;
DxUVH=SDxUVH/ncount;
DyVVH=SDyVVH/ncount;


[vort tmp]=curl(X,Y,um,vm);

% --------- residual
Rx=DxSxx+DySxy+PgrdX+FRCX+DxUUH+DyUVH;
Ry=DySyy+DxSxy+PgrdY+FRCY+DyVVH+DxUVH;
% ---------

eval(['cd ' fdir_curr]);


nn2=390;  % anti
nn1=370;  % middle
nn3=350;  % node 

mm1=950;
mm2=900;
mm3=800;
mm4=625;

yy1=Y(nn2,1);
yy2=Y(nn1,1);
yy3=Y(nn3,1);

xx1=X(1,mm1);
xx2=X(1,mm2);
xx3=X(1,mm3);
xx4=X(1,mm4);

ax1=[-700 0 -225 175];
txt_x=-650;
txt_y=150;

fig=figure(1);
clf
wid=8.0;
len=10.0;
%set(fig,'units','inches','paperunits','inches','papersize',...
%    [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
colormap jet
t = tiledlayout(2,2,'TileSpacing','Compact','Padding','Compact');
nexttile
pcolor(X,Y,eta),shading interp
caxis([-0.9 1.9])
axis(ax1)
hold on
%plot([-748 -748],[-500 500],'k--','LineWidth',2)
%ht=text(-700,-100,'slope toe');
%set(ht,'Rotation',90)
%set(ht,'FontSize',14)
%set(ht,'Color','k')
plot([-1000 0],[yy1 yy1],'w-','LineWidth',2)
%plot([-1000 0],[yy2 yy2],'w-','LineWidth',2)
plot([-1000 0],[yy3 yy3],'w-','LineWidth',2)

plot([xx1 xx1],[-1000 1000],'w--','LineWidth',2)
plot([xx2 xx2],[-1000 1000],'w--','LineWidth',2)
plot([xx3 xx3],[-1000 1000],'w--','LineWidth',2)
plot([xx4 xx4],[-1000 1000],'w--','LineWidth',2)

text(-250,17,'X 1')
%text(-180,-27,'Prof 2')
text(-250,-70,'X 2')


text(xx1-35,75,'Y4')
text(xx2-35,75,'Y3')
text(xx3-20,75,'Y2')
text(xx4-20,75,'Y1')

%xlabel('x (m)')
ylabel('y (m)')
text(txt_x, txt_y, '(a)','Color','w','FontSize',14)

cbar=colorbar;
set(get(cbar,'ylabel'),'String','\eta ( m ) ')

set(gca, 'LineWidth',  1)

nexttile
%pcolor(X,Y,Hsig),shading interp
vbh=[0.0:1.0:3:0];
contourf(X,Y,Hsig,[0:0.1:3]),shading interp
caxis([0 3.0])
axis(ax1)
hold on
%plot([-1000 0],[yy2 yy2],'w-','LineWidth',2)
plot([-1000 0],[yy1 yy1],'w-','LineWidth',2)
plot([-1000 0],[yy3 yy3],'w-','LineWidth',2)
plot([xx1 xx1],[-1000 1000],'w--','LineWidth',2)
plot([xx2 xx2],[-1000 1000],'w--','LineWidth',2)
plot([xx3 xx3],[-1000 1000],'w--','LineWidth',2)
plot([xx4 xx4],[-1000 1000],'w--','LineWidth',2)

%xlabel('x (m)')
%ylabel('y (m)')
cbar=colorbar;
set(get(cbar,'xlabel'),'String','wave height ( m ) ')
text(txt_x, txt_y, '(b)','Color','w','FontSize',14)

nexttile
vb=[-0.2:0.03:-0.02 0.01:0.03:0.1 0.1:0.02:0.3];
contourf(X,Y,etam,vb),shading interp
caxis([-0.3 0.3])
axis(ax1)
hold on
%plot([-1000 0],[yy2 yy2],'w-','LineWidth',2)
plot([-1000 0],[yy1 yy1],'w-','LineWidth',2)
plot([-1000 0],[yy3 yy3],'w-','LineWidth',2)
plot([xx1 xx1],[-1000 1000],'w--','LineWidth',2)
plot([xx2 xx2],[-1000 1000],'w--','LineWidth',2)
plot([xx3 xx3],[-1000 1000],'w--','LineWidth',2)
plot([xx4 xx4],[-1000 1000],'w--','LineWidth',2)
text(txt_x, txt_y, '(c)','Color','k','FontSize',14)

xlabel('x (m)')
ylabel('y (m)')
cbar=colorbar;
set(get(cbar,'xlabel'),'String','setup ( m ) ')

set(gca, 'LineWidth',  1)

nexttile
pcolor(X,Y,vort),shading interp
hold on
sk=8;
sc=50;
quiver(X(1:sk:end,1:sk:end),Y(1:sk:end,1:sk:end),um(1:sk:end,1:sk:end)*sc,vm(1:sk:end,1:sk:end)*sc,0)
axis(ax1)
%plot([-1000 0],[yy2 yy2],'w-','LineWidth',2)
plot([-1000 0],[yy1 yy1],'w-','LineWidth',2)
plot([-1000 0],[yy3 yy3],'w-','LineWidth',2)
plot([xx1 xx1],[-1000 1000],'w--','LineWidth',2)
plot([xx2 xx2],[-1000 1000],'w--','LineWidth',2)
plot([xx3 xx3],[-1000 1000],'w--','LineWidth',2)
plot([xx4 xx4],[-1000 1000],'w--','LineWidth',2)
xlabel('x (m)')
cbar=colorbar;
set(get(cbar,'ylabel'),'String','vort ( 1/s ) ')
text(txt_x, txt_y, '(d)','Color','k','FontSize',14)
set(gca, 'LineWidth',  1)

xlabel('x (m)')
%ylabel('y (m)')

set(fig, 'PaperPositionMode', 'auto')

print('-depsc2',['plots/ideal_4_frame_4prof_',foldname, '.eps'])
print('-djpeg',['plots/ideal_4_frame_4prof_',foldname, '.jpg'])


fig=figure(3)
clf

ax1=[-175 175 -0.001 0.0012];
ax2=[-175 175 -0.01 0.012];
ax3=[-175 175 -0.05 0.075];
ax4=[-175 175 -0.05 0.075];
txt_x=-160;
txt_y=0.09;

wid=9.0;
len=10.0;
set(fig,'units','inches','paperunits','inches','papersize',...
    [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
colormap jet

subplot(4,1,[4])

plot(y,PgrdY(:,mm1),'b',y,DySyy(:,mm1),'r', y,DxSxy(:,mm1),'r--',y,DyVVH(:,mm1),'k',y,DxUVH(:,mm1),'k--',y,FRCY(:,mm1),'b:',y,Ry(:,mm1),'c--','LineWidth',1)
grid
ylabel('m^2/s^2')
xlabel('y (m)')
legend('$gH\frac{\partial \bar{\eta}}{\partial y}$','$\frac{\partial Syy}{\partial y}$', '$ \frac{\partial Sxy}{\partial x}$','$\frac{\partial \bar{Q}\bar{Q}/H}{\partial y}$', '$\frac{\partial \bar{P}\bar{Q}/H}{\partial x}$','$\bar{\tau}_y$','Ry','fontsize',14,'interpreter','latex','Orientation','horizontal')

axis(ax1)

text(-150,0.0014,'(d)','FontSize',14)

subplot(4,1,[3])
plot(y,PgrdY(:,mm2),'b',y,DySyy(:,mm2),'r', y,DxSxy(:,mm2),'r--',y,DyVVH(:,mm2),'k',y,DxUVH(:,mm2),'k--',y,FRCY(:,mm2),'b:',y,Ry(:,mm2),'c--','LineWidth',1)
grid
ylabel('m^2/s^2')
%xlabel('y (m)')
legend('$gH\frac{\partial \bar{\eta}}{\partial y}$','$\frac{\partial Syy}{\partial y}$', '$ \frac{\partial Sxy}{\partial x}$','$\frac{\partial \bar{Q}\bar{Q}/H}{\partial y}$', '$\frac{\partial \bar{P}\bar{Q}/H}{\partial x}$','$\bar{\tau}_y$','Ry','fontsize',14,'interpreter','latex','Orientation','horizontal')

text(-150,0.014,'(c)','FontSize',14)
axis(ax2)

subplot(4,1,[2])
plot(y,PgrdY(:,mm3),'b',y,DySyy(:,mm3),'r', y,DxSxy(:,mm3),'r--',y,DyVVH(:,mm3),'k',y,DxUVH(:,mm3),'k--',y,FRCY(:,mm3),'b:',y,Ry(:,mm3),'c--','LineWidth',1)
grid
ylabel('m^2/s^2')
%xlabel('y (m)')
legend('$gH\frac{\partial \bar{\eta}}{\partial y}$','$\frac{\partial Syy}{\partial y}$', '$ \frac{\partial Sxy}{\partial x}$','$\frac{\partial \bar{Q}\bar{Q}/H}{\partial y}$', '$\frac{\partial \bar{P}\bar{Q}/H}{\partial x}$','$\bar{\tau}_y$','Ry','fontsize',14,'interpreter','latex','Orientation','horizontal')

text(-150,0.09,'(b)','FontSize',14)
axis(ax3)

subplot(4,1,[1])
plot(y,PgrdY(:,mm4),'b',y,DySyy(:,mm4),'r', y,DxSxy(:,mm4),'r--',y,DyVVH(:,mm4),'k',y,DxUVH(:,mm4),'k--',y,FRCY(:,mm4),'b:',y,Ry(:,mm4),'c--','LineWidth',1)
grid
ylabel('m^2/s^2')
%xlabel('y (m)')
legend('$gH\frac{\partial \bar{\eta}}{\partial y}$','$\frac{\partial Syy}{\partial y}$', '$ \frac{\partial Sxy}{\partial x}$','$\frac{\partial \bar{Q}\bar{Q}/H}{\partial y}$', '$\frac{\partial \bar{P}\bar{Q}/H}{\partial x}$','$\bar{\tau}_y$','Ry','fontsize',14,'interpreter','latex','Orientation','horizontal')

text(-150,0.09,'(a)','FontSize',14)
axis(ax4)


print('-depsc2',['plots/momentum_xy2_', foldname, '.eps'])
print('-djpeg',['plots/momentum_xy2_', foldname, '.jpg'])

fig=figure(2);
clf

txt_x=-690;
txt_y=0.09;

ax1=[-700 -40 -0.04 0.075];
ax2=[-700 -40 -0.1 0.135];
ax3=[-700 -40 -0.015 0.025];
ax4=[-700 -40 -0.035 0.09];

wid=9.0;
len=10.0;
set(fig,'units','inches','paperunits','inches','papersize',...
    [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
colormap jet

subplot(4,1,[1])

plot(x,PgrdX(nn2,:),'b',x,DxSxx(nn2,:),'r', x,DySxy(nn2,:),'r--',x,DxUUH(nn2,:),'k',x,DyUVH(nn2,:),'k--',x,FRCX(nn2,:),'b:',x,Rx(nn2,:),'c--','LineWidth',1)

axis(ax1)
grid
ylabel('m^2/s^2')
legend('$gH\frac{\partial \bar{\eta}}{\partial x}$','$\frac{\partial Sxx}{\partial x}$', '$ \frac{\partial Sxy}{\partial y}$','$\frac{\partial \bar{P}\bar{P}/H}{\partial x}$', '$\frac{\partial \bar{P}\bar{Q}/H}{\partial y}$','$\bar{\tau}_x$','Rx','fontsize',14,'interpreter','latex','Orientation','horizontal')

text(-690,0.086,'(a)','FontSize',14)

subplot(4,1,[2])
plot(x,PgrdX(nn3,:),'b',x,DxSxx(nn3,:),'r',x, DySxy(nn3,:),'r--',x,DxUUH(nn3,:),'k',x,DyUVH(nn3,:),'k--',x,FRCX(nn3,:),'b:',x,Rx(nn3,:),'c--','LineWidth',1)

axis(ax2)
grid
ylabel('m^2/s^2')

text(-690,0.15,'(b)','FontSize',14)

legend('$gH\frac{\partial \bar{\eta}}{\partial x}$','$\frac{\partial Sxx}{\partial x}$', '$ \frac{\partial Sxy}{\partial y}$','$\frac{\partial \bar{P}\bar{P}/H}{\partial x}$', '$\frac{\partial \bar{P}\bar{Q}/H}{\partial y}$','$\bar{\tau}_x$','Rx','fontsize',14,'interpreter','latex','Orientation','horizontal')


subplot(4,1,[3])

plot(x,PgrdY(nn2,:),'b',x,DySyy(nn2,:),'r', x,DxSxy(nn2,:),'r--',x,DyVVH(nn2,:),'k',x,DxUVH(nn2,:),'k--',x,FRCY(nn2,:),'b:',x,Ry(nn2,:),'c--','LineWidth',1)

axis(ax3)
grid
ylabel('m^2/s^2')
legend('$gH\frac{\partial \bar{\eta}}{\partial y}$','$\frac{\partial Syy}{\partial y}$', '$ \frac{\partial Sxy}{\partial x}$','$\frac{\partial \bar{Q}\bar{Q}/H}{\partial y}$', '$\frac{\partial \bar{P}\bar{Q}/H}{\partial x}$','$\bar{\tau}_y$','Ry','fontsize',14,'interpreter','latex','Orientation','horizontal')

text(-690,0.028,'(c)','FontSize',14)


subplot(4,1,[4])
plot(x,PgrdY(nn3,:),'b',x,DySyy(nn3,:),'r', x,DxSxy(nn3,:),'r--',x,DyVVH(nn3,:),'k',x,DxUVH(nn3,:),'k--',x,FRCY(nn3,:),'b:',x,Ry(nn3,:),'c--','LineWidth',1)

axis(ax4)
grid
xlabel('x (m)')
ylabel('m^2/s^2')
text(-690,0.1,'(d)','FontSize',14)

legend('$gH\frac{\partial \bar{\eta}}{\partial y}$','$\frac{\partial Syy}{\partial y}$', '$ \frac{\partial Sxy}{\partial x}$','$\frac{\partial \bar{Q}\bar{Q}/H}{\partial y}$', '$\frac{\partial \bar{P}\bar{Q}/H}{\partial x}$','$\bar{\tau}_y$','Ry','fontsize',14,'interpreter','latex','Orientation','horizontal')

print('-depsc2',['plots/momentum_xy1_', foldname, '.eps'])
print('-djpeg',['plots/momentum_xy1_', foldname, '.jpg'])

return
