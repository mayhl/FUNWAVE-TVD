clear all
fdir='/Users/fengyanshi15/tmp1/';

m=1024;
dx=1.0;
SLP=0.05;
Xslp = 800.0;

% bathy
x=[0:m-1]*dx;
dep=zeros(m)+10.0;
dep(x>Xslp)=10.0-(x(x>Xslp)-Xslp)*SLP;

% wavemaker and sponge
wd=10;
x_wm=[250-wd 250+wd 250+wd 250-wd 250-wd];
x_sponge=[0 180 180 0 0]; 
yy=[-10 -10 10 10 -10];

wid=8;
len=4;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf

eta=load([fdir 'eta_00014']);
plot(x,-dep,'k',x,eta,'b','LineWidth',2)
hold on
plot(x_wm,yy,'r')
text(x_wm(2),0.6,'wavemaker','Color','r','LineWidth',2)
plot(x_sponge,yy,'k')
text(x_sponge(1)+20,0.6,'sponge layer','Color','k','LineWidth',2)
axis([0 1024 -1 1])
grid
xlabel('x(m)')
ylabel('eta(m)')

