clear all
fdir='/Users/fengyanshi15/tmp1/';

m=1024;
dx=1.0;
SLP=0.05;
Xslp = 750.0;

% bathy
x=[0:m-1]*dx;
dep=zeros(m)+10.0;
dep(x>Xslp)=10.0-(x(x>Xslp)-Xslp)*SLP;

% wavemaker and sponge

files=[1 6 11 17];

wid=6;
len=8;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf

for num=1:length(files)
fnum=sprintf('%.5d',files(num));
eta=load([fdir 'eta_' fnum]);

subplot(length(files), 1, num);

plot(x,-dep,'k',x,eta,'b','LineWidth',2)
hold on
axis([0 1024 -1.5 1.5])
grid
xlabel('x(m)')
ylabel('eta(m)')
title([' Time = ' num2str(files(num)*10) ' sec '])

end
