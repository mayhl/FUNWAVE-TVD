clear all
fdir='/Users/fengyanshi15/FUNWAVE_GITHUB/BENCHMARKS_test/simple_inlet_case/saved_results/irr_30deg_brkwater_abs/';

dep=load('../bathy/dep_shoal_inlet.txt');

[n,m]=size(dep);
dx=2.0;
dy=2.0;
x=[0:m-1]*dx;
y=[0:n-1]*dy;

x_sponge=[0 180 180 0 0];
y_sponge=[0 0 y(end) y(end) 0];
x_wavemaker=[240 260 260 240 240];
y_wavemaker=[0 0 y(end) y(end) 0];


nfile=[5 30];
min={'150' '900'};

wid=8;
len=5;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf


for num=1:length(nfile)
    
fnum=sprintf('%.5d',nfile(num));
eta=load([fdir 'eta_' fnum]);
mask=load([fdir 'mask_' fnum]);

eta(mask==0)=NaN;

subplot(1,length(nfile), num)

pcolor(x,y,eta),shading flat
hold on
caxis([-0.5 2])
title([' Time = ' min{num} ' sec '])

hold on
plot(x_sponge,y_sponge,'g--','LineWidth',2)
h1=text(50,1000,'Sponge','Color','w');
set(h1, 'rotation', 90)

plot(x_wavemaker,y_wavemaker,'r-','LineWidth',2)
h2=text(300,1200,'Wavemaker','Color','w');
set(h2, 'rotation', 90)

if num==1
ylabel(' y (m) ')
end

xlabel(' x (m) ')
%cbar=colorbar;
%set(get(cbar,'ylabel'),'String','\eta (m) ')


end
%print -djpeg eta_inlet_shoal_irr.jpg