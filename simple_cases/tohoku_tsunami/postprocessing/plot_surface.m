clear all
fdir='/Users/fengyanshi15/tmp1/';

dep=load('../external_files/depth_30min.txt');

[n,m]=size(dep);
dx=0.5;
dy=0.5;
x=[0:m-1]*dx+132.01667;
y=[0:n-1]*dy-59.98333;


nfile=[2 9];
hr={'1' '8'};

wid=5;
len=7;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf


for num=1:length(nfile)
    
fnum=sprintf('%.5d',nfile(num));
eta=load([fdir 'eta_' fnum]);

eta(dep<0)=NaN;

subplot(length(nfile),1, num)

pcolor(x,y,eta),shading flat
hold on
caxis([-0.1 0.1])
title([' Time = ' hr{num} ' hr '])

if num==1
caxis([-0.5 0.5])
else
caxis([-0.05 0.05])
end

ylabel(' Lat (deg) ')
xlabel(' Lon (deg) ')
cbar=colorbar;
set(get(cbar,'ylabel'),'String','\eta (m) ')
set(gcf,'Renderer','zbuffer');

end
%print -djpeg eta_inlet_shoal_irr.jpg