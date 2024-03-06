clear all
fdir='output/';
fdir1='output_nopropeller/';


dep=load([fdir 'dep_00000']);

[n,m]=size(dep);

dx=2.0;
dy=2.0;
x=[0:m-1]*dx;
y=[0:n-1]*dy;



nfile=[35];


wid=16;
len=8;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf
colormap jet

for num=1:length(nfile)
    
fnum=sprintf('%.5d',nfile(num));
eta=load([fdir 'eta_' fnum]);
ch=load([fdir 'C_' fnum]);
ds=load([fdir 'DchgS_' fnum]);
db=load([fdir 'DchgB_' fnum]);

eta1=load([fdir1 'eta_' fnum]);
ch1=load([fdir1 'C_' fnum]);
ds1=load([fdir1 'DchgS_' fnum]);
db1=load([fdir1 'DchgB_' fnum]);

subplot(421)
pcolor(x,y,eta1),shading flat
hold on
caxis([-0.3 1.5])
title([' without propeller, Time = ' num2str(nfile(num)*1.0) ' sec '])

cbar=colorbar;
set(get(cbar,'ylabel'),'String',' \eta (m) ')


%xlabel(' x (m) ')
ylabel(' y (m) ')

subplot(423)
pcolor(x,y,ch1*100),shading flat
hold on
caxis([0.0 0.01])
cbar=colorbar;
set(get(cbar,'ylabel'),'String',' c (%) ')


%xlabel(' x (m) ')
ylabel(' y (m) ')


subplot(425)
pcolor(x,y,ds1),shading flat
hold on
caxis([-0.002 0.002])
cbar=colorbar;
set(get(cbar,'ylabel'),'String',' S-load-induced (m) ')
%xlabel(' x (m) ')
ylabel(' y (m) ')

subplot(427)
pcolor(x,y,db1),shading flat
hold on
caxis([-0.002 0.002])
cbar=colorbar;
set(get(cbar,'ylabel'),'String',' B-load-induced (m) ')
xlabel(' x (m) ')
ylabel(' y (m) ')

subplot(422)
pcolor(x,y,eta),shading flat
hold on
caxis([-0.3 1.5])
title([' with propeller, Time = ' num2str(nfile(num)*1.0) ' sec '])

cbar=colorbar;
set(get(cbar,'ylabel'),'String',' \eta (m) ')


%xlabel(' x (m) ')
%ylabel(' y (m) ')

subplot(424)
pcolor(x,y,ch*100),shading flat
hold on
caxis([0.0 0.01])
cbar=colorbar;
set(get(cbar,'ylabel'),'String',' c (%) ')


%xlabel(' x (m) ')
%ylabel(' y (m) ')


subplot(426)
pcolor(x,y,ds),shading flat
hold on
caxis([-0.002 0.002])
cbar=colorbar;
set(get(cbar,'ylabel'),'String',' S-load-induced (m) ')
%xlabel(' x (m) ')
%ylabel(' y (m) ')

subplot(428)
pcolor(x,y,db),shading flat
hold on
caxis([-0.002 0.002])
cbar=colorbar;
set(get(cbar,'ylabel'),'String',' B-load-induced (m) ')
xlabel(' x (m) ')
%ylabel(' y (m) ')

end
print -djpeg100 compare.jpg
