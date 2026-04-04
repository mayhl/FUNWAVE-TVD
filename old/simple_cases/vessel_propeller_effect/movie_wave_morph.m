clear all
fdir='output/';
fdir1='output_nopropeller/';

dep=load([fdir 'dep_00000']);


[n,m]=size(dep);

dx=2.0;
dy=2.0;
x=[0:m-1]*dx;
y=[0:n-1]*dy;



nfile=[1:1:75];


myVideo = VideoWriter('videoOut.mp4','MPEG-4');
myVideo.FrameRate = 10;  
myVideo.Quality = 100;
%vidHeight = 576; %this is the value in which it should reproduce
%vidWidth = 1024; %this is the value in which it should reproduce
open(myVideo);

wid=8;
len=6;
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

subplot(311)
pcolor(x,y,eta),shading flat
hold on
caxis([-0.3 1.5])
title([' Time = ' num2str(nfile(num)*1.0) ' sec '])

cbar=colorbar;
set(get(cbar,'ylabel'),'String',' \eta (m) ')


%xlabel(' x (m) ')
ylabel(' y (m) ')

subplot(312)
pcolor(x,y,ch1*100),shading flat
hold on
caxis([0.0 0.01])
cbar=colorbar;
set(get(cbar,'ylabel'),'String',' c ')
title('Sediment concentration without propeller')

%xlabel(' x (m) ')
ylabel(' y (m) ')


subplot(313)
pcolor(x,y,ch*100),shading flat
hold on
caxis([0.0 0.01])
cbar=colorbar;
set(get(cbar,'ylabel'),'String',' c ')
title('Sediment concentration with propeller')


pause(0.1)

F = print('-RGBImage','-r300');
mov(num).cdata = F;

writeVideo(myVideo,mov(num).cdata);

end
close(myVideo)
