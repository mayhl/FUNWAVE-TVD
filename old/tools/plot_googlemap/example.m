clear all
source='DMI1_Sid';  
region = 5;        

ax1=[-123.425 -123.3751   48.6373   48.6752];
ax2=[-123.425 -123.3751   48.6373   48.6752];
ax3=[-123.444 -123.4125 48.425 48.44];
ax4=[-123.442 -123.424 48.44 48.451];
ax5=[-123.466 -123.448 48.448 48.460];

look_height=200.;
look_h=210;
look_v=75;  % larger lighter
col=[0.0 0.7 1.0];

root='/Volumes/2TB_element/DHI_CRD/';
fdir_results=[root 'RESULTS/' source '/'];
fdir_depth=[fdir_results 'DEPTH/'];
fdir_curr=[root 'MOVIES/'];


%results_gridD=['/Volumes/2TB_element/DHI_CRD/RESULTS/' source '/'];
%saved_plotD=['../../saved_results/' source '/'];
%depth_gridD='/Volumes/2TB_element/DHI_CRD/GRID/victoria/Victoria_4m_2566x2094_conv.txt';

fnames=[40:1:160];   % --------- CHANGE

cmax=5;   % ------------ CHANGE
cmin=-5;

GridX={[388130.0 5375207.0],[444428.0 5354918.0],[463859.0 5360820],[474289.0 5359708.0],[468689.10,5387227.81]};

DimsX={[3086 2170],[2422 1647],[2566 2094],[1810 2492],[926 1048]};

m=DimsX{region}(1);  
n=DimsX{region}(2);

xllcorner=GridX{region}(1);  
yllcorner=GridX{region}(2);

x0=xllcorner;
y0=yllcorner;
x1=xllcorner+(m-1)*4;
y1=yllcorner+(n-1)*4;

[lat0 lon0]=utm2ll(x0,y0,10,'WGS84');
[lat1 lon1]=utm2ll(x1,y1,10,'WGS84');

lat=lat0+(0:n-1)*(lat1-lat0)/(n-1.0);
lon=lon0+(0:m-1)*(lon1-lon0)/(m-1.0);

latG=[lat(1) lat(end)];
lonG=[lon(1) lon(end)];

len=10;
wid=len*m/n*1.4;

set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
%[X Y]=meshgrid(x,y);

% Set up file and options for creating the movie
vidObj = VideoWriter('movie5.avi');  % Set filename to write video file
vidObj.FrameRate=10;  % Define the playback framerate [frames/sec]
open(vidObj);


for num=1:length(fnames)

fnum=sprintf('%.5d',fnames(num));
ftim=sprintf('%.2f',(fnames(num)-1)*30/60.0);
eta_name=['eta_' fnum];
mask_name=['mask_' fnum];
time=[ftim 'min'];

eval(['cd ' fdir_results]);
fileID=fopen(eta_name);
eta=fread(fileID,DimsX{region},'*single');
fclose(fileID);
eta=eta';

fileID=fopen(mask_name);
mask=fread(fileID,DimsX{region},'*single');
fclose(fileID);
mask=mask';

eta(mask<1)=NaN;

eval(['cd ' fdir_curr]);

clf
%subplot(4,4,[1 5])
colormap jet
plot(lonG,latG,'.r','MarkerSize',1)
plot_google_map('maptype','satellite','APIKey','your key here')
hold on
%hsurf=surface(lon,lat,eta*1.+0.0,'FaceColor',[0.0 0.8 1.0],'EdgeColor','none','CDataMapping','direct','DiffuseStrength',0.5);
%lightangle(210,50)
%lightangle(210,50)
pcolor(lon,lat,eta),shading flat
%axis([lon(1) lon(end) lat(1) 48.5])
axis([lon(1) lon(end) lat(1) lat(end)])
xlabel('Longitude(^\circ)');
ylabel('Latitude(^\circ)')
h_bar=colorbar;
ylabel(h_bar,'elevation (m) ')
%set(h_bar,'Location','SouthOutside')
caxis([cmin cmax])
title(['Time = ' time]);
xx=[ax2(1) ax2(2) ax2(2) ax2(1) ax2(1)];
yy=[ax2(3) ax2(3) ax2(4) ax2(4) ax2(3)];
plot(xx,yy,'w','LineWidth',2)

pause(0.2)

    currframe=getframe(gcf);
    writeVideo(vidObj,currframe);  % Get each recorded frame and write it to filename defined above

end
close(vidObj)

%------------------------- END
