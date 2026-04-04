clear all

% folder of results
fdir_results='/Volumes/BigSur_2022/FRF_TIDE/Case_15deg/';

% dimensions
m=660;
n=800;
DimsX={[m n]};

station=load(['../work/station.txt']);
sta_i=station(:,1);
sta_j=station(:,2);

% depth
fname=[fdir_results 'dep.out'];
fileID=fopen(fname);
dep=fread(fileID,DimsX{1},'*single');
fclose(fileID);
dep=dep';
dep=flipud(dep);
dep=fliplr(dep);


% image from google earth, not the frf bathy has a rotation, cannot use google_map
RGB = imread('frf_03.jpg');

% match (x,y) and image approximately
x=[1:m]*1.+105;
y=[n:-1:1]*1.0+220;
[X Y]=meshgrid(x,y);

fig=figure(1);
clf
colormap jet
wid=6;
len=8;
set(fig,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);

% read over -------

% make up breaker and foam for visualization 


% eta without breaker
dep(dep<0)=NaN;

B=imrotate(RGB,1);
A=imagesc(B);
hold on
pcolor(X,Y,-dep),shading flat
for k=1:length(station)
plot(X(sta_j(k),sta_i(k)),Y(sta_j(k),sta_i(k)),'ko','MarkerFaceColor','r');
txt=['G' num2str(14-k,'%.d')];
text(X(sta_j(k),sta_i(k))-20,Y(sta_j(k)-15,sta_i(k)),txt,'FontSize',8,'Color','w');
end

caxis([-10 5])
plot([110 500],[620 620],'w:','LineWidth',2)
axis([30 765 250 950])
colorbar
xlabel('x (m)')
ylabel('y (m)')


print -djpeg100 plots_movies/depth_station.jpg
print -depsc2 plots_movies/depth_station.eps




