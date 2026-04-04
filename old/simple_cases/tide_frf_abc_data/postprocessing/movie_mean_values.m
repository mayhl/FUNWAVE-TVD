clear all

% folder of results
fdir_results='/Volumes/BigSur_2022/FRF_TIDE/Case_15deg/';

% dimensions
m=660;
n=800;
DimsX={[m n]};

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
y=[n:-1:1]*1.0+180;
[X Y]=meshgrid(x,y);

% define movie file and parameters
myVideo = VideoWriter('videoOut.mp4','MPEG-4');
myVideo.FrameRate = 10;  
myVideo.Quality = 100;
vidHeight = 576; %this is the value in which it should reproduce
vidWidth = 1024; %this is the value in which it should reproduce
open(myVideo);


% file range
files=[1:119];

fig=figure(1);
colormap jet

for k=1:length(files) 

numb=files(k);

fnum=sprintf('%.5d',numb);

% read files -----------------------
fname=[fdir_results 'etamean_' fnum];
fileID=fopen(fname);
eta=fread(fileID,DimsX{1},'*single');
fclose(fileID);

eta=eta';
eta=flipud(eta);
eta=fliplr(eta);


fname=[fdir_results 'umean_' fnum];
fileID=fopen(fname);
um=fread(fileID,DimsX{1},'*single');
fclose(fileID);
um=um';
um=flipud(um);
um=-fliplr(um);

fname=[fdir_results 'vmean_' fnum];
fileID=fopen(fname);
vm=fread(fileID,DimsX{1},'*single');
fclose(fileID);
vm=vm';
vm=flipud(vm);
vm=fliplr(vm);

[vort vort1]=curl(X,Y,um,vm);

% read over -------


clf

subplot(121)
B=imrotate(RGB,1);
A=imagesc(B);
hold on
pcolor(X,Y,eta),shading flat

caxis([0 1.5])
plot([110 500],[620 620],'w:','LineWidth',2)
axis([30 600 250 950])

tim=num2str(1000+numb*2000.0,'%.1f'); 

title(['time = ' tim ' sec'])

subplot(122)
B=imrotate(RGB,1);
A=imagesc(B);
hold on
pcolor(X,Y,vort),shading flat
plot([110 500],[620 620],'w:','LineWidth',2)
sk=16;
sc=4;
quiver(X(1:sk:end,1:sk:end),Y(1:sk:end,1:sk:end),um(1:sk:end,1:sk:end)*sc,vm(1:sk:end,1:sk:end)*sc,0,'Color','k')
axis([30 600 250 950])
caxis([-0.5 0.5])
title(['time = ' tim ' sec'])

pause(0.1)


% save image
F = print('-RGBImage','-r300');
J = imresize(F,[vidHeight vidWidth]);
mov(k).cdata = J;

writeVideo(myVideo,mov(k).cdata);

end

close(myVideo)




