clear all

dx=60.0/3600.0; % 60 times 
id_count=0;
icount=0;

% NC
x1=-79.1;
x2=-76.7;
y1=33.5;
y2=34.746666666666670;

x=[x1:dx:x2];  % 
y=[y1:dx:y2];  %


id_count=id_count+1;
% east
for j=1:length(y)
icount=icount+1;
sta(icount,1)=y(j);
sta(icount,2)=x(end);
sta(icount,3)=id_count;
end

id_count=id_count+1;
% west
for j=1:length(y)
icount=icount+1;
sta(icount,1)=y(j);
sta(icount,2)=x(1);
sta(icount,3)=id_count;
end

id_count=id_count+1;
% south
for i=1:length(x)
icount=icount+1;
sta(icount,1)=y(1);
sta(icount,2)=x(i);
sta(icount,3)=id_count;
end

id_count=id_count+1;
% north
for i=1:length(x)
icount=icount+1;
sta(icount,1)=y(end);
sta(icount,2)=x(i);
sta(icount,3)=id_count;
end

disp(['icount= ' num2str(icount) '?=' num2str(2*length(x)+2*length(y))]);

figure(1)
clf
plot(sta(:,2),sta(:,1));
txt=['x-dir: ' num2str(length(x)) ' y-dir: ' num2str(length(y)), ' total: ', num2str(2*length(x)+2*length(y))];
title(txt)

print('-djpeg100','plots/NestBC_NC.jpg')

icount_last=icount;

clear x y
% RI -------

x1=-74.5;
x2=-69.5;
y1=40.25;
y2=42.24666666666667; % actually y(end)=42.233333;

x=[x1:dx:x2];  % 301
y=[y1:dx:y2];  % 120

id_count=id_count+1;
% east
for j=1:length(y)
icount=icount+1;
sta(icount,1)=y(j);
sta(icount,2)=x(end);
sta(icount,3)=id_count;
end

id_count=id_count+1;
% west
for j=1:length(y)
icount=icount+1;
sta(icount,1)=y(j);
sta(icount,2)=x(1);
sta(icount,3)=id_count;
end

id_count=id_count+1;
% south
for i=1:length(x)
icount=icount+1;
sta(icount,1)=y(1);
sta(icount,2)=x(i);
sta(icount,3)=id_count;
end

id_count=id_count+1;
% north
for i=1:length(x)
icount=icount+1;
sta(icount,1)=y(end);
sta(icount,2)=x(i);
sta(icount,3)=id_count;
end

disp(['icount= ' num2str(icount-icount_last) '?=' num2str(2*length(x)+2*length(y))]);
disp(['total= ' num2str(icount)]);

figure(2)
clf
plot(sta(icount_last+1:icount,2),sta(icount_last+1:icount,1));
txt=['x-dir: ' num2str(length(x)) ' y-dir: ' num2str(length(y)), ' total for block1+block2: ', num2str(icount)];
title(txt)

print('-djpeg100','plots/NestBC_RI.jpg')

% ---- FL ---------------------
icount_last=icount;
clear x y

x1=-81.0;
x2=-79.0;
y1=24.0;
y2=26.75; % 

x=[x1:dx:x2];  %
y=[y1:dx:y2];  % 

id_count=id_count+1;
% east
for j=1:length(y)
icount=icount+1;
sta(icount,1)=y(j);
sta(icount,2)=x(end);
sta(icount,3)=id_count;
end

id_count=id_count+1;
% west
for j=1:length(y)
icount=icount+1;
sta(icount,1)=y(j);
sta(icount,2)=x(1);
sta(icount,3)=id_count;
end

id_count=id_count+1;
% south
for i=1:length(x)
icount=icount+1;
sta(icount,1)=y(1);
sta(icount,2)=x(i);
sta(icount,3)=id_count;
end

id_count=id_count+1;
% north
for i=1:length(x)
icount=icount+1;
sta(icount,1)=y(end);
sta(icount,2)=x(i);
sta(icount,3)=id_count;
end

disp(['icount= ' num2str(icount-icount_last) '?=' num2str(2*length(x)+2*length(y))]);
disp(['total for all blocks= ' num2str(icount)]);

figure(3)
clf
plot(sta(icount_last+1:icount,2),sta(icount_last+1:icount,1));
txt=['x-dir: ' num2str(length(x)) ' y-dir: ' num2str(length(y)), ' total for all blocks: ', num2str(icount)];
title(txt)

print('-djpeg100','plots/NestBC_FL.jpg') 


fid = fopen('station_NC_RI_FL.txt', 'wt');
  fprintf(fid, ['%f %f %d', '\n'], sta');
fclose(fid);





