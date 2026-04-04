clear all
close all

% input
foutput='/Volumes/DISK_2020_5/NTHMP_Wilmington/Results/PRT_GridA/';
fgrid='/Users/fengyanshi15/work/NTHMP_2021/Make_Coupling/mk_stations/';
fcur='/Users/fengyanshi15/work/NTHMP_2021/Make_Coupling/mk_coupling_files/';

coupling_info=load([fgrid 'station_NC_RI_FL.txt']);
num_blocks=coupling_info(end,3)/4;

iscale=[10 10 10];  % spacial refine scale for all blocks
itscale = [1 1 1]; % coarsen time scale, skip x output time steps

starttime  = [6000 8000 6000];   % in seconds, adjust to truncate time series
stoptime=starttime+18000.0; % 5 hours

%stoptime   = [11000 11000];% Inf; % in sec. indicate Inf to include all.

coupling_filename={'coupling_PRT_NC.txt','coupling_PRT_RI.txt','coupling_PRT_FL.txt'}
logfile='log_PRT_AB.txt';


if length(coupling_filename) ~= num_blocks || ...
   length(starttime) ~= num_blocks || ...
   length(stoptime) ~= num_blocks || ...
   length(iscale) ~= num_blocks || ...
   length(itscale) ~= num_blocks
disp('provide parameters for ALL blocks')
return
end

% east 1, west 2, south 3, north 4

first_read=true(4,num_blocks);

for kk=1:num_blocks
side_block(1,kk)=0;
side_block(2,kk)=0;
side_block(3,kk)=0;
side_block(4,kk)=0;
for k=1:length(coupling_info)
if coupling_info(k,3)==1+(kk-1)*4
  if first_read(1,kk)
    st_side_block(1,kk)=k;
    first_read(1,kk)=false;
  end
side_block(1,kk)=side_block(1,kk)+1;
end
if coupling_info(k,3)==2+(kk-1)*4
  if first_read(2,kk)
    st_side_block(2,kk)=k;
    first_read(2,kk)=false;
  end
side_block(2,kk)=side_block(2,kk)+1;
end
if coupling_info(k,3)==3+(kk-1)*4
  if first_read(3,kk)
    st_side_block(3,kk)=k;
    first_read(3,kk)=false;
  end
side_block(3,kk)=side_block(3,kk)+1;
end
if coupling_info(k,3)==4+(kk-1)*4
  if first_read(4,kk)
    st_side_block(4,kk)=k;
    first_read(4,kk)=false;
  end
side_block(4,kk)=side_block(4,kk)+1;
end
end
end

for kk=1:num_blocks
for kkk=1:4
en_side_block(kkk,kk)=st_side_block(kkk,kk)+side_block(kkk,kk)-1;
end
end

Finfo = fopen(logfile,'w');           

fprintf(Finfo,'Start to run ...');

disp(['number of blocks: ' num2str(num_blocks)])
fprintf(Finfo,['\n' 'number of blocks: ' num2str(num_blocks)]);

for kk=1:num_blocks
txt=['Block ' num2str(kk) ': ' num2str(side_block(1,kk)) '|' ...
num2str(side_block(2,kk)) '|' ...
num2str(side_block(3,kk)) '|' ...
num2str(side_block(4,kk))];
disp(txt)
fprintf(Finfo,['\n' txt]);
end

for kk=1:num_blocks
txt=['Block ' num2str(kk)];
disp(txt)
fprintf(Finfo,['\n' txt]);
for kkk=1:4
txt=['Side ' num2str(kkk) ' Start: ' num2str(st_side_block(kkk,kk)) ...
' ,End: ' num2str(en_side_block(kkk,kk))];
disp(txt)
fprintf(Finfo,['\n' txt]);
end
end

% start to interpolate

%find number of time steps and times
sample = load([foutput 'sta_0001']);


% from east bc
for kk=1:num_blocks

fprintf(Finfo,['\n' 'Start block: ' num2str(kk)]);

start = find(sample(:,1) > starttime(kk),1,'first');
stop  = find(sample(:,1) < stoptime(kk),1,'last');
time  = sample(start:itscale(kk):stop,1);
steps = length(time);

for kkk=1:4

icount=0;
for kkkk=st_side_block(kkk,kk):en_side_block(kkk,kk)
data=load(fullfile(foutput,sprintf('sta_%04d',kkkk)));
icount=icount+1;
eta(:,icount)=data(start:itscale(kk):stop,2);
u(:,icount)=data(start:itscale(kk):stop,3);
v(:,icount)=data(start:itscale(kk):stop,4);
end

[tsize msize]=size(eta);

% interpolate
% check refine grid numbers
tmp=length([1:msize]);
tmp1=length([1:1/iscale(kk):msize]);

disp(['Side number (EWSN)' num2str(kkk)]); % *****
txt=[num2str((tmp-1)*iscale(kk)+1) '?=' num2str(tmp1)];
disp(txt);
fprintf(Finfo,['Side number (EWSN)' num2str(kkk)] ); % *****
fprintf(Finfo,'\nCheck if coarse to fine is consistent ...  ');
fprintf(Finfo,[txt]);

itcount=0;
for nt=1:tsize
itcount=itcount+1;
eta_fine{kkk}(itcount,:)=interp1(1:msize,eta(nt,:),1:1/iscale(kk):msize);
u_fine{kkk}(itcount,:)=interp1(1:msize,u(nt,:),1:1/iscale(kk):msize);
v_fine{kkk}(itcount,:)=interp1(1:msize,v(nt,:),1:1/iscale(kk):msize);
end

% plot
figure
clf
xx_fine=[0:tmp1-1]+1;
xx_coarse=[0:tmp-1]*iscale(kk)+1;
plot(xx_fine,eta_fine{kkk}(floor(tsize/2),:))
hold on

plot(xx_coarse,eta(floor(tsize/2),:),'ro')
txt1=['Coarse grid number: ' num2str(tmp) ' Fine grid number' num2str(tmp1)];
title([txt1 'check' txt])
xlabel('fine grid points')
ylabel('eta')
grid

clear u v eta tmp tmp1
end

% write out block

%   data structure (space,var,time)

for sp=1:size(eta_fine{1},2)
for ti=1:size(eta_fine{1},1)
eastfine(sp,1,ti)=u_fine{1}(ti,sp);
eastfine(sp,2,ti)=v_fine{1}(ti,sp);
eastfine(sp,3,ti)=eta_fine{1}(ti,sp);
end
end
for sp=1:size(eta_fine{2},2)
for ti=1:size(eta_fine{2},1)
westfine(sp,1,ti)=u_fine{2}(ti,sp);
westfine(sp,2,ti)=v_fine{2}(ti,sp);
westfine(sp,3,ti)=eta_fine{2}(ti,sp);
end
end
for sp=1:size(eta_fine{3},2)
for ti=1:size(eta_fine{3},1)
southfine(sp,1,ti)=u_fine{3}(ti,sp);
southfine(sp,2,ti)=v_fine{3}(ti,sp);
southfine(sp,3,ti)=eta_fine{3}(ti,sp);
end
end
for sp=1:size(eta_fine{4},2)
for ti=1:size(eta_fine{4},1)
northfine(sp,1,ti)=u_fine{4}(ti,sp);
northfine(sp,2,ti)=v_fine{4}(ti,sp);
northfine(sp,3,ti)=eta_fine{4}(ti,sp);
end
end

filename=[foutput coupling_filename{kk}];
FIN = fopen(filename,'w');           

% log file
fprintf(Finfo,'coupling data\nboundary info: num of points, start point');
fprintf(Finfo,'\nEAST\n\t%d\t\t%d',size(eastfine,1),1);
fprintf(Finfo,'\nWEST\n\t%d\t\t%d',size(westfine,1),1);
fprintf(Finfo,'\nSOUTH\n\t%d\t\t%d',size(southfine,1),1);
fprintf(Finfo,'\nNORTH\n\t%d\t\t%d',size(northfine,1),1);
% end log file

fprintf(FIN,'coupling data\nboundary info: num of points, start point');
fprintf(FIN,'\nEAST\n\t%d\t\t%d',size(eastfine,1),1);
fprintf(FIN,'\nWEST\n\t%d\t\t%d',size(westfine,1),1);
fprintf(FIN,'\nSOUTH\n\t%d\t\t%d',size(southfine,1),1);
fprintf(FIN,'\nNORTH\n\t%d\t\t%d',size(northfine,1),1);
fprintf(FIN,'\nTIME SERIES');
for t = 1:length(time)
    disp(sprintf('Writing Time Step No. %d    of   %d',t,length(time) ))
    fprintf(FIN,'\n\t%f',time(t));
    printside(FIN,'EAST',eastfine,t)
    printside(FIN,'WEST',westfine,t)
    printside(FIN,'SOUTH',southfine,t)
    printside(FIN,'NORTH',northfine,t)
end
fclose(FIN);
disp('Finished!')
disp(sprintf('NOTE: This coupling file starts at time = %d sec',sample(start,1)))
disp(sprintf('      ends at time = %d sec',sample(stop,1)))

fprintf(Finfo,['\n' sprintf('NOTE: This coupling file starts at time = %d sec',sample(start,1))]);
fprintf(Finfo,['\n' sprintf('      ends at time = %d sec',sample(stop,1))]);
fprintf(Finfo,['\n' 'Total time steps: ' num2str(length(time))]);
fprintf(Finfo,['\n' 'Time interval is AROUND: ' num2str(time(2)-time(1))]);

clear eta_fine u_fine v_fine eastfine westfine southfine northfine

end
fclose(Finfo);



