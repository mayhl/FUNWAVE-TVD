clear all
% beach
nx_total=500;
ny_total=30;
dx=2.0;
dy=2.0;
dep_offshore=10.0;
slope=0.025;
X_toe=560.0;
x=[0:nx_total-1]*dx;
y=[0:ny_total-1]*dy; 
dep=zeros(ny_total,nx_total)+dep_offshore;
for j=1:ny_total
for i=1:nx_total
if x(i)>X_toe
dep(j,i)=dep_offshore-slope*(x(i)-X_toe);
end
end
end

% cell info
cell_m=4;
cell_n=4;
obs_m=3;
obs_n=3;
dep_bottom=9999.0;
dep_top=-2.5;

% grid info
m=nx_total*cell_m;
n=ny_total*cell_n;
m_start=400;
n_start=1;
nx_blocks=100;
ny_blocks=30;

m_obs1=1+floor((cell_m-obs_m)/2);
m_obs2=1+floor((cell_m-obs_m)/2)+obs_m-1;
n_obs1=1+floor((cell_n-obs_n)/2);
n_obs2=1+floor((cell_n-obs_n)/2)+obs_n-1;

dep_cell=zeros(cell_n,cell_m);
dep_cell(n_obs1:n_obs2,m_obs1:m_obs2)=dep_top;

dep_full=zeros([n,m]);
for j=1:n
for i=1:m
jj=floor((j-1)/cell_n)+1;
ii=floor((i-1)/cell_m)+1;
dep_full(j,i)=dep(jj,ii);
end
end

icount=0;
for j=1:ny_blocks
for i=1:nx_blocks
m1=(m_start-1)*cell_m+(i-1)*cell_m+1;
m2=m1+cell_m-1;
n1=(n_start-1)*cell_n+(j-1)*cell_n+1;
n2=n1+cell_n-1;

j_coarse=n_start+(j-1);
i_coarse=m_start+(i-1);
dep_cell_add(:,:)=dep(j_coarse,i_coarse)+dep_cell(:,:);
dep_full(n1:n2,m1:m2)=dep_cell_add(:,:);

icount=icount+1;
for jj=1:cell_n
for ii=1:cell_m
dep_sub_writeout(icount,1)=i+m_start-1;
dep_sub_writeout(icount,2)=j+n_start-1;
dep_sub_writeout(icount,2+(jj-1)*cell_n+ii)=dep_cell_add(jj,ii);
end
end

end
end

dep_level=dep_full;
dep_level(dep_full<0.0)=0.0;

% output subgrid
for j=1:ny_total
for i=1:nx_total
n1=(j-1)*cell_n+1;
n2=n1+cell_n-1;
m1=(i-1)*cell_m+1;
m2=m1+cell_m-1;
dep_sub(j,i)=sum(sum(dep_level(n1:n2,m1:m2)))/cell_m/cell_m;
end
end

figure(1)
clf
pcolor(-dep_full),shading flat
colorbar
tit=['m x n = ' num2str(m) 'x' num2str(n)];
title(tit)
print('-djpeg',['plots/full_grid_2d.jpg'])

figure(2)
clf
pcolor(-dep_sub)
colorbar
tit=['sub m x n = ' num2str(nx_total) 'x' num2str(ny_total)];
title(tit)
print('-djpeg',['plots/sub_grid_2d.jpg'])

save -ASCII dep_sub_500x30.txt dep_sub
save -ASCII dep_full_2000x120.txt dep_full
fid = fopen('dep_sub_info.txt', 'wt');
  fprintf(fid, ['%5d','%5d', repmat('%6.1f',1,cell_m*cell_n),'\n'], dep_sub_writeout');
fclose(fid);






