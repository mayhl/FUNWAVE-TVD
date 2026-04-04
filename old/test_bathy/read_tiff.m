clear all
%uiopen('ncei_nintharcsec_dem_J1043928.tif');
%dep=-flipud(ncei_nintharcsec_dem_J1043928);

name=['myfile'  ];
name1=[name '.tif']
t = Tiff(name1,'r');
data=read(t);

dep=-flipud(data);

pcolor(-dep),shading flat
dep1=dep(1:100,1:72);
fid = fopen('dep_72x100.txt','wt');
for i=1:size(dep1,1)
fprintf(fid,'%6.2f ',dep1(i,:));
fprintf(fid,'\n');
end
fclose(fid);

% filter

edge=5; % set edge points you dont want filter

K=0.11111*ones(3);
tmp = conv2(dep1,K,'same');

dep1(1+edge:end-edge,1+edge:end-edge)=tmp(1+edge:end-edge,1+edge:end-edge);

dep2=double(dep1);

save('-ASCII',['filtered.txt'],'dep2');

