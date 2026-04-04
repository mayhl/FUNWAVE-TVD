clear all

fdir='your bathy folder/';
dep=load([fdir 'depth.txt']);

edge=5; % set edge points you dont want filter

K=0.11111*ones(3);
tmp = conv2(dep,K,'same');

dep(1+edge:end-edge,1+edge:end-edge)=tmp(1+edge:end-edge,1+edge:end-edge);

save('-ASCII',[fdir 'new_depth.txt'],'dep');