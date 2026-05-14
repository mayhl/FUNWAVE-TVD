clear all
fdir='/Users/fengyanshi/TMP/tmp2/';

num=input('num = ?');
fnum=sprintf('%.4d',num);

eta=load([fdir 'eta_' fnum]);
clf
plot(eta')