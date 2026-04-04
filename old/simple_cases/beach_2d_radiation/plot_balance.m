fdir ='output/';

% x direction
num=3;
fnum=sprintf('%.5d',num);
PgrdX=load([fdir 'PgrdX_' fnum]);
FRCX=load([fdir 'FRCX_' fnum]);
DxSxx=load([fdir 'DxSxx_' fnum]);
DySxy=load([fdir 'DySxy_' fnum]);
DxUUH=load([fdir 'DxUUH_' fnum]);
DyUVH=load([fdir 'DyUVH_' fnum]);

PgrdY=load([fdir 'PgrdY_' fnum]);
FRCY=load([fdir 'FRCY_' fnum]);
DySyy=load([fdir 'DySyy_' fnum]);
DxSxy=load([fdir 'DxSxy_' fnum]);
DyVVH=load([fdir 'DyVVH_' fnum]);
DxUVH=load([fdir 'DxUVH_' fnum]);


ny=25;
figure(1)
clf
subplot(211)
plot(PgrdX(ny,:),'r')
hold on
plot(DxSxx(ny,:),'b')
plot(DySxy(ny,:),'b--')
plot(FRCX(ny,:),'k-')
plot(DxUUH(ny,:),'c')
plot(DyUVH(ny,:),'c--')
legend('PgrdX','DxSxx','DySxy','FRCX','DxUUH','DyUVH','Location','NorthWest')
grid
xlabel('grid point')
ylabel('m^2/s^2')

subplot(212)
plot(PgrdY(ny,:),'r')
hold on
plot(DySyy(ny,:),'b--')
plot(DxSxy(ny,:),'b-')
plot(FRCY(ny,:),'k-')
plot(DyVVH(ny,:),'c--')
plot(DxUVH(ny,:),'c-')
legend('PgrdY','DySyy','DxSxy','FRCY','DyVVH','DxUVH','Location','NorthWest')
grid
xlabel('grid point')
ylabel('m^2/s^2')

print -djpeg100 balance.jpg




