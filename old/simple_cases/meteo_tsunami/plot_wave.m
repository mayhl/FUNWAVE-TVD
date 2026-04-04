clear all
fdir='/Users/fengyanshi15/tmp1/';

eta=load([fdir 'eta_00001']);

[n,m]=size(eta);
dx=500.0;
dy=500.0;
x=[0:m-1]*dx;
y=[0:n-1]*dy;


nfile=[11 16 21 26 31 36];
min={'1.0' '1.5' '2.0' '2.5' '3.0' '3.5'};

wid=6;
len=8;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf


for num=1:length(nfile)
    
fnum=sprintf('%.5d',nfile(num));
eta=load([fdir 'eta_' fnum]);

subplot(6,1,num)
plot(x(:),eta(50,:),'LineWidth',1.5)
axis([0 300000 -0.5 1])
grid

title([' Time = ' min{num} ' hr '])


ylabel(' eta (m) ')

if(num==length(nfile))
xlabel(' x (m) ')
end


set(gcf,'Renderer','zbuffer')

end
