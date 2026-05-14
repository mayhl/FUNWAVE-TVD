clear all

% theory
g=9.81;
h=[2:0.5:40];
lambda=20.0;
k=2.0*pi/lambda;
kh=k.*h;
sigma=sqrt(g*k*tanh(kh));
T=2.*pi./sigma;

% funwave
data_fun=load('funwave.txt');
h_fun=data_fun(:,1);
k_fun=2.0*pi/lambda;
kh_fun=k_fun.*h_fun;
T_fun=data_fun(:,4);

% highly dispersive
data=load('layers_3.txt');
h_num=data(:,1);
k_num=2.0*pi/lambda;
kh_num=k_num.*h_num;
T_num=data(:,5);

fig=figure(1);
clf
wid=6;
len=4;
set(fig,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
plot(kh,T,'k-',kh_fun,T_fun,'r--','LineWidth',2)
grid
hold on
plot(kh_num,T_num,'b--','LineWidth',2)

xlabel('kh')
ylabel('T(s)')
axis([0 12 3.25 4.8])
plot([pi pi],[0 5],'k--')
%text(pi+0.1, 4, 'kh = \pi')
legend('Theory','Boussinesq','3-layer Highly Dispersive Model','kh=\pi')

eval(['mkdir ' 'plots'])

fname=['plots/T_kh.jpg'];
print('-djpeg',fname)






