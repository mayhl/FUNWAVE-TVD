clear all

% folder of results
fdir_results='/Users/fengyanshi/TMP/tmp3/';


% depth
fname=[fdir_results 'dep.out'];
dep=load(fname);
[n m]=size(dep);

% dimensions
x=[0:m-1]*0.06;

files=[33:150];

% define movie file and parameters
myVideo = VideoWriter(['videoOut.mp4'],'MPEG-4');
myVideo.FrameRate = 10;  
myVideo.Quality = 100;
%vidHeight = 576; %this is the value in which it should reproduce
%vidWidth = 1024; %this is the value in which it should reproduce
open(myVideo);

fig=figure(1);
colormap jet

wid=16;
len=12;
set(fig,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[1 1 wid len]);

for k=1:length(files) 

numb=files(k);

fnum=sprintf('%.5d',numb);

% read files -----------------------
fname=[fdir_results 'eta_' fnum];
eta=load(fname);
eta=eta';
fname=[fdir_results 'u_' fnum];
u=load(fname);
u=u';
fname=[fdir_results 'mask_' fnum];
mask=load(fname);
mask=mask';

fname=[fdir_results 'DchgS_' fnum];
Ds=load(fname);
Ds=Ds';
fname=[fdir_results 'DchgB_' fnum];
Db=load(fname);
Db=Db';

fname=[fdir_results 'C_' fnum];
C=load(fname);
C=C';

fname=[fdir_results 'dep_' fnum];
new_dep=load(fname);
new_dep=new_dep';

fname=[fdir_results 'Pick_' fnum];
pick=load(fname);
pick=pick';

fname=[fdir_results 'Depo_' fnum];
Depo=load(fname);
Depo=Depo';

fname=[fdir_results 'Pavg_' fnum];
picka=load(fname);
picka=picka';

fname=[fdir_results 'Ca_' fnum];
Ca=load(fname);
Ca=Ca';

fname=[fdir_results 'Cb_' fnum];
Cb=load(fname);
Cb=Cb';

fname=[fdir_results 'BedStr_' fnum];
str=load(fname);
str=str';

fname=[fdir_results 'TauEx_' fnum];
TauEx=load(fname);
TauEx=TauEx';

fname=[fdir_results 'Aval_' fnum];
Aval=load(fname);
Aval=Aval';

fname=[fdir_results 'Hpo_' fnum];
Hpo=load(fname);
Hpo=Hpo';

uu=u;
etaa=eta;
eta(mask<1)=NaN;
u(mask<1)=NaN;

Fr=abs(u)./sqrt(9.81*(dep'+eta));


%new_dep=dep'-Ds-Db;

% read over -------

% check

D50=0.00012;
WS = 0.07;
Sdensity = 2.68;
grav=9.8;
viscosity=0.000001;
Shields_cr = 0.05;
k_s = 2.5*D50;
Tau_cr=(Sdensity-1.0)*grav*D50*Shields_cr;

u_c=u;

Tau_xy= 0.16./(1.0+log(k_s./(30.0.*Hpo))).^2.*(u_c.^2.);

% OK

clf

subplot(8,3,1)

plot(x,eta,'r-','LineWidth',1)
hold on
plot(x,-new_dep,'k-','LineWidth',2)
grid
axis([0 65 -1.0 1.0])
xlabel('x(m)')
ylabel('eta (m)')

time1=['time: ' num2str(numb) ' s'];
title(time1)

subplot(8,3,[2 3])

plot(x,-new_dep,'b-',x,-dep','k--','LineWidth',2)
grid
hold on
plot(x,eta,'r-','LineWidth',1)
axis([34.0 44 -0.2 0.4])
xlabel('x(m)')
ylabel('z (m)')
legend('new dep','init dep','\eta')

time1=['time: ' num2str(numb) ' s'];
title(time1)

subplot(8,3,4)
plot(x,u,'b-','LineWidth',1)
grid
axis([0 65 -5.0 5.0])
xlabel('x(m)')
ylabel('u (m/s)')

subplot(8,3,[5 6])
plot(x,u,'b-','LineWidth',1)
grid
axis([34 44 -4.0 4.0])
hold on
plot(x,Fr,'r-','LineWidth',1)
xlabel('x(m)')
ylabel('u (m/s)')
legend('u','Froude')

subplot(8,3,7)

plot(x,pick,'r-','LineWidth',1)
hold on
axis([0 65 0.0 0.02])
xlabel('x(m)')
ylabel('Pickup')
grid

subplot(8,3,[8 9])

plot(x,pick,'r-','LineWidth',1)
hold on
axis([34 44 0.0 0.02])
xlabel('x(m)')
ylabel('Pickup')
legend('pickup')
grid


subplot(8,3,10)

plot(x,Depo,'k-','LineWidth',2)
grid
axis([0 65 0.0 0.01])
xlabel('x(m)')
ylabel('Depo')

subplot(8,3,[11 12])

plot(x,Depo,'k-','LineWidth',2)
grid
axis([34 44 0.0 0.01])
xlabel('x(m)')
ylabel('Depo')
legend('deposition')

subplot(8,3,[13])

plot(x,C,'k-','LineWidth',2)
grid
axis([0 65 0.0 1.0])
xlabel('x(m)')
ylabel('C (non-dimensional)')

subplot(8,3,[14 15])

plot(x,C,'k-','LineWidth',2)
grid
axis([34 44 0.0 1.0])
xlabel('x(m)')
ylabel('C (non-dimensional)')
legend('non-dim C')


subplot(8,3,[16])

plot(x,Cb,'k-','LineWidth',2)
hold on
plot(x,Ca,'r-','LineWidth',2)
grid
axis([0 65 0.0 2.0])
xlabel('x(m)')
ylabel('Cb/Ca')
%legend('Cb','capped Cb')

subplot(8,3,[17 18])

plot(x,Cb,'k-','LineWidth',2)
hold on
plot(x,Ca,'r-','LineWidth',2)
grid
axis([0 65 0.0 2.0])
xlabel('x(m)')
ylabel('Cb/Ca')
legend('Cb','capped Cb')


subplot(8,3,[19])

plot(x,log10(str),'k-','LineWidth',2)
grid
axis([0 65 -6.0 -1.0])
hold on
plot([0 100],[log10(Tau_cr) log10(Tau_cr)],'r--','LineWidth',2)
%plot(x,log10(TauEx),'b-','LineWidth',2)
%legend('log_{10} \tau_b','log_{10}\tau_c')

xlabel('x(m)')
ylabel('Stress')


subplot(8,3,[20 21])

plot(x,log10(str),'k-','LineWidth',2)
grid
axis([34 44 -6.0 -1.0])
hold on
plot([0 100],[log10(Tau_cr) log10(Tau_cr)],'r--','LineWidth',2)
%plot(x,log10(TauEx),'b-','LineWidth',2)
legend('log_{10} \tau_b','log_{10}\tau_c')

xlabel('x(m)')
ylabel('Stress')

subplot(8,3,[22])

plot(x,Ds,'b-','LineWidth',1)
hold on
plot(x,Db,'k-','LineWidth',1)
plot(x,Aval,'c-','LineWidth',1)
plot(x,Ds+Db+Aval,'r-','LineWidth',2)
grid
axis([0 65 -0.3 0.3])

xlabel('x(m)')
ylabel('bed change')

subplot(8,3,[23 24])

plot(x,Ds,'b-','LineWidth',1)
hold on
plot(x,Db,'k-','LineWidth',1)
plot(x,Aval,'c-','LineWidth',1)
plot(x,Ds+Db,'r-','LineWidth',2)
grid
axis([34 44 -0.3 0.3])
hold on
legend('Sus','Bed','Ava','Tot')

xlabel('x(m)')
ylabel('bed change')

pause(0.1)


% save image
F = print('-RGBImage','-r300');
%J = imresize(F,[vidHeight vidWidth]);
mov(k).cdata = F;

writeVideo(myVideo,mov(k).cdata);

end

close(myVideo)




