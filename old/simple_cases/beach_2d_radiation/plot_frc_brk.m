fdir ='/Users/fengyanshi/TMP/tmp2/';

n=50;
m=250;
dx=2.0;
dy=2.0;

x=[0:m-1]*dx;
y=[0:n-1]*dy;

% x direction
num=21;
fnum=sprintf('%.5d',num);

eta=load([fdir 'eta_' fnum]);
bx=load([fdir 'BrkSrcX_' fnum]);
fx=load([fdir 'FrcInsX_' fnum]);

figure(1)
clf
colormap jet

subplot(131)
pcolor(x,y,eta),shading flat
axis([300 500 0 90])
xlabel('x(m)')
ylabel('y(m)')
cbar=colorbar;
set(get(cbar,'ylabel'),'String','\eta (m)  ')
title('inst \eta')

subplot(132)
pcolor(x,y,bx),shading flat
axis([300 500 0 90])
xlabel('x(m)')
%ylabel('y(m)')
cbar=colorbar;
set(get(cbar,'ylabel'),'String','(h+\eta)R_{bx} (m^2/s^2)  ')
title('inst brk stress in x')

subplot(133)
pcolor(x,y,fx),shading flat
axis([300 500 0 90])
xlabel('x(m)')
%ylabel('y(m)')
cbar=colorbar;
set(get(cbar,'ylabel'),'String','-C_d uU (m^2/s^2)  ')
title('inst fric stress in x')

print -djpeg100 break_frc_inst.jpg

% averaged stuff
num=4;
fnum=sprintf('%.5d',num);


FRCX=load([fdir 'FRCX_' fnum]);
BrkDX=load([fdir 'BrkDissX_' fnum]);



figure(2)
clf
colormap jet

subplot(131)
pcolor(x,y,eta),shading flat
axis([300 500 0 90])
xlabel('x(m)')
ylabel('y(m)')
cbar=colorbar;
set(get(cbar,'ylabel'),'String','\eta (m)  ')
title('inst \eta')

subplot(132)
pcolor(x,y,BrkDX),shading flat
axis([300 500 0 90])
xlabel('x(m)')
%ylabel('y(m)')
cbar=colorbar;
set(get(cbar,'ylabel'),'String','(h+\eta)R_{bx} (m^2/s^2)')
title('avg brk stress')

subplot(133)
pcolor(x,y,FRCX),shading flat
axis([300 500 0 90])
xlabel('x(m)')
%ylabel('y(m)')
cbar=colorbar;
set(get(cbar,'ylabel'),'String','-C_d uU (m^2/s^2)')
title('avg fric stress')


print -djpeg100 break_frc_avg.jpg




