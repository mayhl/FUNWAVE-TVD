clear all
dep=load('../bathy/dep_shoal_inlet.txt');
[n,m]=size(dep);
dx=2.0;
dy=2.0;
x=[0:m-1]*dx;
y=[0:n-1]*dy;

x_sponge=[0 180 180 0 0];
y_sponge=[0 0 y(end) y(end) 0];
x_wavemaker=[240 260 260 240 240];
y_wavemaker=[0 0 y(end) y(end) 0];

wid=5;
len=6;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf

pcolor(x,y,-dep),shading flat

cbar=colorbar;
    set(get(cbar,'ylabel'),'String',' -dep (m) ')

hold on
plot(x_sponge,y_sponge,'g--','LineWidth',2)
text(10,1000,'Sponge','Color','g')
plot(x_wavemaker,y_wavemaker,'w-','LineWidth',2)
text(270,1200,'Wavemaker','Color','w')

    
caxis([-10 3])
xlabel('x (m)')
ylabel('y (m)')
print -djpeg inlet_shoal.jpg