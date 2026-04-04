clear; clc; close all;
%% Plot Current Field
%% input
fdir = '../Case_DATA2D_2Waves/';
x = 2:2:500;
y = 2:2:1000;
file_no = 1;
%%
[xx,yy] = meshgrid(x,y);
file_no = sprintf('%.5d',file_no);
u = load([fdir 'output/umean_' file_no]);
v = load([fdir 'output/vmean_' file_no]);
mask = load([fdir 'output/mask_00000']);
depth = load([fdir 'output/dep.out']);
u(mask==0) = nan;
v(mask==0) = nan;
%% plot
pcolor(x,y,-depth), shading flat
h = colorbar;
xlabel('X, m'); ylabel('Y, m'); ylabel(h,'elevation, m');
caxis([-8 2])
xlim([150 500])
ylim([0 1000])
pbaspect([350 1000 1])
%%
hold on
skx=5;
sky=5;
cc=75;
quiver(x(1:skx:end,1:skx:end),y(1:sky:end,1:sky:end), ...
    cc*u(1:sky:end,1:skx:end),cc*v(1:sky:end,1:skx:end), ...
    'w','AutoScale','off');
quiver(170,60,cc/4,0,'w','AutoScale','off');
text(170,40,'25 cm/s','FontSize',15,'Color', 'white')

%% Plot sand in Dry Areas
hold on
XX1D = xx(:);
YY1D = yy(:);
onedim = u(:);
XX_Dry = XX1D(isnan(onedim));
YY_Dry = YY1D(isnan(onedim));
colors = copper;
color_sand = colors(190,:);
plot(XX_Dry,YY_Dry,'.','Color',color_sand,'MarkerSize',20);
%% save the jpg file
set(gca,'fontsize', 15)
saveas(gcf,[fdir 'figures/current.jpg'])
close 
