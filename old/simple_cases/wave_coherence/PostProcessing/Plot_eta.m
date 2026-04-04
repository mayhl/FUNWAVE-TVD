clear; clc; close all;
%% Plot Surface Elevation snapshot
%% input
fdir = '../Case_DATA2D_2Waves/';
x = 2:2:500;
y = 2:2:1000;
file_no = 5;
%%
[xx,yy] = meshgrid(x,y);
file_no = sprintf('%.5d',file_no);
eta = load([fdir 'output/eta_' file_no]);
mask = load([fdir 'output/mask_' file_no]);
eta(mask==0) = nan;
%% plot
pcolor(x,y,eta), shading flat
h = colorbar;
colormap jet;
xlabel('X, m'); ylabel('Y, m'); ylabel(h,'\eta, m');
caxis([-0.75 0.75])
xlim([150 inf])
pbaspect([350 1000 1])
%% Plot sand in Dry Areas
hold on
XX1D = xx(:);
YY1D = yy(:);
onedim = eta(:);
XX_Dry = XX1D(isnan(onedim));
YY_Dry = YY1D(isnan(onedim));
colors = copper;
color_sand = colors(190,:);
plot(XX_Dry,YY_Dry,'.','Color',color_sand,'MarkerSize',20);
%% save the jpg file
set(gca,'fontsize', 15)
saveas(gcf,[fdir 'figures/eta_fileno_' num2str(file_no) '.jpg'])
close 
