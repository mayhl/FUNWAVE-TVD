clear; clc; close all;
%% Plot Wave Height
%% input
fdir = '../Case_DATA2D_1Wave/';
x = 2:2:500;
y = 2:2:1000;
file_no = 1;
%%
[xx,yy] = meshgrid(x,y);
file_no = sprintf('%.5d',file_no);
H = load([fdir 'output/Hrms_' file_no]);
mask = load([fdir 'output/mask_00000']);
H(mask==0) = nan;
%% plot
pcolor(x,y,H), shading flat
h = colorbar;
colormap jet;
xlabel('X, m'); ylabel('Y, m'); ylabel(h,'Hrms, m');
caxis([0 0.8])
xlim([150 inf])
pbaspect([350 1000 1])
%% Plot sand in Dry Areas
hold on
XX1D = xx(:);
YY1D = yy(:);
onedim = H(:);
XX_Dry = XX1D(isnan(onedim));
YY_Dry = YY1D(isnan(onedim));
colors = copper;
color_sand = colors(190,:);
plot(XX_Dry,YY_Dry,'.','Color',color_sand,'MarkerSize',20);
%% save the jpg file
set(gca,'fontsize', 15)
saveas(gcf,[fdir 'figures/Hrms.jpg'])
close 
