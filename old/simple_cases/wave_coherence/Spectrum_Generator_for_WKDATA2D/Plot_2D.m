function Plot_2D(theta,freq,Spectrum_Energy,theta_mean, ...
    Coherent_Percentage,fp)
figure(21)
scatter(theta*180/pi,freq,75,Spectrum_Energy,'filled')
xlim([-pi/2+theta_mean*pi/180-0.2 pi/2+theta_mean*pi/180+0.2]*180/pi)
ylim([0.03 0.26])
set(gca,'fontsize', 12)
grid on
h = colorbar;
set(gca,'Box','on');
caxis([0 4])
%% line on fp and theta mean
xline(theta_mean,'-.k','linewidth',1)
yline(fp,'-.k','linewidth',1)

xlabel('\theta (rad)')
ylabel('f (Hz)')
ylabel(h,'E (m^2/Hz/rad)')

xticks([-90:22.5:90]);

saveas(gcf,fullfile(['figures/2D_Spectrum_' num2str(Coherent_Percentage) 'p.png']))

% large view
ylim([0.09 0.11])
pbaspect([3 1 1])
saveas(gcf,fullfile(['figures/2D_Spectrum_detail_' num2str(Coherent_Percentage) 'p.png']))
close all
