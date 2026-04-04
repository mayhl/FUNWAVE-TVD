clear; clc; close all;
%% Plot Spectrum
fdir = '../Case_DATA2D_100_Spectrum/';

fid=fopen([fdir 'LOG.txt'],'r');
%read the file
j=0;
i=0;
while 1
    tline = fgetl(fid);
    if i==1
        j=j+1;
        output(j,1)=str2double(tline(6:12)); %freq
        output(j,2)=str2double(tline(16:24)); %input theta
        output(j,3)=str2double(tline(28:36)); %output theta
        tline = fgetl(fid);
        output(j,4)=str2double(tline(6:12)); %output Amplitude
        i=0;
    end
    if strfind(tline, 'Freq,Input Dire,PBC Dire,Amplitude,Phase')>0
        i=1;
        continue
    end
    %%
    if ~ischar(tline)
        break
    end
end

Amp_i = output(:,4);
Hmo_i = Amp_i*2*sqrt(2);

f_i = output(:,1);
theta_in = output(:,2);
theta_pbc = output(:,3);

H = sqrt(sum(Hmo_i.^2));
theta_mean = 0;
fp = 0.1;


freq_resolution = (f_i(end)-f_i(1))/length(f_i);
E_i = (Hmo_i./4).^2 / freq_resolution / pi;
%%
figure()
scatter(theta_in,f_i,50,E_i,'filled')
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

saveas(gcf,fullfile([fdir 'figures/2D_Spectrum.png']))


% large view
ylim([0.09 0.11])
pbaspect([3 1 1])
saveas(gcf,fullfile([fdir 'figures/2D_Spectrum_detail.png']))
close all
