function Plot_1Dth(freq,theta,Spectrum_Energy,freq_resolution,mfreq, ...
    Coherent_Percentage,theta_mean)
%% 1d spectrum, m2/rad
theta_unique = unique(theta);
mtheta = length(theta_unique);
Syy1d_th = zeros(1,mtheta);

%% get the repetitions of each freq
for i = 1:mfreq
    reps(i) = 0;
    for j = 1:mfreq
        if freq(i) == freq(j)
            reps(i) = reps(i) + 1;
        end
    end
end

%%
Syy1d_th_unique = zeros(1,mtheta);
for j = 1:mtheta
    for i = 1:mfreq
        if theta_unique(j) == theta(i)
            Syy1d_th_unique(j) = Syy1d_th_unique(j)+Spectrum_Energy(i) ...
                *freq_resolution;
        end
    end
end
Hmo_Syy1d_th = 4*sqrt(sum(Syy1d_th*pi));
Hmo_Syy1d_th = 0.01*round(Hmo_Syy1d_th*100);
%% Plot
figure(12), hold on
plot(theta_unique,Syy1d_th_unique,'linewidth',1.3)
xlim([-pi/2+theta_mean*pi/180-0.2 pi/2+theta_mean*pi/180+0.2])
%% borders for the figure
xline(-pi/2+theta_mean*pi/180-0.2,'k')
xline(+pi/2+theta_mean*pi/180+0.2,'k')
yline(0.0005,'k'); yline(0,'k');
% ylim([0 0.0005])

xlabel('$\theta (rad)$','Interpreter','latex')
ylabel('$E (m^2/rad)$','Interpreter','latex')

% title(['Hmo = ' num2str(Hmo_Syy1d_th) ' m']);
grid on
grid minor

pbaspect([3 1 1])
set(gca,'fontsize', 24)

saveas(gcf,fullfile(['1Dth_' num2str(Coherent_Percentage) 'p.png']))