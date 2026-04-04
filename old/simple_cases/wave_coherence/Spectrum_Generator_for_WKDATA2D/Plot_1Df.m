function Plot_1Df(freq,freq_no_coh,Spectrum_Energy,freq_resolution,mfreq,Coherent_Percentage)

%% 1d spectrum, m2/Hz
k = 0;
for i = 1:mfreq
    if (i>1)&&(freq(i) == freq(i-1))
        reps_condensed(k) = reps_condensed(k)+1;
    else
        k = k+1;
        reps_condensed(k) = 1;
    end
end

k = 0;
for i = 1:mfreq
    if (i>1)&&(freq(i) == freq(i-1))
        Syy1d_f(k) = Syy1d_f(k)+Spectrum_Energy(i)*pi;
    else
        k = k+1;
        Syy1d_f(k) = Spectrum_Energy(i)*pi;
        freq_condensed(k) = freq(i);
        freq_index(k) = i;
    end
end

Hmo_Syy1d_f = 4*sqrt(sum(Syy1d_f.*freq_resolution));
Hmo_Syy1d_f = 0.01*round(Hmo_Syy1d_f*100);

Syy1d_f_whole = zeros(size(freq_no_coh));
Syy1d_f_whole(freq_index)=Syy1d_f;

figure(11)
plot(Syy1d_f_whole,freq_no_coh,'linewidth',1.3), hold on
set(gca, 'XDir','reverse')

ylim([0.04 0.25]); 
% xlim([0 0.3]);

% ylabel('$f (Hz)$','Interpreter','latex')
xlabel('$E (m^2/Hz)$','Interpreter','latex')

% title(['Hmo = ' num2str(Hmo_Syy1d_f) ' m']);

grid on
grid minor

% xticks([0 0.2])

pbaspect([1 2 1])
set(gca,'fontsize', 18)

saveas(gcf,fullfile(['1Df_' num2str(Coherent_Percentage) 'p_vertical.png']))

close
%%
figure(1111)
Syy1d_f_whole_ma = movmean(Syy1d_f_whole,31);
plot(Syy1d_f_whole_ma,freq_no_coh,'linewidth',1.3)
set(gca, 'XDir','reverse')

ylim([0.04 0.25]); 
xlim([0 0.45]);

% ylabel('$f (Hz)$','Interpreter','latex')
xlabel('$E (m^2/Hz)$','Interpreter','latex')

% title(['Hmo = ' num2str(Hmo_Syy1d_f) ' m']);

grid on
grid minor

% xticks([0 0.2])

pbaspect([1 2 1])
set(gca,'fontsize', 18)

saveas(gcf,fullfile(['1Df_' num2str(Coherent_Percentage) 'p_vertical_ma.png']))