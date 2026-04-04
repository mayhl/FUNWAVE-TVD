function [idx_theta,freq_resolution,freq,theta,Spectrum_Energy,Hmo_each, ...
    reps,AG,host_freqs_idx] = Zero_Coherence_Spectrum

load inputs.mat;

%% Frequency
freq_resolution = (fmax-fmin)/(mfreq-1);
Energy_total = 0;
for kf = 1:mfreq
    freq(kf)=fmin+(kf-1)*freq_resolution;
    omiga_spec=2*pi*freq(kf)*sqrt(depth/g);
    phi = 1-0.5*(2-omiga_spec)^2;
    if omiga_spec<=1
        phi=0.5*omiga_spec^2;
    elseif omiga_spec>=2
        phi=1;
    end
    if freq(kf)>fp
        sigma_spec = 0.09;
    else
        sigma_spec = 0.07;
    end
    
    Etma(kf) = g^2*freq(kf)^(-5)*(2*pi)^(-4)*phi*exp(-5/4*(freq(kf)/fp)^(-4)) ...
        *gamma_spec^(exp(-(freq(kf)/fp-1)^2/(2*sigma_spec^2)));
    EnergyBin(kf)=Etma(kf)*freq_resolution;
    Energy_total=Energy_total+EnergyBin(kf);
end
%% Directional
N_spec = 20/sigma_theta;
% lock fp and thetamean to a component
[diff,~] = min(abs(freq-fp));
freq = freq+diff;

[~,displace_theta] = min(abs(freq-fp));
idx_theta = mod(displace_theta,mtheta);
for kf = 1:mfreq
    ktheta_temp = mod(kf-idx_theta,mtheta);
    if ktheta_temp<=0
        ktheta_temp = ktheta_temp + mtheta;
    end
    theta(kf) = (-1)^(kf)*(-pi*1.0/2.0 + ...
        2.0/2.0*pi*(floor(ktheta_temp/2.0 - ...
        0.5))/(real(mtheta)-1.0));
    theta(kf) = 0.1*round(theta(kf)*180/pi*10)+theta_mean;
    theta(kf) = theta(kf)*pi/180;
    % Directional Weight
    AG(kf) = 1/2/pi;
    for k_n = 1 : N_spec
        AG(kf) = AG(kf) + (1/pi)*exp(-0.5*(k_n*sigma_theta)^2) ...
            *cos(k_n*(theta(kf)-theta_mean*pi/180));
    end
    AG(kf)=abs(AG(kf));
end
%% identify host freqs
host_freqs_idx = idx_theta:mtheta:mfreq;
if host_freqs_idx(1) == 0
    host_freqs_idx(1)=[];
end
if host_freqs_idx(end) ~= mfreq
    host_freqs_idx(end+1) = mfreq;
end
%% calibrating AG
alpha_spec = Hmo^2/16/Energy_total;
correction_coeff = Energy_total/sum(AG.*EnergyBin);
for kf = 1:mfreq
    AG(kf) = AG(kf) * correction_coeff;
    Hmo_each(kf)=4.0*sqrt((alpha_spec*EnergyBin(kf)*AG(kf)));
    Spectrum_Energy(kf) = (Hmo_each(kf)/4).^2 / freq_resolution / pi;
    %     E1(kf) = (Hmo^2/16/Ef)*(AG(kf)/pi)*Etma(kf);
end

Hs_output = sqrt(sum(Hmo_each.^2));

%% repetition of each freq
% in new wavemaker with no coherence it is equal to zero
reps = ones(1,mfreq);














