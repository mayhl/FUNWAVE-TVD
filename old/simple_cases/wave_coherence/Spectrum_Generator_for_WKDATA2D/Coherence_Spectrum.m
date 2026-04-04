function [freq_out,Spectrum_Energy,Hmo_each,reps,AG] = ...
    Coherence_Spectrum(idx_theta,freq_in,freq_resolution,theta,host_freqs_idx)

%% load the inputs
load inputs.mat
%%
% use the previous spectrum with no coherents and move some freqs and then
% redistribute the energy
%% Should we compute the new spectrum? if there are any coherent waves, yes
%% identify freqs with mean direction
host_freqs = freq_in(host_freqs_idx);
%% identify the pool of frequencies to be selected and eliminate the
% arrowheads
freqs_pool = freq_in;
freqs_pool(host_freqs_idx)=[];
%% Number of Coherent Wave Components
Num_Coherent = round(Coherent_Percentage/100*mcomponent);
%% a temporary frequency list to be modified
freq_temp = freq_in;
%% number of repetitions for each freq
reps = ones(mfreq,1);
%%
%%%%% put the while energy statement here
% create some non-repeating random integers and choose the coherents
% freqs
Num_Coherent_temp = 0;
while Num_Coherent_temp < Num_Coherent
    %% randomly select the host
    mfreq_temp = length(freqs_pool);
    guest_idx = ceil(rand*mfreq_temp);
    guest_freq_temp = freqs_pool(guest_idx);
    % to make sure this is not selected again
    freqs_pool(guest_idx) = [];
    % find the host freq
    freq_diff = host_freqs - guest_freq_temp;
    freq_diff(freq_diff<0) = inf;
    [~, host_idx] = min(freq_diff);
    host_freq_temp = host_freqs(host_idx);
    host_idx_whole = host_freqs_idx(host_idx);
    freq_temp(freq_temp==guest_freq_temp) = host_freqs(host_idx);
    %% count the number of coherent waves
    reps(host_idx_whole) = reps(host_idx_whole)+1;
    reps(freq_temp==host_freq_temp) = reps(host_idx_whole);
    coherent_idx = find(reps>1);
    Num_Coherent_temp = length(coherent_idx);
end
%%
freq_coherent = freq_temp;
%% generate energy of spectrum
%% Generate the energy of the spectrum in frequency axis
Energy_total = 0;
for kf = 1:mfreq
    omiga_spec=2*pi*freq_coherent(kf)*sqrt(depth/g);
    phi = 1-0.5*(2-omiga_spec)^2;
    if omiga_spec<=1
        phi=0.5*omiga_spec^2;
    elseif omiga_spec>=2
        phi=1;
    end
    if freq_coherent(kf)>fp
        sigma_spec = 0.09;
    else
        sigma_spec = 0.07;
    end

    Etma(kf) = g^2*freq_coherent(kf)^(-5)*(2*pi)^(-4)*phi*exp(-5/4*(freq_coherent(kf)/fp)^(-4)) ...
        *gamma_spec^(exp(-(freq_coherent(kf)/fp-1)^2/(2*sigma_spec^2)));
    EnergyBin(kf)=Etma(kf)*freq_resolution;
    Energy_total=Energy_total+EnergyBin(kf);
    % 79.6287
    % Directional Weight
    N_spec = 20/sigma_theta;
    AG(kf) = 1/2/pi;
    for k_n = 1 : N_spec
        AG(kf) = AG(kf) + (1/pi)*exp(-0.5*(k_n*sigma_theta)^2) ...
            *cos(k_n*(theta(kf)-theta_mean*pi/180));
    end
end
%% calibrating AG
alpha_spec = Hmo^2/16/Energy_total;
correction_coeff = Energy_total/sum(AG.*EnergyBin);
%% calculating Hmo of each wave component
AG = AG * correction_coeff;
Hmo_each=4.0*sqrt((alpha_spec*EnergyBin.*AG));
Spectrum_Energy = (Hmo_each/4).^2./freq_resolution/(pi);
%% Check if the output Hmo is the same as the input
Hmo_output = sqrt(sum(Hmo_each.^2));
% replace the variable names with the simple ones
freq_out = freq_coherent;