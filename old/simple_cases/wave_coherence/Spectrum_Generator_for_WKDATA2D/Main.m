clear; clc; close all
%% Inputs
Get_Inputs; load inputs.mat;
%% No coherence spectrum generation
[idx_theta,freq_resolution,freq,theta,Spectrum_Energy,Hmo_each, ...
    reps,AG,host_freqs_idx] = Zero_Coherence_Spectrum;
%% Coherent waves
if Coherent_Percentage > 0
    [freq,Spectrum_Energy,Hmo_each,reps,AG] = ...
        Coherence_Spectrum(idx_theta,freq,freq_resolution,theta,host_freqs_idx);
end
%% Plots
%% Plot 2d Spectrum
Plot_2D(theta,freq,Spectrum_Energy,theta_mean,Coherent_Percentage,fp)
close all
% %% Plot 2d Spectrum
% Plot_Polar2D(theta,freq,Spectrum_Energy,theta_mean,Coherent_Percentage,fp)
% close all
%%
% if Coherent_Percentage==100
% Surf_3D(theta,freq,Spectrum_Energy,theta_mean,mtheta)
% close all
% end
% %% Plot 1d Spectrum - Frequency Axis
% Plot_1Df_v2(freq,freq_no_coh,Spectrum_Energy,freq_resolution,mfreq, ...
%     Coherent_Percentage,idx_theta,mtheta,host_freqs_idx);
% close all
% %% Plot 1d Spectrum - Directional Axis
% Plot_1Dth(freq,theta,Spectrum_Energy,freq_resolution,mfreq,Coherent_Percentage,theta_mean);
% close all
%% Write the spectrum to an input file
Write_to_file(theta,freq,Hmo_each,mfreq,mtheta,Coherent_Percentage,fp, ...
    phase_include)

