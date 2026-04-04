clear; clc; close all;
%% Plot Spectrum
fdir = '../Case_NEWIRR_100P_Coherence/';

fp = 0.10; % user should enter this value (peak frequency)

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
        output(j,5)=str2double(tline(16:24)); %output Phase
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

f_i = output(:,1);
theta_in = output(:,2);
theta_pbc = output(:,3);
Amp_i = output(:,4);
Phase_i = output(:,5);
mfreq = length(f_i);

%% write data
fname=['wave2d.txt'];
precision = 5;
fid=fopen(fname,'w');
fprintf(fid,'%5i   - NumFreq \n',mfreq);
fprintf(fid,['%10.' num2str(precision) 'f   - PeakPeriod  \n'],1/fp);
fprintf(fid,['%10.' num2str(precision) 'f   - Freq \n'],f_i);
fprintf(fid,['%10.' num2str(precision) 'f   - Dire \n'],theta_in);
fprintf(fid,['%10.' num2str(precision) 'f   - Amp \n'],Amp_i);
fprintf(fid,['%10.' num2str(precision) 'f   - phase \n'],Phase_i);
fclose(fid);
