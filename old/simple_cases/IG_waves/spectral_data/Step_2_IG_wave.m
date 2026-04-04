% ---------------------------------------------------------------------
% IG component generation
%    for FUNWAVE wind wave and IG wave input
%    Fengyan Shi 04/06/2021
%
% input files: (from Step 1: Step_1_read_write_spec.m)
%  SPC_dep_pf.txt; contains depth and peak frequency
%  SPC_frq.txt;    contains frequency components
%  SPC_angle.txt;  contains wave angle (single angle)
%  SPC_HMO.txt;    contains Hmo of wave components (note: not amplitude)
%
%  output file: spectrum_random_phase.txt
%
% ---------------------------------------------------------------------
%
clear all
close all

% input parameters
g=9.81;
LowFreqRange=0.03; %range from left and right of peak frequency
fre_lowcut=0.0667; % 15s
fre_highcut=0.3; % 3 s

% load data
otherp=load('SPC_dep_pf.txt');
h=otherp(1);
peakf=otherp(2);

fre1=load('SPC_frq.txt');
[tmp n1]=min(abs(fre1-fre_lowcut));
[tmp n2]=min(abs(fre1-fre_highcut));
fre=fre1(n1:n2);

angle=load('SPC_angle.txt');
spc1=load('SPC_HMO.txt');
spc=spc1(n1:n2)';

% low frequency range:
f1=peakf-LowFreqRange; 
f2=peakf+LowFreqRange;

% other varables
Amp_each=sqrt(2)/4.0*spc'; 
Phi_each=zeros(length(Amp_each),1);
Omega_each=2*pi*fre;

% randomise phase
rng(1); % make repeatable
Phi_each=2*pi*rand(length(Phi_each),1);

% calculate subharmonics

m=length(Amp_each);

icount=0;
for i=1:m-1
for j=i+1:m
a=[Amp_each(i) Amp_each(j)];
f=[fre(i) fre(j)];

  % sort out using low frequency range

fnm=diff(f);
if f(1)>f1 & f(2)<f2
icount=icount+1;

omega=2*pi*f;
k=wvnum_omvec(h,omega,g);
lambda=2*pi./k;
knm=diff(k);
omega_nm=2*pi*fnm;
C=(omega(1)-omega(2))*((omega(1)*omega(2))^2/g^2 + k(1)*k(2))-0.5*(omega(1)*k(2)^2/cosh(k(2)*h)^2 - omega(2)*k(1)^2/cosh(k(1)*h)^2);

Dnm=-g*k(1)*k(2)/(2*omega(1)*omega(2)) + ...
     (omega(1)^2+omega(2)^2-omega(1)*omega(2))/(2*g) - ...
     C*g*(omega(1)-omega(2))/(omega(1)*omega(2)*(g*knm*tanh(knm*h)-(omega(1)-omega(2))^2));

anm(icount,1)=Dnm*a(1)*a(2); 

Fnm(icount,1)=fnm;
Knm(icount,1)=knm;
OMEGA_nm(icount,1)=2*pi*fnm;
PHI_nm(icount,1)=Phi_each(j)-Phi_each(i);
fre_used(icount,1)=fre(i);
spc_used(icount,1)=spc(i);

end % end sort out

end
end

% arrange freq from small to large with resolution 

freq_low=Fnm;
anm_low=anm;
phase_low=PHI_nm;
OMEGA_nm_low=2*pi*freq_low;

% write out for funwave-tvd ---------------------------------

fname='spectrum_random_phase.txt';
NumFreq = length(fre)+length(freq_low);
NumDir = 1;
PeakPeriod = 1./peakf;
Freq=[freq_low; fre];
Dire=0.0;
Amp1=[anm_low; Amp_each];
Eng1=[0.5*anm_low.^2; 0.5*Amp_each.^2];
Phase1=[phase_low; Phi_each];

fid=fopen(fname,'w');
fprintf(fid,'%5i %5i   - NumFreq NumDir \n',NumFreq,NumDir);
fprintf(fid,'%10.3f   - PeakPeriod  \n',PeakPeriod);
fprintf(fid,'%10.3f   - Freq \n',Freq');
fprintf(fid,'%10.3f   - Dire \n',Dire');
dlmwrite(fname,Amp1,'delimiter','\t','-append','precision',5);
dlmwrite(fname,Phase1,'delimiter','\t','-append','precision',5);

fclose(fid);


% plots ------------------------------------------------

% energy

time=[0:0.1:1000];
Wave_each=Amp_each.*cos(Omega_each.*time+Phi_each);
Wave_total=sum(Wave_each);
IGW_each=anm_low.*cos(OMEGA_nm_low.*time+freq_low+phase_low);
IGW_total=sum(IGW_each);

WaveIG_each=Amp1.*cos(2*pi*Freq.*time+Phase1);
WaveIG_total=sum(WaveIG_each);

Etotal=sum(sum(spc.^2))/16.0;
Hrms=sqrt(8*Etotal);
Hsig=sqrt(16*Etotal);

E=spc.^2/8.0;

figure(1)
plot(fre,spc,'o'),shading flat
hold on
plot(fre_used,spc_used,'rx'),shading flat
legend('data','used for IG')
xlabel('f(Hz)')
ylabel('Hmo (each component)')
txt=['Hsig= ' num2str(Hsig) ' m, ' '  Nfrq=' num2str(length(fre))];
title(txt)
grid
print -djpeg100 plots/windwave_spec_random.jpg

figure(2)
subplot(211)
plot(Fnm,'ko')
hold on 
plot(freq_low,'rx')
legend('all Fnm','integrated')
xlabel('number of components')
ylabel('Low Frequency')
subplot(212)
plot(anm,'ko')
hold on
plot(anm_low,'rx')
legend('all Fnm','integrated')
xlabel('number of components')
ylabel('Amplitude')
title('Frequency and amplitude of IG components')

print -djpeg100 plots/IG_components_random.jpg

figure(3)
plot(time,Wave_total,'r')
hold on
plot(time,IGW_total*10,'b')
grid
xlabel('time(s)')
ylabel('\eta')
title('time series of elevation')
legend('wind wave','IG \times 10')
print -djpeg100 plots/windwave_and_IG_random.jpg

figure(4)
plot(time,WaveIG_total)
title('time series of elevation')
legend('wind wave + IG')
grid
print -djpeg100 plots/wave_plus_IG_random.jpg





