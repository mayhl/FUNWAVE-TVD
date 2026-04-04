% ---------------------------------------------------------------------
% Read and generate frequency components of wind waves for IG generation
%    and FUNWAVE for wind wave only
%    Fengyan Shi 04/06/2021
%
% input file: input_data_case1.txt
% output files: 
%  SPC_dep_pf.txt; contains depth and peak frequency
%  SPC_frq.txt;    contains frequency components
%  SPC_angle.txt;  contains wave angle (single angle)
%  SPC_HMO.txt;    contains Hmo of wave components (note: not amplitude)
%  spectrum_windwave_only.txt; this is for windwave only, no IG components
%
% ---------------------------------------------------------------------
%
clear all
hs=load('input_data_case1.txt');
depth=13.0;
peak_freq=0.158;

f=hs(:,1);
Ed=hs(:,2);

f_IG_data=hs(1:100,1);
E_IG_data=hs(1:100,3);

for i=1:length(f)-1
    E(i)=Ed(i)*(f(i+1)-f(i));
end

E_total=sum(E);
Hrms=sqrt(8*E_total);
Hsig=Hrms*sqrt(2);

df=f(2)-f(1);  % for equal increment

sk=30;    % average every 30 points
trun=1601; % T=3.4s truncated
count=0;
for ii=1:sk:trun
    count=count+1;
    EE(count)=sum(E(ii:ii+sk-1));
    ff(count)=f(ii+floor(sk/2));
end
ff(count+1)=f(trun+floor(sk+sk/2));
EE(count+1)=sum(E(trun+sk:end));  % rest of energy
dff=ff(2)-ff(1);


E_t=sum(EE);
Hrms1=sqrt(8*E_t);
Hsig1=Hrms1*sqrt(2);

Hsig_resolved=4*sqrt(sum(EE(2:end-1)));

Amp=sqrt(2.0.*EE(2:end-1));
fff=ff(2:end-1);


figure(1)
clf

plot(f(1:end-1),E/df,'LineWidth',1.0,'Color','k')
grid
hold on
plot(ff(2:end-1),EE(2:end-1)/dff,'LineWidth',2.0,'Color','r')
plot(f_IG_data,E_IG_data,'k--','LineWidth',2.0)
axis([0.0 0.4 0 8.0])

xlabel('f (Hz)')
ylabel('WSD (m^2/Hz)')
legend('Data','Filtered for model input')


print -djpeg100 plots/spc_input.jpg
print -depsc2 plots/spc_input.eps

% 

% output for IG wave generation

f_writeout=fff';
angle_writeout=0;
hmo_writeout=Amp'*2.0*sqrt(2);
otherp(1,1)=depth;
otherp(2,1)=peak_freq;

save -ASCII SPC_dep_pf.txt otherp;
save -ASCII SPC_frq.txt f_writeout;
save -ASCII SPC_angle.txt angle_writeout;
save -ASCII SPC_HMO.txt hmo_writeout;

% write data for funwave with wind wave only

fname='spectrum_windwave_only.txt';

PeakPeriod=1/peak_freq;
NumFreq=length(Amp);
NumDir=1;
Freq=fff;
Dire=0;
Amp1=Amp;

% write data
fid=fopen(fname,'w');
fprintf(fid,'%5i %5i   - NumFreq NumDir \n',NumFreq,NumDir);
fprintf(fid,'%10.3f   - PeakPeriod  \n',PeakPeriod);
fprintf(fid,'%10.3f   - Freq \n',Freq');
fprintf(fid,'%10.3f   - Dire \n',Dire');
dlmwrite(fname,Amp1,'delimiter','\t','-append','precision',5);

fclose(fid);







