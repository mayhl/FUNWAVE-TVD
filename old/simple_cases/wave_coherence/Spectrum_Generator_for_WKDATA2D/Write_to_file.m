function Write_to_file(theta,freq,Hmo_each,mfreq,mtheta,...
    Coherent_Hmo_Percentage,fp,phase_include)
%% Get the final theta and Amp for writing to the output file
theta_deg=theta*180/pi;
Amp = real(Hmo_each/2/sqrt(2));
Hmo_output = sqrt(sum((2*sqrt(2)*Amp).^2));

%% sort the variables
Temp = [freq' theta_deg' Amp'];

Temp = sortrows(Temp,1);

freq = Temp(:,1); 
theta_deg = Temp(:,2);
Amp = Temp(:,3);
phase_info = rand([length(freq),1])*2*180;
%% write to file
%% Write the data to a file for new spectrum
% write 1D
fname=['wave2d_' num2str(Coherent_Hmo_Percentage) 'p.txt'];
precision = 5;
% write data
fid=fopen(fname,'w');
fprintf(fid,'%5i   - NumFreq \n',mfreq);
fprintf(fid,['%10.' num2str(precision) 'f   - PeakPeriod  \n'],1/fp);
fprintf(fid,['%10.' num2str(precision) 'f   - Freq \n'],freq);
fprintf(fid,['%10.' num2str(precision) 'f   - Dire \n'],theta_deg);
fprintf(fid,['%10.' num2str(precision) 'f   - Amp \n'],Amp);
if phase_include == 1
fprintf(fid,['%10.' num2str(precision) 'f   - phase \n'],phase_info);
end
fclose(fid);
