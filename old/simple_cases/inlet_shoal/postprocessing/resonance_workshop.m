clear

fdir='../saved_results/gauges/';


nfile=[1:3];
shift(1:3)=1000;

p1='b-';
p2='r-';
p3='c-';

figure
clf


for num=1:length(nfile)
    
fnum=sprintf('%.4d',nfile(num));
eta=load([fdir 'sta_' fnum]);
eta1=eta(shift(num):end,2);
dt=1.0/0.5;
noverlap=512;
nfft=1048;
nps=512;
%nfft=1048*4;
%window=hanning(nfft);
window=bartlett(nfft/2);

[pxx1,F]=pwelch(eta1,window,noverlap,nfft,dt);

eval(['pstr=p' num2str(num) ';'])

plot(F,pxx1,pstr,'LineWidth',2)
%semilogx(F,pxx1,pstr)

axis([0 0.3 0 2])
xlabel('Frequency (Hz)')
ylabel('Spectral Density (m*m*s)')
hold on

end

legend('gauge 1','gauge 2','gauge 3')

grid
