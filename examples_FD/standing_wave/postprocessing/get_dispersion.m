clear all

cases=[10 20 30 40];

for k=1:length(cases)
fcases=['../results/layers_3_' num2str(cases(k)) 'm/'];

sta=load([fcases 'sta_0002']);

seg=sta(4500:4800,2);
apeak=max(seg)
ii=find(seg==apeak);
i=ii(1)+4500;
time(k)=sta(i,1)/13
end



