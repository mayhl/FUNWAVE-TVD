clear all
%----40m
cases=[3 6 9 12 15 18 21 24];

for k=1:length(cases)
fcases=['../results/layers_' num2str(cases(k)) '_40m/'];
sta=load([fcases 'sta_0002']);

dep_40m_max1(k)=max(sta(200:500,2));
dep_40m_max2(k)=max(sta(4500:4800,2));
end

%----30m
cases=[3 6 9 12 15 18 21 24];

for k=1:length(cases)
fcases=['../results/layers_' num2str(cases(k)) '_30m/'];
sta=load([fcases 'sta_0002']);

dep_30m_max1(k)=max(sta(200:500,2));
dep_30m_max2(k)=max(sta(4500:4800,2));
end


%----20m
cases=[3 6 9 12 15 18 21 24];

for k=1:length(cases)
fcases=['../results/layers_' num2str(cases(k)) '_20m/'];
sta=load([fcases 'sta_0002']);

dep_20m_max1(k)=max(sta(200:500,2));
dep_20m_max2(k)=max(sta(4500:4800,2));
end

%----10m
cases=[3 6 9 12 15 18 21 24];

for k=1:length(cases)
fcases=['../results/layers_' num2str(cases(k)) '_10m/'];
sta=load([fcases 'sta_0002']);

dep_10m_max1(k)=max(sta(200:500,2));
dep_10m_max2(k)=max(sta(4500:4800,2));
end


plot(cases,dep_40m_max1,'b',cases,dep_40m_max2,'r')
hold on
plot(cases,dep_30m_max1,'b-o',cases,dep_30m_max2,'r-o')
plot(cases,dep_20m_max1,'b-*',cases,dep_20m_max2,'r-*')
plot(cases,dep_10m_max1,'b--',cases,dep_10m_max2,'r--')


eval(['mkdir ' 'plots'])

fname=['plots/tmp.jpg'];
print('-djpeg',fname)

data(1,1)=10.0;
data(1,2:9)=dep_10m_max1(:);
data(1,10:17)=dep_10m_max2(:);

data(2,1)=20.0;
data(2,2:9)=dep_20m_max1(:);
data(2,10:17)=dep_20m_max2(:);

data(3,1)=30.0;
data(3,2:9)=dep_30m_max1(:);
data(3,10:17)=dep_30m_max2(:);

data(4,1)=40.0;
data(4,2:9)=dep_40m_max1(:);
data(4,10:17)=dep_40m_max2(:);

save -ASCII amp_decay.txt data




