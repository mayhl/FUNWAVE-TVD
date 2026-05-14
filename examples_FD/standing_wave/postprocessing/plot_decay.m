clear all

data=load('amp_decay.txt');

layers=[3 6 9 12 15 18 21 24];

h=data(:,1);
amp1=data(:,2:9);
amp2=data(:,10:17);

for k=1:length(h)

for klayers=1:8
A1=amp1(k,klayers);
A2=amp2(k,klayers);
dA(k,klayers)=0.5*(A1^2-A2^2)./A1^2/50.0; % 0.5 A^2 /50sec
end

end 

lambda=20.0;

fig=figure(1);
clf
wid=6;
len=4;
set(fig,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 2 wid len],'paperposition',[0 0 wid len]);

for k=1:length(h)
K=2.0*pi/lambda;
kh=K*h(k);
if k==1
else
hold on
end
plot(layers(:),dA(k,:),'-o','LineWidth',1,'MarkerSize',5)
txt=['kh=' num2str(kh,'%.2f')];
%text(0.1,dA(k,1)-0.0005,txt)
end
grid
xticks([3 6 9 12 15 18 21 24])
xlabel('Number of Layers')
ylabel('Relative Energy Decay Rate (1/s)')
legend('kh=3.14','kh=6.28','kh=9.42','kh=12.57')

fname=['plots/decay_rate.jpg'];
print('-djpeg',fname)



