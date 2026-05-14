clear all

data=load('numbproc_time.txt');
numb=data(:,1);
time=data(:,2);
time=time/time(1);

bar(numb,time);
grid
xlabel('Number of layers')
ylabel('Normalized computational time')
print('-djpeg','plots/efficiency.jpg')