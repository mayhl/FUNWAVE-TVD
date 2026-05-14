clear all
h=figure(1);
wid=5;
len=5;
set(h,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
err=load('summary.txt');
plot(err(:,1),err(:,2)*100,'ko-',err(:,1),err(:,3)*100,'bo-',err(:,1),err(:,4)*100,'co-','LineWidth',1)
grid
axis([2 6 0 6.0])
legend('kh = \pi','kh = 2\pi','kh = 3\pi')
xlabel('vertical layers')
ylabel('Err (%)');
print -djpeg100 Err.jpg