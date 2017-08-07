clear all

m=500;
n=500;
dx=2.0;
dy=2.0;

x=[0:m-1]*dx-m/2*dx;
y=[0:n-1]*dy-n/2*dy;

[X,Y]=meshgrid(x,y);

R1=450;
R2=100;
Slope=0.24;
Slope_is=0.24;

dep=zeros(n,m)+10.0;

for j=1:n
    for i=1:m
        r=sqrt(X(j,i)^2+Y(j,i)^2);
        if r>R1
            dep(j,i)=10.0-(r-R1)*Slope;
        end
        if r<R2
            dep(j,i)=10.0-(R2-r)*Slope_is;
        end
        
    end
end
dep(dep<-2.0)=-2.0;


pcolor(X,Y,-dep),shading flat

Rs=R2-40.0;
x0=Rs;
y0=0.0;
speed0=10.0;

t=[0:300];
xship(1)=x0;
yship(1)=y0;
Rship=250;
actsp(1)=0.0;
t0=50;

for it=2:length(t)
    if(t(it)<t0)
    rship=Rs+(Rship-Rs)*t(it)/t0;
    else
        rship=Rship;
    end
    omega=speed0/Rship;
    angle=t(it)*omega;
    xship(it)=rship*cos(angle);
    yship(it)=rship*sin(angle);
    actsp(it)=sqrt((xship(it)-xship(it-1))^2+(yship(it)-yship(it-1))^2)/(t(it)-t(it-1));
end

hold on
plot(xship,yship,'w--')

vessel(:,1)=t;
vessel(:,2)=xship+m/2*dx;;
vessel(:,3)=yship+n/2*dy;;
%vessel(:,4)=actsp;

save -ASCII depth.txt dep
save -ASCII vessel_0001 vessel








    
    
    
    
    
      