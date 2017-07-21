% Clearing Matlab workspace
clear all

% Setting up partition
m=500;
n=500;
dx=2.0;
dy=2.0;
x=[0:m-1]*dx-m/2*dx;
y=[0:n-1]*dy-n/2*dy;

% Creating 2D X and Y grid values
[X,Y]=meshgrid(x,y);

% Dimensions of plot window 
wid=5;
len=5;
set(gcf,'units','inches','paperunits','inches','papersize', [wid len],'position',[1 1 wid len],'paperposition',[0 0 wid len]);
clf

% Circular depth profile parameters
R1=450;
Slope=0.24;

R2=100;
Slope_is=0.24;

% Initalizing depth data at 10 meters 
dep=zeros(n,m)+10.0;

% Generating circular moat 
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

% Fixing land height to be 2m max
% Note: negative number correspond to land
dep(dep<-2.0)=-2.0;

% Inital position
Rs=R2-40.0;
x0=Rs;
y0=0.0;

% Vessel speed
speed0=10.0;

% Time parition for vessel simulation
t=[0:300];

% Inital position and velocity
xship(1)=x0;
yship(1)=y0;
actsp(1)=0.0;

% Radius of eventual vessel path
Rship=250;

% Time for vessel to reach circular path 
t0=50;


for it=2:length(t)
    
    % Vessel radius
    if(t(it)<t0)
        % Vessel approaching circular path
        rship=Rs+(Rship-Rs)*t(it)/t0;
    else
        % Vessel at circular path
        rship=Rship;
    end
    
    % Angular speed based of current radius
    omega=speed0/Rship;
    
    % Angular position 
    angle=t(it)*omega;
    
    % Cartesian position 
    xship(it)=rship*cos(angle);
    yship(it)=rship*sin(angle);
    
    % Corrected speed
    actsp(it)=sqrt((xship(it)-xship(it-1))^2+(yship(it)-yship(it-1))^2)/(t(it)-t(it-1));
end

% Plotting depth and ship path
pcolor(X,Y,-dep),shading flat
hold on
plot(xship,yship,'w--')
hold off

vessel(:,1)=t;

% Shifting positions by half a grid space
vessel(:,2)=xship+m/2*dx;
vessel(:,3)=yship+n/2*dy;
%vessel(:,4)=actsp;

% Saving data to file
save -ASCII depth.txt dep
save -ASCII vessel_0001 vessel








    
    
    
    
    
      