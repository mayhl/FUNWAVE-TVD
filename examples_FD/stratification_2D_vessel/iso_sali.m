
fdir = '/Users/fengyanshi/TMP/tmp3/';

m=128;
n=32;
l=30;
dx=5.;
dy=5.;
dz=10.0/l;

num=input('num=');

x=[0:m-1]*dx;
y=[0:n-1]*dy;
z=[0:l-1]*dz;

[X,Y,Z]=meshgrid(x,y,z);


fnum=sprintf('%.4d',num);
sali=load([fdir 'sali_' fnum]);

sali3d1=reshape(sali,[n,l,m]);
sali3d=permute(sali3d1,[1 3 2]);
sali2d=squeeze(sali3d(1,:,:))';


rx=[1:m];
ry=[1:n];
rz=[1:l];

clf

sali_c=23.5;

[faces,verts,colors] = isosurface(X(ry,rx,rz),Y(ry,rx,rz),Z(ry,rx,rz),sali3d(ry,rx,rz),sali_c,X(ry,rx,rz));
p=patch('Vertices', verts, 'Faces', faces, ... 
    'FaceVertexCData', colors, ... 
    'FaceColor','blue', ... 
    'edgecolor', 'none');


view([13 74])
%isosurface(X(ry,rx,rz),Y(ry,rx,rz),Z(ry,rx,rz),q(ry,rx,rz),-0.0001);
camlight; lighting gouraud
%lightangle(-35,70)
%axis([-700 1200 -700 100  -40 10])
grid
xlabel('Easting (m) ')
ylabel('Northing (m) ')
h=get(gca,'ylabel');
set(h,'rotation',-75)
h=get(gca,'xlabel');
set(h,'rotation',0)

zlabel('z (m) ')









