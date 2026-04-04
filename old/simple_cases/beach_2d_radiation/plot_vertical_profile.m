clear all
fdir = '/Users/fyshi/TMP/tmp4/';

files=[24];

dep=load([fdir 'dep.out']);
[n m]=size(dep);

z_num=10;


for k=1:length(files)

fnum=sprintf('%.5d',files(k));

eta=load([fdir 'eta_' fnum],'-ASCII');
mask=load([fdir 'mask_' fnum],'-ASCII');
u=load([fdir 'u_' fnum],'-ASCII');
v=load([fdir 'v_' fnum],'-ASCII');
Ax=load([fdir 'Ax_' fnum],'-ASCII');
Ay=load([fdir 'Ay_' fnum],'-ASCII');
Bx=load([fdir 'Bx_' fnum],'-ASCII');
By=load([fdir 'By_' fnum],'-ASCII');

% ---------------------------------
% u(z)=(za-z)Ax+0.5(za^2-z^2)Bx
% v(z)=(za-z)Ay+0.5(za^2-z^2)By
% ---------------------------------

for l=1:z_num
z(l,:,:)=-dep(:,:)*(l-1)/(z_num-1);
end

za=-0.5528*dep+0.4472*eta;
for l=1:z_num
zl=squeeze(z(l,:,:));
U(l,:,:)=u+(za-zl).*Ax+0.5*(za.^2-zl.^2).*Bx;
V(l,:,:)=v+(za-zl).*Ay+0.5*(za.^2-zl.^2).*By;
end



end




