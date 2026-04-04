clear all
T=12.42*3600;
t=[0:2000:96000];
z=1.0*sin(t/T*2*pi);
u=0.0*z;
v=0.0*z;
write_out(:,1)=t;
write_out(:,2)=z;
write_out(:,3)=u;
write_out(:,4)=v;
write_out=write_out';

fname='tide_data_west.txt';
fid=fopen(fname,'w');

fprintf(fid,'tide data (time,eta,u,v) \n');
fprintf(fid,['%10.1f','%10.4f','%10.4f','%10.4f', '\n'],write_out);

fclose(fid);
