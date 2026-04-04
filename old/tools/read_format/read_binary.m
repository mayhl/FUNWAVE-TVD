clear all
m=3190;
n=10100;
fileID=fopen('eta_00006');
eta=fread(fileID,[m n],'*single');
fclose(fileID);
eta=eta';
pcolor(eta),shading flat


