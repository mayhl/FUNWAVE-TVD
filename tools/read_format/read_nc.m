clear all

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%%%%%%%                               %%%%%%%%%%%%
        %%%%%%%%%%%     Read netCDF data files    %%%%%%%%%%%%
        %%%%%%%%%%%  Written by Christian Stranne %%%%%%%%%%%%
        %%%%%%%%%%%          2011-04-02           %%%%%%%%%%%%
        %%%%%%%%%%%                               %%%%%%%%%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%
% This script loads netCDF files into MATLAB and displays info about the 
% dimensions and variables. I wrote this since can't find a function in the
% built-in set of netCDF functions included in MATLAB that displays all 
% header info of the file (equivalent to the old 'ncdump' function). 
%
% Replace the string 'filename.nc' with the file name you want to load.
% Make sure that the nc-file is located in the same folder as this script.

clear all

nc_filename = 'virginia_beach_13_mhw_2007.nc'; 
                                 
ncid=netcdf.open(nc_filename,'nowrite'); 


% Get information about the contents of the file.
[numdims, numvars, numglobalatts, unlimdimID] = netcdf.inq(ncid);

disp(' '),disp(' '),disp(' ')
disp('________________________________________________________')
disp('^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~')
disp(['VARIABLES CONTAINED IN THE netCDF FILE: ' nc_filename ])
disp(' ')
for i = 0:numvars-1
    [varname, xtype, dimids, numatts] = netcdf.inqVar(ncid,i);
    disp(['--------------------< ' varname ' >---------------------'])
    flag = 0;
    for j = 0:numatts - 1
        attname1 = netcdf.inqAttName(ncid,i,j);
        attname2 = netcdf.getAtt(ncid,i,attname1);
        disp([attname1 ':  ' num2str(attname2)])
        if strmatch('add_offset',attname1)
            offset = attname2;
        end
        if strmatch('scale_factor',attname1)
            scale = attname2;
            flag = 1;
        end        
    end
    disp(' ')
    
    if flag
        eval([varname '= double(double(netcdf.getVar(ncid,i))*scale + offset);'])
    else
        eval([varname '= double(netcdf.getVar(ncid,i));'])   
    end
end
disp('^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~')
disp('________________________________________________________')
disp(' '),disp(' ')

dep=Band1';
sk=10;
pcolor(lon(1:sk:end),lat(1:sk:end),dep(1:sk:end,1:sk:end)),shading flat
demcmap(dep);
set(gca, 'LineWidth',  2)
xlabel('Longitude(^\circ)');
ylabel('Latitude(^\circ)')

mst=1;
men=6500;
nst=2000;
nen=8000;

dep1=dep(nst:nen,mst:men);
lat1=lat(nst:nen);
lon1=lon(mst:men);

save -ASCII dep_median.txt dep1
save -ASCII lat_median.txt lat1
save -ASCII lon_median.txt lon1

figure
sk=1;
pcolor(lon1(1:sk:end),lat1(1:sk:end),dep1(1:sk:end,1:sk:end)),shading flat
demcmap(dep1);
set(gca, 'LineWidth',  2)
xlabel('Longitude(^\circ)');
ylabel('Latitude(^\circ)')



