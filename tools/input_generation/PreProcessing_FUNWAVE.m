%%This script produces FUNWAVE input files from UTM-grid data that is saved
%%in .mat format (and creates figures for visualization) 
%%Includes DEM layer, friction layer (optional) and obstacle layer (optional) 
%%Helpful outputs are displayed in the console.
%%Thomas Becker (University of Rhode Island, Technische Universitaet Braunschweig)
%%2023

clc; clear; close all; 

%% Basic INPUT (DEM):

%Input folder Containing all UTM input files (in .mat format, all files
%must be defined on the same UTM grid) 
Input_folder = '.\Inputs\';

%Output folder to store the files that are produced to be used as input
%in FUNWAVE simulations
Output_folder = '.\Outputs\';

%Switch to turn on (1) and off (0) preprocessing for cyclic lateral (south
%and north in local Funwave grid) boundary conditions
switch_cyclic_BC = 1;

%Filenames of .mat input of DEM on UTM grid
%(Data of input DEM in higher negative values for greater water depths)
name_input_data_DEM = 'measured_DEM.mat';
name_input_x_DEM = 'X_UTM_DEM.mat';
name_input_y_DEM = 'Y_UTM_DEM.mat';


% Name for Output DEM file
filename_Output_DEM = 'DEP.txt';

%Orgin coordinates in UTM grid, Rotation angle alpha (counterclockwise
%rotation of local grid compared to global UTM grid
x_origin_UTM = 282450; %m UTM
y_origin_UTM = 4575600; %m UTM
alpha_rotation = 105.5; %deg

%dx (cell length in x and y direction), local domain extents in x and y
%directions
dx_cells = 2; %m
x_extent_local = 8800; %m
y_extent_local = 2000; %m

%Sponge layer thicknesses
%(If a cyclic BC is used, the north and south sponge layer thicknesses are
%thicknesses over which the northern and southern edge are interpolated
%into each other. In that case, the Funwave input for those two is '0')
sponge_thickness_west = 300; %m (at x=0)
sponge_thickness_east = 200; %m (at x=nx)
sponge_thickness_south = 100; %m (at y=0)
sponge_thickness_north = 100; %m (at y=ny)


%Wavemaker specifications (Water depth at wavemaker and incident wave peak
%period) 
depth_wavemaker = 30.85; %m
Tp_wavemaker = 14.2; %s

%Wavelength factors for flat wavemaker section length
a_wavemaker = 1.5; %flat section length offshore of wavemaker center line is a * wavelength (recommended: 0.5<a<2)
b_wavemaker = 1.0; %flat section length onshore of wavemaker center line is b * wavelength

%Length of interpolation section between flat wavemaker section and actual
%bathymetry
l_trans = 100; %m  



%% INPUT for Adding Friction file

%Read Friction data and produce Funwave input from it: 
%Switch on (1) or off (0)  
switch_friction_file = 1;

%Filenames of .mat input of Friction Coefficient on UTM (Manning or Chezy
%depending on makefile of FUNWAVE)
name_input_data_Fric = 'manning.mat';
name_input_x_Fric = 'X_UTM_Manning.mat';
name_input_y_Fric = 'Y_UTM_Manning.mat';

% Name for Output Friction file
filename_Output_Fric = 'MANN.txt';


%Friction value for cells outside the real domain (for wavemaker and sponge
%layer sections of full domain; check if Manning or Chezy coefficient is
%used) 
friction_outside = 0.02;


%% INPUT for Adding Obstacle file

%Read Obstacle data and produce Funwave input from it: 
%Switch on (1) or off (0)  
switch_obstacle_file = 1;

%Filenames of .mat input of Obstacles on UTM (Binary: 1=No
%Obstacle, 0=Obstacle) 
name_input_data_Obs = 'Obstacles.mat';
name_input_x_Obs = 'X_UTM_obst.mat';
name_input_y_Obs = 'Y_UTM_obst.mat';

% Name for Output Friction file
filename_Output_Obs = 'OBS.txt';



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% No Input below %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read the DEM file

%Establish code directory
code_dir = pwd;

%Change to Input folder to read DEM data
cd(Input_folder)

%Data is expected to be saved in structures containing only one field, this
%field is opened to extract the respective arrays for data and grid

data = load(name_input_data_DEM);
fieldNames = fieldnames(data);
arrayFieldName = fieldNames{1};
bathyDEM = data.(arrayFieldName);

data = load(name_input_x_DEM);
fieldNames = fieldnames(data);
arrayFieldName = fieldNames{1};
Xutm = data.(arrayFieldName);

data = load(name_input_y_DEM);
fieldNames = fieldnames(data);
arrayFieldName = fieldNames{1};
Yutm = data.(arrayFieldName);

%change back to Code Directory
cd(code_dir);


    % Change sign (greater water depths are larger positives in FUNWAVE)
    bathyDEM = -bathyDEM;

    %% Rotate the UTM grid to local coordinates

    %Convert the angle of rotation to radians 
    rotation_angle = -pi/180*alpha_rotation; 

    %create a roation matrix to transform the local grid into the
    %global grid
    rot_mat = [cos(rotation_angle) -sin(rotation_angle); sin(rotation_angle) cos(rotation_angle)];


    %Shift the UTM grid according to local origin
    translated_x = Xutm - x_origin_UTM;
    translated_y = Yutm - y_origin_UTM;

    %Rotate the grid
    rotated_grid = rot_mat * [translated_x(:)'; translated_y(:)'];
    X_UTM_local = reshape(rotated_grid(1,:), size(Xutm));
    Y_UTM_local = reshape(rotated_grid(2,:), size(Yutm));

    
    %% Create new grid (without wavemaker and sponge layers and reinterpolate the DEM on it) 

    %extents in cells
    nx_extent_local = ceil(x_extent_local/dx_cells); 
    ny_extent_local = ceil(y_extent_local/dx_cells);

    %create x and y local vectors from input
    X_local = linspace(0, dx_cells*nx_extent_local, nx_extent_local);
    Y_local = linspace(0, dx_cells*ny_extent_local, ny_extent_local);

    % create x meash from these
    [X_local, Y_local] = meshgrid(X_local, Y_local);

    %reinterpolate the bathymetry on it
    zb_no_ghosts = griddata(X_UTM_local, Y_UTM_local, bathyDEM, X_local, Y_local);

    %plot the Bathymetry
    demcmap(-zb_no_ghosts) %for the color of the z axis being the one of demcmap plotting
    surf(X_local, Y_local, -zb_no_ghosts)
    shading interp
    view(3) %3D perspective
    xlabel('X[m]')
    ylabel('Y[m]')
    zlabel('Z[m]')
    daspect([1 1 0.02]) %set aspect ratios manually
    title('Input DEM in Real Domain')
    colorbar
    print('DEM_Initial.png','-dpng','-r400')
    savefig('DEM_Initial.fig')


    %% Calculate the extents and bathymetry for the internal wavemaker

    %Flat bottom for wavemaker for 0.5 wavelengths offshore of wavemaker
    %and 1 wavelength onshore of wavemaker

    %Deep water wavelength of peak period wave
    L0 = 9.81*Tp_wavemaker^2/(2*pi);

    %find the wavelength at the wavemaker depth (dispersion relation)
    L=100; %set starting wavelength of 100m
    while abs(L-L0*tanh(2*pi*depth_wavemaker/L))>0.001 %error margin =1mm
        Lnew=L0*tanh(2*pi*depth_wavemaker/L); 
        L=Lnew;
    end

    %Calculate the number of flat cells required offshore of wavemaker
    %(round up)
    nx_wavemaker_west = ceil(a_wavemaker*L/dx_cells);

    %Calculate the number of flat cells required onshore of wavemaker
    nx_wavemaker_east = ceil(b_wavemaker*L/dx_cells);

    
    %Add a layer onshore of the flat section to interpolate between flat
    %depth and actual DEM over a defined length
    nx_interpolation = ceil(l_trans/dx_cells);

    %Get the depths at the offshore edge of the real DEM (to interpolate
    %between the wavemaker depth and them)
    depths_edge_offshore = zb_no_ghosts(:,1);

    %Initialize the Matrix for the interpolated water depths between
    %wavemaker section and real DEM
    zb_interpolation = NaN(ny_extent_local, nx_interpolation);

    %Run through the matrix and make every line (in x-direction) a linear
    %interpolation between the real DEM edge depth and the wavemaker depth
    for i=1:ny_extent_local
        zb_interpolation(i,:)=linspace(depth_wavemaker,depths_edge_offshore(i),nx_interpolation);
    end



    %% Calculate the extents and DEM for the sponge layers and add to bathymetry

    %Calculate the respective extents of the sponge layers on x-end and
    %y-end
    nx_sponge_west = ceil(sponge_thickness_west/dx_cells);
    nx_sponge_east = ceil(sponge_thickness_east/dx_cells);
    ny_sponge_south = ceil(sponge_thickness_south/dx_cells);
    ny_sponge_north = ceil(sponge_thickness_north/dx_cells);


    %Add all sections along x and y axes for total number of cells per
    %direction
    nx_total = nx_sponge_west + nx_wavemaker_west + nx_wavemaker_east + nx_interpolation + nx_extent_local + nx_sponge_east;
    ny_total = ny_sponge_south + ny_extent_local + ny_sponge_north;

    %Create the according mesh
    x_domain = linspace(0, dx_cells*nx_total, nx_total);
    y_domain = linspace(0, dx_cells*ny_total, ny_total);

    [X_domain, Y_domain] = meshgrid(x_domain, y_domain);

    %Initialize the DEM matrix for the entire domain
    zb_domain = NaN(ny_total, nx_total);

    
    %Fill in the full DEM

    %fill in the measured DEM
    zb_domain((ny_sponge_south+1):(ny_total-ny_sponge_north), (nx_sponge_west + nx_wavemaker_west + nx_wavemaker_east + nx_interpolation+1):(nx_total-nx_sponge_east))=zb_no_ghosts;

    %fill in western sponge layer and constant depth wavemaker section with
    %the constant value of the wavemaker depth
    zb_domain((ny_sponge_south+1):(ny_total-ny_sponge_north), 1:(nx_sponge_west+nx_wavemaker_west+nx_wavemaker_east))=depth_wavemaker;
    
    %fill in the interpolation DEM offshore between wavemaker and measured
    %DEM
    zb_domain((ny_sponge_south+1):(ny_total-ny_sponge_north), (nx_sponge_west+nx_wavemaker_west+nx_wavemaker_east+1):(nx_sponge_west + nx_wavemaker_west + nx_wavemaker_east + nx_interpolation))=zb_interpolation;

    %Calculate the column values that make up the eastern sponge layer
    %(these are the values at the eastern edge of the measured DEM) and
    %assign these values to all cells in the east 
    vector_sponge_east = zb_domain((ny_sponge_south+1):(ny_total-ny_sponge_north),nx_total-nx_sponge_east);
    
    for i=(nx_total-nx_sponge_east+1):nx_total
        zb_domain((ny_sponge_south+1):(ny_total-ny_sponge_north),i) = vector_sponge_east;
    end
    


    %Calculate the row vectors that make up the southern and northern
    %real domain edge (all values on the first and last rows of the
    %already value-assigned domain matrix section)
    vector_sponge_south = zb_domain(ny_sponge_south+1,:);
    vector_sponge_north = zb_domain(ny_total-ny_sponge_north,:);

    %if the boundary condition is non-cyclic, copy the edges as the sponge layers 
    if switch_cyclic_BC == 0
        for i=1:ny_sponge_south
            zb_domain(i,:) = vector_sponge_south;
        end
        
        for i=(ny_total-ny_sponge_north+1):ny_total
            zb_domain(i,:) = vector_sponge_north;
        end

    %if the boundary condition is cyclic, replace the sponge layers with an
    %interpolation from both sides, where the resulting southmost and
    %northmost edge of the sponge layers are equal
    else
        %Number of interpolated vectors between south and north edge of
        %DEM is on less than the total sponge layer cell extent because the
        %respective last vector is identical;
        n_vectors_cyclic = ny_sponge_south+ny_sponge_north-1;

        %initialize a matrix to store the interpolated vectors
        interpolatedVectors = zeros(numel(vector_sponge_south), n_vectors_cyclic);

        %Interpolate the elements between north and south vectors linearily
        for i = 1:numel(vector_sponge_south)
            interpolatedVectors(i, :) = interp1([0 1], [vector_sponge_north(i) vector_sponge_south(i)], linspace(0, 1, n_vectors_cyclic));
        end

        %Add these vectors to the respective sides of the full domain
        for i=1:ny_sponge_south
            zb_domain(i,:) = interpolatedVectors(:, i+ny_sponge_north-1);
        end

        for i=1:ny_sponge_north
            zb_domain(i+ny_extent_local+ny_sponge_south,:) = interpolatedVectors(:,i);
        end

    end

    %Plot the full DEM
    demcmap(-zb_domain) %for the color of the z axis being the one of demcmap plotting
    surf(X_domain, Y_domain, -zb_domain)
    shading interp
    view(3) %3D perspective
    xlabel('X[m]')
    ylabel('Y[m]')
    zlabel('Z[m]')
    daspect([1 1 0.02]) %set aspect ratios manually
    title('Input DEM in Full Funwave Domain')
    colorbar
    print('DEM_Full.png','-dpng','-r400')
    savefig('DEM_Full.fig')
    
    %% Display relevant parameters

    %Display the number of cells in x-direction
    disp(strcat('Number of cells in x-direction Mglob = ' , num2str(nx_total)))

    %Display the number of cells in y-direction
    disp(strcat('Number of cells in y-direction Nglob = ' , num2str(ny_total)))

    %Calculate and display the position [m] of the wavemaker
    pos_wavemaker = (nx_sponge_west+nx_wavemaker_west)*dx_cells;
    disp(strcat('Position of the internal wavemaker = ' , num2str(pos_wavemaker), ' [m]'))

    %Calculate and display the position [m] of the wavemaker
    disp(strcat('Last cell in x-direction of flat wavemaker section: No.' , num2str(nx_sponge_west+nx_wavemaker_west+nx_wavemaker_east)))

    %Display the number of the first and last real grid cells in x and y
    %direction (cells derived from the original measured DEM)
    disp(strcat('First real domain cell in x-direction: No.', num2str((nx_sponge_west + nx_wavemaker_west + nx_wavemaker_east + nx_interpolation+1))))
    disp(strcat('Last real domain cell in x-direction: No.', num2str((nx_total-nx_sponge_east))))


    disp(strcat('First real domain cell in y-direction: No.', num2str((ny_sponge_south+1))))
    disp(strcat('Last real domain cell in y-direction: No.', num2str((ny_total-ny_sponge_north))))

    %Display reminder to set North and South sponge layer thickness to
    %zero in Funwave input if cyclic BC is active
    if switch_cyclic_BC == 1
        disp('Cyclic Boundary condition used: Set North and South Sponge layer thicknesses to 0.0 in Funwave input!')
    end 

    %% Create a .txt DEM file

    %Display in console that DEM file is being written
    fprintf('\n')
    disp('DEM data computed. Outputfile is being written.')


    %Change to output folder
    cd(Output_folder)


    %Create the textfile for the DEM data and open it for writing
    fileID = fopen(filename_Output_DEM, 'w');

    %Write the DEM textfile

    %iterate through the calculated nodes in y-direction
    for i=1:ny_total
      %iterate through the calculated nodes in x-direction
      for j=1:nx_total
      %write the current water depth value of the matrix into the
      %input file
      fprintf(fileID, '%d ', zb_domain(i,j));  
      end
      %create a new line for the next ny
      fprintf(fileID, '\n');
    end
    
    % Close the DEM file
    fclose(fileID);

    %Change back to code directory
    cd(code_dir)

    disp('DEM file is completed.')


    %% Friction Data Part

    if switch_friction_file ==1
        %% Read the friction Data

        %Change to Input folder to read DEM data
        cd(Input_folder)
    
        %Data is expected to be saved in structures containing only one field, this
        %field is opened to extract the respective arrays for data and grid
        
        data = load(name_input_data_Fric);
        fieldNames = fieldnames(data);
        arrayFieldName = fieldNames{1};
        friction_UTM = data.(arrayFieldName);
        
        data = load(name_input_x_Fric);
        fieldNames = fieldnames(data);
        arrayFieldName = fieldNames{1};
        Xutm = data.(arrayFieldName);
        
        data = load(name_input_y_Fric);
        fieldNames = fieldnames(data);
        arrayFieldName = fieldNames{1};
        Yutm = data.(arrayFieldName);
        
        %change back to Code Directory
        cd(code_dir);
    
    
    
        %% Shift the Friction UTM grid to local coordinates and reinterpolate on the local grid
            
        %Shift the UTM grid according to local origin
        translated_x = Xutm - x_origin_UTM;
        translated_y = Yutm - y_origin_UTM;
    
        %Rotate the grid
        rotated_grid = rot_mat * [translated_x(:)'; translated_y(:)'];
        X_UTM_local = reshape(rotated_grid(1,:), size(Xutm));
        Y_UTM_local = reshape(rotated_grid(2,:), size(Yutm));


        %reinterpolate the Friction matrix on the local grid
        friction_no_ghosts = griddata(X_UTM_local, Y_UTM_local, friction_UTM, X_local, Y_local);

        %Plot the local friction map
        mapshow(X_local, Y_local, friction_no_ghosts,'DisplayType', 'surface')
        colormap turbo
        xlabel('X[m]')
        ylabel('Y[m]')
        axis tight
        title('Friction Coefficient in Real Domain')
        colorbar
        print('Friction_Initial.png','-dpng','-r400')
        savefig('Friction_Initial.fig')
        
        %% Fill in the whole domain Friction grid with local values and outside filler value 

        %Initialize the friction matrix for the entire domain (filler
        %values will not be replaced later and are inserted here)
        friction_domain = friction_outside*ones(ny_total, nx_total);

        %Fill in the real domain values (at the correct indices)
        friction_domain((ny_sponge_south+1):(ny_total-ny_sponge_north), (nx_sponge_west + nx_wavemaker_west + nx_wavemaker_east + nx_interpolation+1):(nx_total-nx_sponge_east))=friction_no_ghosts;


        %Plot the global friction map
        mapshow(X_domain, Y_domain, friction_domain, 'DisplayType', 'surface')
        colormap turbo
        xlabel('X[m]')
        ylabel('Y[m]')
        axis tight
        title('Friction Coefficient in Full Funwave Domain')
        colorbar
        print('Friction_Full.png','-dpng','-r400')
        savefig('Friction_Full.fig')


        %% Create a .txt Friction file
        
        %Display in console that friction file is being written
        fprintf('\n')
        disp('Friction data computed. Outputfile is being written.')


        %Change to output folder
        cd(Output_folder)
    
        %Create the textfile and open it for writing
        fileID = fopen(filename_Output_Fric, 'w');
    
        %Write the Friction textfile
    
        %iterate through the calculated nodes in y-direction
        for i=1:ny_total
          %iterate through the calculated nodes in x-direction
          for j=1:nx_total
          %write the current coefficient value of the matrix into the
          %output file
          fprintf(fileID, '%d ', friction_domain(i,j));  
          end
          %create a new line for the next ny
          fprintf(fileID, '\n');
        end
        
        % Close the friction file
        fclose(fileID);
    
        %Change back to code directory
        cd(code_dir)

        disp('Friction file is completed.')
    end



%% Obstacle Data Part

    if switch_obstacle_file ==1
        %% Read the obstacle Data

        %Change to Input folder to read DEM data
        cd(Input_folder)
    
        %Data is expected to be saved in structures containing only one field, this
        %field is opened to extract the respective arrays for data and grid
        
        data = load(name_input_data_Obs);
        fieldNames = fieldnames(data);
        arrayFieldName = fieldNames{1};
        obstacles_UTM = data.(arrayFieldName);
        
        data = load(name_input_x_Obs);
        fieldNames = fieldnames(data);
        arrayFieldName = fieldNames{1};
        Xutm = data.(arrayFieldName);
        
        data = load(name_input_y_Obs);
        fieldNames = fieldnames(data);
        arrayFieldName = fieldNames{1};
        Yutm = data.(arrayFieldName);
        
        %change back to Code Directory
        cd(code_dir);
    
    
    
        %% Shift the Obstacle UTM grid to local coordinates and reinterpolate on the local grid
        
        %Shift the UTM grid according to local origin
        translated_x = Xutm - x_origin_UTM;
        translated_y = Yutm - y_origin_UTM;
    
        %Rotate the grid
        rotated_grid = rot_mat * [translated_x(:)'; translated_y(:)'];
        X_UTM_local = reshape(rotated_grid(1,:), size(Xutm));
        Y_UTM_local = reshape(rotated_grid(2,:), size(Yutm));


        %reinterpolate the obstacle matrix on the local grid
        obst_no_ghosts = griddata(X_UTM_local, Y_UTM_local, obstacles_UTM, X_local, Y_local);


        %The matrix resulting from the reinterpolation could contain
        %decimal numbers between 0 and 1. It is rounded to stay in binary
        %format (either 0 or 1).
        obst_no_ghosts = round(obst_no_ghosts);

        %create a black/white colorbar
        colors_blackwhite = [0 0 0; 1 1 1];
        

        %Plot the local obstacle map
        mapshow(X_local, Y_local, obst_no_ghosts,'DisplayType', 'surface')
        xlabel('X[m]')
        ylabel('Y[m]')
        axis tight
        colorbar('Ticks', [0, 1], 'TickLabels', {'Obstacle', 'No Obstacle'});
        colormap(colors_blackwhite);
        title('Obstacles in Real Domain')
        print('Obstacles_Initial.png','-dpng','-r400')
        savefig('Obstacles_Initial.fig')

        
        %% Fill in the whole domain Obstacle grid with local values 

        %Initialize the obstacle matrix for the entire domain (Outside of
        %the domain: No obstacles, so cells have value 1)
        obst_domain = ones(ny_total, nx_total);

        %Fill in the real domain values (at the correct indices)
        obst_domain((ny_sponge_south+1):(ny_total-ny_sponge_north), (nx_sponge_west + nx_wavemaker_west + nx_wavemaker_east + nx_interpolation+1):(nx_total-nx_sponge_east))=obst_no_ghosts;

        %Plot the global obstacle map
        mapshow(X_domain, Y_domain, obst_domain, 'DisplayType', 'surface')
        xlabel('X[m]')
        ylabel('Y[m]')
        axis tight
        colorbar('Ticks', [0, 1], 'TickLabels', {'Obstacle', 'No Obstacle'});
        colormap(colors_blackwhite);
        title('Obstacles in Full Funwave Domain')
        print('Obstacles_Full.png','-dpng','-r400')
        savefig('Obstacles_Full.fig')


        %% Create a .txt Obstacle file
    
        %Display in console that obstacle file is being written
        fprintf('\n')
        disp('Obstacle data computed. Outputfile is being written.')

        %Change to output folder
        cd(Output_folder)
    
        %Create the textfile and open it for writing
        fileID = fopen(filename_Output_Obs, 'w');
    
        %Write the DEM textfile
    
        %iterate through the calculated nodes in y-direction
        for i=1:ny_total
          %iterate through the calculated nodes in x-direction
          for j=1:nx_total
          %write the current binary value of the matrix into the
          %output file
          fprintf(fileID, '%i ', obst_domain(i,j));  
          end
          %create a new line for the next ny
          fprintf(fileID, '\n');
        end
        
        % Close the DEM file
        fclose(fileID);
    
        %Change back to code directory
        cd(code_dir)

        disp('Obstacle file is completed.')
    end

