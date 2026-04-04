function Get_Inputs

mfreq = 1125;   % number of frequencies
mcomponent = mfreq; % number of wave components
mtheta = 25;    % number of directions

Hmo = 1;
fp = 0.1;      % peak freq
fmax=0.25;      % maximum freq
fmin=0.04;      % minimum freq
gamma_spec = 5; % TMA coefficient
theta_mean = 0;% mean direction in degress
sigma_theta = 10*pi/180; % directional spreading

depth = 8; %m % wk water depth
g = 9.81; %m/s2

% include random phase in input txt file or let funwave choose random
% number
phase_include = 1;

%%
% how many of the wave components are coherent
Coherent_Percentage = 100;

%% Twin percentage must be within 0 to 100 percent
if Coherent_Percentage<0
    Coherent_Percentage=0;
elseif Coherent_Percentage>100
    Coherent_Percentage=100;
end
%% Cutting extra digits from theta
theta_mean = 0.1*round(theta_mean*10);

save inputs.mat