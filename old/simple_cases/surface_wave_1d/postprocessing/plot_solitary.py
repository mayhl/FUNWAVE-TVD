### PLOT SOLITARY WAVE CASE ###

# import necessary modules
import numpy as np               
import matplotlib.pyplot as plt

# write your OWN PC folder path for fdir
# Remember that we use for Mac & Linux machines '/', while on windows '\'
fdir = '/Users/Gaby/Desktop/Postprocessing-Workshop/simple_cases_output/surface_wave_1d/surface_wave_1D/' 

m = 1024      
dx = 1.0      
SLP = 0.05    
Xslp = 750.0


# Bathymetry computation
x = [float(xa)*dx for xa in range(m)]
dep = np.zeros((m,m))+10.0

for i in range(len(x)):
     if i < Xslp:
         dep[i,0]=dep[i,0]
     else:
         dep[i,0]=10-(x[i]-Xslp)*SLP

DEP = dep*-1  # - values are under MWL=0, + values are over MWL=0 

# Locate Wavemaker and sponge
files = [1,6,11,17]
wid = 6
length = 8


## Plot Output
fig = plt.figure(figsize=(7,8),dpi=600)

for num in range(len(files)):
    fnum = '%.5d' % files[num]

   
    eta = np.loadtxt(fdir+'eta_'+fnum)

    ax = fig.add_subplot(len(files),1,num+1)
    fig.subplots_adjust(hspace=1,wspace=.5)
    plt.plot(np.asarray(x),DEP[:,0],'k',np.asarray(x),eta[0,:],'b',linewidth=2)
    plt.grid()
    plt.xlabel('X (m)')
    plt.ylabel(r'$\eta$'+' (m)')
    ax.axis([0, 1024, -1.5, 1.5])

    title = 'Time = %s sec' % (files[num]*10)
    plt.title(title)


# save figure
fig.savefig('eta_1d_solitary.png', dpi=fig.dpi) # save figure
