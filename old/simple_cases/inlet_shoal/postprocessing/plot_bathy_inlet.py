### PLOT inlet_shoal bathymetry ###

# import necessary modules
import numpy as np               
import matplotlib.pyplot as plt


# write your OWN PC folder path for dep.
# Remember that we use for Mac & Linux machines '/', while on windows '\'
dep=np.loadtxt('/Users/Gaby/Desktop/Postprocessing-Workshop/simple_cases_output/Intel_Shoal/dep_shoal_inlet.txt')

# define bathy location
n,m = np.shape(dep)
dx = 2.0
dy = 2.0

x = np.asarray([float(xa)*dx for xa in range(m)])
y = np.asarray([float(ya)*dy for ya in range(n)])

# define wavemaker and sponge location
x_sponge = [0,180,180,0,0]
y_sponge = [0,0,y[len(y)-1],y[len(y)-1],0]

x_wavemaker = [240, 260, 260, 240, 240]
y_wavemaker = [0, 0, y[len(y)-1],y[len(y)-1],0]


# figure size option 
wid=6    # width
length=5 # length

# Plot figure
fig = plt.figure(figsize=(wid,length),dpi=200)
ax = fig.add_subplot(1,1,1)
fig.subplots_adjust(hspace=1,wspace=.25)

plt.pcolor(x, y, -1*dep,cmap='terrain')
plt.axis('tight')  
plt.ylabel('Y (m)')
plt.xlabel('X (m)')
plt.hold(True)

# plot sponge and wavemaker
plt.plot(x_sponge,y_sponge,'k--',linewidth=3)
plt.text(10,1000,'Sponge',color='w',rotation=90)
plt.plot(x_wavemaker,y_wavemaker,'k-',linewidth=3)
plt.text(270,1200,'Wavemaker',color='w',rotation=90)

# figure colorbar
cbar=plt.colorbar()
cbar.set_label('Bathymetry (m)', rotation=90)

# save figure
fig.savefig('inlet__shoal_bathy.png', dpi=fig.dpi)
