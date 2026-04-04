### PLOT WAVE for inlet_shoal CASE ###

# import necessary modules
import numpy as np               
import matplotlib.pyplot as plt


# write your OWN PC folder path for fdir and dep.
# Remember that we use for Mac & Linux machines '/', while on windows '\'
fdir = '/Users/Gaby/Desktop/Postprocessing-Workshop/simple_cases_output/Intel_Shoal/inlet_shoal/'

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


nfile = [5, 11]      # range of eta files you want to plot   
min = ['150','900']  # time  you want to plot

# figure size option 
wid=10   # width
length=5 # length


# Plot Figure
fig = plt.figure(figsize=(wid,length),dpi=200)

for num in range(len(nfile)):
    fnum= '%.5d' % nfile[num]
    eta = np.loadtxt(fdir+'eta_'+fnum)
    mask = np.loadtxt(fdir+'mask_'+fnum)

    eta_masked = np.ma.masked_where(mask==0,eta)  # do not plot where mask = 0

    ax = fig.add_subplot(1,len(nfile),num+1)
    fig.subplots_adjust(hspace=1,wspace=.25)
    plt.pcolor(x, y, eta_masked,cmap='coolwarm')
    plt.axis('tight')
    title = 'Time = '+min[num]+ ' sec'
    plt.title(title)
    plt.hold(True)

    # plot sponge and wavemaker
    plt.plot(x_sponge,y_sponge,'g--',linewidth=3)
    plt.text(50,1000,'Sponge',color='g',rotation=90)
    plt.plot(x_wavemaker,y_wavemaker,'k-',linewidth=3)
    plt.text(300,1200,'Wavemaker',color='k',rotation=90)
   
    if num == 0:
        plt.ylabel('Y (m)')
        plt.xlabel('X (m)')
    else:
        plt.xlabel('X (m)')
        cbar=plt.colorbar()
        cbar.set_label(r'$\eta$'+' (m)', rotation=90)

# save figure        
fig.savefig('inlet__shoal_wave.png', dpi=fig.dpi)
