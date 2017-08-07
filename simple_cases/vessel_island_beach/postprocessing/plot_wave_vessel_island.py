### Plot vessel_island wave  ###

# import necessary modules
import numpy as np               
import matplotlib.pyplot as plt


# write your OWN PC folder path for fdir & fdep
# Remember that we use for Mac & Linux machines '/', while on windows '\'
fdir = '/Users/Gaby/Desktop/Postprocessing-Workshop/simple_cases_output/vessel_island/vessel_island_beach/'

fdep ='/Users/Gaby/Desktop/Postprocessing-Workshop/simple_cases_output/vessel_island/'

mask=np.loadtxt(fdir+'eta_00001')

# ask user for plot start and end numbers
ns = int(raw_input("Input plot start number: ns = "))
ne = int(raw_input("Input plot end number: ne = "))


dep1 = np.loadtxt(fdep+'depth.txt')

n,m = np.shape(dep1)

x0 = 0.0
y0 = 2.0
dx = 1.0

x = np.asarray([(float(xa)*dx)+x0 for xa in range(m)])
y = np.asarray([(float(ya)*dx)+y0 for ya in range(n)])


# figure size option 
wid = 8   # width
length = 6 # length



for num in range(ns,ne):

    # plot figure
    fig = plt.figure(num,figsize=(wid,length),dpi=200)
    fnum= '%.5d' % num
    eta = np.loadtxt(fdir+'eta_'+fnum)

    eta_masked = np.ma.masked_where(eta>10.0,eta) # do nt plot where eta>10. 0

    plt.pcolor(x, y, eta_masked,cmap='coolwarm')
    cbar = plt.colorbar()
    cbar.set_label(r'$\eta$'+' (m)', rotation=90)
    plt.clim(-0.5, 0.5)
    
    plt.ylabel('Y (m)')
    plt.xlabel('X (m)')
    plt.axis('tight')


    NUM = num-1
    hour = '%.4d' % (NUM*2.5)
    title = 'Time = '+ hour + ' sec'
    plt.title(title)

    name = 'fig_eta_%.4d' % (num)

    # save figure
    fig.savefig(name+'.jpg', dpi=fig.dpi)
    
