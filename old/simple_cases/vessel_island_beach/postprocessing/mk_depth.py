### Plot vessel_island wave  ###

# import necessary modules
import numpy as np               
import matplotlib.pyplot as plt



# compute bathymetry
m = 500   # x dimension points
n = 500   # y dimension points
dx = 2.0
dy = 2.0

x = np.asarray([(float(xa)*dx)-m/2*dx for xa in range(m)])
y = np.asarray([(float(ya)*dy)-n/2*dy for ya in range(n)])

X, Y = np.meshgrid(x,y)

R1 = 450
R2 = 100
Slope = 0.24
Slope_is = 0.24

dep = np.zeros((n,m))+10.0

for j in range(n):
    for i in range(m):
        
        r = np.sqrt(X[j,i]**2 + Y[j,i]**2)
        
        if r > R1:
            dep[j,i] = 10.0 - (r-R1)*Slope

        elif r<R2:
            dep[j,i] = 10.0 - (R2-r)*Slope_is

            

loc = np.where(dep<-2.0)  # locate indexes where dep < -2.0
dep[loc] = -2.0           # sustitute those values by -2.0

# figure size option 
wid = 8    # width
length = 6 # length

# plot figure
fig = plt.figure(figsize=(wid,length),dpi=200)
plt.pcolor(x, y, -1*dep,cmap='terrain')         #plot depth in figure
cbar=plt.colorbar()
cbar.set_label('Bathymetry (m)', rotation=90)

plt.ylabel('Y (m)')
plt.xlabel('X (m)')
plt.axis('tight')

Rs = R2 - 40.0
x0 = Rs
y0 = 0.0
speed0 = 10.0

xship = []
xship.append(x0)

yship = []
yship.append(y0)

actsp = []
actsp.append(0.0)

t0 = 50
Rship = 250

t = range(0,301)
for it in range(1,len(t)):
    if t[it] < t0:
        rship = Rs + (Rship-Rs)*t[it]/t0

    else:
        rship = Rship

    omega = speed0/Rship
    angle = t[it]*omega

    xship.append(rship*np.cos(angle))
    yship.append(rship*np.sin(angle))

    ACTSP = np.sqrt((xship[it]-xship[it-1])**2 + (yship[it]-yship[it-1])**2)/(t[it]-t[it-1])
    actsp.append(ACTSP)

    
plt.hold(True)
plt.plot(xship,yship,'w--',linewidth = 2)  # plot ship in figure

# save figure
fig.savefig('mk_depth.png', dpi=fig.dpi)



# create vessel file 
vessel = np.zeros((len(t),4))
vessel[:,0] = t
vessel[:,1] =np.asarray(xship)+m/2*dx
vessel[:,2] = np.asarray(yship)+n/2*dy
vessel[:,3] = np.asarray(actsp)

np.savetxt('depth.txt',dep)  
np.savetxt('vessel_001.txt',vessel)

