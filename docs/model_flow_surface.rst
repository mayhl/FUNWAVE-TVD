Surface Flow Technique
***********************

Different from a traditional CFD model where the water surface is governed by a separate surface equation such as in the Volume of Fluid method (VOF) or the Marker and Cell (MAC) method, a so-called``surface flow" technique was used by transforming the Navier-Stokes equations in Cartesian coordinates into the $\sigma$ coordinates. The governing equation for free surface can be written as

.. math::

      \frac{\partial D}{\partial t}+\frac{\partial }{\partial x} (D \int_0^1 u d \sigma)+\frac{\partial }{\partial y} (D \int_0^1 v d \sigma)=0
 
The use of :math:`\sigma` coordinate is consistent with FUNWAVE-TVD, in which the reference elevation :math:`z_\alpha` is in the :math:`\sigma` coordinate as in Kennedy et al. (2001). 