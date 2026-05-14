.. fully dispersive model documentation master file, created by
   sphinx-quickstart on Sun Feb 26 11:21:26 2023.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Highly Dispersive Nonhydrostatic Wave Model
==================================================

.. figure:: images/general/eta_googlemap_zm.png
   :width: 510px
   :height: 350px
   :align: right  


.. figure:: images/general/web_eta_F08_9layers.jpg
   :width: 400px
   :height: 250px
   :align: right  

.. figure:: images/examples/flow_bottom.png
   :width: 350px
   :height: 230px
   :align: right  


This website documents the development of a highly-dispersive wave model which can be used for modeling ship-wakes and wind waves in relatively deep water. A highly dispersive model was developed based on the surface flow technique used for a non-hydrostatic model. It has a multiple layer system, which can be configured by a user for a wave application in the highly dispersive wave regime. The development of the highly-dispersive wave model followed the existing FUNWAVE-TVD model framework. The model I/O files are consistent with that of FUNWAVE-TVD, facilitating the existing FUNWAVE-TVD users to use the new solver. The program is maitained  in the `GITHUB repository <https://github.com/fengyanshi/Fully_dispersive_model>`_

.. toctree::
   :maxdepth: 2

   intro
   model
   modules
   setup
   examples
   references


SEARCH IN SITE
==================

* :ref:`search`
