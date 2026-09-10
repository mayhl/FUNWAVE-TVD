#!/usr/bin/env python3
"""Generate the Pintado-Patino dam-break input fields (CACR-16-02 sec 4.2).

Domain 11 m x 3 cells at 1 cm: gate at x = 1.0 m (reservoir eta +0.72 m over
h0 = 0.06 m), sand beach 1:7 from the toe at x = 5.5 m.  Emits the eta IC and
the hard-bottom z_s field (sand thickness above the flume floor; zero on the
bare floor seaward of the toe).  Bathymetry itself is the analytic slope type.
"""

import numpy as np

M, N = 1100, 3
dx = 0.01
x = (np.arange(M) + 0.5) * dx

eta = np.where(x < 1.0, 0.72, 0.0)
np.savetxt("eta_ic.txt", np.tile(eta, (N, 1)), fmt="%.4f")

zs = np.where(x > 5.5, (x - 5.5) / 7.0, 0.0)
np.savetxt("hard_bottom.txt", np.tile(zs, (N, 1)), fmt="%.6f")
print("eta_ic.txt + hard_bottom.txt written")
