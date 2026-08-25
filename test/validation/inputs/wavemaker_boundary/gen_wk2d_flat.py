"""Generator for the wm_data2d fixture (data/wk2d_flat.txt).

To keep the fixture reviewable we commit it as plain text and regenerate it
bit-identically from this script; run from this directory:

    uv run python gen_wk2d_flat.py

WK_DATA2D layout (see src/model/2d/wavemaker.f90 data2d_init_compute):
  nfreq ndir / PeakPeriod / freq per line / dir per line /
  amp(1:nfreq) row per dir / [optional phase rows -- omitted: seeded draw]

The table is the shared-rig ladder: 41 frequencies 0.08..0.28 Hz equal-df
(df = 0.005 Hz, endpoint-inclusive -> T_rec = 200 s), 7 directions
-30..30 deg (inside the engine's |dir| < 60 filter), flat amplitude
a = Hm0 / (4 sqrt(N/2)) with N = 41*7 so the component-sum Hm0
4 sqrt(sum(a^2)/2) equals the 1.0 m target exactly.
"""

import math
from pathlib import Path

HM0 = 1.0
NFREQ, NDIR = 41, 7
FMIN, FMAX = 0.08, 0.28
PEAK_PERIOD = 1.0 / 0.12

amp = HM0 / (4.0 * math.sqrt(NFREQ * NDIR / 2.0))
df = (FMAX - FMIN) / (NFREQ - 1)

lines = [f"{NFREQ} {NDIR}", f"{PEAK_PERIOD:.6f}"]
lines += [f"{FMIN + k * df:.6f}" for k in range(NFREQ)]
lines += [f"{-30.0 + 10.0 * i:.1f}" for i in range(NDIR)]
lines += [" ".join(f"{amp:.8f}" for _ in range(NFREQ)) for _ in range(NDIR)]

out = Path(__file__).parent / "data" / "wk2d_flat.txt"
out.write_text("\n".join(lines) + "\n")
print(f"wrote {out} (amp = {amp:.8f} m)")
