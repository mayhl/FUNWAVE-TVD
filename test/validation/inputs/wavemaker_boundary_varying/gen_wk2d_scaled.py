"""Generator for the spatially-varying boundary-feed fixtures.

Two anchor spectra for the wavemaker_boundary_varying case, committed as plain
text and regenerated bit-identically from this script; run from this directory:

    uv run python gen_wk2d_scaled.py

The two anchors are SCALED copies of one flat single-direction ladder so the
component amplitudes (hence Hm0) interpolate linearly along the face: anchor B
= 3 x anchor A, giving Hm0_A = 0.5 m and Hm0_B = 1.5 m.  A single direction
(theta = 0) keeps each alongshore column quasi-independent, so a near-boundary
station reads its LOCAL fed Hm0 without alongshore-propagation smearing.

WK_DATA2D layout (see src/model/2d/wavemaker.f90 read_one_spectrum):
  nfreq ndir / PeakPeriod / freq per line / dir per line /
  amp(1:nfreq) row per dir / [phase rows omitted -> shared seeded draw]

The ladder is the shared rig: 41 frequencies 0.08..0.28 Hz equal-df
(df = 0.005 Hz, endpoint-inclusive -> T_rec = 200 s), one direction at 0 deg,
flat amplitude a = Hm0 / (4 sqrt(N/2)) with N = 41 so the component-sum Hm0
4 sqrt(sum(a^2)/2) equals the target exactly.
"""

import math
from pathlib import Path

NFREQ, NDIR = 41, 1
FMIN, FMAX = 0.08, 0.28
PEAK_PERIOD = 1.0 / 0.12
HM0 = {"A": 0.5, "B": 1.5}  # anchor targets (B = 3 A -> linear Hm0 profile)

df = (FMAX - FMIN) / (NFREQ - 1)
data = Path(__file__).parent / "data"
data.mkdir(exist_ok=True)

for tag, hm0 in HM0.items():
    amp = hm0 / (4.0 * math.sqrt(NFREQ * NDIR / 2.0))
    lines = [f"{NFREQ} {NDIR}", f"{PEAK_PERIOD:.6f}"]
    lines += [f"{FMIN + k * df:.6f}" for k in range(NFREQ)]
    lines += ["0.0"]
    lines += [" ".join(f"{amp:.8f}" for _ in range(NFREQ)) for _ in range(NDIR)]
    out = data / f"wk2d_{tag}.txt"
    out.write_text("\n".join(lines) + "\n")
    print(f"wrote {out} (Hm0 = {hm0:.2f} m, amp = {amp:.8f} m)")
