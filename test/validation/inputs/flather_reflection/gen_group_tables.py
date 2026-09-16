"""Generator for the flather_reflection fixtures (data/group_s*.txt, data/zero.txt).

To keep the fixtures reviewable we commit them as plain text and regenerate
them bit-identically from this script; run from this directory:

    uv run python gen_group_tables.py

WK_DATA2D layout (see src/model/2d/wavemaker.f90 data2d_init_compute):
  nfreq ndir / PeakPeriod / freq per line / dir per line /
  amp(1:nfreq) row per dir / phase(1:nfreq) row per dir (degrees)

One coherent wave group: a Gaussian amplitude ladder (61 lines, 0.085-0.165
Hz, sigma 0.012 Hz about the 8 s peak; T_rec = 750 s, longer than the run,
so the group passes once) with every line phased to peak together at
T0 = 60 s at the source (phase = 360 f T0, the engine's cos(phi - w t)).
Each sweep point spreads the SAME per-line energy over 7 directions with a
raised-cosine weight w(theta) = cos^2(pi theta / (2 (theta_max + dtheta)))
(edges stay non-zero); s00 is the single-direction group.  The engine
drops |dir| >= 60, so 45 is the widest point.  zero.txt is the zero external
target that turns the east forcing face into a pure Flather radiator.
"""

import math
from pathlib import Path

NFREQ, FMIN, FMAX = 61, 0.085, 0.165
FPEAK, SIGMA_F = 0.125, 0.012
T0 = 60.0  # group centre at the source (s)
A_PEAK = 0.25  # linear crest amplitude of the focused group (m)
POINTS = {"s00": (0.0, 0.0), "s15": (15.0, 5.0), "s30": (30.0, 10.0), "s45": (45.0, 15.0)}

df = (FMAX - FMIN) / (NFREQ - 1)
freqs = [FMIN + k * df for k in range(NFREQ)]
gauss = [math.exp(-0.5 * ((f - FPEAK) / SIGMA_F) ** 2) for f in freqs]
amp = [A_PEAK * g / sum(gauss) for g in gauss]
phase = [(360.0 * f * T0) % 360.0 for f in freqs]

here = Path(__file__).parent
for tag, (theta_max, step) in POINTS.items():
    if theta_max == 0.0:
        dirs, w = [0.0], [1.0]
    else:
        dirs = [-theta_max + step * i for i in range(int(round(2 * theta_max / step)) + 1)]
        w = [math.cos(math.pi * d / (2.0 * (theta_max + step))) ** 2 for d in dirs]
    scale = [math.sqrt(wi / sum(w)) for wi in w]  # sum_i a_ki^2 = a_k^2 at every point
    lines = [f"{NFREQ} {len(dirs)}", f"{1.0 / FPEAK:.6f}"]
    lines += [f"{f:.6f}" for f in freqs]
    lines += [f"{d:.1f}" for d in dirs]
    lines += [" ".join(f"{a * s:.8f}" for a in amp) for s in scale]
    lines += [" ".join(f"{p:.6f}" for p in phase) for _ in dirs]
    out = here / "data" / f"group_{tag}.txt"
    out.write_text("\n".join(lines) + "\n")
    hm0 = 4.0 * math.sqrt(0.5 * sum((a * s) ** 2 for a in amp for s in scale))
    print(f"wrote {out.name}: {len(dirs)} dirs, component Hm0 {hm0:.4f} m")

zero = here / "data" / "zero.txt"
zero.write_text("flather_reflection zero: t eta u v\n" + "".join(f"{5.0 * k:.1f} 0.000000 0.000000 0.0\n" for k in range(121)))
print(f"wrote {zero.name} (0-600 s)")
