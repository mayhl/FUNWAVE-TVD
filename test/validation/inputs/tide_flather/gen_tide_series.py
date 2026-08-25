"""Generator for the tide_flather fixtures (data/tide_*.txt).

Committed as plain text and regenerated bit-identically; run from this dir:

    uv run python gen_tide_series.py

Tide-BC file layout (src/model/2d/tide.f90 series_open): a header line then
`t eta u v` records.  All four fixtures share one slow sinusoid,
eta = A sin(2 pi t / T), A = 0.5 m, T = 300 s (>> the 45 s basin transit at
h = 8 m, so the interior co-oscillates), sampled every 5 s to t = 900 s:

  tide_eta.txt    -- eta only (u = v = 0); co-oscillation target, and the
                     eta-only progressive arm
  tide_etauv.txt  -- eta AND the progressive-consistent u = eta sqrt(g/h),
                     the full incident Riemann invariant
  tide_uvonly.txt -- u only (eta = 0), the velocity-only arm
  tide_zero.txt   -- zeros, the radiating (target-0) downwave face
"""

import math
from pathlib import Path

A, T, H, G = 0.5, 300.0, 8.0, 9.81
C = math.sqrt(G / H)                       # u = eta * sqrt(g/h) for a progressive wave
DT, NT = 5.0, 181                          # 0 .. 900 s

out = Path(__file__).parent / "data"
specs = {
    "tide_eta":    lambda e: (e, 0.0),
    "tide_etauv":  lambda e: (e, e * C),
    "tide_uvonly": lambda e: (0.0, e * C),
    "tide_zero":   lambda e: (0.0, 0.0),
}
for name, f in specs.items():
    lines = [f"tide_flather {name}: t eta u v"]
    for k in range(NT):
        t = k * DT
        eta = A * math.sin(2.0 * math.pi * t / T)
        e, u = f(eta)
        lines.append(f"{t:.1f} {e:.6f} {u:.6f} 0.0")
    (out / f"{name}.txt").write_text("\n".join(lines) + "\n")
print(f"wrote 4 fixtures to {out} (u/eta = sqrt(g/h) = {C:.4f})")
