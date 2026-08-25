"""Generator for the wind_setup fixture (data/wind_ramp.txt).

Committed as plain text, regenerated bit-identically; run from this dir:

    uv run python gen_wind.py

Wind-file layout (src/model/2d/meteo.f90 constant_wind_setup): a title line, an
integer record count, then `t WU WV` rows (grid-frame components, m/s; linearly
interpolated in time, zero before the first record).  A uniform +x wind ramps
0 -> 40 m/s over 2400 s (~2 seiche periods of the 4 km / h=5 m basin) to suppress
the onset seiche, then holds.  The steady stress drives the setup the oracle gates.
"""

from pathlib import Path

RECORDS = [(0.0, 0.0, 0.0), (2400.0, 40.0, 0.0), (20000.0, 40.0, 0.0)]
lines = ["wind_setup ramped +x wind", str(len(RECORDS))]
lines += [f"{t:.1f} {wu:.1f} {wv:.1f}" for t, wu, wv in RECORDS]
(Path(__file__).parent / "data" / "wind_ramp.txt").write_text("\n".join(lines) + "\n")
print("wrote data/wind_ramp.txt")
