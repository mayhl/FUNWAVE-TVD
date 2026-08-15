#!/usr/bin/env python3
"""Generate the initial-condition fields and per-rank decks for poisson_scale_3d.

The 3D reader takes bathymetry as Nglob rows of Mglob values
(src/model/3d/old/io.F, DEPTH_TYPE == 'CELL_CENTER') and every hot-start 3D
field as Kglob blocks of Nglob rows of Mglob values (read_3d in kernel.F), so
the arrays are written in exactly that order.

Emits, next to this script:
  data/depth.txt data/eta0.txt data/u0.txt data/v0.txt data/w0.txt data/p0.txt
  np<N>.yaml  -- the base deck with decomposition pinned to PX x PY

Pinning is not cosmetic: PX and PY must divide Mglob and Nglob exactly, because
INDEX_LOCAL truncates Mglob/PX without a remainder check.  The script refuses to
emit a deck whose decomposition does not divide the grid.
"""

from pathlib import Path
import argparse
import math
import re
import sys

HERE = Path(__file__).resolve().parent
DECK = HERE / "poisson_scale_3d.yaml"

# grid defaults, kept in step with poisson_scale_3d.yaml; --grid overrides them
# so a sweep can vary the sigma-layer count or the tile size without editing the
# tracked deck, and --out keeps each variant out of the tracked data/ directory
MGLOB, NGLOB, KGLOB = 192, 96, 16
DX = DY = 0.2
DEPTH = 10.0       # flat bed, m
AMP = 0.1          # standing-wave amplitude, m
# wavelengths across the basin -> kh = 2*pi*NWAVE/(Mglob*dx)*h.  Widening the
# grid at fixed NWAVE stretches the wavelength and walks kh down toward the
# hydrostatic limit, which de-loads the very solve this deck exists to stress,
# so a sweep that changes Mglob must raise NWAVE to hold kh
NWAVE = 2

# rank ladder: np -> (PX, PY)
LADDER = {
    1: (1, 1),
    2: (2, 1),
    4: (2, 2),
    8: (4, 2),
    16: (4, 4),
    32: (8, 4),
    64: (8, 8),
}


def fmt_rows(rows):
    return "".join("".join(f"{v:16.7e}" for v in row) + "\n" for row in rows)


def write_fields(out):
    out.mkdir(parents=True, exist_ok=True)

    lx = MGLOB * DX
    k = 2.0 * math.pi * NWAVE / lx
    # x = (i-1)*dx matches the convention of the existing standing-wave deck
    eta = [AMP * math.cos(k * i * DX) for i in range(MGLOB)]

    (out / "depth.txt").write_text(fmt_rows([[DEPTH] * MGLOB] * NGLOB))
    (out / "eta0.txt").write_text(fmt_rows([eta] * NGLOB))

    zeros = fmt_rows([[0.0] * MGLOB] * (NGLOB * KGLOB))
    for name in ("u0", "v0", "w0", "p0"):
        (out / f"{name}.txt").write_text(zeros)

    return k


def write_decks(base, out):
    for np_, (px, py) in LADDER.items():
        if MGLOB % px or NGLOB % py:
            sys.exit(f"np={np_}: {px}x{py} does not divide {MGLOB}x{NGLOB}")
        text = re.sub(r"grid_size: \[.*?\]", f"grid_size: [{MGLOB}, {NGLOB}, {KGLOB}]", base)
        text = re.sub(r"nx_proc: \d+", f"nx_proc: {px}", text)
        text = re.sub(r"ny_proc: \d+", f"ny_proc: {py}", text)
        (out / f"np{np_}.yaml").write_text(text)
        cells = (MGLOB // px) * (NGLOB // py) * KGLOB
        print(f"np{np_:<3} PX x PY = {px}x{py}   cells/rank = {cells}")


def parse_args():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--grid", nargs=3, type=int, metavar=("M", "N", "K"),
                   default=[MGLOB, NGLOB, KGLOB])
    p.add_argument("--nwave", type=int, default=NWAVE,
                   help="wavelengths across the basin; integer keeps the closed-basin "
                        "mode's zero normal flux at both x walls (default: %(default)s)")
    p.add_argument("--out", type=Path, default=HERE,
                   help="directory for data/ and np<N>.yaml (default: alongside this script)")
    return p.parse_args()


if __name__ == "__main__":
    args = parse_args()
    MGLOB, NGLOB, KGLOB = args.grid
    NWAVE = args.nwave
    if NWAVE < 1:
        sys.exit(f"--nwave must be >= 1, got {NWAVE}")
    args.out.mkdir(parents=True, exist_ok=True)

    kh = write_fields(args.out / "data") * DEPTH
    print(f"grid {MGLOB}x{NGLOB}x{KGLOB} = {MGLOB * NGLOB * KGLOB} cells, kh = {kh:.3f}")
    write_decks(DECK.read_text(), args.out)
