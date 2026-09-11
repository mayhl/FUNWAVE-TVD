#!/usr/bin/env python3
"""Domain sketch for the dispersion_2d case, drawn from the deck itself.

Every dimension here is read out of kh_a.yaml rather than typed, so the figure
cannot disagree with the case it illustrates; the sweep decks differ only in
depth and mode number, and the geometry drawn is the shared one.

Diagram scripts take the output path as their one argument and write SVG; the
mkdocs pre-build hook supplies it (see docs/hooks.py).
"""

from __future__ import annotations

import sys
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
import yaml  # noqa: E402

REPO = Path(__file__).resolve().parents[3]
DECK = REPO / "test" / "validation" / "inputs" / "dispersion_2d" / "kh_a.yaml"


def main(out: Path) -> None:
    """Draw the basin, the initial surface that starts the seiche, and the gauge."""
    deck = yaml.safe_load(DECK.read_text())
    nx, ny = deck["grid"]["n_cells"]
    dx = deck["grid"]["cell_size"][0]
    depth = deck["grid"]["bathymetry"]["depth"]
    amp = deck["initial"]["sine_mode"]["amplitude"]
    gauge_x = next(c for c in deck["output"]["channels"] if c.get("type") == "station")["x"][0]
    length = nx * dx

    fig, ax = plt.subplots(figsize=(7.0, 2.8))

    # water column and bed
    ax.fill_between([0, length], [-depth, -depth], [0, 0], color="#cfe3f7", zorder=0)
    ax.plot([0, length], [0, 0], color="#1f4e79", lw=1.2)
    ax.plot([0, length], [-depth, -depth], color="#6b4f2a", lw=2.0)

    # closed basin: reflective walls on both ends carry the seiche
    for x in (0.0, length):
        ax.plot([x, x], [-depth, 0.35], color="#444", lw=2.5)
    ax.text(length / 2, 1.0, "closed basin, reflective walls, no forcing", ha="center", fontsize=9)

    # the initial condition: one half-wave tilted across the basin (drawn
    # exaggerated), released at t = 0; the surface then rocks between this
    # shape and its mirror at the seiche period
    xs = np.linspace(0, length, 400)
    tilt = 0.22 * np.cos(np.pi * xs / length)
    ax.plot(xs, tilt, color="#1f4e79", lw=1.4, ls="--")
    ax.plot(xs, -tilt, color="#1f4e79", lw=0.8, ls=":", alpha=0.6)
    ax.annotate(
        f"t = 0:  eta = A cos(pi x / L),  A = {amp:g} m",
        xy=(length * 0.3, 0.22 * np.cos(np.pi * 0.3)),
        xytext=(length * 0.42, 0.62),
        fontsize=8.5,
        color="#1f4e79",
        arrowprops=dict(arrowstyle="-", color="#1f4e79", lw=0.8),
    )
    ax.text(length * 0.8, 0.3, "t = T/2", fontsize=8, ha="center", color="#1f4e79", alpha=0.7)

    # the gauge sits near a wall (an antinode), where the oscillation is largest
    ax.plot([gauge_x], [0.0], marker="v", color="#dc2626", ms=7, zorder=5)
    ax.text(gauge_x + 0.4, -0.55, f"gauge, x = {gauge_x:g} m", fontsize=8.5, color="#dc2626", va="top")

    ax.annotate(
        "",
        xy=(0, -depth - 0.32),
        xytext=(length, -depth - 0.32),
        arrowprops=dict(arrowstyle="<->", color="#444", lw=1.0),
    )
    ax.text(length / 2, -depth - 0.62, f"L = {length:g} m  ({nx} x {ny} cells, dx = {dx:g} m)", ha="center", fontsize=9)
    ax.text(length * 0.02, -depth / 2, f"h = {depth:g} m  (kh_a; the sweep varies h)", fontsize=9, va="center")

    ax.set_xlim(-1.0, length + 1.0)
    ax.set_ylim(-depth - 1.1, 1.25)
    ax.axis("off")
    fig.tight_layout()
    fig.savefig(out, format="svg", bbox_inches="tight")


if __name__ == "__main__":
    main(Path(sys.argv[1]))
