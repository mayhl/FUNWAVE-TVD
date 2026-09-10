"""Tolerance-block contract shared by the validation oracles and the regression postprocs.

The runner hands each postproc its own ``tolerances:`` sub-block already
unwrapped by kind; keys the postproc never reads are the silent failure mode
(a misspelt gate passes on its default), so every entry point declares the
keys it accepts and checks them first.
"""

from __future__ import annotations


def check_keys(tolerances: dict, accepted: tuple[str, ...], oracle: str) -> None:
    """Raise on tolerance keys the oracle never reads (a misspelt gate would pass on its default)."""
    unknown = set(tolerances) - set(accepted)
    if unknown:
        raise ValueError(f"{oracle}: unknown tolerance key(s) {sorted(unknown)}; accepted: {accepted}")
