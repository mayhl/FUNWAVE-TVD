#!/usr/bin/env python3
"""Generate Fortran config-defaults constants from src/model/registry.yaml.

The registry is the single source of truth for YAML parameter defaults
(see .private_docs/STANDARDS.md).  The generated module is COMMITTED to the
repo so Fortran builds never depend on Python; run this script after editing
registry.yaml and commit both files together.

SCOPE FREEZE (registry v1): defaults + metadata only.  Do NOT grow this into
validation (types/constraints/conditionals) — the flat parameter list cannot
express variant defaults (e.g. wavemaker Ntheta/Sigma_Theta differ by type)
or nesting.  Registry v2 (post-Step 6, own design brief) replaces it with a
hierarchical schema from which read_input bodies and the unknown-key gate are
generated against the yaml read_* validation primitives.

Usage:
    uv run scripts/gen_registry.py            # regenerate in place
    uv run scripts/gen_registry.py --check    # verify committed file is in sync
                                              # (exit 1 + diff if stale)
"""

from __future__ import annotations

import argparse
import difflib
import re
import sys
from pathlib import Path

import yaml

REPO = Path(__file__).resolve().parent.parent
REGISTRY = REPO / "src" / "model" / "registry.yaml"
OUTPUT = REPO / "src" / "model" / "2d" / "config_defaults.f90"

HEADER = """\
! allow(E001)
! =================================================================
!  GENERATED FILE — DO NOT EDIT.
!  Source:    src/model/registry.yaml
!  Generator: scripts/gen_registry.py   (rerun after registry edits)
!  Sync test: scripts/gen_registry.py --check
! =================================================================
!> @file config_defaults.f90
!> @brief Generated YAML-parameter default constants (registry single source).
module model_config_defaults_mod
   implicit none
   public

"""

FOOTER = "\nend module model_config_defaults_mod\n"


def fortran_default(value) -> str | None:
    """Render a registry default as the string the yaml readers expect."""
    if value is None:
        return None
    if isinstance(value, bool):
        return "YES" if value else "NO"
    return str(value)


def const_name(yaml_path: str) -> str:
    section, key = yaml_path.split(".", 1)
    key = re.sub(r"[^A-Za-z0-9]", "_", key)
    return f"DEF_{section.upper()}_{key.upper()}"


def validate(reg: dict) -> list[str]:
    errors = []
    seen_paths: set[str] = set()
    for p in reg.get("parameters", []):
        path = p.get("yaml_path")
        if not path or "." not in path:
            errors.append(f"parameter {p.get('name')}: missing/invalid yaml_path")
            continue
        if path in seen_paths:
            errors.append(f"duplicate yaml_path: {path}")
        seen_paths.add(path)
        for field in ("name", "units", "default", "latex"):
            if field not in p:
                errors.append(f"{path}: missing field {field!r}")
    for v in reg.get("variables", []):
        if not v.get("standard_name") and not v.get("funwave_name"):
            errors.append(f"variable {v.get('name')}: needs standard_name or funwave_name")
    return errors


def generate(reg: dict) -> str:
    lines = [HEADER]
    params = sorted(reg.get("parameters", []), key=lambda p: p["yaml_path"])
    section = None
    for p in params:
        default = fortran_default(p.get("default"))
        if default is None:
            continue  # no default -> reader handles absence itself
        sec = p["yaml_path"].split(".")[0]
        if sec != section:
            lines.append(f"   ! ── {sec} ──\n")
            section = sec
        name = const_name(p["yaml_path"])
        lines.append(f'   character(*), parameter :: {name} = "{default}"\n')
    lines.append(FOOTER)
    return "".join(lines)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true", help="verify committed output matches the registry")
    args = ap.parse_args()

    reg = yaml.safe_load(REGISTRY.read_text())
    errors = validate(reg)
    if errors:
        print("registry.yaml validation FAILED:", file=sys.stderr)
        for e in errors:
            print(f"  - {e}", file=sys.stderr)
        return 2

    new = generate(reg)
    if args.check:
        old = OUTPUT.read_text() if OUTPUT.exists() else ""
        if old == new:
            print(f"OK: {OUTPUT.relative_to(REPO)} is in sync with registry.yaml")
            return 0
        print(
            f"STALE: {OUTPUT.relative_to(REPO)} does not match registry.yaml — rerun: uv run scripts/gen_registry.py",
            file=sys.stderr,
        )
        sys.stderr.writelines(
            difflib.unified_diff(
                old.splitlines(keepends=True), new.splitlines(keepends=True), fromfile="committed", tofile="generated"
            )
        )
        return 1

    OUTPUT.write_text(new)
    print(f"wrote {OUTPUT.relative_to(REPO)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
