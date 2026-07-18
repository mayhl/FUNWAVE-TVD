#!/usr/bin/env python3
"""Generate Fortran config-defaults constants from src/model/registry.yaml.

The registry is the single source of truth for YAML parameter defaults
(see .private_docs/STANDARDS.md).  The generated module is COMMITTED to the
repo so Fortran builds never depend on Python; run this script after editing
registry.yaml and commit both files together.

The registry is a DESCRIPTIVE metadata catalog, NOT a schema code is generated
from: the hand-written readers are the source of truth for config structure and
validation.  This script only reads defaults out of the registry to emit the
DEF_ constants; it does not generate readers.

Registry v2 reshapes the flat `parameters:` list into a hierarchical `sections:`
tree (readability/de-dup + docs/example-config generation).  Both are read here;
a section key's default emits the same DEF_ constant it did as a flat parameter.

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


def _subst(value, face: str):
    """Substitute face placeholders in a per-face metadata string.

    {face}=west  {Face}=West  {F}=W  (lowercase / capitalized / upper initial).
    """
    if not isinstance(value, str):
        return value
    return value.replace("{face}", face).replace("{Face}", face.capitalize()).replace("{F}", face[0].upper())


def _entry(yaml_path: str, meta: dict) -> dict:
    meta = meta or {}
    return {"name": yaml_path.rsplit(".", 1)[-1], "yaml_path": yaml_path, "default": meta.get("default")}


def section_params(reg: dict):
    """Flatten the hierarchical `sections:` tree into (yaml_path, default) dicts.

    Handles flat `keys:` (a key may carry a dotted sub-path, e.g.
    `spectrum.freq.peak`, mirroring the deck) and `per_face:` (faces x keys,
    {face}/{Face}/{F} templated with per-face `overrides:`).  A key with no
    `default` is skipped by the defaults generator, exactly like a flat
    parameter without one.
    """
    for sec_name, sec in (reg.get("sections") or {}).items():
        for key, meta in (sec.get("keys") or {}).items():
            yield _entry(f"{sec_name}.{key}", meta)

        pf = sec.get("per_face")
        if pf:
            overrides = pf.get("overrides") or {}
            for face in pf["faces"]:
                for subkey, meta in (pf.get("keys") or {}).items():
                    resolved = {k: _subst(v, face) for k, v in (meta or {}).items()}
                    resolved.update(overrides.get(f"{face}.{subkey}") or {})
                    yield _entry(f"{sec_name}.{face}.{subkey}", resolved)


def all_params(reg: dict) -> list[dict]:
    return list(reg.get("parameters", [])) + list(section_params(reg))


def validate(reg: dict) -> list[str]:
    errors = []
    seen_paths: set[str] = set()
    for p in all_params(reg):
        path = p.get("yaml_path")
        if not path or "." not in path:
            errors.append(f"parameter {p.get('name')}: missing/invalid yaml_path")
            continue
        if path in seen_paths:
            errors.append(f"duplicate yaml_path: {path}")
        seen_paths.add(path)
    for p in reg.get("parameters", []):
        for field in ("name", "units", "default", "latex"):
            if field not in p:
                errors.append(f"{p.get('yaml_path')}: missing field {field!r}")
    for v in reg.get("variables", []):
        if not v.get("standard_name") and not v.get("funwave_name"):
            errors.append(f"variable {v.get('name')}: needs standard_name or funwave_name")
    return errors


def generate(reg: dict) -> str:
    lines = [HEADER]
    params = sorted(all_params(reg), key=lambda p: p["yaml_path"])
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
