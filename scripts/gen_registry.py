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
DOCS_OUTPUT = REPO / "docs" / "guide" / "config_reference.md"
META_OUTPUT = REPO / "src" / "model" / "field_metadata.f90"

# character component length of type_var_meta (core output_channel.f90)
META_LEN = 64

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
        for field in ("units", "long_name", "standard_name"):
            s = v.get(field)
            if s is None:
                errors.append(f"variable {v.get('name')}: missing field {field!r}")
            elif len(str(s)) > META_LEN:
                errors.append(f"variable {v.get('name')}: {field} exceeds META_LEN ({META_LEN})")
            elif '"' in str(s):
                errors.append(f"variable {v.get('name')}: {field} contains a double quote")
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


META_HEADER = """\
! allow(E001)
! =================================================================
!  GENERATED FILE — DO NOT EDIT.
!  Source:    src/model/registry.yaml
!  Generator: scripts/gen_registry.py   (rerun after registry edits)
!  Sync test: scripts/gen_registry.py --check
! =================================================================
!> @file field_metadata.f90
!> @brief Generated CF attribute catalog for output field variables.
module model_field_metadata_mod
   use core_output_channel_mod, only: type_var_meta
   implicit none
   public

contains

   !> To look up CF variable attributes by registry field name; an
   !> uncataloged name returns blank meta, which the writer renders
   !> as no attrs.
   pure function field_meta(name) result(m)
      character(*), intent(in) :: name
      type(type_var_meta) :: m

      select case (trim(name))
"""

META_FOOTER = """\
      end select
   end function field_meta

end module model_field_metadata_mod
"""


def generate_metadata(reg: dict) -> str:
    """Render the variables catalog as the field_meta lookup (registry order)."""
    lines = [META_HEADER]
    for v in reg.get("variables", []):
        lines.append(f'      case ("{v["name"]}")\n')
        lines.append(f'         m%units = "{v["units"]}"\n')
        lines.append(f'         m%long_name = "{v["long_name"]}"\n')
        if v.get("standard_name"):
            lines.append(f'         m%standard_name = "{v["standard_name"]}"\n')
    lines.append(META_FOOTER)
    return "".join(lines)


DOCS_HEADER = """\
<!-- =================================================================
  GENERATED FILE — DO NOT EDIT.
  Source:    src/model/registry.yaml
  Generator: scripts/gen_registry.py   (rerun after registry edits)
  Sync test: scripts/gen_registry.py --check
================================================================= -->

# Configuration Reference

To configure a run, we provide one YAML file whose top-level sections each
control one model component; a section marked **required** must appear in every
deck, while the remaining sections are presence-gated — omitting the section
disables the component, and within a section, keys with no default are likewise
presence-derived (setting them enables the associated behaviour).  Keys are
listed by their dotted sub-path, e.g. `spectrum.freq.peak` denotes

```yaml
wavemaker:
  spectrum:
    freq:
      peak: 0.1
```

The **Legacy** column gives the corresponding `input.txt` parameter name from
FUNWAVE-TVD, for migrating old decks; `—` marks keys with no legacy
counterpart.

"""


def _md_escape(text: str) -> str:
    return str(text).replace("|", "\\|").replace("<", "\\<").replace("\n", " ").strip()


def _md_default(value) -> str:
    if value is None:
        return "—"
    if isinstance(value, bool):
        return f"`{'true' if value else 'false'}`"
    return f"`{value}`"


def _md_desc(meta: dict) -> str:
    parts = []
    variant = meta.get("variant")
    if variant:
        parts.append(f"*({'/'.join(variant)})*")
    doc = meta.get("doc")
    if doc:
        parts.append(_md_escape(doc))
    values = meta.get("values")
    if values:
        parts.append("One of " + " \\| ".join(f"`{v}`" for v in values) + ".")
    return " ".join(parts) or "—"


def _md_row(key: str, meta: dict) -> str:
    meta = meta or {}
    legacy = meta.get("legacy")
    legacy_cell = f"`{legacy}`" if legacy else "—"
    units = meta.get("units")
    units_cell = _md_escape(units) if units else "—"
    return f"| `{key}` | {_md_default(meta.get('default'))} | {legacy_cell} | {units_cell} | {_md_desc(meta)} |\n"


TABLE_HEAD = "| Key | Default | Legacy | Units | Description |\n|---|---|---|---|---|\n"


def generate_docs(reg: dict) -> str:
    """Render the sections tree as the markdown configuration reference.

    One table per section (key, default, legacy name, units, description);
    per_face keys render once with <face> placeholders and override notes.
    """
    lines = [DOCS_HEADER]
    for sec_name, sec in (reg.get("sections") or {}).items():
        lines.append(f"## `{sec_name}:`\n\n")
        badge = "**Required.**  " if sec.get("required") else ""
        doc = sec.get("doc")
        if badge or doc:
            lines.append(badge + (_md_escape(doc) if doc else "") + "\n\n")

        keys = sec.get("keys") or {}
        if keys:
            lines.append(TABLE_HEAD)
            for key, meta in keys.items():
                lines.append(_md_row(key, meta))
            lines.append("\n")

        pf = sec.get("per_face")
        if pf:
            faces = " / ".join(f"`{f}:`" for f in pf["faces"])
            lines.append(f"### Per-face keys ({faces})\n\n")
            lines.append(TABLE_HEAD)
            overrides = pf.get("overrides") or {}
            for subkey, meta in (pf.get("keys") or {}).items():
                row_meta = dict(meta or {})
                notes = []
                for face in pf["faces"]:
                    ov = overrides.get(f"{face}.{subkey}")
                    if ov:
                        ov_bits = ", ".join(f"{k} `{v}`" for k, v in ov.items())
                        notes.append(f"{face}: {ov_bits}")
                if notes:
                    row_meta["doc"] = (row_meta.get("doc") or "") + "  (" + "; ".join(notes) + ")"
                lines.append(_md_row(f"<face>.{subkey}", _subst_meta(row_meta)))
            lines.append("\n")
    return "".join(lines)


def _subst_meta(meta: dict) -> dict:
    """Render {face}/{Face}/{F} placeholders generically for the docs table."""
    out = {}
    for k, v in meta.items():
        if isinstance(v, str):
            v = v.replace("{face}", "<face>").replace("{Face}", "<Face>").replace("{F}", "<F>")
        out[k] = v
    return out


def _check_one(path: Path, new: str) -> bool:
    old = path.read_text() if path.exists() else ""
    if old == new:
        print(f"OK: {path.relative_to(REPO)} is in sync with registry.yaml")
        return True
    print(
        f"STALE: {path.relative_to(REPO)} does not match registry.yaml — rerun: uv run scripts/gen_registry.py",
        file=sys.stderr,
    )
    sys.stderr.writelines(
        difflib.unified_diff(old.splitlines(keepends=True), new.splitlines(keepends=True), fromfile="committed", tofile="generated")
    )
    return False


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
    new_docs = generate_docs(reg)
    new_meta = generate_metadata(reg)
    if args.check:
        ok = _check_one(OUTPUT, new)
        ok = _check_one(DOCS_OUTPUT, new_docs) and ok
        ok = _check_one(META_OUTPUT, new_meta) and ok
        return 0 if ok else 1

    OUTPUT.write_text(new)
    print(f"wrote {OUTPUT.relative_to(REPO)}")
    DOCS_OUTPUT.write_text(new_docs)
    print(f"wrote {DOCS_OUTPUT.relative_to(REPO)}")
    META_OUTPUT.write_text(new_meta)
    print(f"wrote {META_OUTPUT.relative_to(REPO)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
