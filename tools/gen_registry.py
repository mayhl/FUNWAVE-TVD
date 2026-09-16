#!/usr/bin/env python3
"""Check src/model/registry.yaml against the code it describes, and render its docs table.

The code is the source of truth: the hand-written readers carry every default
as the string literal at its read site, and field_metadata.f90 carries the CF
attributes the NetCDF writer stamps.  The registry is a DESCRIPTIVE catalog of
the same facts (plus legacy names, units and doc strings the code has no place
for), so nothing here emits Fortran; `--check` fails when the two disagree,
naming the key, and the only file written is docs/guide/config_reference.md.

Registry v2 is a hierarchical `sections:` tree (flat `keys:` maps with dotted
sub-paths, `per_face:` templating for the four boundaries); the flat
`parameters:` list is still read for completeness.

Usage:
    uv run tools/gen_registry.py            # regenerate config_reference.md
    uv run tools/gen_registry.py --check    # verify the readers, field_metadata.f90
                                              # and the committed docs table agree
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
READERS = REPO / "src" / "model" / "2d"
DOCS_OUTPUT = REPO / "docs" / "guide" / "config_reference.md"
META_OUTPUT = REPO / "src" / "model" / "field_metadata.f90"

# character component length and flag capacity of type_var_meta (core output_channel.f90)
META_LEN = 64
FLAGS_MAX = 4
COMMENT_LEN = 160


def fortran_default(value) -> str | None:
    """Render a registry default as the string a read site carries."""
    if value is None:
        return None
    if isinstance(value, bool):
        return "YES" if value else "NO"
    return str(value)


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
    """Every parameter entry, top-level and per-section."""
    return list(reg.get("parameters", [])) + list(section_params(reg))


def validate(reg: dict) -> list[str]:
    """Consistency errors in the registry, empty when clean."""
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
    for sec_name, sec in (reg.get("sections") or {}).items():
        metas = list((sec.get("keys") or {}).items())
        if sec.get("per_face"):
            metas += list((sec["per_face"].get("keys") or {}).items())
        for key, meta in metas:
            adv = (meta or {}).get("advanced")
            if adv is not None and adv is not True:
                errors.append(f"{sec_name}.{key}: advanced must be true or absent (binary tier)")
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
        for field in ("funwave_name", "flag_meanings"):
            s = v.get(field)
            if s is not None and len(str(s)) > META_LEN:
                errors.append(f"variable {v.get('name')}: {field} exceeds META_LEN ({META_LEN})")
        s = v.get("comment")
        if s is not None and len(str(s)) > COMMENT_LEN:
            errors.append(f"variable {v.get('name')}: comment exceeds COMMENT_LEN ({COMMENT_LEN})")
        if s is not None and '"' in str(s):
            errors.append(f"variable {v.get('name')}: comment contains a double quote")
        vals, meanings = v.get("flag_values"), v.get("flag_meanings")
        if (vals is None) != (meanings is None):
            errors.append(f"variable {v.get('name')}: flag_values and flag_meanings go together")
        elif vals is not None:
            if not isinstance(vals, list) or not all(isinstance(x, (int, float)) for x in vals):
                errors.append(f"variable {v.get('name')}: flag_values must be a list of numbers")
            elif len(vals) > FLAGS_MAX:
                errors.append(f"variable {v.get('name')}: flag_values exceeds FLAGS_MAX ({FLAGS_MAX})")
            elif len(vals) != len(str(meanings).split()):
                errors.append(f"variable {v.get('name')}: flag_meanings count differs from flag_values")
    return errors


DOCS_HEADER = """\
<!-- =================================================================
  GENERATED FILE — DO NOT EDIT.
  Source:    src/model/registry.yaml
  Generator: tools/gen_registry.py   (rerun after registry edits)
  Sync test: tools/gen_registry.py --check
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

Each section lists its common keys first; an **Advanced** table beneath holds
the keys meant for research on the model rather than for production runs
(closure coefficients, scheme choices, reproducibility and tuning controls).
Every advanced key has a default, so a production deck never needs to set one.

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
        common = {k: m for k, m in keys.items() if not (m or {}).get("advanced")}
        advanced = {k: m for k, m in keys.items() if (m or {}).get("advanced")}
        if common:
            lines.append(TABLE_HEAD)
            for key, meta in common.items():
                lines.append(_md_row(key, meta))
            lines.append("\n")
        if advanced:
            lines.append("**Advanced**\n\n" + TABLE_HEAD)
            for key, meta in advanced.items():
                lines.append(_md_row(key, meta))
            lines.append("\n")

        pf = sec.get("per_face")
        if pf:
            faces = " / ".join(f"`{f}:`" for f in pf["faces"])
            lines.append(f"### Per-face keys ({faces})\n\n")
            overrides = pf.get("overrides") or {}
            rows = {False: [], True: []}
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
                rows[bool(row_meta.get("advanced"))].append(_md_row(f"<face>.{subkey}", _subst_meta(row_meta)))
            for adv in (False, True):
                if rows[adv]:
                    lines.append(("**Advanced**\n\n" if adv else "") + TABLE_HEAD)
                    lines.extend(rows[adv])
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


# reader module per registry section; the stem is the section name except
# where the module is named for what it holds rather than its deck block
READER_OF = {"grid": "geometry", "dispersion": "physics", "hot_start": "hotstart"}
# a presence-derived key (registry default ~) is read against a sentinel the
# code then tests for absence; these are the sentinels in use
SENTINELS = {"", "-999999.0"}


def _statements(path: Path) -> list[str]:
    """Source lines with Fortran continuations joined, so a read call is one string."""
    out, buf = [], ""
    for line in path.read_text().splitlines():
        code = line.split("!", 1)[0] if '"' not in line else line
        buf += code.rstrip()
        if buf.endswith("&"):
            buf = buf[:-1]
            continue
        out.append(buf)
        buf = ""
    return out


def check_reader_defaults(reg: dict) -> list[str]:
    """Every `default="..."` at a read site must equal the registry default of that key.

    The key at a read site is a leaf (the sub-block objects carry no path), so
    it is matched against every registry path in the module's section ending in
    that leaf -- per-face keys share one default by construction.
    """
    by_section: dict[str, dict[str, set[str | None]]] = {}
    for p in section_params(reg):
        section, leaf = p["yaml_path"].split(".", 1)[0], p["yaml_path"].rsplit(".", 1)[-1]
        by_section.setdefault(section, {}).setdefault(leaf, set()).add(fortran_default(p["default"]))

    errors = []
    for section, keys in by_section.items():
        path = READERS / f"{READER_OF.get(section, section)}.f90"
        if not path.exists():
            continue
        for stmt in _statements(path):
            for m in re.finditer(r'%read[a-z_]*\(\s*"([^"]+)"(.*?)$', stmt):
                leaf = m.group(1).rsplit(".", 1)[-1]
                d = re.search(r'default\s*=\s*"([^"]*)"', m.group(2))
                if d is None or leaf not in keys:
                    continue
                if d.group(1) not in keys[leaf] and not (None in keys[leaf] and d.group(1) in SENTINELS):
                    want = ", ".join(sorted(str(v) for v in keys[leaf]))
                    errors.append(f'{path.relative_to(REPO)}: {section}.{leaf} reads default "{d.group(1)}", registry says {want}')
    return errors


def check_field_meta(reg: dict) -> list[str]:
    """field_meta() and the registry `variables:` block must carry the same CF attributes."""
    code: dict[str, dict[str, str]] = {}
    name = None
    for line in META_OUTPUT.read_text().splitlines():
        m = re.match(r'\s*case \("([^"]+)"\)', line)
        if m:
            name = m.group(1)
            code[name] = {}
            continue
        m = re.match(r'\s*m%(units|long_name|standard_name|funwave_name|flag_meanings|comment) = "([^"]*)"', line)
        if m and name:
            code[name][m.group(1)] = m.group(2)
        m = re.match(r"\s*m%flag_values\(1:\d+\) = \[([^\]]*)\]", line)
        if m and name:
            code[name]["flag_values"] = " ".join(str(float(x.replace("_SP", ""))) for x in m.group(1).split(","))
        m = re.match(r"\s*m%fill_value = ([-0-9.eE+]+)_SP", line)
        if m and name:
            code[name]["fill_value"] = str(float(m.group(1)))

    errors = []
    for v in reg.get("variables", []):
        want = {k: str(v.get(k) or "") for k in ("units", "long_name", "standard_name", "funwave_name", "flag_meanings", "comment")}
        if v.get("standard_name"):
            want["funwave_name"] = ""
        want["flag_values"] = " ".join(str(float(x)) for x in v.get("flag_values") or [])
        want["fill_value"] = str(float(v["fill_value"])) if v.get("fill_value") is not None else ""
        got = code.pop(v["name"], None)
        if got is None:
            errors.append(f"variable {v['name']}: in registry.yaml, not in field_metadata.f90")
            continue
        got = {k: got.get(k, "") for k in want}
        if got != want:
            errors.append(f"variable {v['name']}: field_metadata.f90 {got} vs registry {want}")
    for name in code:
        errors.append(f"variable {name}: in field_metadata.f90, not in registry.yaml")
    return errors


def _check_one(path: Path, new: str) -> bool:
    old = path.read_text() if path.exists() else ""
    if old == new:
        print(f"OK: {path.relative_to(REPO)} is in sync with registry.yaml")
        return True
    print(
        f"STALE: {path.relative_to(REPO)} does not match registry.yaml — rerun: uv run tools/gen_registry.py",
        file=sys.stderr,
    )
    sys.stderr.writelines(
        difflib.unified_diff(old.splitlines(keepends=True), new.splitlines(keepends=True), fromfile="committed", tofile="generated")
    )
    return False


def main() -> int:
    """Regenerate the outputs, or --check the committed ones."""
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

    new_docs = generate_docs(reg)
    if args.check:
        ok = _check_one(DOCS_OUTPUT, new_docs)
        for problem in check_reader_defaults(reg) + check_field_meta(reg):
            print(f"MISMATCH: {problem}", file=sys.stderr)
            ok = False
        if ok:
            print("OK: readers and field_metadata.f90 agree with registry.yaml")
        return 0 if ok else 1

    DOCS_OUTPUT.write_text(new_docs)
    print(f"wrote {DOCS_OUTPUT.relative_to(REPO)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
