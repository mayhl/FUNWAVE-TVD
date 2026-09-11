import os
import shutil
import subprocess
import sys
from pathlib import Path

from mkdocs.structure.files import File

# the validation page generator lives with the other repo tools, not under docs/
# (every *.py under docs/ is a diagram script the pre-build hook would run)
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from tools import gen_validation_docs as validation  # noqa: E402

_generated_pages: dict[str, str] = {}


def on_config(config, **kwargs):
    """Grow the Validation nav from the suite config: one row per case.

    Derived pages are built, never committed, so the nav cannot list them by
    hand; the generator returns the subtree and this replaces the placeholder
    entry in mkdocs.yml.
    """
    pages, section_nav, figures = validation.build(docs_dir=Path(config["docs_dir"]))
    _generated_pages.clear()
    _generated_pages.update(pages)
    config["nav"] = [section_nav if isinstance(item, dict) and "Validation" in item else item for item in config["nav"]]

    # board figures the record names, copied under docs/_generated so the
    # pages' links resolve; binaries never enter the source tree
    docs = Path(config["docs_dir"])
    for case, variant, fig in figures:
        src = validation.REPORT.parent / fig["path"]
        dst = docs / validation.figure_target(case, variant, fig)
        if src.exists():
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src, dst)
    return config


def on_files(files, config, **kwargs):
    """Add the generated validation pages to the build."""
    for src_uri, content in _generated_pages.items():
        files.append(File.generated(config, src_uri, content=content))
    return files


def on_pre_build(config, **kwargs):
    """Render diagram scripts that sit beside the pages they illustrate.

    A diagram's source is a Python script living with its page; it takes the
    output path as its one argument and writes an SVG.  Output lands under
    docs/_generated (untracked) mirroring the source's own path, and is rebuilt
    only when missing or older than the script -- so a fresh clone renders every
    diagram with no artifact bundle, unlike the board figures.
    """
    docs = Path(config["docs_dir"])
    for script in sorted(docs.rglob("*.py")):
        if script.name == "hooks.py" or "__pycache__" in script.parts:
            continue
        rel = script.relative_to(docs)
        out = docs / "_generated" / rel.with_suffix(".svg")
        if out.exists() and out.stat().st_mtime >= script.stat().st_mtime:
            continue
        out.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run([sys.executable, str(script), str(out)], check=True)


def on_post_build(config, **kwargs):
    """Copy Doxygen HTML output into the MkDocs site directory."""
    src = os.path.join(os.path.dirname(config["docs_dir"]), "build", "doc", "html")
    dst = os.path.join(config["site_dir"], "doxygen")
    if os.path.isdir(src):
        shutil.copytree(src, dst, dirs_exist_ok=True)
    else:
        os.makedirs(dst, exist_ok=True)
        with open(os.path.join(dst, "index.html"), "w") as f:
            f.write(
                "<html><body><p>Doxygen docs not built yet. "
                "Run <code>cmake --build build --target doc</code> first.</p></body></html>"
            )
