import os
import shutil
import subprocess
import sys
from pathlib import Path


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
