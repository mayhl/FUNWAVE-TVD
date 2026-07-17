import os
import shutil


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
