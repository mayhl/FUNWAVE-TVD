# Diagnostic PNGs are best-effort — they must never fail a regression comparison.
# kaleido (plotly's static-image backend) now requires a system Chrome, which
# headless HPC compute nodes lack; make write_image non-fatal package-wide so a
# missing Chrome degrades to "no PNG" instead of erroring every test. Interactive
# HTML figures (write_html) need no browser and are unaffected.
import plotly.graph_objects as _go

_orig_write_image = _go.Figure.write_image


def _best_effort_write_image(self, *args, **kwargs):
    try:
        return _orig_write_image(self, *args, **kwargs)
    except Exception:
        return None


_go.Figure.write_image = _best_effort_write_image
