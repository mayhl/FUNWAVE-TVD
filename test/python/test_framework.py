"""Table tests of the framework's pure functions: case expansion, deck overrides,
rank sizing, the legacy parsers, output discovery and the oracle helpers.

Nothing here runs the model; `uv run pytest -q` runs in seconds.
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import pytest
import yaml

from test.framework.regression_runner import RegressionRunner, _merge_tolerances, _run_steps, _value_tag
from test.framework.run_output import _parse_txt, get_output_variables
from test.framework.tolerances import check_keys
from test.regression.postproc.standing_wave import G, _wave_number
from test.validation.oracles._lab import nrmse_pct


def _expand(sims):
    return RegressionRunner._expand_simulations(None, sims)


def test_value_tag_renders_names():
    assert _value_tag(0.45) == "0p45"
    assert _value_tag(-1) == "m1"
    assert _value_tag(2.0) == "2"
    assert _value_tag(True) == "on"
    assert _value_tag("eddy_viscosity") == "eddy_viscosity"


def test_input_files_fan_out_keeps_the_case_name():
    out = _expand([{"name": "flume", "input_files": ["a.yaml", "b.yaml"]}])
    assert [s["name"] for s in out] == ["flume_a", "flume_b"]
    assert {s["case"] for s in out} == {"flume"}
    assert [s["deck"] for s in out] == ["a", "b"]
    single = _expand([{"name": "one", "input_files": ["a.yaml"]}])
    assert single[0]["deck"] is None


def test_variants_merge_overrides_and_tolerances():
    sim = {
        "name": "c",
        "input_file": "d.yaml",
        "overrides": {"breaking.model": "eddy_viscosity"},
        "tolerances": {"surf": {"height_nrmse_pct": 15.0, "setup_rms_pct": 2.0}},
        "variants": {
            "shock": {"overrides": {"breaking.model": "shock_capturing"}, "tolerances": {"surf": {"height_nrmse_pct": 25.0}}}
        },
    }
    (s,) = _expand([sim])
    assert s["name"] == "c_shock" and s["variant"] == "shock"
    assert s["overrides"] == {"breaking.model": "shock_capturing"}
    assert s["tolerances"]["surf"] == {"height_nrmse_pct": 25.0, "setup_rms_pct": 2.0}


def test_sweep_expands_the_cartesian_product_into_variants():
    out = _expand([{"name": "c", "input_file": "d.yaml", "sweep": {"breaking.cbrk1": [0.4, 0.45], "breaking.nu_scale": [1, 2]}}])
    assert [s["name"] for s in out] == [
        "c_cbrk1_0p4_nu_scale_1",
        "c_cbrk1_0p4_nu_scale_2",
        "c_cbrk1_0p45_nu_scale_1",
        "c_cbrk1_0p45_nu_scale_2",
    ]
    assert out[-1]["overrides"] == {"breaking.cbrk1": 0.45, "breaking.nu_scale": 2}


def test_sweep_multiplies_into_explicit_variants_and_np_sweep():
    sim = {
        "name": "v",
        "input_file": "d.yaml",
        "variants": {"shock": {"overrides": {"breaking.model": "shock_capturing"}}},
        "sweep": {"numerics.cfl": [0.4, 0.5]},
        "np_sweep": [1, 2],
    }
    out = _expand([sim])
    assert [s["name"] for s in out] == ["v_shock_cfl_0p4_np1", "v_shock_cfl_0p4_np2", "v_shock_cfl_0p5_np1", "v_shock_cfl_0p5_np2"]
    assert out[0]["overrides"] == {"breaking.model": "shock_capturing", "numerics.cfl": 0.4}
    assert out[1]["np_pin"] == 2 and out[1]["sweep_group"] == "v_shock_cfl_0p4"


def test_merge_tolerances_updates_per_kind_blocks():
    merged = _merge_tolerances({"surf": {"a": 1, "b": 2}, "other": 5}, {"surf": {"b": 3}, "other": 6})
    assert merged == {"surf": {"a": 1, "b": 3}, "other": 6}


def test_override_deck_sets_dotted_keys_on_the_copy(tmp_path: Path):
    deck = tmp_path / "d.yaml"
    deck.write_text("breaking: {model: eddy_viscosity, cbrk1: 0.45}\noutput: {channels: [{name: a}]}\n")
    RegressionRunner._override_deck(deck, {"breaking.cbrk1": 0.4, "numerics.cfl": 0.5, "output.channels": [{"name": "b"}]})
    cfg = yaml.safe_load(deck.read_text())
    assert cfg["breaking"] == {"model": "eddy_viscosity", "cbrk1": 0.4}
    assert cfg["numerics"] == {"cfl": 0.5}
    assert cfg["output"]["channels"] == [{"name": "b"}]  # a list is replaced wholesale, never appended


@pytest.mark.parametrize("np_want,m,n", [(8, 250, 10), (14, 400, 200), (92, 1600, 3), (4, 6, 6)])
def test_factor_decomp_fits_the_grid_and_budget(np_want, m, n):
    px, py = RegressionRunner._factor_decomp(np_want, m, n)
    assert px * py <= np_want
    # an axis only splits into subdomains of at least 4 cells; unsplit axes may be thinner
    assert px == 1 or m // px >= 4
    assert py == 1 or n // py >= 4
    assert (px >= py) == (m >= n) or px == py


def test_factor_decomp_small_grid_is_serial():
    assert RegressionRunner._factor_decomp(8, 5, 3) == (1, 1)


def test_grid_cells_reads_yaml_and_legacy(tmp_path: Path):
    y = tmp_path / "d.yaml"
    y.write_text("grid: {n_cells: [250, 10]}\n")
    assert RegressionRunner._grid_cells(y) == (250, 10, 1)
    t = tmp_path / "input.txt"
    t.write_text("Mglob = 100\nNglob = 20 ! comment\nKglob = 5\n")
    assert RegressionRunner._grid_cells(t) == (100, 20, 5)
    assert RegressionRunner._grid_cells(tmp_path / "missing.yaml") is None


def test_run_steps_reads_the_last_step_line(tmp_path: Path):
    (tmp_path / "funwave.log").write_text("INFO: step 1  t = 0.01\nINFO: step 587  t = 6.0\n")
    assert _run_steps(tmp_path) == 587
    assert _run_steps(tmp_path / "nowhere") is None


def test_parse_txt_takes_pairs_and_strips_comments(tmp_path: Path):
    t = tmp_path / "input.txt"
    t.write_text("Mglob = 100  Nglob = 20 ! trailing\n! DX = 9\nDX = 0.5\n")
    assert _parse_txt(t) == {"Mglob": "100", "Nglob": "20", "DX": "0.5"}


def test_output_discovery_by_kind_and_blow_up_sentinel(tmp_path: Path):
    for name in (
        "fields/eta_00000",
        "fields/eta_00001",
        "fields/eta_99999",
        "envelope/eta_max_00001",
        "stats/eta_mean_00001",
        "stats/other.txt",
    ):
        p = tmp_path / name
        p.parent.mkdir(exist_ok=True)
        p.write_bytes(b"")
    field = {v.prefix: v for v in get_output_variables(tmp_path, kind="field")}
    stats = {v.prefix: v for v in get_output_variables(tmp_path, kind="statistics")}
    assert set(field) == {"eta", "eta_max"}
    assert field["eta"].unstable and (field["eta"].first, field["eta"].last) == (0, 1)
    assert not field["eta_max"].unstable
    assert set(stats) == {"eta_mean"}


def test_nrmse_norms_and_check_keys():
    m = np.array([1.0, 2.0, -4.0])
    assert nrmse_pct(m, m) == 0.0
    assert math.isclose(nrmse_pct(m, m + 0.4, norm="max"), 10.0)
    assert math.isclose(nrmse_pct(m, m + 0.6, norm="range"), 10.0)
    assert nrmse_pct(np.zeros(3), np.ones(3)) == math.inf
    check_keys({"a": 1}, ("a", "b"), "x")
    with pytest.raises(ValueError):
        check_keys({"c": 1}, ("a",), "x")


def test_wave_number_limits():
    h = 10.0
    omega = 2.0 * math.pi / 2.0  # deep water at h = 10 m
    assert math.isclose(_wave_number(omega, h), omega**2 / G, rel_tol=1e-3)
    omega = 2.0 * math.pi / 200.0  # shallow water
    assert math.isclose(_wave_number(omega, h), omega / math.sqrt(G * h), rel_tol=1e-3)
