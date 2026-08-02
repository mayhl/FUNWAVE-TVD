#!/usr/bin/env python3
"""
Convert a legacy FUNWAVE-TVD flat input.txt to the new nested YAML format.

For 3-D inputs (Kglob present) a minimal geometry stub is written instead and
the source file is copied to input.txt so the legacy READ_INPUT can find it.

Usage:
    python scripts/convert_input.py input.txt [output.yaml]

If output.yaml is omitted the result is written to stdout.
Unknown keys are collected and emitted as a comment block at the top of the
output file so nothing is silently dropped.
"""

import argparse
import re
import shutil
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------


def parse_input_txt(path: Path) -> dict[str, str]:
    """Return {KEY: raw_value_string} from a legacy input.txt file."""
    params: dict[str, str] = {}
    with open(path) as f:
        for line in f:
            line = re.sub(r"[!#].*", "", line).strip()
            if not line:
                continue
            m = re.match(r"(\w+)\s*[=:]\s*(.*)", line)
            if m:
                key = m.group(1).strip()
                val = m.group(2).strip()
                params[key] = val
    return params


# ---------------------------------------------------------------------------
# Value coercion
# ---------------------------------------------------------------------------

_BOOL_TRUE = {"T", "TRUE", "YES"}
_BOOL_FALSE = {"F", "FALSE", "NO"}


def _bool(s: str) -> bool:
    return s.upper() in _BOOL_TRUE


def _auto(s: str):
    """Coerce to bool > int > float > str."""
    if s.upper() in _BOOL_TRUE:
        return True
    if s.upper() in _BOOL_FALSE:
        return False
    try:
        return int(s)
    except ValueError:
        pass
    try:
        return float(s)
    except ValueError:
        pass
    return s


# ---------------------------------------------------------------------------
# Variable → OUT_* map  (old key → YAML variable name)
# DEPTH_OUT is handled separately (no time component / static field).
# ROLLER appears in both breaking physics and output — handled explicitly.
# OUT_NU was keyed as 'OUT_NU' in old format; renamed to 'NU' in new YAML.
# ---------------------------------------------------------------------------

_VAR_FLAGS: dict[str, str] = {
    "U": "U",
    "V": "V",
    "ETA": "ETA",
    "ETAscreen": "ETAscreen",
    "Hmax": "Hmax",
    "Hmin": "Hmin",
    "Umax": "Umax",
    "MFmax": "MFmax",
    "VORmax": "VORmax",
    "MASK": "MASK",
    "MASK9": "MASK9",
    "Umean": "Umean",
    "Vmean": "Vmean",
    "ETAmean": "ETAmean",
    "WaveHeight": "WaveHeight",
    "SXL": "SXL",
    "SXR": "SXR",
    "SYL": "SYL",
    "SYR": "SYR",
    "SourceX": "SourceX",
    "SourceY": "SourceY",
    "FrcX": "FrcX",
    "FrcY": "FrcY",
    "BrkdisX": "BrkdisX",
    "BrkdisY": "BrkdisY",
    "P": "P",
    "Q": "Q",
    "Fx": "Fx",
    "Fy": "Fy",
    "Gx": "Gx",
    "Gy": "Gy",
    "AGE": "AGE",
    "ROLLER": "ROLLER",  # also a breaking physics flag — see convert()
    "UNDERTOW": "UNDERTOW",
    "OUT_NU": "NU",  # renamed: OUT_NU → NU
    "TMP": "TMP",
    "Radiation": "Radiation",
}

# ---------------------------------------------------------------------------
# Wavemaker parameter sets per type
# ---------------------------------------------------------------------------

_WK_PARAMS: dict[str, list[str]] = {
    "LEF_SOL": ["AMP", "DEP", "LAGTIME"],
    "WK_REG": [
        "Xc_WK",
        "Yc_WK",
        "DEP_WK",
        "Tperiod",
        "AMP_WK",
        "Theta_WK",
        "Time_ramp",
        "Delta_WK",
        "Ywidth_WK",
    ],
    "WK_IRR": [
        "Xc_WK",
        "Yc_WK",
        "DEP_WK",
        "FreqPeak",
        "FreqMin",
        "FreqMax",
        "Hmo",
        "GammaTMA",
        "ThetaPeak",
        "Sigma_Theta",
        "Nfreq",
        "Ntheta",
        "Time_ramp",
        "Delta_WK",
        "Ywidth_WK",
    ],
    "WK_TIME_SERIES": [
        "NumWaveComp",
        "PeakPeriod",
        "WaveCompFile",
        "Xc_WK",
        "Yc_WK",
        "DEP_WK",
        "Time_ramp",
        "Delta_WK",
        "Ywidth_WK",
    ],
    "WK_NEW_IRR": [
        "Xc_WK",
        "Yc_WK",
        "DEP_WK",
        "FreqPeak",
        "FreqMin",
        "FreqMax",
        "Hmo",
        "GammaTMA",
        "ThetaPeak",
        "Sigma_Theta",
        "Time_ramp",
        "Delta_WK",
        "Ywidth_WK",
        "WaveCompFile",
    ],
    "LEFT_BC_IRR": [
        "NumWaveComp",
        "PeakPeriod",
        "WaveCompFile",
        "DEP_WK",
        "Time_ramp",
        "Delta_WK",
    ],
    "ABS_1D": [
        "NumWaveComp",
        "PeakPeriod",
        "WaveCompFile",
        "DEP_WK",
        "Time_ramp",
        "Delta_WK",
    ],
    "JON_2D": [
        "Xc_WK",
        "Yc_WK",
        "DEP_WK",
        "FreqPeak",
        "FreqMin",
        "FreqMax",
        "Hmo",
        "GammaTMA",
        "ThetaPeak",
        "Sigma_Theta",
        "Nfreq",
        "Ntheta",
        "Time_ramp",
        "Delta_WK",
        "Ywidth_WK",
    ],
    "JON_1D": [
        "Xc_WK",
        "Yc_WK",
        "DEP_WK",
        "FreqPeak",
        "FreqMin",
        "FreqMax",
        "Hmo",
        "GammaTMA",
        "Nfreq",
        "Time_ramp",
        "Delta_WK",
    ],
    "TMA_1D": [
        "Xc_WK",
        "Yc_WK",
        "DEP_WK",
        "FreqPeak",
        "FreqMin",
        "FreqMax",
        "Hmo",
        "GammaTMA",
        "Nfreq",
        "Time_ramp",
        "Delta_WK",
    ],
}

# Common optional wavemaker keys not in any type-specific list
_WK_COMMON = [
    "WaveMakerCurrentBalance",
    "WaveMakerCd",
    "WAVEMAKER_Cbrk",
]


def _put(dst: dict, block: str, key: str, val):
    if val is not None:
        dst.setdefault(block, {})[key] = val


def _convert_abs(pop_val):
    """Legacy ABS -> spectrum-only wavemaker entry + west face block (config
    reorg rung 3b): the face owns the relaxation strip (nee WidthWaveMaker/
    R_,A_sponge_wavemaker; required keys, no legacy defaults exist) and the
    series depth (nee DepthWaveMaker, DEP_WK fallback); WAVE_DATA_TYPE keys
    the spectrum model + directionality like legacy io.F."""
    wdt = str(pop_val("WAVE_DATA_TYPE") or "").upper()
    if wdt.startswith("DATA"):
        spec: dict = {"type": "spectrum_2d"}
        v = pop_val("WaveCompFile")
        if v is not None:
            spec["file"] = v
        spec["format"] = wdt
    else:
        spec = {"type": "jonswap" if wdt.startswith("JON") else "tma"}
        for k, yk in (("Hmo", "hm0"), ("GammaTMA", "gamma")):
            v = pop_val(k)
            if v is not None:
                spec[yk] = v
        for k, yk in (("FreqPeak", "peak"), ("FreqMin", "min"), ("FreqMax", "max")):
            _put(spec, "freq", yk, pop_val(k))
        if "1D" in wdt:
            for k in ("ThetaPeak", "Sigma_Theta", "Ntheta"):
                pop_val(k)  # legacy forces 1D (Ntheta = 1); consume silently
        else:
            for k, yk in (("ThetaPeak", "peak"), ("Sigma_Theta", "spread")):
                _put(spec, "directional", yk, pop_val(k))
            # legacy 2D defaults (io.F ABS block) differ from the reader's;
            # spread is a required key now, so always emit it
            d = spec.setdefault("directional", {})
            d.setdefault("peak", 0.0)
            d.setdefault("spread", 10.0)
            _put(spec, "discretization", "theta_bins", pop_val("Ntheta"))
            spec.setdefault("discretization", {}).setdefault("theta_bins", 24)
        _put(spec, "discretization", "freq_bins", pop_val("Nfreq"))
        spec.setdefault("discretization", {}).setdefault("freq_bins", 45)
        eq = pop_val("EqualEnergy")
        if eq is not None:
            _put(spec, "discretization", "equal_energy", eq)
    wm = {"name": "absorbing", "spectrum": spec}

    depth = pop_val("DepthWaveMaker")
    if depth is None:
        depth = pop_val("DEP_WK")
    forcing: dict = {"wavemaker": "absorbing"}
    if depth is not None:
        forcing["depth"] = depth
    west: dict = {"forcing": forcing}

    # generating-absorbing (nee TIDAL_BC_GEN_ABS): west tide target rides the
    # forcing block and the relaxation_cells tide profile absorbs — the strip
    # keys stay unused (legacy reads them into an ignored sponge_maker)
    gen_abs = pop_val("TIDAL_BC_GEN_ABS")
    if gen_abs:
        pop_val("TideBcType")  # file presence selects DATA like rung 2
        tf = pop_val("TideWestFileName")
        if tf is not None:
            forcing["file"] = tf
        te = pop_val("TideWest_ETA")
        if te is not None:
            forcing["eta"] = te
        for k in ("WidthWaveMaker", "R_sponge_wavemaker", "A_sponge_wavemaker"):
            pop_val(k)
        return wm, west

    sponge: dict = {}
    v = pop_val("WidthWaveMaker")
    if v is not None:
        sponge["width"] = v
    direct = {}
    for k, yk in (("R_sponge_wavemaker", "r"), ("A_sponge_wavemaker", "a")):
        v = pop_val(k)
        if v is not None:
            direct[yk] = v
    if direct:
        sponge["direct"] = direct
    if sponge:
        west["sponge"] = sponge
    return wm, west


def _convert_wavemaker(wm_type: str, pop_val):
    """Legacy WAVEMAKER type + flat keys -> spectrum/source/limiter entry
    (config reorg rung 3a), plus a boundaries.west block for ABS (rung 3b).
    Returns (wavemaker_entry, west_face_or_None).  ABS_1D/LEFT_BC_IRR keep
    their legacy shape until the char-BC track (the reader rejects them).
    freq stays frequency (exact); period {...} is hand-authoring only."""
    if wm_type.startswith("ABS") and wm_type != "ABS_1D":
        # legacy dispatch is the PREFIX WaveMaker(1:3)=='ABS' — real decks
        # spell it ABSORBING_GENERATING
        return _convert_abs(pop_val)
    if wm_type in ("LEF_SOL", "ABS_1D", "LEFT_BC_IRR"):
        wm = {"type": wm_type}
        for k in _WK_PARAMS.get(wm_type, []):
            v = pop_val(k)
            if v is not None:
                wm[k] = v
        return wm, None

    spec_map = {
        "WK_REG": ("regular", False, False),
        "WK_IRR": ("tma", True, False),
        "TMA_1D": ("tma", False, False),
        "JON_1D": ("jonswap", False, False),
        "JON_2D": ("jonswap", True, False),
        "WK_NEW_IRR": ("tma", True, True),
        "WK_TIME_SERIES": ("components", False, False),
    }
    if wm_type not in spec_map:
        raise SystemExit(f"convert_input: unsupported WAVEMAKER type {wm_type}")
    stype, directional, single_dir = spec_map[wm_type]

    wm: dict = {}
    spec: dict = {"type": stype}
    if stype == "regular":
        for k, yk in (("AMP_WK", "amplitude"), ("Tperiod", "period"), ("Theta_WK", "direction")):
            v = pop_val(k)
            if v is not None:
                spec[yk] = v
    elif stype == "components":
        for k, yk in (("NumWaveComp", "n"), ("PeakPeriod", "period_peak"), ("WaveCompFile", "file")):
            v = pop_val(k)
            if v is not None:
                spec[yk] = v
    else:
        for k, yk in (("Hmo", "hm0"), ("GammaTMA", "gamma")):
            v = pop_val(k)
            if v is not None:
                spec[yk] = v
        for k, yk in (("FreqPeak", "peak"), ("FreqMin", "min"), ("FreqMax", "max")):
            _put(spec, "freq", yk, pop_val(k))
        if directional:
            for k, yk in (("ThetaPeak", "peak"), ("Sigma_Theta", "spread")):
                _put(spec, "directional", yk, pop_val(k))
            # legacy directional defaults (io.F) differ from the reader;
            # spread is a required key now, so always emit it
            spec.setdefault("directional", {}).setdefault("spread", 10.0)
            _put(spec, "discretization", "theta_bins", pop_val("Ntheta"))
            spec.setdefault("discretization", {}).setdefault("theta_bins", 24)
        else:
            pop_val("Ntheta")  # consume a stray 1D Ntheta silently, like before
        _put(spec, "discretization", "freq_bins", pop_val("Nfreq"))
        spec.setdefault("discretization", {}).setdefault("freq_bins", 45)
        if single_dir:
            spec["discretization"]["method"] = "single_dir_per_freq"
        v = pop_val("alpha_c")
        if v is not None:
            _put(spec, "discretization", "coherence_percent", v)
        eq = pop_val("EqualEnergy")
        if eq is not None:
            _put(spec, "discretization", "equal_energy", eq)
        if wm_type == "WK_NEW_IRR":
            _put(spec, "discretization", "file", pop_val("WaveCompFile"))
    wm["spectrum"] = spec

    for k, yk in (
        ("Xc_WK", "x_center"),
        ("Yc_WK", "y_center"),
        ("DEP_WK", "depth"),
        ("Delta_WK", "delta"),
        ("Ywidth_WK", "y_width"),
        ("Time_ramp", "time_ramp"),
        ("WaveMakerCd", "current_cd"),
    ):
        _put(wm, "source", yk, pop_val(k))
    pop_val("WaveMakerCurrentBalance")  # presence of current_cd carries it

    if pop_val("ETA_LIMITER"):
        for k, yk in (("CrestLimit", "crest"), ("TroughLimit", "trough")):
            _put(wm, "limiter", yk, pop_val(k))

    return wm, None


# ---------------------------------------------------------------------------
# Main converter
# ---------------------------------------------------------------------------


def convert(params: dict[str, str], deck_dir: Path | None = None) -> tuple[dict, list[str]]:
    """
    Build the nested YAML dict from flat params.
    Returns (yaml_dict, unknown_keys).
    deck_dir resolves relative side files (STATIONS_FILE) named by the deck.
    """
    consumed: set[str] = set()
    unknown: list[str] = []

    def pop(key, default=None):
        consumed.add(key)
        return params.get(key, default)

    def pop_val(key, default=None):
        v = pop(key)
        return _auto(v) if v is not None else default

    def pop_bool(key, default=False):
        v = pop(key)
        return _bool(v) if v is not None else default

    def pop_str(key, default=None):
        v = pop(key)
        return v if v is not None else default

    out: dict = {}

    # ---- geometry ----------------------------------------------------------
    geo: dict = {}

    dx = pop_val("DX")
    dy = pop_val("DY")
    if dx is not None and dy is not None:
        geo["cell_size"] = [dx, dy]
    elif dx is not None:
        geo["cell_size"] = [dx, dx]
    elif dy is not None:
        geo["cell_size"] = [dy, dy]

    depth_type = pop_str("DEPTH_TYPE", "flat").lower()
    if depth_type == "data":
        depth_type = "file"

    mg = pop_val("Mglob")
    ng = pop_val("Nglob")
    if mg is not None and ng is not None:
        geo["n_cells"] = [mg, ng]

    # n_procs is an atomic pair; a lone PX/PY has no answer for the other
    # axis, so it falls through to auto-decompose
    px = pop_val("PX")
    py = pop_val("PY")
    if px is not None and py is not None:
        geo["n_procs"] = [px, py]

    bathy: dict = {"type": depth_type}
    if depth_type == "file":
        df2 = pop_str("DEPTH_FILE")
        if df2 is not None:
            bathy["file"] = df2
        ftype = pop_str("DEPTH_FTYPE")
        if ftype is not None:
            bathy["file_type"] = ftype.lower()
        bc = pop_bool("BATHY_CORRECTION")
        if "BATHY_CORRECTION" in params:
            bathy["correction"] = bc
        sbd = pop_val("SmoothBelowDepth")
        if sbd is not None:
            bathy["smooth_below_depth"] = sbd
        scap = pop_val("SlopeCap")
        if scap is not None:
            bathy["slope_cap"] = scap
        # dims ride the unified grid.n_cells (already emitted from Mglob/Nglob)
    else:
        pop("DEPTH_FILE")
        pop("DEPTH_FTYPE")
        pop("BATHY_CORRECTION")
        pop("SmoothBelowDepth")
        pop("SlopeCap")
        df = pop_val("DEPTH_FLAT")
        if df is not None:
            bathy["depth"] = df
        if depth_type == "slope":
            slp = pop_val("SLP")
            if slp is not None:
                bathy["slope"] = slp
            xslp = pop_val("Xslp")
            if xslp is not None:
                bathy["x0"] = xslp
        else:
            pop("SLP")
            pop("Xslp")

    geo["bathymetry"] = bathy
    out["grid"] = geo

    # ---- simulation --------------------------------------------------------
    sim: dict = {}
    title = pop_str("TITLE")
    if title is not None:
        sim["title"] = title
    tt = pop_val("TOTAL_TIME")
    if tt is not None:
        sim["total_time"] = tt
    ts = pop_val("PLOT_START_TIME")
    if ts is not None:
        sim["t_start"] = ts
    # PLOT_INTV / PLOT_INTV_STATION / StationOutputBuffer land under output:
    plot_intv = pop_val("PLOT_INTV")
    plot_intv_station = pop_val("PLOT_INTV_STATION")
    station_buffer = pop_val("StationOutputBuffer")
    si = pop_val("SCREEN_INTV")
    if si is not None:
        sim["screen_interval"] = si

    out["simulation"] = sim

    # ---- hot_start ---------------------------------------------------------
    ini = pop_bool("INI_UVZ")
    if ini:
        hs: dict = {}
        hs["eta_file"] = pop_str("ETA_FILE", "")
        hs["u_file"] = pop_str("U_FILE", "")
        hs["v_file"] = pop_str("V_FILE", "")
        mf = pop_str("MASK_FILE")
        if mf:
            hs["mask_file"] = mf
        hst = pop_val("HotStartTime")
        if hst is not None:
            hs["time"] = hst
        bd = pop_bool("BED_DEFORMATION")
        if "BED_DEFORMATION" in params:
            hs["bed_deformation"] = bd
        rn = pop_val("HOT_START_RES_NUM")
        if rn is not None:
            hs["output_start_number"] = rn
        out["hot_start"] = hs
    else:
        for k in ("ETA_FILE", "U_FILE", "V_FILE", "MASK_FILE", "HotStartTime", "BED_DEFORMATION", "HOT_START_RES_NUM"):
            pop(k)

    # ---- wavemaker / initial -------------------------------------------------
    # Initial-condition types (INI_*/N_WAVE) moved to the initial: section.
    wm_type = pop_str("WAVEMAKER", "NONE")
    if wm_type in ("INI_SOL", "INI_SOLITARY"):
        sol: dict = {}
        for k, yk in (("AMP", "amplitude"), ("DEP", "depth"), ("XWAVEMAKER", "x_center")):
            v = pop_val(k)
            if v is not None:
                sol[yk] = v
        if not pop_bool("SolitaryPositiveDirection", True):
            sol["direction"] = "-x"
        out.setdefault("initial", {})["solitary"] = sol
        wm_type = "NONE"
    elif wm_type == "INI_SINE":
        sine: dict = {}
        for k, yk in (("AMP", "amplitude"), ("DEP", "depth"), ("mode_x", "mode_x"), ("mode_y", "mode_y")):
            v = pop_val(k)
            if v is not None:
                sine[yk] = v
        out.setdefault("initial", {})["sine_mode"] = sine
        wm_type = "NONE"
    elif wm_type in ("INI_REC", "INI_Gau", "INI_GAU", "INI_DIP", "N_WAVE"):
        # pending in the modern engine (hump/n_wave blocks gate at init);
        # emit the block anyway so the gate fires loudly instead of the
        # keys vanishing into the unknown-key comment block
        shape = {"INI_REC": "rect", "INI_DIP": "dipole"}.get(wm_type, "gaussian")
        if wm_type == "N_WAVE":
            nw: dict = {}
            for k, yk in (
                ("x1_Nwave", "x1"),
                ("x2_Nwave", "x2"),
                ("a0_Nwave", "a0"),
                ("gamma_Nwave", "gamma"),
                ("dep_Nwave", "depth"),
            ):
                v = pop_val(k)
                if v is not None:
                    nw[yk] = v
            out.setdefault("initial", {})["n_wave"] = nw
        else:
            hp: dict = {"shape": shape}
            for k, yk in (("AMP", "amplitude"), ("Xc", "x_center"), ("Yc", "y_center"), ("WID", "width"), ("GauRadius", "radius")):
                v = pop_val(k)
                if v is not None:
                    hp[yk] = v
            out.setdefault("initial", {})["hump"] = hp
        wm_type = "NONE"
    if wm_type.upper() not in ("NONE", "NOTHING"):
        wm, west = _convert_wavemaker(wm_type, pop_val)
        if wm is not None:
            out["wavemaker"] = wm
        if west is not None:
            out.setdefault("boundaries", {})["west"] = west

    # ---- sponge -> boundaries face blocks (config reorg rung 2) -------------
    # Legacy global coefficients replicate onto every face with width > 0;
    # a type sub-block is present iff its legacy flag was T.
    ds = pop_bool("DIFFUSION_SPONGE")
    di = pop_bool("DIRECT_SPONGE")
    fs = pop_bool("FRICTION_SPONGE")
    widths = {f: pop_val(f"Sponge_{f}_width") for f in ("west", "east", "south", "north")}
    r_sp, a_sp = pop_val("R_sponge"), pop_val("A_sponge")
    cd_sp, nu_sp = pop_val("CDsponge"), pop_val("Csp")
    if ds or di or fs:
        for f, w in widths.items():
            if w is None or str(w) in ("0", "0.0"):
                continue
            # always emit the coefficients (legacy io.F defaults as fallback)
            # -- a bare empty sub-block dumps as YAML null and would read as
            # absent, silently dropping the sponge type
            sp: dict = {"width": w}
            if di:
                sp["direct"] = {"r": r_sp or "0.85", "a": a_sp or "5.0"}
            if fs:
                sp["friction"] = {"cd": cd_sp or "0.0"}
            if ds:
                sp["diffusion"] = {"nu": nu_sp or "0.1"}
            bnd = out.setdefault("boundaries", {})
            if f in bnd:
                # ABS emitted a west block above; a face carries ONE sponge
                raise SystemExit(f"convert_input: boundaries.{f} conflict — face already owned by the wavemaker conversion")
            bnd[f] = {"sponge": sp}

    # ---- obstacle / breakwater ---------------------------------------------
    # file presence = obstacle mask; breakwater block presence = breakwater
    # drag (nee the OBSTACLE/BREAKWATER dispatcher bools)
    obs = pop_bool("OBSTACLE")
    bw = pop_bool("BREAKWATER")
    if obs or bw:
        ob: dict = {}
        of = pop_str("OBSTACLE_FILE")
        bf = pop_str("BREAKWATER_FILE")
        ba = pop_val("BreakWaterAbsorbCoef")
        if obs and of:
            ob["file"] = of
        if bw and bf:
            # always emit the coefficient (legacy io.F default as fallback) --
            # a bare empty sub-block dumps as YAML null and reads as absent
            ob["breakwater"] = {"file": bf, "absorb_coef": ba if ba is not None else 10.0}
        if ob:
            out["obstacle"] = ob
    else:
        for k in ("OBSTACLE_FILE", "BREAKWATER_FILE", "BreakWaterAbsorbCoef"):
            pop(k)

    # ---- friction ----------------------------------------------------------
    # exactly one of cd | manning | file; there is no legacy Manning key.
    # IN_Cd selects the map, under which legacy ignores the scalar Cd.
    in_cd = pop_bool("IN_Cd")
    cd = pop_val("Cd")
    cd_file = pop_str("CD_FILE")
    if in_cd and cd_file:
        out["friction"] = {"file": cd_file}
        if cd not in (None, 0, 0.0):
            unknown.append(f"Cd = {cd} superseded by CD_FILE (IN_Cd) -- dropped")
    elif cd not in (None, 0, 0.0):
        out["friction"] = {"cd": cd}

    # ---- physics / boundaries / initial --------------------------------------
    # C_smg intentionally not consumed (Smagorinsky was amputated upstream);
    # it falls through to the unknown-key comment block.
    if pop_bool("PERIODIC"):
        out.setdefault("boundaries", {})["periodic"] = ["y"]
    wl = pop_val("WATER_LEVEL")
    if wl is not None:
        geo["water_level"] = wl

    disp: dict = {}
    if not pop_bool("DISPERSION", True):
        disp["scheme"] = "nswe"
        for k in ("Gamma1", "Gamma2", "Gamma3"):
            pop_val(k)  # kernels off — the preset owns the triple
    else:
        gam = {yk: pop_val(k) for k, yk in
               (("Gamma1", "gamma1"), ("Gamma2", "gamma2"), ("Gamma3", "gamma3"))}
        if any(v is not None for v in gam.values()):
            disp.update({yk: (1.0 if v is None else v) for yk, v in gam.items()})
    v = pop_val("Beta_ref")
    if v is not None:
        disp["beta_ref"] = v
    if disp:
        out["dispersion"] = disp

    # ---- numerics ----------------------------------------------------------
    # Time_Scheme dropped: the modern stepper is RK3-only (falls through to
    # the unknown-key comment block if present).
    nu: dict = {}
    for k, yk in (("CONSTRUCTION", "flux_solver"), ("HIGH_ORDER", "reconstruction")):
        v = pop_str(k)
        if v is not None:
            nu[yk] = v.lower()
    for k, yk in (("CFL", "cfl"), ("FroudeCap", "froude_cap")):
        v = pop_val(k)
        if v is not None:
            nu[yk] = v
    # DT_fixed lands here (nee simulation time_stepping): presence = fixed
    # step.  cfl and dt are exclusive in the reader, so an explicit legacy
    # CFL yields to dt; the halving cap then uses the default 0.5 -- warn
    # when that changes the cap
    dt_fixed = pop_val("DT_fixed")  # legacy key: non-zero value implies fixed dt
    if dt_fixed is not None and dt_fixed != 0.0:
        cfl = nu.pop("cfl", None)
        if cfl is not None and cfl != 0.5:
            print(
                f"WARNING: DT_fixed with CFL={cfl}: cfl dropped (exclusive"
                " with numerics dt); the fixed-dt stability cap now uses"
                " the default 0.5",
                file=sys.stderr,
            )
        nu["dt"] = dt_fixed
    # legacy folded the MinDepth/MinDepthFrc pair to their minimum (old io.F)
    md = pop_val("MinDepth")
    mdf = pop_val("MinDepthFrc")
    floors = [v for v in (md, mdf) if v is not None]
    if floors:
        nu["min_depth"] = min(floors)
    if nu:
        out["numerics"] = nu

    # ---- breaking ----------------------------------------------------------
    br: dict = {}
    # nee VISCOSITY_BREAKING (T -> eddy_viscosity, default; F -> shock_capturing);
    # WAVEMAKER_VIS folds in as the third model (shock globally + Kennedy zone)
    if not pop_bool("VISCOSITY_BREAKING", True):
        br["model"] = "shock_capturing"
    if pop_bool("WAVEMAKER_VIS"):
        br["model"] = "wavemaker_viscosity"
    roller = pop_bool("ROLLER")
    if roller:
        br["roller"] = True
    # SHOW_BREAKING retired — the engine derives the diagnostics pass
    pop_bool("SHOW_BREAKING", True)
    for k, yk in (
        ("Cbrk1", "cbrk1"),
        ("Cbrk2", "cbrk2"),
        ("visbrk", "visbrk"),
        ("nu_bkg", "nu_bkg"),
        ("SWE_ETA_DEP", "swe_eta_dep"),
    ):
        v = pop_val(k)
        if v is not None:
            br[yk] = v
    # zone overrides live on the wavemaker source block now
    for k, yk in (("WAVEMAKER_Cbrk", "cbrk"), ("WAVEMAKER_visbrk", "visbrk")):
        v = pop_val(k)
        if v is not None and isinstance(out.get("wavemaker"), dict):
            out["wavemaker"].setdefault("source", {}).setdefault("breaking", {})[yk] = v
    if br:
        out["breaking"] = br

    # ---- output ------------------------------------------------------------
    op: dict = {}
    if plot_intv is not None:
        op["interval"] = plot_intv
    rf = pop_str("RESULT_FOLDER")
    if rf:
        op["result_folder"] = rf
    # legacy default is ASCII but the modern default is binary, so the
    # format is always emitted to preserve the deck's meaning
    fio = pop_str("FIELD_IO_TYPE")
    op["format"] = (fio or "ASCII").lower()
    # stations retired -> inline point channel: the "i j" pairs are global
    # interior indices, mapped to cell-centre coords x = (i-1)*dx.
    # NumberStations only gates the block (legacy N < line count truncated;
    # the derived count reads every line)
    ns = pop_val("NumberStations")
    sf = pop_str("STATIONS_FILE")
    if sf and (ns is None or ns > 0):
        sta_path = (deck_dir / sf) if deck_dir is not None else Path(sf)
        if dx is None or dy is None:
            unknown.append(f"STATIONS_FILE = {sf} (no DX/DY to map i j to x y) -- port to output: channels: manually")
        elif not sta_path.is_file():
            unknown.append(f"STATIONS_FILE = {sf} (not found at convert time) -- port to output: channels: manually")
        else:
            pairs = [ln.split() for ln in sta_path.read_text().splitlines() if ln.strip()]
            op["channels"] = [
                {
                    "name": "sta",
                    "type": "station",
                    "x": [(int(float(p[0])) - 1) * dx for p in pairs],
                    "y": [(int(float(p[1])) - 1) * dy for p in pairs],
                    "variables": ["eta", "u", "v"],
                    "interval": plot_intv_station if plot_intv_station is not None else 1.0,
                }
            ]
    if station_buffer is not None:
        unknown.append("StationOutputBuffer (channels flush every interval, no buffer) -- dropped")
    ores = pop_val("OUTPUT_RES")
    if ores is not None:
        unknown.append("OUTPUT_RES (the stride was never consumed) -- dropped")
    ebv = pop_val("EtaBlowVal")
    if ebv is not None:
        op["blowup_threshold"] = ebv
    # means: presence block; absent legacy keys fall to the legacy LARGE
    # defaults so a half-specified deck stays bitwise (window never closes)
    ti = pop_val("T_INTV_mean")
    st = pop_val("STEADY_TIME")
    if ti is not None or st is not None:
        op["means"] = {
            "interval": ti if ti is not None else 999999.0,
            "steady_time": st if st is not None else 999999.0,
        }

    # first-arrival map (nee numerics OUT_Time/ArrTimeMin); min_height is
    # always written so the block never serialises as a bare null key
    if _bool(pop("OUT_Time") or "F"):
        atm = pop_val("ArrTimeMin")
        op["arrival_time"] = {"min_height": atm if atm is not None else 0.001}
    else:
        pop("ArrTimeMin")

    # depth_out — static field, separate from variables list
    depth_out = pop_bool("DEPTH_OUT")
    if depth_out:
        op["depth_out"] = True

    # OUT_* flags → variables list
    variables: list[str] = []
    for old_key, new_name in _VAR_FLAGS.items():
        v = pop(old_key)
        if v is not None and _bool(v):
            # ROLLER also drives breaking.roller (already consumed above);
            # here we only add to variables if the output flag is true.
            variables.append(new_name)
    if variables:
        op["variables"] = variables

    if op:
        out["output"] = op

    # ---- coupling ----------------------------------------------------------
    cf = pop_str("COUPLING_FILE")
    if cf:
        out["coupling"] = {"file": cf}

    # ---- meteo (atmospheric forcing) ---------------------------------------
    # Sub-model blocks: presence = model on (nee the MeteoGausian/
    # WindConstantField/WindHollandModel/SlideModel dispatcher bools).
    mt: dict = {}
    mg = pop_bool("MeteoGausian")
    wcf = pop_bool("WindConstantField")
    whm = pop_bool("WindHollandModel")
    sm = pop_bool("SlideModel")
    if mg or wcf or whm or sm:
        wf = pop("WindForce")
        ap = pop("AirPressure")
        wwi = pop_bool("WindWaveInteraction")
        cdw = pop_val("Cdw")
        wcp = pop_val("WindCrestPercent")

        def wind_knobs() -> dict:
            d: dict = {}
            if cdw is not None:
                d["cd"] = cdw
            if wwi:
                d["wave_interaction"] = True
                if wcp is not None:
                    d["crest_percent"] = wcp
            # interaction off: legacy forced the crest mask inert (LARGE), so
            # a lone WindCrestPercent is dropped rather than emitted
            return d

        if mg:
            gf = pop_str("METEO_GAUSIAN_FILE")
            if not gf:
                raise SystemExit("convert_input: MeteoGausian requires METEO_GAUSIAN_FILE")
            mt["gaussian"] = {"file": gf}
        if wcf:
            cwf = pop_str("CONSTANT_WIND_FILE")
            if not cwf:
                raise SystemExit("convert_input: WindConstantField requires CONSTANT_WIND_FILE")
            if wf is not None and not _bool(wf):
                # a no-force constant wind is inert in legacy -- dropped
                unknown.append("WindConstantField with WindForce = F (inert in legacy) -- dropped")
            else:
                mt["wind"] = {"file": cwf, **wind_knobs()}
        if whm:
            sf = pop_str("STORM_FILE")
            if not sf:
                raise SystemExit("convert_input: WindHollandModel requires STORM_FILE")
            h: dict = {"file": sf}
            if ap is not None and _bool(ap):
                h["air_pressure"] = True
            # legacy WindForce default is WindConstantField's value
            force = _bool(wf) if wf is not None else "wind" in mt
            if force:
                h["wind_force"] = True
                h.update(wind_knobs())
            mt["holland"] = h
        if sm:
            slf = pop_str("SLIDE_FILE")
            if not slf:
                raise SystemExit("convert_input: SlideModel requires SLIDE_FILE")
            mt["slide"] = {"file": slf}
        # OUT_METEO (default YES) -> output: variables: field dumps, per
        # model applicability (Pstorm = any pressure model, U/Vstorm = Holland)
        om = pop("OUT_METEO")
        if om is None or _bool(om):
            met_vars = []
            if "gaussian" in mt or "holland" in mt or "slide" in mt:
                met_vars.append("Pstorm")
            if "holland" in mt:
                met_vars += ["Ustorm", "Vstorm"]
            if met_vars:
                out.setdefault("output", {}).setdefault("variables", []).extend(met_vars)
    if mt:
        out["meteo"] = mt

    # ---- subgrid -------------------------------------------------------------
    sgf = pop_str("DEPTH_SUBGRID_FILE")
    if sgf:
        sg: dict = {"depth_file": sgf}
        sgr = pop_val("SubMainGridRatio")
        if sgr is not None:
            sg["ratio"] = sgr
        sgp = pop("Porosity")
        if sgp is not None:
            sg["write_porosity"] = _bool(sgp)
        out["subgrid"] = sg

    # ---- precipitation -------------------------------------------------------
    # RainWaveInteraction is left unconsumed on purpose: it is dead in legacy
    # (no consumer), so it lands in the unknown-key comment block.
    rff = pop_str("RAINFALL_FILE")
    if rff:
        if pop("OUT_PRECIPITATION") is not None:
            unknown.append("OUT_PRECIPITATION (dead in legacy: no writer) -- dropped")
        out["precipitation"] = {"file": rff}

    # ---- foam ----------------------------------------------------------------
    # No legacy dispatcher bool: the keys exist when the deck drove a -DFOAM
    # build, so any foam knob keys the block.
    fo: dict = {}
    for old_key, new_key in (
        ("f_source", "source_coef"),
        ("FoamTimeScale", "time_scale"),
        ("BurstTimeNonBreaking", "burst_time_non_breaking"),
        ("MinThick", "min_thickness"),
        ("CdFoam", "cd"),
    ):
        v = pop_val(old_key)
        if v is not None:
            fo[new_key] = v
    if pop("PLOT_INTV_FOAM") is not None:
        unknown.append("PLOT_INTV_FOAM (dead in legacy: empty stub writer) -- dropped")
    if fo:
        out["foam"] = fo

    # ---- tracer ----------------------------------------------------------------
    tf = pop_str("TRACER_FILE")
    if tf:
        out["tracer"] = {"file": tf}

    # ---- vessel ----------------------------------------------------------------
    vfold = pop_str("VESSEL_FOLDER")
    if vfold:
        vs: dict = {"folder": vfold}
        nv = pop_val("NumVessel")
        if nv is not None:
            vs["count"] = nv
        if pop_bool("PROPELLER"):
            vs["propeller"] = True
        if pop_bool("DEEP_DRAFT"):
            # clearance anchors the block; legacy defaults FrictionMethod ON,
            # so cd is emitted explicitly whenever the drag is active
            cl = pop_val("CLEARANCE")
            dd: dict = {"clearance": cl if cl is not None else 1.0}
            mm = pop("MaskMethod")
            if mm is not None:
                dd["mask"] = _bool(mm)
            fm = pop("FrictionMethod")
            if fm is None or _bool(fm):
                cdd = pop_val("CdDeepDraft")
                dd["cd"] = cdd if cdd is not None else 0.1
            else:
                pop("CdDeepDraft")
            vm = pop("ViscosityMethod")
            if vm is not None and _bool(vm):
                vdd = pop_val("VisDeepDraft")
                dd["nu"] = vdd if vdd is not None else 0.1
            else:
                pop("VisDeepDraft")
            vs["deep_draft"] = dd
        else:
            for k in ("MaskMethod", "FrictionMethod", "ViscosityMethod", "CLEARANCE", "CdDeepDraft", "VisDeepDraft"):
                pop(k)
        # OUT_VESSEL (default YES) -> output: vessel: (resistance series) +
        # variables: Pves/VesUp/VesVp; interval 0 = legacy every-step SMALL
        ov = pop("OUT_VESSEL")
        piv = pop_val("PLOT_INTV_VESSEL")
        if ov is None or _bool(ov):
            ves_vars = ["Pves"]
            if vs.get("propeller"):
                ves_vars += ["VesUp", "VesVp"]
            opv = out.setdefault("output", {})
            opv.setdefault("variables", []).extend(ves_vars)
            opv["vessel"] = {"interval": piv if piv is not None else 0.0}
        out["vessel"] = vs

    # ---- sediment --------------------------------------------------------------
    # No legacy dispatcher bool: the keys exist when the deck drove a
    # -DSEDIMENT build, so any primary sediment key opens the block.
    # Kappa1/Kappa2 (read + echoed, never used) and k_coh (inert, NOTE 16)
    # fall to the unknown-comment block.
    sed_keys = ("Sed_Scheme", "D50", "Bed_Change", "CohesiveSediment", "Sdensity", "WS", "Shields_cr")
    if any(params.get(k) is not None for k in sed_keys):
        sd: dict = {}
        ssch = pop_str("Sed_Scheme")
        if ssch is not None:
            # legacy prefix dispatch: anything not Upw* selects the weighted upwind
            sd["scheme"] = "upwinding" if ssch[:3] == "Upw" else "tvd"
        for old_key, new_key in (
            ("D50", "d50"),
            ("Sdensity", "specific_gravity"),
            ("n_porosity", "porosity"),
            ("WS", "settling_velocity"),
            ("Shields_cr", "shields_cr"),
            ("MinDepthPickup", "min_depth_pickup"),
            ("ReductionParameter", "reduction_parameter"),
            ("C_limiter", "c_limiter"),
            ("Morph_interval", "morph_interval"),
            ("Shields_cr_bedload", "shields_cr_bedload"),
            ("Morph_factor", "morph_factor"),
        ):
            v = pop_val(old_key)
            if v is not None:
                sd[new_key] = v
        pr = pop("PickupReduction")
        if pr is not None:
            if _bool(pr):
                sd["pickup_reduction"] = True
            else:
                # reader rejects a dead reduction_parameter under reduction-off
                sd["pickup_reduction"] = False
                sd.pop("reduction_parameter", None)
        for old_key, new_key in (("Bed_Change", "bed_change"), ("BedLoad", "bedload")):
            v = pop(old_key)
            if v is not None:
                sd[new_key] = _bool(v)
        if pop_bool("Hard_bottom"):
            hbf = pop_str("Hard_bottom_file")
            if not hbf:
                raise SystemExit("convert_input: Hard_bottom requires Hard_bottom_file")
            sd["hard_bottom"] = {"file": hbf}
        else:
            pop("Hard_bottom_file")
        if pop_bool("Avalanche"):
            tp = pop_val("Tan_phi")
            av: dict = {"tan_phi": tp if tp is not None else 0.7}
            ai = pop_val("Aval_interval")
            if ai is not None:
                av["interval"] = ai
            sd["avalanche"] = av
        else:
            for k in ("Tan_phi", "Aval_interval"):
                pop(k)
        if pop_bool("CohesiveSediment"):
            tc = pop_val("Tau_cr_coh")
            co: dict = {"tau_cr": tc if tc is not None else 0.001}
            sb = pop("SoftBed")
            if sb is not None:
                co["soft_bed"] = _bool(sb)
            for old_key, new_key in (
                ("Tau_crd_coh", "tau_crd"),
                ("E_coh", "e"),
                ("alpha_coh", "alpha"),
                ("a_coh", "a"),
                ("b_coh", "b"),
                ("n_coh", "n"),
                ("m_coh", "m"),
            ):
                v = pop_val(old_key)
                if v is not None:
                    co[new_key] = v
            sd["cohesive"] = co
        else:
            for k in ("SoftBed", "Tau_cr_coh", "Tau_crd_coh", "E_coh", "alpha_coh", "a_coh", "b_coh", "n_coh", "m_coh"):
                pop(k)
        fb: dict = {}
        for old_key, new_key in (
            ("SedimentMassSource", "mass_source"),
            ("SedimentMomentDC", "moment_dc"),
            ("SedimentMomentEXG", "moment_exg"),
        ):
            v = pop(old_key)
            if v is not None and _bool(v):
                fb[new_key] = True
        if fb:
            sd["feedback"] = fb
        out["sediment"] = sd

    # ---- collect unknown keys ----------------------------------------------
    for k in params:
        if k not in consumed:
            unknown.append(f"{k} = {params[k]}")

    return out, unknown


# ---------------------------------------------------------------------------
# YAML serialiser (no external ruamel dependency — hand-rolled for readability)
# ---------------------------------------------------------------------------


def _yaml_value(v) -> str:
    if isinstance(v, bool):
        return "true" if v else "false"
    if isinstance(v, list):
        return "[" + ", ".join(str(i) for i in v) + "]"
    if isinstance(v, str) and (" " in v or ":" in v or not v):
        return f'"{v}"'
    return str(v)


def _dump_yaml(d: dict, indent: int = 0) -> list[str]:
    lines: list[str] = []
    pad = "  " * indent
    for k, v in d.items():
        if isinstance(v, dict):
            lines.append(f"{pad}{k}:")
            lines.extend(_dump_yaml(v, indent + 1))
        elif isinstance(v, list) and v and all(isinstance(i, dict) for i in v):
            lines.append(f"{pad}{k}:")
            for item in v:
                entry = _dump_yaml(item, indent + 2)
                lines.append(f"{pad}  - " + entry[0].strip())
                lines.extend(entry[1:])
        else:
            lines.append(f"{pad}{k}: {_yaml_value(v)}")
    return lines


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def _convert_3d(src: Path, dst_yaml: Path) -> None:
    """Full 3-D conversion: map legacy input.txt keys to 3D YAML schema.

    Copies src → input.txt so the legacy ref binary (vendor/3d-fd) can still
    find it when preprocess_ref: true.  The dev binary reads only the YAML.
    """
    shutil.copy2(src, "input.txt")
    params = parse_input_txt(src)

    def pop(key, default=None):
        return params.get(key, default)

    def pop_val(key, default=None):
        v = pop(key)
        return _auto(v) if v is not None else default

    def pop_bool(key, default=False):
        v = pop(key)
        return _bool(v) if v is not None else default

    def pop_str(key, default=None):
        return params.get(key, default)

    out: dict = {}

    # ---- geometry ------------------------------------------------------------
    nx = pop_val("Mglob", 0)
    ny = pop_val("Nglob", 0)
    nz = pop_val("Kglob", 0)
    dx = pop_val("DX", 1.0)
    dy = pop_val("DY", 1.0)

    geo: dict = {
        "grid_size": [nx, ny, nz],
        "cell_size": [dx, dy],
    }
    ivgrd = pop_val("IVGRD", 1)
    if ivgrd != 1:
        geo["ivgrd"] = ivgrd
    grd_r = pop_val("GRD_R")
    if grd_r is not None:
        geo["grd_r"] = grd_r

    px = pop_val("PX")
    py = pop_val("PY")
    if px is not None or py is not None:
        decomp: dict = {}
        if px is not None:
            decomp["nx_proc"] = px
        if py is not None:
            decomp["ny_proc"] = py
        geo["decomposition"] = decomp

    depth_type = pop_str("DEPTH_TYPE", "CELL_CENTER")
    ana_bathy = pop_bool("ANA_BATHY", False)
    depth_file = pop_str("DEPTH_FILE")
    bathy: dict = {"type": depth_type, "analytic": ana_bathy}
    if depth_file:
        bathy["file"] = depth_file
    geo["bathymetry"] = bathy

    ibot = pop_val("Ibot")
    cd0 = pop_val("Cd0")
    zob = pop_val("Zob")
    min_dep = pop_val("MinDep")
    bot: dict = {}
    if ibot is not None:
        bot["roughness_type"] = ibot
    if cd0 is not None:
        bot["cd"] = cd0
    if zob is not None:
        bot["zob"] = zob
    if min_dep is not None:
        bot["min_depth"] = min_dep
    if bot:
        geo["bottom"] = bot

    out["geometry"] = geo

    # ---- simulation ----------------------------------------------------------
    sim: dict = {}
    total_time = pop_val("TOTAL_TIME")
    sim_steps = pop_val("SIM_STEPS")
    plot_start = pop_val("PLOT_START")
    plot_intv = pop_val("PLOT_INTV")
    screen_intv = pop_val("SCREEN_INTV")
    cfl = pop_val("CFL")
    if total_time is not None:
        sim["total_time"] = total_time
    if sim_steps is not None:
        sim["sim_steps"] = sim_steps
    if plot_start is not None:
        sim["plot_start"] = plot_start
    if plot_intv is not None:
        sim["plot_intv"] = plot_intv
    if screen_intv is not None:
        sim["screen_intv"] = screen_intv
    if cfl is not None:
        sim["cfl"] = cfl

    dt_ini = pop_val("DT_INI")
    dt_min = pop_val("DT_MIN")
    dt_max = pop_val("DT_MAX")
    if any(v is not None for v in (dt_ini, dt_min, dt_max)):
        ts: dict = {}
        if dt_ini is not None:
            ts["dt_ini"] = dt_ini
        if dt_min is not None:
            ts["dt_min"] = dt_min
        if dt_max is not None:
            ts["dt_max"] = dt_max
        sim["time_stepping"] = ts

    nstat = pop_val("NSTAT", 0)
    plot_intv_stat = pop_val("PLOT_INTV_STAT")
    stations_file = pop_str("STATIONS_FILE")
    if nstat or plot_intv_stat or stations_file:
        stat: dict = {"count": nstat or 0}
        if plot_intv_stat is not None:
            stat["interval"] = plot_intv_stat
        if stations_file:
            stat["file"] = stations_file
        sim["stations"] = stat

    if sim:
        out["simulation"] = sim

    # ---- physics -------------------------------------------------------------
    phys: dict = {}
    barotropic = pop_bool("BAROTROPIC", True)
    non_hydro = pop_bool("NON_HYDRO", False)
    high_order = pop_str("HIGH_ORDER")
    time_order = pop_str("TIME_ORDER")
    convection = pop_str("CONVECTION")
    adv_hllc = pop_bool("HLLC", False)
    tramp = pop_val("TRAMP")
    periodic_x = pop_bool("PERIODIC_X", False)
    periodic_y = pop_bool("PERIODIC_Y", False)
    ext_force = pop_bool("EXTERNAL_FORCING", False)
    froude_cap = pop_val("FROUDE_CAP")

    phys["barotropic"] = barotropic
    phys["non_hydro"] = non_hydro
    if high_order:
        phys["high_order"] = high_order
    if time_order:
        phys["time_order"] = time_order
    if convection:
        phys["convection"] = convection
    if adv_hllc:
        phys["adv_hllc"] = adv_hllc
    if tramp:
        phys["tramp"] = tramp
    if periodic_x:
        phys["periodic_x"] = periodic_x
    if periodic_y:
        phys["periodic_y"] = periodic_y
    if ext_force:
        phys["external_forcing"] = ext_force
    if froude_cap is not None:
        phys["froude_cap"] = froude_cap

    wave_avg_on = pop_bool("WAVE_AVERAGE_ON", False)
    wave_avg_start = pop_val("WAVE_AVERAGE_START")
    wave_avg_end = pop_val("WAVE_AVERAGE_END")
    waveheight_id = pop_val("WaveheightID")
    if wave_avg_on or wave_avg_start or wave_avg_end or waveheight_id:
        wa: dict = {"active": wave_avg_on}
        if wave_avg_start is not None:
            wa["t_start"] = wave_avg_start
        if wave_avg_end is not None:
            wa["t_end"] = wave_avg_end
        if waveheight_id is not None:
            wa["height_id"] = waveheight_id
        phys["wave_average"] = wa

    if phys:
        out["physics"] = phys

    # ---- turbulence ----------------------------------------------------------
    turb: dict = {}
    viscous_flow = pop_bool("VISCOUS_FLOW", False)
    ivturb = pop_val("IVTURB")
    ihturb = pop_val("IHTURB")
    viscosity = pop_val("VISCOSITY")
    schmidt = pop_val("Schmidt")
    cvs = pop_val("Cvs")
    chs = pop_val("Chs")
    viscous_number = pop_val("VISCOUS_NUMBER")
    if viscous_flow:
        turb["viscous_flow"] = viscous_flow
    if ivturb is not None:
        turb["ivturb"] = ivturb
    if ihturb is not None:
        turb["ihturb"] = ihturb
    if viscosity is not None:
        turb["visc"] = viscosity
    if schmidt is not None:
        turb["schmidt"] = schmidt
    if cvs is not None:
        turb["cvs"] = cvs
    if chs is not None:
        turb["chs"] = chs
    if viscous_number is not None:
        turb["viscous_number"] = viscous_number
    if turb:
        out["turbulence"] = turb

    # ---- solver --------------------------------------------------------------
    isolver = pop_val("ISOLVER")
    itmax = pop_val("ITMAX")
    tol = pop_val("TOL")
    slv: dict = {}
    if isolver is not None:
        slv["solver_type"] = isolver
    if itmax is not None:
        slv["max_iter"] = itmax
    if tol is not None:
        slv["tolerance"] = tol
    if slv:
        out["solver"] = slv

    # ---- wavemaker -----------------------------------------------------------
    wm_type = pop_str("WAVEMAKER", "nothing")
    if wm_type and wm_type.lower() != "nothing":
        wm: dict = {"type": wm_type}
        for k3d, yml in [
            ("Wave_Comp_File", "wave_comp_file"),
            ("Dep_Ser", "dep_ser"),
            ("U_FLOW_LEFT", "u_flow_left"),
            ("U_FLOW_RIGHT", "u_flow_right"),
            ("AMP", "amp"),
            ("PER", "per"),
            ("DEP", "dep"),
            ("THETA", "theta"),
            ("Xsource_West", "xsource_west"),
            ("Xsource_East", "xsource_east"),
            ("Ysource_Suth", "ysource_suth"),
            ("Ysource_Nrth", "ysource_nrth"),
            ("Hm0", "hm0"),
            ("Tp", "tp"),
            ("Freq_Min", "freq_min"),
            ("Freq_Max", "freq_max"),
            ("NumFreq", "num_freq"),
        ]:
            v = pop_val(k3d) if k3d not in ("Wave_Comp_File",) else pop_str(k3d)
            if v is not None:
                wm[yml] = v
        out["wavemaker"] = wm

    # ---- boundary conditions -------------------------------------------------
    bc: dict = {}
    bc_x0 = pop_val("BC_X0")
    bc_xn = pop_val("BC_Xn")
    bc_y0 = pop_val("BC_Y0")
    bc_yn = pop_val("BC_Yn")
    bc_z0 = pop_val("BC_Z0")
    bc_zn = pop_val("BC_Zn")
    if bc_x0 is not None:
        bc["bc_x0"] = bc_x0
    if bc_xn is not None:
        bc["bc_xn"] = bc_xn
    if bc_y0 is not None:
        bc["bc_y0"] = bc_y0
    if bc_yn is not None:
        bc["bc_yn"] = bc_yn
    if bc_z0 is not None:
        bc["bc_z0"] = bc_z0
    if bc_zn is not None:
        bc["bc_zn"] = bc_zn
    boundary_type = pop_str("BOUNDARY")
    boundary_file = pop_str("BOUNDARY_FILE")
    if boundary_type:
        bc["boundary_type"] = boundary_type
    if boundary_file:
        bc["boundary_file"] = boundary_file
    if bc:
        out["boundary_conditions"] = bc

    # ---- sponge --------------------------------------------------------------
    sponge_on = pop_bool("SPONGE_ON", False)
    if sponge_on:
        sp: dict = {}
        for k3d, yml in [
            ("Sponge_West_Width", "west_width"),
            ("Sponge_East_Width", "east_width"),
            ("Sponge_South_Width", "south_width"),
            ("Sponge_North_Width", "north_width"),
            ("R_Sponge", "r_sponge"),
            ("A_Sponge", "a_sponge"),
        ]:
            v = pop_val(k3d)
            if v is not None:
                sp[yml] = v
        out["sponge"] = sp

    # ---- hot start -----------------------------------------------------------
    hotstart = pop_bool("HOTSTART", False)
    if hotstart:
        hs: dict = {}
        for k3d, yml in [
            ("Eta_HotStart_File", "eta_file"),
            ("U_HotStart_File", "u_file"),
            ("V_HotStart_File", "v_file"),
            ("W_HotStart_File", "w_file"),
            ("P_HotStart_File", "p_file"),
            ("Sali_HotStart_File", "sali_file"),
            ("Temp_HotStart_File", "temp_file"),
            ("Rho_HotStart_File", "rho_file"),
            ("TKE_HotStart_File", "tke_file"),
            ("EPS_HotStart_File", "eps_file"),
        ]:
            v = pop_str(k3d)
            if v:
                hs[yml] = v
        if hs:
            out["hot_start"] = hs

    # ---- output --------------------------------------------------------------
    result_folder = pop_str("RESULT_FOLDER", "./output/")
    field_io_type = pop_str("FIELD_IO_TYPE", "ASCII")
    _3d_var_map = {
        "OUT_DEP": "DEP",
        "OUT_ETA": "ETA",
        "OUT_U": "U",
        "OUT_V": "V",
        "OUT_W": "W",
        "OUT_P": "P",
        "OUT_K": "TKE",
        "OUT_D": "EPS",
        "OUT_S": "S",
        "OUT_C": "MU",
        "OUT_B": "BUB",
        "OUT_A": "A",
        "OUT_T": "T",
        "OUT_F": "F",
        "OUT_G": "G",
        "OUT_I": "SALI",
        "OUT_Z": "TEMP",
        "OUT_M": "RHO",
    }
    vars_on = [yml for k3d, yml in _3d_var_map.items() if pop_bool(k3d, False)]
    op: dict = {"result_folder": result_folder}
    # always emitted: the modern default flipped to binary
    op["format"] = field_io_type.lower()
    if vars_on:
        op["variables"] = vars_on
    out["output"] = op

    # ---- write YAML ----------------------------------------------------------
    header = [
        "# FUNWAVE-TVD 3D input — converted from legacy input.txt",
        f"# Source: {src}",
        "",
    ]
    body = "\n".join(header + _dump_yaml(out)) + "\n"
    dst_yaml.write_text(body)


def main():
    parser = argparse.ArgumentParser(description="Convert a legacy FUNWAVE input.txt to the new YAML format.")
    parser.add_argument("input", type=Path, help="Path to legacy input.txt")
    parser.add_argument("output", type=Path, nargs="?", help="Output YAML path (default: stdout)")
    args = parser.parse_args()

    params = parse_input_txt(args.input)

    # 3-D inputs (Kglob present) get a minimal geometry stub so the unified
    # funwave launcher can detect dimensionality; full conversion is deferred
    # until READ_INPUT is replaced by a YAML reader in the 3D path.
    if "Kglob" in params:
        if args.output is None:
            raise SystemExit("convert_input: output path required for 3-D inputs")
        _convert_3d(args.input, args.output)
        return

    yaml_dict, unknown = convert(params, deck_dir=args.input.parent)

    header_lines: list[str] = [
        "# FUNWAVE-TVD input — converted from legacy input.txt",
        f"# Source: {args.input}",
        "#",
    ]
    if unknown:
        header_lines += [
            "# WARNING: the following keys were not recognised and have been dropped.",
            "# Review and add them manually if needed:",
        ] + [f"#   {u}" for u in unknown]
    else:
        header_lines.append("# No unrecognised keys.")
    header_lines.append("")

    body = "\n".join(header_lines + _dump_yaml(yaml_dict)) + "\n"

    if args.output:
        args.output.write_text(body)
        print(f"Written to {args.output}", file=sys.stderr)
        if unknown:
            print(f"WARNING: {len(unknown)} unrecognised key(s) — see comment block in output.", file=sys.stderr)
    else:
        sys.stdout.write(body)


if __name__ == "__main__":
    main()
