#!/usr/bin/env python3
"""
Convert a legacy FUNWAVE-TVD flat input.txt to the new nested YAML format.

Usage:
    python scripts/convert_input.py input.txt [output.yaml]

If output.yaml is omitted the result is written to stdout.
Unknown keys are collected and emitted as a comment block at the top of the
output file so nothing is silently dropped.
"""

import argparse
import re
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
            line = re.sub(r'[!#].*', '', line).strip()
            if not line:
                continue
            m = re.match(r'(\w+)\s*[=:]\s*(.*)', line)
            if m:
                key = m.group(1).strip()
                val = m.group(2).strip()
                params[key] = val
    return params


# ---------------------------------------------------------------------------
# Value coercion
# ---------------------------------------------------------------------------

_BOOL_TRUE  = {'T', 'TRUE', 'YES'}
_BOOL_FALSE = {'F', 'FALSE', 'NO'}

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
    'U':         'U',
    'V':         'V',
    'ETA':       'ETA',
    'ETAscreen': 'ETAscreen',
    'Hmax':      'Hmax',
    'Hmin':      'Hmin',
    'Umax':      'Umax',
    'MFmax':     'MFmax',
    'VORmax':    'VORmax',
    'MASK':      'MASK',
    'MASK9':     'MASK9',
    'Umean':     'Umean',
    'Vmean':     'Vmean',
    'ETAmean':   'ETAmean',
    'WaveHeight':'WaveHeight',
    'SXL':       'SXL',
    'SXR':       'SXR',
    'SYL':       'SYL',
    'SYR':       'SYR',
    'SourceX':   'SourceX',
    'SourceY':   'SourceY',
    'FrcX':      'FrcX',
    'FrcY':      'FrcY',
    'BrkdisX':   'BrkdisX',
    'BrkdisY':   'BrkdisY',
    'P':         'P',
    'Q':         'Q',
    'Fx':        'Fx',
    'Fy':        'Fy',
    'Gx':        'Gx',
    'Gy':        'Gy',
    'AGE':       'AGE',
    'ROLLER':    'ROLLER',   # also a breaking physics flag — see convert()
    'UNDERTOW':  'UNDERTOW',
    'OUT_NU':    'NU',       # renamed: OUT_NU → NU
    'TMP':       'TMP',
    'Radiation': 'Radiation',
}

# ---------------------------------------------------------------------------
# Wavemaker parameter sets per type
# ---------------------------------------------------------------------------

_WK_PARAMS: dict[str, list[str]] = {
    'LEF_SOL': ['AMP_SOLI', 'DEP_SOLI', 'LAG_SOLI'],
    'INI_SOL': ['AMP_SOLI', 'DEP_SOLI', 'Xwavemaker'],
    'INI_REC': ['Xc', 'Yc', 'WID', 'AMP'],
    'INI_Gau': ['AMP', 'Xc', 'Yc', 'WID', 'GauRadius'],
    'WK_REG': [
        'Xc_WK', 'Yc_WK', 'DEP_WK', 'Tperiod', 'AMP_WK',
        'Theta_WK', 'Time_ramp', 'Delta_WK', 'Ywidth_WK',
    ],
    'WK_IRR': [
        'Xc_WK', 'Yc_WK', 'DEP_WK', 'FreqPeak', 'FreqMin', 'FreqMax',
        'Hmo', 'GammaTMA', 'ThetaPeak', 'Sigma_Theta', 'Nfreq', 'Ntheta',
        'Time_ramp', 'Delta_WK', 'Ywidth_WK',
    ],
    'WK_TIME_SERIES': [
        'NumWaveComp', 'PeakPeriod', 'WaveCompFile',
        'Xc_WK', 'Yc_WK', 'DEP_WK', 'Time_ramp', 'Delta_WK', 'Ywidth_WK',
    ],
    'WK_NEW_IRR': [
        'Xc_WK', 'Yc_WK', 'DEP_WK', 'FreqPeak', 'FreqMin', 'FreqMax',
        'Hmo', 'GammaTMA', 'ThetaPeak', 'Sigma_Theta',
        'Time_ramp', 'Delta_WK', 'Ywidth_WK', 'WaveCompFile',
    ],
    'LEFT_BC_IRR': [
        'NumWaveComp', 'PeakPeriod', 'WaveCompFile', 'DEP_WK',
        'Time_ramp', 'Delta_WK',
    ],
    'ABS_1D': [
        'NumWaveComp', 'PeakPeriod', 'WaveCompFile', 'DEP_WK',
        'Time_ramp', 'Delta_WK',
    ],
    'JON_2D': [
        'Xc_WK', 'Yc_WK', 'DEP_WK', 'FreqPeak', 'FreqMin', 'FreqMax',
        'Hmo', 'GammaTMA', 'ThetaPeak', 'Sigma_Theta', 'Nfreq', 'Ntheta',
        'Time_ramp', 'Delta_WK', 'Ywidth_WK',
    ],
    'JON_1D': [
        'Xc_WK', 'Yc_WK', 'DEP_WK', 'FreqPeak', 'FreqMin', 'FreqMax',
        'Hmo', 'GammaTMA', 'Nfreq', 'Time_ramp', 'Delta_WK',
    ],
    'TMA_1D': [
        'Xc_WK', 'Yc_WK', 'DEP_WK', 'FreqPeak', 'FreqMin', 'FreqMax',
        'Hmo', 'GammaTMA', 'Nfreq', 'Time_ramp', 'Delta_WK',
    ],
}

# Common optional wavemaker keys not in any type-specific list
_WK_COMMON = [
    'WaveMakerCurrentBalance', 'WaveMakerCd',
    'WAVEMAKER_Cbrk',
]

# ---------------------------------------------------------------------------
# Main converter
# ---------------------------------------------------------------------------

def convert(params: dict[str, str]) -> tuple[dict, list[str]]:
    """
    Build the nested YAML dict from flat params.
    Returns (yaml_dict, unknown_keys).
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

    dx = pop_val('DX'); dy = pop_val('DY')
    if dx is not None and dy is not None:
        geo['cell_size'] = [dx, dy]
    elif dx is not None:
        geo['cell_size'] = [dx, dx]
    elif dy is not None:
        geo['cell_size'] = [dy, dy]

    depth_type = pop_str('DEPTH_TYPE', 'flat').lower()
    if depth_type == 'data':
        depth_type = 'file'

    mg = pop_val('Mglob'); ng = pop_val('Nglob')
    if depth_type in ('flat', 'slope'):
        if mg is not None and ng is not None:
            geo['grid_size'] = [mg, ng]

    px = pop_val('PX');  py = pop_val('PY')
    if px is not None or py is not None:
        decomp: dict = {}
        if px is not None: decomp['nx_proc'] = px
        if py is not None: decomp['ny_proc'] = py
        geo['decomposition'] = decomp

    bathy: dict = {'type': depth_type}
    if depth_type == 'file':
        df2 = pop_str('DEPTH_FILE')
        if df2 is not None: bathy['file'] = df2
        ftype = pop_str('DEPTH_FTYPE')
        if ftype is not None: bathy['file_type'] = ftype.lower()
        bc = pop_bool('BATHY_CORRECTION')
        if 'BATHY_CORRECTION' in params: bathy['correction'] = bc
        if mg is not None: bathy['nx'] = mg
        if ng is not None: bathy['ny'] = ng
    else:
        pop('DEPTH_FILE'); pop('DEPTH_FTYPE'); pop('BATHY_CORRECTION')
        df = pop_val('DEPTH_FLAT')
        if df is not None: bathy['depth'] = df
        if depth_type == 'slope':
            slp = pop_val('SLP')
            if slp is not None: bathy['slope'] = slp
            xslp = pop_val('Xslp')
            if xslp is not None: bathy['x0'] = xslp
        else:
            pop('SLP'); pop('Xslp')

    geo['bathymetry'] = bathy
    out['geometry'] = geo

    # ---- simulation --------------------------------------------------------
    sim: dict = {}
    title = pop_str('TITLE')
    if title is not None: sim['title'] = title
    tt = pop_val('TOTAL_TIME')
    if tt is not None: sim['total_time'] = tt
    ts = pop_val('PLOT_START_TIME')
    if ts is not None: sim['t_start'] = ts
    pi = pop_val('PLOT_INTV')
    if pi is not None: sim['output_interval'] = pi
    si = pop_val('SCREEN_INTV')
    if si is not None: sim['screen_interval'] = si
    pis = pop_val('PLOT_INTV_STATION')
    if pis is not None: sim['plot_intv_station'] = pis
    sob = pop_val('StationOutputBuffer')
    if sob is not None: sim['station_output_buffer'] = sob

    fixed_dt = pop_bool('FIXED_DT')
    dt = pop_val('DT')
    if fixed_dt or dt is not None:
        ts_block: dict = {'fixed_dt': fixed_dt}
        if dt is not None: ts_block['dt'] = dt
        sim['time_stepping'] = ts_block
    else:
        pop('FIXED_DT'); pop('DT')  # mark consumed even if absent

    out['simulation'] = sim

    # ---- hot_start ---------------------------------------------------------
    ini = pop_bool('INI_UVZ')
    if ini:
        hs: dict = {}
        hs['eta_file']   = pop_str('ETA_FILE',  '')
        hs['u_file']     = pop_str('U_FILE',    '')
        hs['v_file']     = pop_str('V_FILE',    '')
        mf = pop_str('MASK_FILE')
        if mf: hs['mask_file'] = mf
        hst = pop_val('HotStartTime')
        if hst is not None: hs['time'] = hst
        bd = pop_bool('BED_DEFORMATION')
        if 'BED_DEFORMATION' in params: hs['bed_deformation'] = bd
        rn = pop_val('HOT_START_RES_NUM')
        if rn is not None: hs['output_start_number'] = rn
        out['hot_start'] = hs
    else:
        for k in ('ETA_FILE', 'U_FILE', 'V_FILE', 'MASK_FILE',
                  'HotStartTime', 'BED_DEFORMATION', 'HOT_START_RES_NUM'):
            pop(k)

    # ---- wavemaker ---------------------------------------------------------
    wm_type = pop_str('WAVEMAKER', 'NONE')
    if wm_type.upper() != 'NONE':
        wm: dict = {'type': wm_type}
        wm_keys = _WK_PARAMS.get(wm_type, []) + _WK_COMMON
        for k in wm_keys:
            v = pop_val(k)
            if v is not None:
                wm[k] = v
        # Apply legacy defaults that differ from the model-layer defaults.
        # Legacy default for all spectral types is Nfreq=45.
        # Ntheta=1 for 1D types, 24 for 2D types.
        if wm_type in ('TMA_1D', 'JON_1D'):
            wm.setdefault('Ntheta', 1)
            wm.setdefault('Nfreq', 45)
        elif wm_type in ('WK_IRR', 'JON_2D', 'WK_NEW_IRR'):
            wm.setdefault('Ntheta', 24)
            wm.setdefault('Nfreq', 45)
        out['wavemaker'] = wm

    # ---- sponge ------------------------------------------------------------
    ds = pop_bool('DIFFUSION_SPONGE')
    di = pop_bool('DIRECT_SPONGE')
    fs = pop_bool('FRICTION_SPONGE')
    any_sponge = ds or di or fs or any(
        params.get(k, '0') not in ('0', '0.0', 'F', 'FALSE', 'NO')
        for k in ('Sponge_west_width', 'Sponge_east_width',
                  'Sponge_south_width', 'Sponge_north_width')
    )
    if any_sponge:
        sp: dict = {
            'diffusion_sponge': ds,
            'direct_sponge':    di,
            'friction_sponge':  fs,
        }
        for k, yk in (('Csp', 'Csp'), ('CDsponge', 'CDsponge'),
                      ('Sponge_west_width',  'Sponge_west_width'),
                      ('Sponge_east_width',  'Sponge_east_width'),
                      ('Sponge_south_width', 'Sponge_south_width'),
                      ('Sponge_north_width', 'Sponge_north_width'),
                      ('R_sponge', 'R_sponge'), ('A_sponge', 'A_sponge')):
            v = pop_val(k)
            if v is not None: sp[yk] = v
        out['sponge'] = sp
    else:
        for k in ('Csp', 'CDsponge', 'Sponge_west_width', 'Sponge_east_width',
                  'Sponge_south_width', 'Sponge_north_width',
                  'R_sponge', 'A_sponge'):
            pop(k)

    # ---- obstacle / breakwater ---------------------------------------------
    obs = pop_bool('OBSTACLE')
    bw  = pop_bool('BREAKWATER')
    if obs or bw:
        ob: dict = {}
        of = pop_str('OBSTACLE_FILE')
        bf = pop_str('BREAKWATER_FILE')
        ba = pop_val('BreakWaterAbsorbCoef')
        if of: ob['obstacle_file'] = of
        if bf: ob['breakwater_file'] = bf
        if ba is not None: ob['BreakWaterAbsorbCoef'] = ba
        out['obstacle'] = ob
    else:
        for k in ('OBSTACLE_FILE', 'BREAKWATER_FILE', 'BreakWaterAbsorbCoef'):
            pop(k)

    # ---- friction ----------------------------------------------------------
    in_cd = pop_bool('IN_Cd')
    cd    = pop_val('Cd')
    cd_file = pop_str('CD_FILE')
    if in_cd or cd not in (None, 0, 0.0) or cd_file:
        fr: dict = {}
        if in_cd:               fr['friction_matrix'] = True
        if cd   is not None:    fr['Cd_fixed'] = cd
        if cd_file:             fr['cd_file']  = cd_file
        out['friction'] = fr

    # ---- physics -----------------------------------------------------------
    ph: dict = {}
    wl = pop_val('WATER_LEVEL')
    if wl is not None: ph['water_level'] = wl
    if pop_bool('PERIODIC'): ph['periodic'] = True
    disp = pop_bool('DISPERSION', True)
    if not disp: ph['dispersion'] = False
    for k, yk in (('Gamma1', 'Gamma1'), ('Gamma2', 'Gamma2'),
                  ('Beta_ref', 'Beta_ref'), ('Gamma3', 'Gamma3'),
                  ('SWE_ETA_DEP', 'SWE_ETA_DEP'), ('C_smg', 'C_smg')):
        v = pop_val(k)
        if v is not None: ph[yk] = v
    vb = pop_bool('VISCOSITY_BREAKING', True)
    if not vb: ph['viscosity_breaking'] = False
    if ph:
        out['physics'] = ph
    else:
        pop('DISPERSION'); pop('VISCOSITY_BREAKING')

    # ---- numerics ----------------------------------------------------------
    nu: dict = {}
    for k, yk in (('Time_Scheme', 'Time_Scheme'),
                  ('CONSTRUCTION', 'CONSTRUCTION'),
                  ('HIGH_ORDER', 'HIGH_ORDER')):
        v = pop_str(k)
        if v is not None: nu[yk] = v
    for k, yk in (('CFL', 'CFL'), ('FroudeCap', 'FroudeCap'),
                  ('MinDepth', 'MinDepth'), ('MinDepthFrc', 'MinDepthFrc')):
        v = pop_val(k)
        if v is not None: nu[yk] = v
    ot = pop_bool('OUT_Time')
    if ot: nu['OUT_Time'] = True
    atm = pop_val('ArrTimeMin')
    if atm is not None: nu['ArrTimeMinH'] = atm
    if nu:
        out['numerics'] = nu

    # ---- breaking ----------------------------------------------------------
    br: dict = {}
    roller = pop_bool('ROLLER')
    if roller: br['roller'] = True
    sb = pop_bool('SHOW_BREAKING', True)
    if not sb: br['show_breaking'] = False
    for k, yk in (('Cbrk1', 'Cbrk1'), ('Cbrk2', 'Cbrk2'),
                  ('WAVEMAKER_Cbrk', 'WAVEMAKER_Cbrk'),
                  ('visbrk', 'visbrk'), ('WAVEMAKER_visbrk', 'WAVEMAKER_visbrk'),
                  ('nu_bkg', 'nu_bkg')):
        v = pop_val(k)
        if v is not None: br[yk] = v
    wvis = pop_bool('WAVEMAKER_VIS')
    if wvis: br['WAVEMAKER_VIS'] = True
    if br:
        out['breaking'] = br

    # ---- output ------------------------------------------------------------
    op: dict = {}
    rf = pop_str('RESULT_FOLDER')
    if rf: op['result_folder'] = rf
    fio = pop_str('FIELD_IO_TYPE')
    if fio: op['field_io_type'] = fio
    ns = pop_val('NumberStations')
    if ns is not None: op['number_stations'] = ns
    sf = pop_str('STATIONS_FILE')
    if sf: op['stations_file'] = sf
    ores = pop_val('OUTPUT_RES')
    if ores is not None: op['output_res'] = ores
    ebv = pop_val('EtaBlowVal')
    if ebv is not None: op['EtaBlowVal'] = ebv
    ti = pop_val('T_INTV_mean')
    if ti is not None: op['T_INTV_mean'] = ti
    st = pop_val('STEADY_TIME')
    if st is not None: op['STEADY_TIME'] = st

    # depth_out — static field, separate from variables list
    depth_out = pop_bool('DEPTH_OUT')
    if depth_out:
        op['depth_out'] = True

    # OUT_* flags → variables list
    variables: list[str] = []
    for old_key, new_name in _VAR_FLAGS.items():
        v = pop(old_key)
        if v is not None and _bool(v):
            # ROLLER also drives breaking.roller (already consumed above);
            # here we only add to variables if the output flag is true.
            variables.append(new_name)
    if variables:
        op['variables'] = variables

    if op:
        out['output'] = op

    # ---- coupling ----------------------------------------------------------
    cf = pop_str('COUPLING_FILE')
    if cf:
        out['coupling'] = {'coupling_file': cf}

    # ---- collect unknown keys ----------------------------------------------
    for k in params:
        if k not in consumed:
            unknown.append(f'{k} = {params[k]}')

    return out, unknown


# ---------------------------------------------------------------------------
# YAML serialiser (no external ruamel dependency — hand-rolled for readability)
# ---------------------------------------------------------------------------

def _yaml_value(v) -> str:
    if isinstance(v, bool):
        return 'true' if v else 'false'
    if isinstance(v, list):
        return '[' + ', '.join(str(i) for i in v) + ']'
    if isinstance(v, str) and (' ' in v or ':' in v or not v):
        return f'"{v}"'
    return str(v)

def _dump_yaml(d: dict, indent: int = 0) -> list[str]:
    lines: list[str] = []
    pad = '  ' * indent
    for k, v in d.items():
        if isinstance(v, dict):
            lines.append(f'{pad}{k}:')
            lines.extend(_dump_yaml(v, indent + 1))
        else:
            lines.append(f'{pad}{k}: {_yaml_value(v)}')
    return lines


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description='Convert a legacy FUNWAVE input.txt to the new YAML format.')
    parser.add_argument('input',  type=Path, help='Path to legacy input.txt')
    parser.add_argument('output', type=Path, nargs='?', help='Output YAML path (default: stdout)')
    args = parser.parse_args()

    params = parse_input_txt(args.input)
    yaml_dict, unknown = convert(params)

    header_lines: list[str] = [
        '# FUNWAVE-TVD input — converted from legacy input.txt',
        f'# Source: {args.input}',
        '#',
    ]
    if unknown:
        header_lines += [
            '# WARNING: the following keys were not recognised and have been dropped.',
            '# Review and add them manually if needed:',
        ] + [f'#   {u}' for u in unknown]
    else:
        header_lines.append('# No unrecognised keys.')
    header_lines.append('')

    body = '\n'.join(header_lines + _dump_yaml(yaml_dict)) + '\n'

    if args.output:
        args.output.write_text(body)
        print(f'Written to {args.output}', file=sys.stderr)
        if unknown:
            print(f'WARNING: {len(unknown)} unrecognised key(s) — see comment block in output.',
                  file=sys.stderr)
    else:
        sys.stdout.write(body)


if __name__ == '__main__':
    main()
