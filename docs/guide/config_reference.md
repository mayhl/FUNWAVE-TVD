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

## `wavemaker:`

Wave generation: a spectrum (shape x discretization) feeding a Wei-Kirby internal source box or a boundary signal.  spectrum.type discriminates the shape; source:/limiter: presence enables the source box / eta limiter. Entry is a mapping or a 1-element sequence (multi-wavemaker pending).  Keys carry their deck sub-path; `variant:` tags which spectrum.type a shape key belongs to (untagged keys apply regardless, e.g. source/limiter).

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `name` | — | — | — | Name (referenced by boundaries.\<face>.forcing.wavemaker). |
| `spectrum.type` | — | `WAVEMAKER` | — | Spectrum shape discriminant. One of `regular` \| `cnoidal` \| `jonswap` \| `tma` \| `spectrum_2d` \| `components`. |
| `spectrum.amplitude` | `0.0` | `AMP_WK` | m | *(regular/cnoidal)* Monochromatic wave amplitude (cnoidal: H/2; the profile is expanded into its harmonics). |
| `spectrum.period` | `0.0` | `Tperiod` | s | *(regular/cnoidal)* Monochromatic wave period. |
| `spectrum.direction` | `0.0` | `Theta_WK` | deg | *(regular/cnoidal)* Monochromatic wave direction (cnoidal: 0 only). |
| `spectrum.hm0` | `0.0` | `Hmo` | m | *(jonswap/tma)* Significant wave height (total eta). |
| `spectrum.gamma` | `3.3` | `GammaTMA` | — | *(jonswap/tma)* Peak enhancement factor. |
| `spectrum.freq.peak` | `0.0` | `FreqPeak` | Hz | *(jonswap/tma)* Peak frequency (exact legacy path). |
| `spectrum.freq.min` | `0.0` | `FreqMin` | Hz | *(jonswap/tma)* Minimum frequency. |
| `spectrum.freq.max` | `0.0` | `FreqMax` | Hz | *(jonswap/tma)* Maximum frequency. |
| `spectrum.period.peak` | — | — | s | *(jonswap/tma)* Hand-authoring alt to freq.peak (reciprocal; mutually exclusive). |
| `spectrum.period.min` | — | — | s | *(jonswap/tma)* Alt to freq.max (note the min\<->max swap). |
| `spectrum.period.max` | — | — | s | *(jonswap/tma)* Alt to freq.min. |
| `spectrum.directional.peak` | `0.0` | `ThetaPeak` | deg | *(jonswap/tma)* Mean direction (presence => 2D spreading). |
| `spectrum.directional.spread` | — | `Sigma_Theta` | deg | *(jonswap/tma)* Directional spread (required -- the block's presence means spreading is wanted). |
| `spectrum.file` | — | `WaveCompFile` | — | *(components/spectrum_2d)* Wave-component / 2D-spectrum data file. |
| `spectrum.locations` | — | — | — | *(spectrum_2d)* Manifest of along-face anchor spectra (`\<coord> file` per line; coordinate all-or-none, omitted => equispaced) for a spatially-varying boundary feed; mutually exclusive with spectrum.file. |
| `spectrum.convention` | `local` | — | — | *(jonswap/tma/spectrum_2d)* Boundary-feed direction frame: local (theta=0 is the fed face inward normal), cartesian (theta=0 is grid +x), nautical (azimuth CW from true North via grid.crs; pending). One of `local` \| `cartesian` \| `nautical`. |
| `spectrum.n` | `1` | `NumWaveComp` | — | *(components)* Number of wave components. |
| `spectrum.period_peak` | `0.0` | `PeakPeriod` | s | *(components)* Peak period (components store a period here). |
| `spectrum.format` | `DATA_1D` | `WAVE_DATA_TYPE` | — | *(spectrum_2d)* 2D-spectrum data format. |
| `source.x_center` | `0.0` | `Xc_WK` | m | Source-box x center (presence of source: => Wei-Kirby). |
| `source.y_center` | `0.0` | `Yc_WK` | m | Source-box y center. |
| `source.depth` | `0.0` | `DEP_WK` | m | Source-box reference depth; absent = the bed mean under the box, refused unless the bed there is flat within 1 %. |
| `source.y_width` | `999999.0` | `Ywidth_WK` | m | Source-box alongshore width (large = full span). |
| `source.time_ramp` | `0.0` | `Time_ramp` | s | Source ramp-up time. |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `seed` | `66` | — | — | Phase-RNG seed for the random spectral realization (reproducible + restart-coherent; entries offset by index). |
| `zero_phase` | `false` | — | — | Zero every component phase instead of the seeded draw (parity/regression builds). |
| `spectrum.normalize` | `band` | — | — | *(jonswap/tma)* Hm0 normalization: band renormalizes the truncated [min, max] band to carry the full Hm0 (legacy); total keeps the band's natural share of the full-spectrum integral. One of `band` \| `total`. |
| `spectrum.discretization.freq_bins` | `45` | `Nfreq` | — | *(jonswap/tma)* Frequency bin count. |
| `spectrum.discretization.theta_bins` | `24` | `Ntheta` | — | *(jonswap/tma)* Directional bin count (requires a directional: block; 1D runs force 1). |
| `spectrum.discretization.equal_energy` | `none` | `EqualEnergy` | — | *(jonswap/tma)* Equal-energy binning per axis; the legacy boolean spellings still read as none/freq. One of `none` \| `freq` \| `dir` \| `both`. |
| `spectrum.discretization.method` | `grid` | — | — | *(jonswap/tma)* Discretization method: directional grid, or one direction per frequency component (nee WK_NEW_*). One of `grid` \| `single_dir_per_freq`. |
| `spectrum.discretization.coherence_percent` | `0.0` | — | % | *(jonswap/tma/spectrum_2d)* Directional-phase coherence: percent by which a frequency's directions share a phase (0 = independent, realistic sea; 100 = the legacy fully-coherent collapse). |
| `spectrum.discretization.group_coherence` | `0.0` | `alpha_c` | % | *(jonswap/tma)* Salatin frequency-grouping coherence: percent of components sharing a frequency (single_dir_per_freq only). |
| `source.delta` | `0.5` | `Delta_WK` | — | Source-box width parameter. |
| `source.current_cd` | — | `WaveMakerCd` | — | Current-balance drag (presence enables; nee WaveMakerCurrentBalance). |
| `source.breaking.cbrk` | `1.0` | `WAVEMAKER_Cbrk` | — | Zone breaking-onset coefficient (Cbrk family vs sqrt(gH), not a scale on cbrk1). |
| `source.breaking.visbrk` | `0.0` | `WAVEMAKER_visbrk` | m2 s-1 | Zone breaking-viscosity coefficient. |
| `limiter.crest` | — | `CrestLimit` | m | Crest elevation limit (nee ETA_LIMITER). Read and stored; the clamp itself is not ported, so the key has no effect yet. |
| `limiter.trough` | — | `TroughLimit` | m | Trough elevation limit; same standing as limiter.crest. |

## `boundaries:`

Per-face boundary conditions.  The reader DERIVES each face's BC from which blocks are present (nothing -> wall; sponge only -> wall + absorbing strip; sponge.direct + forcing -> relaxation to a signal; forcing without direct -> characteristic, pending).  type: is an optional assertion.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `periodic` | — | `PERIODIC` | — | Axis-level periodic pair list, x and/or y (e.g. [y] or [x, y]). |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `sponge.direct.r` | — | — | — | Shared direct-sponge damping ratio all face sponges inherit. |
| `sponge.direct.a` | — | — | — | Shared direct-sponge damping exponent. |
| `sponge.friction.cd` | — | — | — | Shared friction-sponge drag coefficient. |
| `sponge.diffusion.nu` | — | — | m2 s-1 | Shared diffusion-sponge viscosity. |

### Per-face keys (`west:` / `east:` / `south:` / `north:`)

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `<face>.type` | — | — | — | Optional BC assertion; errors at init if it disagrees with the derived BC. |
| `<face>.sponge.width` | `0.0` | `Sponge_<face>_width` | m | Absorbing sponge strip width (0 = no sponge). On a forced or wavemaker-fed face this also sizes the relaxation strip (cells = width/dx at init). |
| `<face>.sponge.pml.width` | `0.0` | — | m | PML sub-strip width measured inward from the face; 0 = half the face sponge width (the hybrid default). Confining the PML (and its NSWE zone) to the outer sub-strip leaves an inner Boussinesq-live pre-strip where the friction taper absorbs the dispersive band before it reaches the SWE interface; set to the face width for a full-strip PML. |
| `<face>.forcing.eta` | — | `Tide<Face>_ETA` | m | Prescribed surface-elevation forcing (constant; file for a series). |
| `<face>.forcing.u` | — | `Tide<Face>_U` | m/s | Prescribed x-velocity forcing. |
| `<face>.forcing.v` | — | `Tide<Face>_V` | m/s | Prescribed y-velocity forcing. |
| `<face>.forcing.file` | — | `Tide<Face>FileName` | — | Time-series forcing file (presence => DATA forcing). |
| `<face>.forcing.wavemaker` | — | — | — | Name of a spectrum-only wavemaker entry driving this face (nee ABS). |
| `<face>.forcing.depth` | — | — | m | Reference depth for the wavemaker forcing series (nee DepthWaveMaker); absent = the bed mean along the fed face, refused unless flat within 1 %.  (west: legacy `DepthWaveMaker`) |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `<face>.sponge.direct.r` | `0.85` | `R_sponge` | — | Direct-sponge damping ratio. |
| `<face>.sponge.direct.a` | `5.0` | `A_sponge` | — | Direct-sponge damping exponent. |
| `<face>.sponge.friction.cd` | `0.0` | `CDsponge` | — | Sponge bottom-friction coefficient. |
| `<face>.sponge.diffusion.nu` | `0.1` | `Csp` | — | Sponge diffusion coefficient. |
| `<face>.sponge.pml.r_target` | `0.001` | — | — | PML target reflection coefficient; sets sigma_max = 3c/(2W) ln(1/R). North/south faces only. |
| `<face>.sponge.pml.h_gate` | `2.0` | — | m | PML depth gate; sigma tapers smoothly to 0 below this depth so the strip hands off to the beach. |
| `<face>.sponge.pml.cd` | `10.0` | — | — | Friction drag auto-enabled over the face strip when the PML is on — the hybrid absorber's inner pre-strip; eq2d showed the bare PML recirculates the kh > 1 band off its SWE interface. Ignored when the face configures friction explicitly; 0 = pure PML. |
| `<face>.forcing.disp_ramp` | — | — | m | Dispersion taper from a characteristic (Flather) face: the dispersive terms are off at the face and smoothstep back to full strength over this distance, so the face solves the shallow-water equations the Flather condition is derived for. Absent = 5 x the deepest still water along the face (logged at init); 0 = off. With the dispersion on at the face its closure sends a third of a wave group's amplitude back (Kr 35 % at kh ~ 1; 0.5 % at 5 depths, 0.3 % at 10), and a strong sustained flow grows a two-cell mode there that no ghost content or strip damps (2 m on a bore flume removes it). A face whose depth varies strongly gets the deep end's length everywhere; set the key to override. Refused on a wavemaker-fed face (its short waves need the dispersion) and on a relaxation face. |

## `numerics:`

Numerical scheme — CFL, Riemann solver, reconstruction, wet/dry floor.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `cfl` | `0.5` | `CFL` | — | CFL number for the adaptive timestep. Exclusive with dt: setting both is a config error. |
| `froude_cap` | `3.0` | `FroudeCap` | — | Velocity cap: \|U\| is scaled down where \|U\|/sqrt(g*max(h, min_depth)) exceeds this. The floor on h means the true Froude number is unbounded as the column dries; this caps speed, not Fr. |
| `min_depth` | `0.1` | `MinDepth` | m | Floor on the water column wherever it divides or scales: the Froude cap, friction, the SWE gate ratio, the dispersion taper and sediment concentration (legacy folded MinDepth/MinDepthFrc). Not the wet/dry threshold, which is h > 0. |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `dt` | — | `DT_fixed` | s | Presence selects a fixed timestep (nee simulation.time_stepping); the default cfl still caps it by halving. Absent = adaptive CFL stepping. |
| `flux_solver` | `hllc` | `CONSTRUCTION` | — | Approximate Riemann solver. One of `hllc` \| `hll`. |
| `tridiag.chunk` | `48` | — | — | Transverse chunk width of the pipelined tridiagonal sweeps. System-tuned; bitwise-neutral (reference sweep flat over 16-64). |
| `tridiag.transpose_min_py` | `40` | — | — | Minimum y-rank count that switches the distributed y solve to the all-to-all transpose. System-tuned latency/bandwidth crossover; bitwise-neutral. |
| `reconstruction` | `fourth` | `HIGH_ORDER` | — | Spatial reconstruction scheme. One of `fourth` \| `fminmod` \| `weno` \| `mlp` \| `basic`. |

## `grid:`

**Required.**  Computational grid — extent, spacing, bathymetry source, MPI decomposition, optional Coriolis + georeferencing.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `coriolis.f` | — | — | s-1 | Constant Coriolis parameter (f-plane; overrides latitude). |
| `coriolis.latitude` | — | — | degrees_north | Reference latitude for the f-plane Coriolis parameter. |
| `cell_size` | — | — | m | Uniform grid spacing [dx, dy] (nee DX/DY); alternative to dx_file/dy_file. |
| `dx_file` | — | — | — | Variable x-spacing file (with dy_file; alternative to cell_size). Not yet implemented in the new path. |
| `dy_file` | — | — | — | Variable y-spacing file (with dx_file). |
| `n_cells` | — | — | — | Global domain size [nx, ny] (nee Mglob/Nglob; grid_size; bathymetry.nx/ny). Required for flat/slope; for file bathymetry absent = inferred from the file (ASCII dimension scan) and present = an origin-anchored subset window, fit-checked under --validate. |
| `origin` | — | — | m | Local coordinates [x0, y0] of the cell (1,1) centre; default [0, 0]. |
| `bathymetry.type` | `file` | `DEPTH_TYPE` | — | Bathymetry source. One of `flat` \| `file` \| `slope`. |
| `bathymetry.depth` | — | `DEPTH_FLAT` | m | Still-water depth (flat and slope types). |
| `bathymetry.slope` | — | `SLP` | — | Bed slope for a sloping-beach bathymetry. |
| `bathymetry.x0` | `0.0` | `Xslp` | m | x location where the slope begins. |
| `bathymetry.file` | — | `DEPTH_FILE` | — | Bathymetry data file (file type). |
| `bathymetry.file_type` | `ascii` | — | — | Bathymetry file format. One of `ascii`. |
| `bathymetry.correction` | `false` | `BATHY_CORRECTION` | — | Apply the bathymetry smoothing/correction pass (file type). |
| `bathymetry.smooth_below_depth` | — | — | m | Correction-pass smoothing floor; absent = off (-LARGE sentinel). |
| `crs.epsg` | — | — | — | EPSG code of the projected horizontal CRS in metres; presence georeferences the grid (absent = local unreferenced). |
| `crs.origin_x` | `0.0` | — | m | Projected easting of the cell (1,1) centre. |
| `crs.origin_y` | `0.0` | — | m | Projected northing of the cell (1,1) centre. |
| `crs.rotation` | `0.0` | — | deg | Grid +x axis angle, CCW from projected east (math convention, NOT compass azimuth); 0 = axis-aligned. |
| `crs.vertical_datum` | — | — | — | Vertical datum name that depths/eta reference (e.g. NAVD88, IGLD85 LWD); output-stamp provenance only. |
| `water_level` | `0.0` | `WaterLevel` | m | Still-water level above the bathymetry datum; added to depth and wavemaker reference depths at init, so output eta is referenced to this level. Survives hotstart (reference-frame property, not an IC). |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `n_procs` | — | — | — | MPI decomposition [px, py] (nee PX/PY; decomposition.nx_proc/ny_proc); absent = auto-decompose. |
| `bathymetry.slope_cap` | `1.0` | — | — | Correction-pass maximum bed slope. |

## `simulation:`

**Required.**  Run control — title, total/start time, and screen-log cadence. time_stepping is retired: numerics.dt presence selects a fixed step.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `screen_interval` | `1.0` | `SCREEN_INTV` | s | Screen-log / monitor cadence. |
| `t_start` | `0.0` | `PLOT_START_TIME` | s | Simulation time at which output begins. |
| `spinup` | `0.0` | — | s | Spin-up duration. The meteo crest envelope does not accumulate before it, and channels may write t_start: spinup to inherit it rather than restating the offset per channel -- so a wavemaker ramp cannot set a maximum that is then reported as a storm peak. 0 = from t=0 (legacy). |
| `title` | — | `TITLE` | — | Run title (reserved for NetCDF global attrs). |
| `total_time` | — | `TOTAL_TIME` | s | Total simulated duration. |

## `initial:`

Initial condition — block presence selects the type (solitary/sine_mode/fields/hump/n_wave). Still-water level lives in grid.water_level.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `fields.eta` | — | — | — | Initial surface-elevation field ref (path, or container#/group/var once NetCDF input lands). |
| `fields.format` | — | — | — | Field-file format override (default from the extension). One of `ascii` \| `binary` \| `netcdf`. |
| `fields.u` | — | — | — | Initial x-velocity field ref (with fields.v; absent = still). |
| `fields.v` | — | — | — | Initial y-velocity field ref (with fields.u). |
| `hump.amplitude` | — | `AMP` | m | Hump amplitude. The hump block is pending: the engine rejects it at init rather than silently ignoring it. |
| `hump.radius` | — | — | m | Gaussian hump radius (legacy used WID for both shapes). |
| `hump.shape` | — | — | — | Hump shape. One of `rect` \| `gaussian` \| `dipole`. |
| `hump.width` | — | `WID` | m | Hump width (rect half-width). |
| `hump.x_center` | — | `Xc` | m | Hump center x. |
| `hump.y_center` | — | `Yc` | m | Hump center y. |
| `n_wave.a0` | — | `a0_Nwave` | m | N-wave amplitude. The n_wave block is pending: the engine rejects it at init rather than silently ignoring it. |
| `n_wave.depth` | — | `dep_Nwave` | m | N-wave still-water depth. |
| `n_wave.gamma` | — | `gamma_Nwave` | — | N-wave shape parameter. |
| `n_wave.x1` | — | `x1_Nwave` | m | N-wave leading position. |
| `n_wave.x2` | — | `x2_Nwave` | m | N-wave trailing position. |
| `sine_mode.amplitude` | `0.0` | `AMP` | m | Standing sine-mode amplitude. |
| `sine_mode.depth` | `0.0` | `DEP` | m | Sine-mode still-water depth. |
| `sine_mode.mode_x` | `1` | — | — | Sine mode number in x. |
| `sine_mode.mode_y` | `0` | — | — | Sine mode number in y. |
| `solitary.amplitude` | `0.0` | `AMP` | m | Solitary-wave amplitude. |
| `solitary.depth` | `0.0` | `DEP` | m | Solitary-wave still-water depth. |
| `solitary.direction` | `+x` | — | — | Solitary-wave propagation direction. One of `+x` \| `-x` \| `+y` \| `-y`. |
| `solitary.x_center` | `0.0` | `XWAVEMAKER` | m | Solitary-wave initial crest x. |
| `solitary.angle` | — | — | deg | Oblique crest angle from +x (presence selects the doubly-periodic tiled train; excludes direction). |
| `solitary.y_center` | `0.0` | — | m | Solitary-wave initial crest y (angle only). |

## `dispersion:`

Boussinesq dispersion — named scheme XOR an explicit atomic Gamma triple.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `scheme` | `fully_nonlinear` | — | — | Named dispersion preset for the Gamma triple; exclusive with explicit gammas. No 1:1 legacy keyword (legacy encoded this via DISPERSION + the Gamma values). One of `fully_nonlinear` \| `weakly_nonlinear` \| `linear` \| `nswe`. |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `beta_ref` | `-0.531` | `Beta_ref` | — | Reference depth level for the Boussinesq operator. |
| `slope_disp_max` | `0.0` | — | — | Bathymetry-slope dispersion gate: \|grad h\| at or above which the dispersive terms are fully off (0 = gate disabled). The Boussinesq derivation assumes a mild slope, so on a near-vertical face digitised into the bathymetry the dispersive operator is outside its own validity and goes marginally unstable; gating back toward NSWE there leaves the geometry and overtopping untouched, unlike smoothing the bathymetry. Static (bathymetry-derived), evaluated once at init. |
| `slope_disp_ramp` | `0.0` | — | — | Slope-gate smoothstep taper width in \|grad h\| units, below slope_disp_max. Required (positive) whenever slope_disp_max is set: a hard switch turns last-bit differences into O(1) residual flips at threshold cells, which is the known blow-up injector. |
| `gamma1` | — | `Gamma1` | — | Dispersion coefficient (atomic triple with gamma2/gamma3; exclusive with scheme). |
| `gamma2` | — | `Gamma2` | — | Dispersion coefficient (atomic triple). |
| `gamma3` | — | `Gamma3` | — | Dispersion coefficient (atomic triple). |

## `breaking:`

Wave breaking — dissipation model, roller, and thresholds (core physics, not presence-gated). Wavemaker-zone overrides live in wavemaker.source.breaking.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `roller` | `false` | `ROLLER` | — | Enable the surface roller (forces model eddy_viscosity |
| `swe_gate` | `false` | — | — | Apply the eta/h SWE-transition gate (mask9 zeroing + swe_eta_ramp taper) under eddy_viscosity, capping the dispersion amplitude at steep bores. Default false leaves dispersion ungated, as the legacy viscous breaker did. |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `cbrk1` | `0.45` | `Cbrk1` | — | Breaking onset threshold coefficient (legacy default 0.65; 0.45 is the 2026-09 breaking-board fit for the split solve with nu_scale 2). |
| `cbrk2` | `0.35` | `Cbrk2` | — | Breaking cessation threshold coefficient. |
| `nu_bkg` | `0.0` | `nu_bkg` | m2 s-1 | Background eddy viscosity added everywhere. |
| `nu_scale` | `2.0` | — | — | Multiplier on the breaker eddy-viscosity magnitude. Every scheme scales nu with cbrk2 x sqrt(gh) x depth, so cbrk2 carried cessation and magnitude together; nu_scale separates them (1 = legacy magnitude; 2 = the 2026-09 breaking-board knee; the wavemaker-zone term is not scaled). |
| `scheme` | `constant` | — | — | Breaker viscosity form: constant = nu fixed at cbrk2 x sqrt(gh) x depth while active (legacy); kennedy = the steepness-ramped form nu = depth x cbrk2 x sqrt(gh) x (1 + B), B ramping over [cbrk1, 2 cbrk1]; kennedy_orig = Kennedy et al. 2000 with the age-ramped onset threshold and T* = 5 sqrt(h/g); static_trans = linear cbrk1 -> cbrk2 transition over t_brk; depth_ratio = onset on eta/depth > swe_eta_dep, no age. One of `constant` \| `kennedy` \| `kennedy_orig` \| `static_trans` \| `depth_ratio`. |
| `model` | `eddy_viscosity` | `VISCOSITY_BREAKING` | — | Breaking dissipation model; eddy_viscosity is the supported closure and the only one the sediment module accepts (shock_capturing is refused with sediment). wavemaker_viscosity (nee WAVEMAKER_VIS) = shock-capturing globally + Kennedy-style viscosity inside the wavemaker zone. none = no breaker and no SWE gate, dispersion everywhere (analytic cases, debugging; refused with sediment). One of `eddy_viscosity` \| `shock_capturing` \| `wavemaker_viscosity` \| `none`. |
| `visbrk` | `0.0` | `visbrk` | m2 s-1 | Breaking eddy-viscosity coefficient (wavemaker_viscosity threshold; read only under that model). |
| `swe_eta_dep` | `0.8` | `SWE_ETA_DEP` | — | Bore-regime eta/depth threshold: the SWE dispersion gate (shock family) and the viscous breaker's extra onset criterion. |
| `swe_eta_ramp` | `0.1` | — | — | eta/depth width of the SWE-gate smoothstep below swe_eta_dep; 0 = legacy hard switch. Read only when the gate exists (model not eddy_viscosity). |
| `wetdry_disp_ramp` | `3.0` | — | — | Wet/dry-proximity dispersion taper: water-column multiples of numerics.min_depth over which disp_w smoothsteps up from 0 at the wet/dry threshold; 0 = off (legacy). Applies under every breaking model — the viscous path has no SWE gate, so swash-edge mask flips otherwise radiate through the dispersive terms. |
| `nu_cap` | `0.0` | — | — | Clamp nu_break at this fraction of the explicit-diffusion stability bound 1/(2 dt (1/dx^2 + 1/dy^2)). The Laplacian integrates with the advective-CFL dt (no viscous term in estimate_dt, matching legacy), so an uncapped breaker viscosity can exceed the stable limit several-fold at energetic breakpoints. 0 = off (legacy, unbounded); 0.5 recommended under the explicit solver (the split solvers never read it). |
| `solver` | `split_implicit` | — | — | Breaker-viscosity integrator: explicit source term (legacy), a once-per-step operator-split ADI solve on Hu/Hv, or the same solve applied to each RK stage's Euler predictor before the SSP blend (stage_split, IMEX form) — both splits unconditionally stable, nu_cap not applied. Read under eddy_viscosity only; the wavemaker_viscosity zone term stays an explicit source. One of `explicit` \| `split_implicit` \| `stage_split`. |
| `theta` | `1.0` | — | — | Implicitness weight of the split breaker-viscosity solve: 1 = backward Euler, 0.5 = Crank-Nicolson. A-stable on [0.5, 1]. Read for the split solvers only. |
| `t_brk` | `20.0` | — | s | Breaking-event age threshold (legacy hard-coded 20 s — the per-wavemaker assignments were dead code). |
| `age_per_stage` | `true` | — | — | Accrue breaker age every RK stage, so age advances three times per step as in legacy (the cbrk defaults were calibrated with it); false = once per time step. |

## `friction:`

Bottom friction — exactly one of cd, manning (n), or file; section absent = zero drag.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `cd` | — | `Cd` | — | Constant quadratic bottom-drag coefficient. |
| `manning` | — | — | s m^{-1/3} | Manning roughness n (converted to Cd per cell). |
| `file` | — | `FRICTION_FILE` | — | Spatially varying Cd map file — one row of Mglob values per global J, ascii or binary by extension. |

## `obstacle:`

Interior obstacles — obstacle mask file and breakwater absorbing block.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `file` | — | `OBSTACLE_FILE` | — | Obstacle mask file (presence enables interior walls). |
| `breakwater.file` | — | `BREAKWATER_FILE` | — | Breakwater location file. |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `breakwater.absorb_coef` | `10.0` | `BreakWaterAbsorbCoef` | — | Breakwater absorption coefficient. |

## `meteo:`

Atmospheric forcing — presence-derived sub-models (gaussian/wind/holland/slide).

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `gaussian.file` | — | `METEO_GAUSIAN_FILE` | — | Gaussian pressure-disturbance file (presence enables; forces pressure coupling). |
| `wind.file` | — | `CONSTANT_WIND_FILE` | — | Constant wind-field file (presence enables wind stress). |
| `holland.file` | — | `STORM_FILE` | — | Holland-model storm-track file. |
| `holland.air_pressure` | `false` | `AirPressure` | — | Apply the storm pressure coupling. |
| `holland.wind_force` | `false` | `WindForce` | — | Apply the storm wind-stress coupling. |
| `slide.file` | — | `SLIDE_FILE` | — | Submarine-landslide forcing file (presence enables; forces pressure coupling). |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `wind.cd` | `0.002` | `Cdw` | — | Wind drag coefficient. |
| `wind.wave_interaction` | `false` | — | — | Modulate wind stress by the wave field. |
| `wind.crest_percent` | — | `WindCrestPercent` | — | Fraction of crest exposed to wind (requires wave_interaction). |
| `holland.cd` | `0.002` | — | — | Wind drag coefficient (Holland). |
| `holland.wave_interaction` | `false` | — | — | Modulate storm wind stress by the wave field. |
| `holland.crest_percent` | — | — | — | Fraction of crest exposed to wind (requires wave_interaction). |

## `subgrid:`

Subgrid porosity (urban flooding) — depth file, ratio, porosity output.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `depth_file` | — | `DEPTH_SUBGRID_FILE` | — | High-resolution subgrid bathymetry file. |
| `write_porosity` | `false` | `Porosity` | — | Output the computed porosity field. |
| `ratio` | `1` | `SubMainGridRatio` | — | Subgrid-to-main-grid refinement ratio. |

## `foam:`

Foam / bubble model — production and decay parameters.

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `source_coef` | `0.05` | `f_source` | — | Foam production coefficient. |
| `time_scale` | `3.8` | `FoamTimeScale` | s | Foam decay time scale. |
| `burst_time_non_breaking` | `1.0` | `BurstTimeNonBreaking` | s | Foam burst time in non-breaking regions. |
| `min_thickness` | `0.01` | `MinThick` | m | Minimum foam-layer thickness tracked. |
| `cd` | `0.5` | `CdFoam` | — | Foam-layer drag coefficient. |

## `precipitation:`

Precipitation — rainfall forcing file.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `file` | — | `RAINFALL_FILE` | — | Rainfall forcing file. |

## `tracer:`

Passive tracers — the tracker table lives in the tracer file.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `file` | — | `TRACER_FILE` | — | Tracer table file (count, cadence, one row per tracker). |

## `vessel:`

Moving vessels — per-hull files in a folder; propeller and deep-draft options.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `folder` | — | `VESSEL_FOLDER` | — | Directory of per-hull vessel_NNNNN files. |
| `count` | `1` | `NumVessel` | — | Number of vessels. |
| `propeller` | `false` | — | — | Enable propeller jets (must match the build's -DPROPELLER). |
| `deep_draft.clearance` | — | `CLEARANCE` | m | Keel clearance above the bed (required for deep_draft). |
| `deep_draft.mask` | `true` | `MaskMethod` | — | Mask cells fully blocked by the hull. |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `deep_draft.cd` | — | `CdDeepDraft` | — | Hull drag coefficient (presence enables hull drag). |
| `deep_draft.nu` | — | `VisDeepDraft` | m^2/s | Hull eddy viscosity (presence enables). |

## `sediment:`

Sediment — single grain size, morphology, avalanching, cohesive, and flow-feedback sub-blocks.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `d50` | — | `D50` | m | Median grain diameter (absent: 0.5 mm sand / 5 nm mud by cohesive). |
| `specific_gravity` | `2.68` | `Sdensity` | — | Sediment specific gravity. |
| `porosity` | `0.47` | `n_porosity` | — | Bed porosity. |
| `settling_velocity` | — | `WS` | m/s | Settling velocity (absent = computed from grain size). |
| `min_depth_pickup` | `0.1` | `MinDepthPickup` | m | Minimum water depth for pickup. |
| `morph_interval` | — | `Morph_interval` | s | Morphology update interval. |
| `bed_change` | `false` | `Bed_Change` | — | Enable bed-level change (morphodynamics). |
| `bedload` | `false` | `BedLoad` | — | Include bed-load transport. |
| `hard_bottom.file` | — | `Hard_bottom_file` | — | Non-erodible hard-bottom mask file (required). |
| `avalanche.tan_phi` | — | `Tan_phi` | — | Tangent of the repose angle (required for avalanche). |
| `cohesive.soft_bed` | `true` | `SoftBed` | — | Track a consolidating soft-bed layer. |
| `cohesive.tau_cr` | — | `Tau_cr_coh` | m2/s2 | Critical erosion shear stress (required for cohesive). |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `scheme` | `upwinding` | `Sed_Scheme` | — | Advection scheme for suspended load. One of `upwinding` \| `tvd`. |
| `solver` | `split_implicit` | — | — | Suspended-load diffusion integrator: a once-per-step operator-split backward-Euler ADI solve on CH (unconditionally stable), or the explicit face flux inside the RK stages (legacy; clamped at the stability bound, which the Elder diffusivity tops under ~0.5 m cells). One of `explicit` \| `split_implicit`. |
| `shields_cr` | `0.055` | `Shields_cr` | — | Critical Shields parameter for suspension. |
| `pickup_ramp` | `0.0` | — | — | Wet/dry-proximity source taper: multiples of min_depth_pickup over which the sand pickup, the bedload flux, and the split-diffusion faces smoothstep up from zero at the cutoff; 0 = off (legacy hard switch). The wetdry_disp_ramp idiom applied to the sediment column. |
| `pickup_reduction` | `true` | `PickupReduction` | — | Cap the bed concentration c_b at reduction_parameter (pickup scaled by min(1, reduction_parameter/c_b)). |
| `reduction_parameter` | `0.65` | `ReductionParameter` | — | The c_b cap (requires pickup_reduction). |
| `c_limiter` | — | `C_limiter` | — | Suspended-concentration limiter (presence enables). |
| `shields_cr_bedload` | — | `Shields_cr_bedload` | — | Critical Shields for bed load (absent = shields_cr). |
| `morph_factor` | `1` | `Morph_factor` | — | Morphological acceleration factor. |
| `avalanche.interval` | — | `Aval_interval` | s | Avalanche relaxation interval (absent = every step). |
| `cohesive.tau_crd` | `0.001` | `Tau_crd_coh` | m2/s2 | Critical deposition shear stress. |
| `cohesive.e` | `0.0001` | `E_coh` | m/s | Erosion-rate coefficient. |
| `cohesive.alpha` | `1.0` | `alpha_coh` | — | Consolidation coefficient. |
| `cohesive.a` | `0.1` | `a_coh` | — | Soft-bed density parameter a. |
| `cohesive.b` | `2.0` | `b_coh` | — | Soft-bed density parameter b. |
| `cohesive.n` | `0.5` | `n_coh` | — | Soft-bed exponent n. |
| `cohesive.m` | `1.5` | `m_coh` | — | Soft-bed exponent m. |
| `feedback.mass_source` | `false` | `SedimentMassSource` | — | Feed erosion/deposition mass back into continuity. |
| `feedback.moment_dc` | `false` | `SedimentMomentDC` | — | Feed the density-current momentum term back. |
| `feedback.moment_exg` | `false` | `SedimentMomentEXG` | — | Feed the sediment-exchange momentum term back. |

## `coupling:`

External-model coupling — coupling data file.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `file` | — | `COUPLING_FILE` | — | Nesting/coupling boundary data file. |

## `hot_start:`

Hot-start (restart) — binary checkpoint set OR ASCII field files, restart time, output numbering.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `checkpoint` | — | — | — | Restart from a binary checkpoint directory (holds core.bin; supersedes the ASCII field/time keys). |
| `bed_deformation` | `false` | `BED_DEFORMATION` | — | Apply bed deformation on restart. |
| `eta_file` | — | `ETA_FILE` | — | Surface-elevation restart field file. |
| `mask_file` | — | `MASK_FILE` | — | Wet/dry mask restart field file. |
| `output_start_number` | `1` | — | — | First output frame number after restart. |
| `time` | `0.0` | `HotStartTime` | s | Simulation time at the restart instant. |
| `u_file` | — | `U_FILE` | — | x-velocity restart field file. |
| `v_file` | — | `V_FILE` | — | y-velocity restart field file. |

## `output:`

**Required.**  Output control — cadence, format, folder, plus means/vessel/arrival-time sub-blocks.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `checkpoint` | — | — | — | Directory to write the hot-start checkpoint set (core.bin) at run end. |
| `depth_out` | `false` | `DEPTH_OUT` | — | Write the still-water depth field. |
| `format` | `binary` | `FIELD_IO_TYPE` | — | Field output format (nee field_io_type; netcdf needs a netcdf-fortran build One of `ascii` \| `binary` \| `netcdf` \| `pnetcdf`. |
| `layout` | `chunked` | — | — | NetCDF file topology; single = one output.nc with streams as groups One of `single` \| `per_stream` \| `chunked`. |
| `max_file_size` | `50.0` | — | GB | Chunk roll-over size; also the predicted-size warning threshold for single/per_stream. |
| `result_folder` | `./output/` | `RESULT_FOLDER` | — | Directory for output files. |
| `vessel.interval` | — | `PLOT_INTV_VESSEL` | s | Vessel resistance-series output cadence. |
| `geometries.name` | — | — | — | Point-set name (referenced by channels.geometry). |
| `geometries.type` | — | — | — | Point-set kind. Station/transect values are BILINEARLY INTERPOLATED from the four surrounding cells, which has a validity consequence worth knowing: dry cells are not neutral (at init they carry eta = -depth - min_depth, and thereafter whatever the last wet step left), so a point whose stencil straddles the wet/dry line returns a contaminated value with no error. Request `mask` on the same channel to detect it -- the weights sum to 1 and mask is 0/1 per cell, so the interpolated mask is exactly the weighted wet fraction of the stencil: 1.0 means every contributing cell is wet and the value is clean, anything less means it is not. A station outside every rank's subdomain is dropped by the interpolator and reads 0.0 in every variable, mask included. Sampling `mask` at the station cadence also gives the per-station wet-step record for free. One of `station` \| `transect`. |
| `geometries.file` | — | — | — | Station coordinate file, one "x y" pair (m) per line -- exclusive with x:/y:. |
| `geometries.x` | — | — | m | Station x-coordinates (equal length with y). |
| `geometries.y` | — | — | m | Station y-coordinates. |
| `geometries.start` | — | — | m | Transect start point [x, y]. |
| `geometries.end` | — | — | m | Transect end point [x, y]. |
| `geometries.n_points` | — | — | — | Transect sample count >= 2 (absent = sampled at min(dx, dy)). |
| `channels.name` | — | — | — | Channel name (file stem \<name>_\<var>.dat). |
| `channels.geometry` | — | — | — | Name of the geometries entry to sample, or the reserved name 'field' for the whole domain (or inline the geometry keys on the channel instead). |
| `channels.variables` | — | — | — | Field-registry variable names, vector-derived names (velocity.mag, velocity.dir -- registry vectors: velocity = [u, v]; statistics on .dir are rejected, direction is circular), and product-derived names (hsig = 4.004*std(eta), the Rayleigh H_1/3 constant; its eta source accumulates hidden when not itself requested). |
| `channels.interval` | — | — | s | Channel flush cadence. |
| `channels.statistics` | — | — | — | Presence makes the channel windowed (per-interval statistics, no snapshots); absence makes it instantaneous. std is about the window mean (rms includes it); shifted moments keep single precision safe. max_time = when the window maximum was set. The event class (first_time, last_time = onset of the first/last event, duration, duration_max, count) needs threshold: and samples wet cells only; an event commits when it closes, so adjacent windows sum exactly; time values hold the fill where nothing triggered. Presets expand in place: envelope = max min max_time, arrival = first_time, inundation = first_time duration duration_max count. One of `min` \| `max` \| `mean` \| `rms` \| `std` \| `max_time` \| `first_time` \| `last_time` \| `duration` \| `duration_max` \| `count` \| `envelope` \| `arrival` \| `inundation`. |
| `channels.threshold.above` | — | — | — | Event condition: sample > value, in the variable's units; a real or a list of reals (one output per value, suffixed _\<value> with p for the point, e.g. h_duration_0p1; a single value takes no suffix). Exactly one of above/below/magnitude. |
| `channels.threshold.below` | — | — | — | Event condition: sample \< value (see above). |
| `channels.threshold.magnitude` | — | — | — | Event condition on a .mag speed variable (velocity.mag): speed > value (see above). |
| `channels.gap` | `0.0` | — | s | Event filter: the condition returning within this many seconds continues the same event (the gap is not counted as duration). 0 = off. The breaking flag flickers at the step scale as a front passes (about four fragments of a few steps per wave per cell); a gap near 0.1 T merges them into one event per wave and larger gaps change nothing. |
| `channels.min_duration` | `0.0` | — | s | Event filter: an event shorter than this is discarded at close without a trace. 0 = off. Keep it below the passage time of the feature at a cell: a breaking front holds a cell for about 0.03 T, so a value near 0.1 T discards every event. |
| `channels.accumulate` | `window` | — | — | Statistics accumulation: window resets at every interval; running accumulates since t_start and writes every interval; total accumulates since t_start and writes once at the end of the run (interval ignored). No overlapping windows: adjacent windows combine exactly in postprocessing. One of `window` \| `running` \| `total`. |
| `channels.t_start` | — | — | s | Channel start time (default simulation t_start). Also accepts the sentinel `spinup`, which resolves to simulation.spinup -- so a campaign whose statistics windows all begin after spin-up states the offset once instead of restating it per channel per run. |
| `channels.t_end` | — | — | s | Channel end time; absent = unbounded (writes to the end of the run). Bounds a high-cadence channel to part of a long record -- e.g. a 1/30 s field dump over the closing seconds while eta writes throughout -- without paying for the cadence over the whole run. |
| `channels.format` | — | — | — | Point file format; default follows the deck format (netcdf/pnetcdf selects a netcdf group in diagnostics.nc One of `ascii` \| `netcdf`. |
| `channels.precision` | `double` | — | — | Field save precision: single halves binary/netcdf storage for high-cadence channels (ascii text unchanged; double = the model working precision). Every channel writes into result_folder/\<name>/ with its own t.out frame index (frame, t, dt) and, for field channels, a grid.txt. One of `single` \| `double`. |

**Advanced**

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `blowup_threshold` | — | `EtaBlowVal` | m | Elevation above which the run aborts (absent = 100*max\|Depth\|). |

