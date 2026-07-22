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

## `wavemaker:`

Wave generation: a spectrum (shape x discretization) feeding a Wei-Kirby internal source box or a boundary signal.  spectrum.type discriminates the shape; source:/limiter: presence enables the source box / eta limiter. Entry is a mapping or a 1-element sequence (multi-wavemaker pending).  Keys carry their deck sub-path; `variant:` tags which spectrum.type a shape key belongs to (untagged keys apply regardless, e.g. source/limiter).

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `name` | — | — | — | Name (referenced by boundaries.\<face>.forcing.wavemaker). |
| `spectrum.type` | — | `WAVEMAKER` | — | Spectrum shape discriminant. One of `regular` \| `jonswap` \| `tma` \| `spectrum_2d` \| `components`. |
| `spectrum.amplitude` | `0.0` | `AMP_WK` | m | *(regular)* Monochromatic wave amplitude. |
| `spectrum.period` | `0.0` | `Tperiod` | s | *(regular)* Monochromatic wave period. |
| `spectrum.direction` | `0.0` | `Theta_WK` | deg | *(regular)* Monochromatic wave direction. |
| `spectrum.hm0` | `0.0` | `Hmo` | m | *(jonswap/tma)* Significant wave height (total eta). |
| `spectrum.gamma` | `3.3` | `GammaTMA` | — | *(jonswap/tma)* Peak enhancement factor. |
| `spectrum.normalize` | `band` | — | — | *(jonswap/tma)* Hm0 normalization: band renormalizes the truncated [min, max] band to carry the full Hm0 (legacy); total keeps the band's natural share of the full-spectrum integral. One of `band` \| `total`. |
| `spectrum.freq.peak` | `0.0` | `FreqPeak` | Hz | *(jonswap/tma)* Peak frequency (exact legacy path). |
| `spectrum.freq.min` | `0.0` | `FreqMin` | Hz | *(jonswap/tma)* Minimum frequency. |
| `spectrum.freq.max` | `0.0` | `FreqMax` | Hz | *(jonswap/tma)* Maximum frequency. |
| `spectrum.period.peak` | — | — | s | *(jonswap/tma)* Hand-authoring alt to freq.peak (reciprocal; mutually exclusive). |
| `spectrum.period.min` | — | — | s | *(jonswap/tma)* Alt to freq.max (note the min\<->max swap). |
| `spectrum.period.max` | — | — | s | *(jonswap/tma)* Alt to freq.min. |
| `spectrum.directional.peak` | `0.0` | `ThetaPeak` | deg | *(jonswap/tma)* Mean direction (presence => 2D spreading). |
| `spectrum.directional.spread` | `0.0` | `Sigma_Theta` | deg | *(jonswap/tma)* Directional spread (legacy 10.0 in directional branch; PENDING). |
| `spectrum.directional.n_bins` | `1` | `Ntheta` | — | *(jonswap/tma)* Directional bin count (legacy 24 in directional branch; PENDING). |
| `spectrum.discretization.freq_bins` | `45` | `Nfreq` | — | *(jonswap/tma)* Frequency bin count. |
| `spectrum.discretization.equal_energy` | `false` | `EqualEnergy` | — | *(jonswap/tma)* Equal-energy frequency binning. |
| `spectrum.discretization.method` | `grid` | — | — | *(jonswap/tma)* Discretization method: directional grid, or one direction per frequency component (nee WK_NEW_*). One of `grid` \| `single_dir_per_freq`. |
| `spectrum.discretization.coherence_percent` | `0.0` | `alpha_c` | % | *(jonswap/tma)* Percent of components sharing a frequency. |
| `spectrum.file` | — | `WaveCompFile` | — | *(components/spectrum_2d)* Wave-component / 2D-spectrum data file. |
| `spectrum.n` | `1` | `NumWaveComp` | — | *(components)* Number of wave components. |
| `spectrum.period_peak` | `0.0` | `PeakPeriod` | s | *(components)* Peak period (components store a period here). |
| `spectrum.format` | `DATA_1D` | `WAVE_DATA_TYPE` | — | *(spectrum_2d)* 2D-spectrum data format. |
| `source.x_center` | `0.0` | `Xc_WK` | m | Source-box x center (presence of source: => Wei-Kirby). |
| `source.y_center` | `0.0` | `Yc_WK` | m | Source-box y center. |
| `source.depth` | `0.0` | `DEP_WK` | m | Source-box reference depth. |
| `source.delta` | `0.5` | `Delta_WK` | — | Source-box width parameter. |
| `source.y_width` | `999999.0` | `Ywidth_WK` | m | Source-box alongshore width (large = full span). |
| `source.time_ramp` | `0.0` | `Time_ramp` | s | Source ramp-up time. |
| `source.current_cd` | — | `WaveMakerCd` | — | Current-balance drag (presence enables; nee WaveMakerCurrentBalance). |
| `limiter.crest` | — | `CrestLimit` | m | Crest elevation limit (presence => eta limiter). |
| `limiter.trough` | — | `TroughLimit` | m | Trough elevation limit. |

## `boundaries:`

Per-face boundary conditions.  The reader DERIVES each face's BC from which blocks are present (nothing -> wall; sponge only -> wall + absorbing strip; sponge.direct + forcing -> relaxation to a signal; forcing without direct -> characteristic, pending).  type: is an optional assertion.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `periodic` | — | `PERIODIC` | — | Axis-level periodic pair list, x and/or y (e.g. [y] or [x, y]). |
| `relaxation_cells` | `30` | `WaveMakerPointNum` | — | Width in cells of the forcing relaxation strip. |

### Per-face keys (`west:` / `east:` / `south:` / `north:`)

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `<face>.type` | — | — | — | Optional BC assertion; errors at init if it disagrees with the derived BC. |
| `<face>.sponge.width` | `0.0` | `Sponge_<face>_width` | m | Absorbing sponge strip width (0 = no sponge). |
| `<face>.sponge.direct.r` | `0.85` | `R_sponge` | — | Direct-sponge damping ratio. |
| `<face>.sponge.direct.a` | `5.0` | `A_sponge` | — | Direct-sponge damping exponent. |
| `<face>.sponge.friction.cd` | `0.0` | `CDsponge` | — | Sponge bottom-friction coefficient. |
| `<face>.sponge.diffusion.nu` | `0.1` | `Csp` | — | Sponge diffusion coefficient. |
| `<face>.forcing.eta` | — | `Tide<Face>_ETA` | m | Prescribed surface-elevation forcing (constant; file for a series). |
| `<face>.forcing.u` | — | `Tide<Face>_U` | m/s | Prescribed x-velocity forcing. |
| `<face>.forcing.v` | — | `Tide<Face>_V` | m/s | Prescribed y-velocity forcing. |
| `<face>.forcing.file` | — | `Tide<Face>FileName` | — | Time-series forcing file (presence => DATA forcing). |
| `<face>.forcing.wavemaker` | — | — | — | Name of a spectrum-only wavemaker entry driving this face (nee ABS). |
| `<face>.forcing.depth` | — | — | m | Reference depth for the wavemaker forcing series (nee DepthWaveMaker).  (west: legacy `DepthWaveMaker`) |

## `numerics:`

Numerical scheme — CFL, Riemann solver, reconstruction, wet/dry floor.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `cfl` | `0.5` | `CFL` | — | CFL number for the adaptive timestep. |
| `flux_solver` | `hllc` | `CONSTR` | — | Approximate Riemann solver. One of `hllc` \| `hll`. |
| `froude_cap` | `3.0` | `FroudeCap` | — | Maximum Froude number (velocity limiter). |
| `min_depth` | `0.1` | `MinDepth` | m | Single wet/dry + friction floor (legacy folded MinDepth/MinDepthFrc). |
| `reconstruction` | `fourth` | `HIGH_ORDER` | — | Spatial reconstruction scheme. One of `fourth` \| `fminmod` \| `weno` \| `mlp` \| `basic`. |

## `grid:`

**Required.**  Computational grid — extent, spacing, bathymetry source, MPI decomposition, optional Coriolis.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `coriolis.f` | — | — | s-1 | Constant Coriolis parameter (f-plane; overrides latitude). |
| `coriolis.latitude` | — | — | degrees_north | Reference latitude for the f-plane Coriolis parameter. |
| `bathy_correction` | `false` | `BATHY_CORRECTION` | — | Apply the bathymetry smoothing/correction pass. |
| `bathy_depth` | — | `Depth_Flat` | m | Still-water depth for a flat bottom. |
| `bathy_file` | — | `DEPTH_FILE` | — | Bathymetry data file. |
| `bathy_slope` | — | `SLP` | — | Bed slope for a sloping-beach bathymetry. |
| `bathy_slope_x0` | — | `Xslp` | m | x location where the slope begins. |
| `bathy_type` | `flat` | `DEPTH_TYPE` | — | Bathymetry generator. One of `flat` \| `slope` \| `data`. |
| `dx` | — | `DX` | m | Grid spacing in x. |
| `dy` | — | `DY` | m | Grid spacing in y. |
| `grid_nx` | — | `Mglob` | — | Number of grid cells in x (global). |
| `grid_ny` | — | `Nglob` | — | Number of grid cells in y (global). |
| `nx_proc` | — | `px` | — | MPI process count in x (absent = auto-decompose). |
| `ny_proc` | — | `py` | — | MPI process count in y (absent = auto-decompose). |

## `simulation:`

**Required.**  Run control — title, total/start time, screen-log cadence, and time-stepping.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `screen_interval` | `1.0` | `SCREEN_INTV` | s | Screen-log / monitor cadence. |
| `t_start` | `0.0` | `PLOT_START_TIME` | s | Simulation time at which output begins. |
| `title` | — | `TITLE` | — | Run title (reserved for NetCDF global attrs). |
| `total_time` | — | `TOTAL_TIME` | s | Total simulated duration. |
| `time_stepping.dt` | — | `DT_fixed` | s | Fixed timestep value (with fixed_dt). |
| `time_stepping.fixed_dt` | `false` | `FIXED_DT` | — | Use a fixed timestep instead of adaptive CFL. |

## `initial:`

Initial condition — block presence selects the type (solitary/sine_mode/fields/hump/n_wave) + still-water level.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `water_level` | `0.0` | `WaterLevel` | m | Uniform still-water level offset (non-zero init-gated pending). |
| `fields.eta` | — | — | — | Initial surface-elevation field ref (path, or container#/group/var once NetCDF input lands). |
| `fields.format` | — | — | — | Field-file format override (default from the extension). One of `ascii` \| `binary` \| `netcdf`. |
| `fields.u` | — | — | — | Initial x-velocity field ref (with fields.v; absent = still). |
| `fields.v` | — | — | — | Initial y-velocity field ref (with fields.u). |
| `hump.amplitude` | — | `AMP_SOLI` | m | Hump amplitude. |
| `hump.radius` | — | `GauRadius` | m | Gaussian hump radius. |
| `hump.shape` | — | — | — | Hump shape. One of `rect` \| `gaussian` \| `dipole`. |
| `hump.width` | — | `WID` | m | Hump width. |
| `hump.x_center` | — | `Xc` | m | Hump center x. |
| `hump.y_center` | — | `Yc` | m | Hump center y. |
| `n_wave.a0` | — | `a0_Nwave` | m | N-wave amplitude. |
| `n_wave.depth` | — | `dep_Nwave` | m | N-wave still-water depth. |
| `n_wave.gamma` | — | `gamma_Nwave` | — | N-wave shape parameter. |
| `n_wave.x1` | — | `x1_Nwave` | m | N-wave leading position. |
| `n_wave.x2` | — | `x2_Nwave` | m | N-wave trailing position. |
| `sine_mode.amplitude` | `0.0` | `AMP_SOLI` | m | Standing sine-mode amplitude. |
| `sine_mode.depth` | `0.0` | `DEP_SOLI` | m | Sine-mode still-water depth. |
| `sine_mode.mode_x` | `1` | `MODE_X` | — | Sine mode number in x. |
| `sine_mode.mode_y` | `0` | `MODE_Y` | — | Sine mode number in y. |
| `solitary.amplitude` | `0.0` | `AMP_SOLI` | m | Solitary-wave amplitude. |
| `solitary.depth` | `0.0` | `DEP_SOLI` | m | Solitary-wave still-water depth. |
| `solitary.direction` | `+x` | `SolitaryPositiveDirection` | — | Solitary-wave propagation direction. One of `+x` \| `-x` \| `+y` \| `-y`. |
| `solitary.x_center` | `0.0` | `XWAVEMAKER` | m | Solitary-wave initial crest x. |
| `solitary.angle` | — | — | deg | Oblique crest angle from +x (presence selects the doubly-periodic tiled train; excludes direction). |
| `solitary.y_center` | `0.0` | — | m | Solitary-wave initial crest y (angle only). |

## `physics:`

Physics — Boussinesq dispersion scheme (Gamma presets + overrides) and the SWE transition depth.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `dispersion.beta_ref` | `-0.531` | `Beta_ref` | — | Reference depth level for the Boussinesq operator. |
| `dispersion.gamma1` | — | `gamma1` | — | Dispersion coefficient Gamma1 (overrides the scheme preset). |
| `dispersion.gamma2` | — | `gamma2` | — | Dispersion coefficient Gamma2 (overrides the scheme preset). |
| `dispersion.gamma3` | — | `gamma3` | — | Dispersion coefficient Gamma3 (overrides the scheme preset). |
| `dispersion.scheme` | `fully_nonlinear` | `DISPERSION` | — | Dispersion preset for Gamma1/2/3. One of `fully_nonlinear` \| `weakly_nonlinear` \| `linear` \| `nswe`. |
| `dispersion.swe_eta_dep` | `0.8` | `SWE_ETA_DEP` | — | eta/depth ratio above which cells switch to shallow-water equations. |

## `breaking:`

Wave breaking — dissipation model, roller, and wavemaker-region overrides (core physics, not presence-gated).

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `cbrk1` | `0.65` | `Cbrk1` | — | Breaking onset threshold coefficient. |
| `cbrk2` | `0.35` | `Cbrk2` | — | Breaking cessation threshold coefficient. |
| `nu_bkg` | `0.0` | `nu_bkg` | m2 s-1 | Background eddy viscosity added everywhere. |
| `roller` | `false` | `ROLLER` | — | Enable surface-roller momentum flux. |
| `show_breaking` | `true` | `SHOW_BREAKING` | — | Output the breaking-index field. |
| `model` | `eddy_viscosity` | `VISCOSITY_BREAKING` | — | Breaking dissipation model. One of `eddy_viscosity` \| `shock_capturing`. |
| `visbrk` | `0.0` | `visbrk` | m2 s-1 | Breaking eddy-viscosity coefficient. |
| `wavemaker_cbrk` | `1.0` | `WAVEMAKER_Cbrk` | — | Breaking threshold scale inside the wavemaker region. |
| `wavemaker_vis` | `false` | `WAVEMAKER_VIS` | — | Enable extra viscosity in the wavemaker region. |
| `wavemaker_visbrk` | `0.0` | `WAVEMAKER_visbrk` | m2 s-1 | Breaking viscosity coefficient in the wavemaker region. |
| `roller_effect` | `false` | `ROLLER_EFFECT` | — | Apply the roller effect to the momentum equations. |

## `friction:`

Bottom friction — exactly one of cd, manning (n), or file; section absent = zero drag.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `cd` | — | `Cd` | — | Constant quadratic bottom-drag coefficient. |
| `manning` | — | — | s m^{-1/3} | Manning roughness n (converted to Cd per cell). |
| `file` | — | `CD_FILE` | — | Spatially varying Cd map file (init-gated pending). |

## `obstacle:`

Interior obstacles — obstacle mask file and breakwater absorbing block.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `file` | — | `OBSTACLE_FILE` | — | Obstacle mask file (presence enables interior walls). |
| `breakwater.file` | — | `BREAKWATER_FILE` | — | Breakwater location file. |
| `breakwater.absorb_coef` | `10.0` | `BreakWaterAbsorbCoef` | — | Breakwater absorption coefficient. |

## `meteo:`

Atmospheric forcing — presence-derived sub-models (gaussian/wind/holland/slide).

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `gaussian.file` | — | `METEO_GAUSIAN_FILE` | — | Gaussian pressure-disturbance file (presence enables; forces pressure coupling). |
| `wind.file` | — | `CONSTANT_WIND_FILE` | — | Constant wind-field file (presence enables wind stress). |
| `wind.cd` | `0.002` | `Cdw` | — | Wind drag coefficient. |
| `wind.wave_interaction` | `false` | `WindWaveInteraction` | — | Modulate wind stress by the wave field. |
| `wind.crest_percent` | — | `WindCrestPercent` | — | Fraction of crest exposed to wind (requires wave_interaction). |
| `holland.file` | — | `STORM_FILE` | — | Holland-model storm-track file. |
| `holland.air_pressure` | `false` | `AirPressure` | — | Apply the storm pressure coupling. |
| `holland.wind_force` | `false` | `WindForce` | — | Apply the storm wind-stress coupling. |
| `holland.cd` | `0.002` | — | — | Wind drag coefficient (Holland). |
| `holland.wave_interaction` | `false` | — | — | Modulate storm wind stress by the wave field. |
| `holland.crest_percent` | — | — | — | Fraction of crest exposed to wind (requires wave_interaction). |
| `slide.file` | — | `SLIDE_FILE` | — | Submarine-landslide forcing file (presence enables; forces pressure coupling). |

## `subgrid:`

Subgrid porosity (urban flooding) — depth file, ratio, porosity output.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `depth_file` | — | `DEPTH_SUBGRID_FILE` | — | High-resolution subgrid bathymetry file. |
| `write_porosity` | `false` | `Porosity` | — | Output the computed porosity field. |
| `ratio` | `1` | `SubMainGridRatio` | — | Subgrid-to-main-grid refinement ratio. |

## `foam:`

Foam / bubble model — production and decay parameters.

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
| `propeller` | `false` | `PROPELLER` | — | Enable propeller jets (must match the build's -DPROPELLER). |
| `deep_draft.clearance` | — | `CLEARANCE` | m | Keel clearance above the bed (required for deep_draft). |
| `deep_draft.mask` | `true` | `MaskMethod` | — | Mask cells fully blocked by the hull. |
| `deep_draft.cd` | — | `CdDeepDraft` | — | Hull drag coefficient (presence enables hull drag). |
| `deep_draft.nu` | — | `VisDeepDraft` | m^2/s | Hull eddy viscosity (presence enables). |

## `sediment:`

Sediment — single grain size, morphology, avalanching, cohesive, and flow-feedback sub-blocks.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `scheme` | `upwinding` | `Sed_Scheme` | — | Advection scheme for suspended load. One of `upwinding` \| `tvd`. |
| `d50` | — | `D50` | m | Median grain diameter (absent: 0.5 mm sand / 5 nm mud by cohesive). |
| `specific_gravity` | `2.68` | `Sdensity` | — | Sediment specific gravity. |
| `porosity` | `0.47` | `n_porosity` | — | Bed porosity. |
| `settling_velocity` | — | `WS` | m/s | Settling velocity (absent = computed from grain size). |
| `shields_cr` | `0.055` | `Shields_cr` | — | Critical Shields parameter for suspension. |
| `min_depth_pickup` | `0.1` | `MinDepthPickup` | m | Minimum water depth for pickup. |
| `pickup_reduction` | `true` | `PickupReduction` | — | Reduce pickup on steep slopes. |
| `reduction_parameter` | `0.65` | `ReductionParameter` | — | Slope pickup-reduction coefficient (requires pickup_reduction). |
| `c_limiter` | — | `C_limiter` | — | Suspended-concentration limiter (presence enables). |
| `morph_interval` | — | `Morph_interval` | s | Morphology update interval. |
| `bed_change` | `false` | `Bed_Change` | — | Enable bed-level change (morphodynamics). |
| `bedload` | `false` | `BedLoad` | — | Include bed-load transport. |
| `shields_cr_bedload` | — | `Shields_cr_bedload` | — | Critical Shields for bed load (absent = shields_cr). |
| `morph_factor` | `1` | `Morph_factor` | — | Morphological acceleration factor. |
| `hard_bottom.file` | — | `Mask_s_File` | — | Non-erodible hard-bottom mask file (required). |
| `avalanche.tan_phi` | — | `tan_phi` | — | Tangent of the repose angle (required for avalanche). |
| `avalanche.interval` | — | `Aval_interval` | s | Avalanche relaxation interval (absent = every step). |
| `cohesive.soft_bed` | `true` | `SoftBed` | — | Track a consolidating soft-bed layer. |
| `cohesive.tau_cr` | — | `Tau_cr_coh` | m2/s2 | Critical erosion shear stress (required for cohesive). |
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
| `output_start_number` | `0` | `FileNumber_HOTSTART` | — | First output frame number after restart. |
| `time` | `0.0` | `HotStartTime` | s | Simulation time at the restart instant. |
| `u_file` | — | `U_FILE` | — | x-velocity restart field file. |
| `v_file` | — | `V_FILE` | — | y-velocity restart field file. |

## `output:`

**Required.**  Output control — cadence, format, folder, plus stations/means/vessel/arrival-time sub-blocks.

| Key | Default | Legacy | Units | Description |
|---|---|---|---|---|
| `arrival_time.min_height` | `0.001` | `ArrTimeMin` | m | Elevation threshold defining first wave arrival. |
| `checkpoint` | — | — | — | Directory to write the hot-start checkpoint set (core.bin) at run end. |
| `depth_out` | `false` | `OUT_DEPTH` | — | Write the still-water depth field. |
| `field_io_type` | `ASCII` | `FIELD_IO_TYPE` | — | Field output format (NETCDF needs a netcdf-fortran build). One of `ASCII` \| `BINARY` \| `NETCDF`. |
| `layout` | `chunked` | — | — | NetCDF file topology; single = one output.nc with streams as groups One of `single` \| `per_stream` \| `chunked`. |
| `max_file_size` | `50.0` | — | GB | Chunk roll-over size; also the predicted-size warning threshold for single/per_stream. |
| `result_folder` | `./output/` | `RESULT_FOLDER` | — | Directory for output files. |
| `stations.file` | — | `STATIONS_FILE` | — | Gauge-station coordinate file (presence enables station output). |
| `means.steady_time` | `0.0` | `STEADY_TIME` | s | Time to begin time-averaging. |
| `means.interval` | — | `T_INTV_mean` | s | Averaging output cadence. |
| `interval` | — | `PLOT_INTV` | s | Global field-output cadence. |
| `blowup_threshold` | — | `EtaBlowVal` | m | Elevation above which the run aborts (absent = 100*max\|Depth\|). |
| `stations.interval` | `1.0` | `PLOT_INTV_STATION` | s | Station-output cadence. |
| `stations.buffer` | `1000` | `StationOutputBuffer` | — | Station time-series buffer length (rows). |
| `vessel.interval` | — | `PLOT_INTV_VESSEL` | s | Vessel resistance-series output cadence. |
| `geometries.name` | — | — | — | Point-set name (referenced by channels.geometry). |
| `geometries.type` | — | — | — | Point-set kind. One of `station` \| `transect`. |
| `geometries.x` | — | — | m | Station x-coordinates (equal length with y). |
| `geometries.y` | — | — | m | Station y-coordinates. |
| `geometries.start` | — | — | m | Transect start point [x, y]. |
| `geometries.end` | — | — | m | Transect end point [x, y]. |
| `geometries.n_points` | — | — | — | Transect sample count (>= 2). |
| `channels.name` | — | — | — | Channel name (file stem \<name>_\<var>.dat). |
| `channels.geometry` | — | — | — | Name of the geometries entry to sample (or inline the geometry keys on the channel instead). |
| `channels.variables` | — | — | — | Field-registry variable names to output. |
| `channels.interval` | — | — | s | Channel flush cadence. |
| `channels.statistics` | — | — | — | Presence makes the channel windowed (per-interval statistics One of `min` \| `max` \| `mean` \| `rms`. |
| `channels.t_start` | — | — | s | Channel start time (default simulation t_start). |
| `channels.format` | — | — | — | Point file format; default follows field_io_type (NETCDF selects a netcdf group in diagnostics.nc One of `ascii` \| `netcdf`. |

