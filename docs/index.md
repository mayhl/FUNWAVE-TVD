# FUNWAVE-TVD

**Full-dispersion, fully nonlinear Boussinesq wave model**

FUNWAVE-TVD solves the fully nonlinear Boussinesq equations of
[Shi et al. (2012)](#) on structured Cartesian grids using a TVD
shock-capturing finite-difference scheme.

## Features

- Fully nonlinear, fully dispersive Boussinesq equations
- MPI-parallelised finite-difference solver
- 2D depth-integrated and 3D layered modes
- Spectral and monochromatic wavemaker boundary conditions
- Sponge layers, tidal forcing, vessel-generated waves

## Quick Start

```bash
git clone https://github.com/mayhl/mayhlFUNWAVE
cd mayhlFUNWAVE
./bin/fun-dev install
./bin/fun-dev unit
```

## Documentation Structure

| Section                               | Description                              |
| ------------------------------------- | ---------------------------------------- |
| [Model](model/overview.md)            | Governing equations and numerical scheme |
| [User Guide](guide/installation.md)   | Installation, configuration, output      |
| [Examples](examples/standing_wave.md) | Step-by-step worked examples             |
| [API Reference](api/index.md)         | Auto-generated source documentation      |
