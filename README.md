# SIACS — Simplified Indoor Air Chemistry Simulator


SIACS is an ODE-based box model for simulating indoor air quality, including
gas-phase photochemistry, ventilation, surface deposition, and indoor emissions
from occupant activities. It integrates the TUV (Tropospheric Ultraviolet and
Visible) radiation model to compute indoor photolysis rates from outdoor solar
conditions and building optical properties.

## Features

- **Gas-phase photochemistry** using the SAPRC99 (~211 reactions) or (later)
  the SAPRC07T mechanism
  
- **Full ventilation model** (infiltration, balanced, unbalanced, and natural
  ventilation; ASHRAE Fundamentals)
- **Indoor light model** — computes photolysis rates from outdoor direct and
  diffuse solar flux, window geometry, glass transmission spectra, and
  artificial lighting (LED, fluorescent, incandescent)
- **Surface deposition** with time-varying, activity-dependent deposition
  velocities (including ozone skin deposition from occupants)
- **Emission sources** linked to occupant activity schedules
- **Aerosol tracking** — PM₂.₅ and PM₂.₅₋₁₀ mass and number
- **Optional analyses**: time derivatives, mass balance component
  separation, first-order sensitivity analysis, uncertainty propagation
- **Two interfaces**: interactive Shiny GUI and scriptable batch mode

## Installation

```r
# Install from a local source tarball (e.g., provided by a collaborator):
install.packages("path/to/SIACS_0.2.0.tar.gz", repos = NULL, type = "source")
```

Once on CRAN (recommended for most users):
```r
install.packages("SIACS")
```

For developers (install from a local package folder):
```r
devtools::install("path/to/SIACS_cran", upgrade = "never")
```

## Quick Start

### GUI Mode (recommended for new users)

```r
SIACS::runSIACSApp()
```

This launches the Shiny app in your browser. Choose **Wizard Mode** for a
step-by-step guided setup, or **Advanced Mode** to upload your own CSV/XLSX
input files directly.

### Batch Mode (scripting)

```r
library(SIACS)

# Prepare your input_data_list and output_list (see vignette for details)
results <- SIACS.batch(
  input_data_list = my_inputs,
  instances       = 1:5,
  OutputList      = my_outputs,
  mechanism       = "SAPRC99",
  use_parallel    = TRUE
)
```

## Input Files

The model reads inputs from CSV and XLSX files. Example input files are
bundled in `inst/extdata/Input/`. Key inputs include:

| File | Description |
|---|---|
| `BoxData.csv` | Building geometry and location |
| `Time.csv` | Simulation start, duration, and time step |
| `PhysicalEnvironmentData.csv` | Temperature, humidity, pressure, ventilation rates (time series) |
| `OutdoorConcentrations.csv` | Outdoor species concentrations — ppm (time series) |
| `EmissionProfiles.csv` | Species emission rates by source activity (g/min) |
| `Activities.csv` | Occupant activity schedule (time series) |
| `Vd&P.csv` | Deposition velocities and penetration factors |
| `IndoorLight.xlsx` | Pre-computed indoor light flux (optional) |

## Output Files

| File | Description |
|---|---|
| `Output{N}.csv` | Time series of all species, physical variables, and emissions |
| `Output{N}.png` | Multi-panel results plot |
| `TimeDerivatives{N}.csv` | dy/dt for all species |
| `MassBalance{N}.xlsx` | Per-species mass balance components |
| `Sensitivity{N}.csv` | First-order sensitivity coefficients |
| `Uncertainty{N}.csv` | Uncertainty propagated from input uncertainties |

## Chemical Mechanisms

| Mechanism | Reactions | Notes |
|---|---|---|
| `SAPRC99` | 211 | Default; well-validated for urban air |
| `SAPRC07T` |  | Upcoming mechanism |

## Documentation

```r
?SIACS           # Core simulation engine documentation
?SIACS.batch     # Batch mode documentation
?runSIACSApp     # GUI launcher documentation
vignette("SIACS_intro")  # Full worked example
```

## Citation

If you use SIACS in your work, please cite:

```
[US EPA]. SIACS: Simulation of Indoor Air Chemistry and Surfaces.
R package version 0526 (Beta) [2026]. [DOI or URL]
```

See `citation("SIACS")` for the full citation in BibTeX format.

## License

GPL-3. See `LICENSE` for details.
