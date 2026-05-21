# SIACS User Guide

## Overview

**SIACS** (Simulation of Indoor Air Chemistry and Surfaces) is an ODE-based single-zone (well-mixed) indoor air box model. It simulates time-varying indoor concentrations by jointly accounting for:

- Gas-phase photochemistry using SAPRC mechanisms (`SAPRC99`, `SAPRC07T`)
- Ventilation (infiltration, balanced, unbalanced, natural)
- Surface deposition and filtration
- Indoor emissions linked to activity schedules
- Indoor photolysis rates ("J-values") derived from outdoor solar and indoor lighting
- Optional post-processing: time derivatives, mass balance components, sensitivity, uncertainty

SIACS supports two user workflows:

- **Interactive GUI** (Shiny), launched with `runSIACSApp()`
- **Batch mode** for scripting and multi-scenario runs, via `SIACS.batch()`

## Why you might use SIACS

Indoor pollutant levels are influenced by multiple drivers that change over time, including ventilation, outdoor air, indoor sources, and removal to surfaces or filters. When chemistry is enabled, reactions can couple those drivers in ways that are hard to reason about without a dynamical model.

SIACS is a practical middle ground between:

- simple steady-state calculations that cannot represent time-varying behavior, and
- highly detailed modeling frameworks that require extensive inputs and specialized configuration.

It is intended for scenario exploration ("what if" studies), hypothesis building, and sensitivity-style comparisons (e.g., how much does changing air exchange or a source schedule shift indoor concentrations).

When running chemistry, SIACS uses SAPRC-based mechanisms (selectable via `mechanism`) with a fixed reaction set (on the order of ~211 reactions) and a mechanism-defined set of species/groups.

## What the package provides

- A core simulation engine (`SIACS()`) that solves a coupled ODE system.
- A batch runner (`SIACS.batch()`) that prepares inputs, computes lighting if needed, runs one or more scenarios, and writes outputs.
- A Shiny interface (`runSIACSApp()`) that helps you configure scenarios interactively.

The package includes bundled example inputs and templates (see `system.file("extdata", "Input", package = "SIACS")`) that you can copy and modify.

## Who this is for

- Users who want a reproducible, scriptable way to simulate indoor concentration time series.
- Users who prefer a GUI for building and running scenarios.
- Users comparing relative effects of interventions (ventilation, filtration, emissions, light) under a defined set of assumptions.

## When SIACS may not fit

SIACS is a single-zone reduced-form model. Consider a different approach if you need:

- multi-room airflow networks or CFD
- detailed heterogeneous/multiphase chemistry beyond the current scope
- tightly coupled building energy/HVAC control simulation
- decision-grade predictions without case-specific validation for your application

## Installation

### From a local source tarball

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

In the app you can:

- Use **Wizard Mode** for a guided, step-by-step setup (defaults are pre-populated from the bundled example scenario)
- Use **Advanced Mode** to upload/edit full CSV/XLSX inputs directly

### Option B: Batch mode (scripting)

At a high level, batch mode requires:

- A named `input_data_list` holding all required input tables per scenario
- An `instances` vector indicating which scenarios to run
- An `OutputList` describing output file paths and optional analyses

```r
library(SIACS)

results <- SIACS.batch(
  input_data_list = my_input_data_list,
  instances       = 1,
  OutputList      = my_output_list,
  mechanism       = "SAPRC99",
  chemistry       = TRUE,
  use_parallel    = FALSE
)

out <- results[[1]]$Simulation1$alldata
head(out[, c("time", "O3", "NO2", "HCHO")])
```

## Conceptual model (what SIACS simulates)

SIACS solves mass-balance ODEs for all tracked species in a single indoor zone.

The governing processes include:

- **Ventilation exchange** with outdoors (species-specific penetration may apply)
- **Indoor sources** (emission profiles activated by time-varying activities)
- **Surface loss** via deposition velocities (optionally activity dependent)
- **Removal via filtration** (HVAC / recirculating filters)
- **Gas-phase chemistry** including photolysis, with indoor photolysis rates computed from the indoor light environment

The ODE system is solved using `deSolve::lsode()` with a user-supplied Jacobian.

## Core equations and concepts

This section summarizes a standard single-zone indoor mass-balance formulation to make the model structure more transparent. It is intended to help you reason about inputs/outputs and design scenarios; it is not a full derivation.

### Mass balance (single-zone box model)

SIACS follows a mass-conservation formulation for each species *x*, combining outdoor exchange, indoor sources, chemistry, and deposition.

A common way to write the conceptual balance is:

```text
dC_i,x/dt = (P_x * a) * C_o,x  -  a * C_i,x  +  S_x/V  -  (v_x * A/V) * C_i,x  -  Σ_n (k_n * ...)
```

Where the terms represent:

- **`C_i,x`**: indoor concentration of species *x*
- **`C_o,x`**: outdoor concentration of species *x*
- **`P_x`**: penetration factor (dimensionless) describing fraction of outdoor species that penetrates indoors
- **`a`**: air exchange rate (time⁻¹)
- **`S_x`**: indoor emission rate (mass/time) for species *x*
- **`V`**: indoor zone volume
- **`A`**: effective indoor surface area available for deposition
- **`v_x`**: deposition velocity (length/time) for species *x*
- **`k_n`**: pseudo-first-order rate constant(s) representing chemical reactions (including photolysis where applicable)

Notes:

- SIACS tracks many species simultaneously, so chemistry terms are part of a coupled ODE system.
- In practice, SIACS uses specific unit systems internally (e.g., molecules/cm³ for some solver states) and converts to user-facing units in outputs.

### Ventilation / airflow components

One way to conceptualize the total ventilation flow rate is as a combination of mechanical ventilation and envelope-driven components:

```text
Q_tot = Q_bal + sqrt(Q_unbal^2 + Q_nat^2 + Q_inf^2)
```

With infiltration parameterized (extended LBNL approach) as:

```text
Q_inf = A_inf * sqrt(k_s * |T_i - T_o| + k_w * U^2)
```

And natural ventilation as a combination of wind- and stack-driven terms:

```text
Q_nat = sqrt(Q_nat,wind^2 + Q_nat,stack^2)
```

Where:

- **`Q_tot`**: total ventilation flow rate
- **`Q_bal`**: balanced mechanical ventilation flow
- **`Q_unbal`**: unbalanced mechanical ventilation flow
- **`Q_nat`**: natural ventilation flow (windows/doors)
- **`Q_inf`**: infiltration (leakage) flow (envelope cracks)
- **`A_inf`**: effective leakage area
- **`k_s`**: stack coefficient
- **`k_w`**: wind coefficient
- **`T_i`, `T_o`**: indoor and outdoor temperature
- **`U`**: wind speed

SIACS uses these flows (together with zone volume) to produce time-varying exchange rates that drive indoor–outdoor transport.

### Time stepping, resolution, and interpolation

- **Output resolution vs. solver steps**: you specify an output time grid (e.g., every minute), but the ODE solver may take smaller internal steps as needed for numerical convergence.
- **Sparse time-series inputs are allowed**: as long as time-dependent inputs are defined at the simulation start and end, SIACS interpolates values as needed during integration.

## Key assumptions and limitations

SIACS is designed as a simplified screening-level tool. Important assumptions and limitations include:

- **Single-zone, well-mixed indoor air**: the modeled space is assumed to be one well-mixed compartment.
- **Gas-phase reactions only (current scope)**: the model is primarily formulated for gas-phase chemistry. Heterogeneous interactions (e.g., surface absorption/desorption and other multiphase chemistry) are not represented in the current model formulation.
- **Chemistry mechanism is not user-editable via inputs**: chemical mechanism details are fixed by the selected mechanism (`SAPRC99` or `SAPRC07T`). Changes require code modification.
- **Time-base differences**: many input time series are expressed in **minutes**, while the ODE solver time grid uses **seconds**.

## Input data quality and recommended practices

### Secondary data and “fit for use” checks

SIACS scenarios are often built from secondary sources (published measurements, public datasets, or outputs from other models). Before relying on a dataset, apply basic “fit for use” checks that match the stakes of your application.

Examples of checks you can apply:

- **Applicability and utility** (relevance to the scenario being modeled)
- **Soundness** (methods and conclusions supported by accepted scientific practice)
- **Clarity and completeness** (assumptions and metadata sufficient to interpret and reproduce)
- **Uncertainty and variability** (uncertainties described and not ignored)
- **Evaluation and review** (peer review / independent technical review)
- **Comparability** (ability to compare/merge across sources)

### Minimal requirements for time-dependent inputs

SIACS can work with limited data. However, for each time-dependent input variable:

- Values should be provided at a minimum at the **start** and **end** of the simulation period.
- SIACS interpolates time-dependent inputs internally using a monotonic spline approach; because interpolation does not inherently guarantee positivity, the implementation includes adjustments intended to ensure non-negativity.

### Spin-up / initial conditions

Initial indoor concentrations can be supplied directly, or estimated to reflect equilibrium at the simulation start. If you rely on internally-estimated equilibrium initial conditions, it is recommended to include a model **spin-up** period under constant conditions representative of the desired initial state. Spin-up time is typically longer under lower air exchange rates.

## Main exported functions

SIACS exports three primary user-facing functions (plus many internal helpers).

### `runSIACSApp()`

Launches the Shiny application.

- **Use when** you want to explore the model interactively, build scenarios, and export output files without writing R code.
- **Returns** no value (blocks while app runs).

### `SIACS.batch()`

Runs one or more independent scenarios programmatically (optionally in parallel).

- **Use when** you want reproducible scripted runs, parameter sweeps, multiple scenarios, or to integrate SIACS into an automated workflow.
- **Returns** an invisible list of per-instance result objects.
- **Side effects** writes output CSV/XLSX/PNG files to the paths given in `OutputList`.

Key arguments:

- `input_data_list`: list of all inputs (each element is a list indexed by instance)
- `instances`: which instance indices to run (e.g., `1:10`)
- `OutputList`: output file paths and analysis toggles (use `"None"` to suppress)
- `use_parallel`, `n_cores`: parallel execution controls
- `mechanism`: `"SAPRC99"` or `"SAPRC07T"`
- `chemistry`: `TRUE` for full chemistry, `FALSE` for physical-only
- `perturbation`: `TRUE` to enable sensitivity/uncertainty analyses

### `SIACS()` (core engine)

Runs a single simulation given fully prepared input objects.

- **Use when** you are developing new workflows or integrating SIACS deeply into custom code.
- In most cases you should call **`SIACS.batch()`** or the **GUI** instead.

## Input data: structure and required elements

In batch mode, `input_data_list` is a **named list**, where each element is itself a **list of length = number of scenarios**.

Minimum required elements (typical):

- `Time`
- `BoxData`
- `PhysicalEnvironment`
- `OutdoorConcentrations`
- `EmissionProfiles`
- `Activities`
- `DepositionV`
- `InitialValues`

Additional elements are required if SIACS must compute indoor lighting, or if you do not provide precomputed light flux:

- `Windows`
- `GlassTransmission`
- Optional: `OutdoorLightDirect`, `OutdoorLightDiffuse`
- Optional: `ArtificialLight` (precomputed) **or** `ArtificialLightList` + `ArtificialLightSpectra` + `ArtificialLightSchedule`
- Optional: `IndoorLight` (precomputed)

### Where to find example input templates

The package includes example input files under:

```r
system.file("extdata", "Input", package = "SIACS")
```

Use these as the authoritative templates for column names, units, and required fields.

## Outputs

### Primary results table

For each simulation, SIACS produces a main time series output table (available in memory as `Simulation{i}$alldata` and typically written to CSV if configured).

The table includes:

- `time` (minutes)
- Indoor concentrations (ppm for gases; µg/m³ for PM)
- Outdoor concentrations (suffix `.O`)
- Emission source strengths (suffix `.S`)
- Physical variables (e.g., temperatures, air exchange rate, light flux)
- Aggregated diagnostic groups (ALK, ARO, OLE, NOy, RO2, HOx and outdoor counterparts)

### Optional analyses

These are controlled by the `OutputList` entries (use `"None"` to disable):

- Time derivatives (dy/dt)
- Mass balance components (XLSX)
- First-order sensitivities
- Uncertainty propagation

Important coupling:

- Mass balance, sensitivity, and uncertainty workflows depend on dy/dt output being enabled.

## Typical workflows

### 1) Run the bundled example in the GUI

- Launch `runSIACSApp()`
- Keep defaults (bundled example)
- Run simulation
- Review CSV/PNG outputs
- Modify one set of inputs (e.g., ventilation schedule) and compare

### 2) Run a single scenario in batch mode using bundled inputs

The introductory vignette provides a worked example. In general, you:

1. Load input files from `system.file("extdata", "Input", package = "SIACS")`
2. Assemble `input_data_list`
3. Create `OutputList`
4. Call `SIACS.batch()`

## Mechanisms

- `SAPRC99`: default mechanism
- `SAPRC07T`: updated SAPRC mechanism

Both include photolysis reactions whose rates are driven by indoor photolysis constants (J-values).

## Troubleshooting

- **Missing or misnamed columns**: Use the bundled example inputs as templates; SIACS expects exact column names in many tables.
- **Duration mismatch errors**: Ensure every time series input spans the full simulation duration (time in minutes for most inputs; the ODE solver time vector is in seconds).
- **Indoor light not provided**: If `IndoorLight` is `NULL`, you must provide window/glass inputs and (optionally) artificial light definitions so SIACS can compute indoor photolysis rates.
- **Parallel file collisions**: In parallel mode, ensure `OutputList` paths are unique per instance; SIACS appends the instance number to many outputs.

## Verification, validation, and checking outputs

### What to check before trusting results

- **Code-level confidence**: when you modify code or add new routines, compare against simple independent calculations or small test cases.
- **Model-level confidence**: compare key outputs against published results, measurements, or alternative models when available for your scenario class.
- **Usability/robustness**: test what happens when inputs are missing or malformed and ensure failures are informative rather than silent.

### Practical output checks you can do

For a given scenario (especially when modifying inputs), check:

- **Reproducibility**: with identical inputs and deterministic settings, repeated runs should produce the same output. As a practical check, you can rerun the same scenario and confirm key outputs match (or differ only within a small numerical tolerance).
- **Sanity checks on mass-balance drivers**:
  - ventilation schedules (units and magnitudes)
  - deposition velocities and filter efficiencies
  - outdoor concentrations and emission schedules
- **Time coverage**: confirm all required time-series inputs cover the full simulated period.

### Sensitivity and uncertainty

Uncertainty arises from:

- **Model-form uncertainty** (approximations of the indoor environment and simplified processes)
- **Measurement/input uncertainty**

SIACS supports optional analyses (sensitivity and uncertainty propagation). For broader uncertainty characterization, Monte Carlo approaches can be used by sampling uncertain parameters from assumed distributions and rerunning the model repeatedly.

## Citation

```r
citation("SIACS")
```

## References and attribution

This guide was written based on the SIACS package source code and package documentation, and it also draws on concepts and terminology from the SIACS Quality Assurance Project Plan (QAPP) included in this repository.

- Development of EPA’s Simplified Indoor Air Chemistry Simulator (SIACS) Model — Version 1.0 (QAPP). Source file(s) in this repository:
  - `Final_SIACS_QAPP_v1_signed_011022.pdf`
  - text extraction used for drafting: `SIACS_qapp.txt`
- Carter, W. P. L. (2000). Documentation of the SAPRC-99 chemical mechanism for VOC reactivity assessment.
- Carter, W. P. L. (2010). Development of the SAPRC-07 chemical mechanism.

## Notes on the PDF in this repository

This repository also contains a PDF (`Final_SIACS_QAPP_v1_signed_011022.pdf`). In this environment I can’t directly parse the binary PDF; I can, however, incorporate content from a text-extracted version such as `SIACS_qapp.txt`.
