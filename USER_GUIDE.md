# SIACS User Guide

## Overview

SIACS (Simulation of Indoor Air Chemistry and Surfaces) is a comprehensive ODE-based box model for simulating indoor air quality. This guide will help you navigate the user interface and understand the available features.

## Getting Started

### Launching the Application

1. **Run the application**:
   ```r
   runApp('main_app.R')
   ```
   The app will automatically open in your default web browser

2. **Choose your simulation mode**:
   - **Standard Mode**: Recommended for new or ordinary users - simplified step-by-step guided setup
   - **Advanced Mode**: For experienced users who are familiar with SIACS and know what input files are fed to the model

## Main Interface Navigation

The application has a navigation bar with the following tabs:

### 1. List of Runs
- View and manage all your simulation configurations
- See simulation details: Run Name, Location, Duration, Chemical Model
- Remove individual simulations using the "Remove" buttons
- Save the entire queue to R environment

### 2. (+) Simulation
- Create new simulation configurations
- Choose between Standard (simplified) and Advanced modes
- Set basic parameters: Run Name, Location, Duration, Chemical Model

### 3. Archive
- Store and retrieve previous simulation configurations

### 4. Help
- Access this user guide and validation requirements

### 5. Credits
- View application credits and version information

## Simulation Modes

### Standard Mode (Recommended for New or Ordinary Users)

The Wizard provides a 12-screen guided configuration process:

#### Screen 1: Simulation Basics
- Run name and location
- Duration and chemical model selection
- Start date and time settings

#### Screen 2: Shelter Configuration
- Select shelter class (1-5):
  - 1: Exposed (no obstructions)
  - 2: Normal (isolated rural house)
  - 3: Normal (buildings across street)
  - 4: Normal (urban, obstacles > one building height away)
  - 5: Well-shielded (adjacent structures < one building height away)
- Number of stories (1-3)

#### Screen 3: Room Geometry
- Floor surface area (m²)
- Room height (m)
- Aspect ratio and orientation
- Number and configuration of windows

#### Screen 4: Ventilation System
- Choose ventilation type:
  - Infiltration only
  - Balanced mechanical ventilation
  - Unbalanced mechanical ventilation
  - Natural ventilation
- Set ventilation parameters

#### Screen 5: Lighting Configuration
- Indoor lighting options:
  - Kowal LED
  - Kowal Incandescent
  - Kowal CFL
- Artificial lighting schedule
- Window properties and geometry

#### Screen 6: Occupant Activities
- Define occupant schedules
- Emission sources and activities
- Activity timing and duration

#### Screen 7: Environmental Conditions
- Outdoor temperature and humidity
- Outdoor concentrations
- Physical environment parameters

#### Screen 8: Initial Conditions
- Chemical species initial concentrations
- Deposition velocities
- Surface properties

#### Screen 9: Time Settings
- Simulation time step
- Total duration
- Output frequency

#### Screen 10: Advanced Options
- Optional analyses and sensitivity
- Mass balance components
- Uncertainty propagation

#### Screen 11: Summary and Validation
- Review all configuration parameters
- Validation checks for required inputs
- Error and warning notifications

#### Screen 12: Output Configuration
- Choose output variables
- Set file naming conventions
- Select analysis options

### Advanced Mode

For experienced users who want to upload their own input files:

#### Input File Categories

**Environment Setup Files:**
1. **Initial Values** - Chemical species initial concentrations
2. **Deposition Velocity** - Surface deposition parameters
3. **Physical Environment** - Temperature, humidity, pressure data
4. **Emission Profiles** - Time-varying emission source definitions

**Source & Activity Setup:**
5. **Outdoor Concentrations** - Background outdoor chemical concentrations
6. **Activities** - Occupant activity schedules and emission rates

**Light & Exposure:**
7. **Indoor Light** - Indoor lighting spectra and intensity
8. **Outdoor Light Direct** - Direct solar radiation components
9. **Outdoor Light Diffuse** - Diffuse solar radiation components
10. **Artificial Light** - Artificial lighting specifications
11. **Artificial Light List** - Lighting fixture inventory
12. **Artificial Light Spectra** - Light spectral data
13. **Artificial Light Schedule** - Lighting usage schedules

**Simulation Timing:**
14. **Time** - Simulation time parameters (start, duration, step)
15. **Windows** - Window geometry and optical properties
16. **Glass Transmission** - Window material transmission spectra
17. **Box Data** - Room geometry and physical parameters

#### File Management Features

- **Preload All Default Input Files**: Load all 17 default files at once
- **Validate All Input Files**: Batch validation of all loaded files
- **Show File Structure**: Diagnostic view of all loaded files
- **Individual File Management**: Upload, import, or create files individually
- **Real-time Validation**: Status indicators for each file
- **Editable Preview Tables**: Modify data directly in the interface

## Validation System

The application includes comprehensive input validation:

### Validation Indicators
- **✓ Valid**: Green checkmark - all validation checks passed
- **⚠ Valid with warnings**: Orange warning - valid but with advisory messages
- **✗ Errors found**: Coral indicator - validation errors that must be fixed

### Common Validation Requirements

**Time Data:**
- Required columns: StartTimeYear, StartTimeMonth, StartTimeDay, StartTime, StartTimeStandard, RelativeStartTime, TimeStep, Duration
- Time column must have strictly increasing values
- Valid temperature and humidity ranges

**Box Data:**
- Required columns: FloorSurfaceArea, RoomHeight
- Physical dimensions must be non-negative

**Physical Environment:**
- Required columns: Time, Ti, To, RH, BP
- Temperature and humidity must be within realistic ranges

**Outdoor Concentrations:**
- Required columns: Time column, species concentration columns
- No negative concentrations allowed

**Emission Profiles:**
- Required columns: ProfileName column
- No duplicate profile names
- No negative emission rates

## Working with Simulations

### Creating a New Simulation

1. Click **"(+) Simulation"** tab
2. Fill in basic simulation details:
   - Run Name: Descriptive name for your simulation
   - Location: Geographic location identifier
   - Duration: Simulation duration in hours
   - Chemical Model: SAPRC99 or SAPRC07T
3. Click **"Continue"** to proceed to configuration

### Managing Simulation Queue

1. Go to **"List of Runs"** tab
2. View all configured simulations
3. Use **"Remove"** buttons to delete individual simulations
4. Click **"Save"** to store the queue in R environment

### Running Simulations

1. Ensure you have simulations in the queue
2. Click **"Run Queue"** to execute all simulations
3. Monitor console output for progress
4. Results are saved to the Output directory

## File Structure and Data Management

### Input Directory Structure
```
Input/
├── InitialValues.csv
├── DepositionVelocity.csv
├── PhysicalEnvironment.csv
├── OutdoorConcentrations.csv
├── EmissionProfiles.csv
├── Activities.csv
├── IndoorLight.xlsx
├── OutdoorLightDirect.csv
├── OutdoorLightDiffuse.csv
├── ArtificialLight.csv
├── ArtificialLightList.csv
├── ArtificialLightSpectra.csv
├── ArtificialLightSchedule.csv
├── Time.csv
├── Windows.csv
├── GlassTransmission.csv
└── BoxData.csv
```

### Output Directory
- Simulation results are automatically saved to the `Output/` directory
- File naming follows the pattern: `[RunName]_[timestamp]_[variable].csv`

## Tips and Best Practices

### For New Users
1. **Start with Wizard Mode** for guided configuration
2. **Use default files** to understand expected data format
3. **Validate inputs** before running simulations
4. **Check validation summary** for detailed error messages

### For Advanced Users
1. **Prepare CSV/XLSX files** according to the required column specifications
2. **Use validation** to catch errors early
3. **Leverage preview tables** to verify data before simulation
4. **Save configurations** for reuse and documentation

### General Tips
1. **Save frequently** to avoid losing work
2. **Use descriptive run names** for easy identification
3. **Check console output** for simulation progress and errors
4. **Review validation warnings** - they may indicate potential issues

## Troubleshooting

### Common Issues

**Application won't start:**
- Ensure all required packages are installed
- Check that main_app.R is in the working directory
- Verify R version compatibility

**Simulation crashes silently with empty Output folder (no log file):**

This usually means a required R package is missing from your installation. SIACS uses two sets of packages: a GUI set that loads when the app starts (these auto-install on first launch) and an *engine* set that the background simulation process needs at run time.

If you saw a successful app launch but a silent crash when clicking **Run Queue**, check `siacs_child_steps.log` in the project folder. A line like:

```
[engine] FAILED library deSolve: there is no package called 'deSolve'
[engine] FATAL: missing package deSolve
```

…tells you which package is missing. The most common culprit is **`deSolve`** — the ODE solver — which is a hard requirement.

To install all engine packages manually, run this once in your R console:

```r
install.packages(c("deSolve", "reshape", "ggplot2", "openxlsx",
                   "doParallel", "foreach", "rstudioapi"))
```

The current SIACS version installs these automatically at app launch, but if you have a restricted package library, no internet access on first run, or a corporate firewall, the auto-install can fail silently. Installing manually first avoids the issue.

**Diagnostic log files:**

When troubleshooting any silent crash, three log files in the project folder tell you exactly where things went wrong:

- `siacs_startup.log` — written when the app launches; contains R version, library paths, OS, OneDrive detection, and per-package load status.
- `siacs_child_started.log` — written the instant the background simulation process launches. If this exists but `siacs_child_steps.log` does not, the simulation crashed before it could load its diagnostic helper.
- `siacs_child_steps.log` — written step-by-step as the simulation engine starts up. The last line tells you which step failed (loading a package, compiling source files, starting the parallel cluster, etc.).

**OneDrive sync conflicts:**

Running SIACS from inside a OneDrive-synced folder can cause silent crashes, because the sync client may lock files mid-run. If you see a yellow warning banner at the top of the Simulation Queue tab, copy SIACS to a non-synced local folder (e.g. `C:\SIACS\`) and run from there.

**Validation errors:**
- Check file formats (CSV/XLSX)
- Verify required columns are present
- Ensure numerical values are in valid ranges

**Simulation failures:**
- Review input data for negative values where inappropriate
- Check time series for continuity
- Verify chemical mechanism compatibility

### Getting Help

1. **Check the Help tab** in the application
2. **Review validation messages** for specific guidance
3. **Consult this user guide** for detailed instructions
4. **Check existing documentation** in the SIACS_cran directory

## Keyboard Shortcuts and Navigation

### Wizard Navigation
- **Next**: Proceed to next wizard screen
- **Previous**: Return to previous screen
- **Save Progress**: Store current wizard state
- **Finish**: Complete wizard and create simulation

### General Navigation
- **Tab navigation**: Click tab names or use Ctrl+Tab
- **Modal windows**: Click outside or use Escape key
- **Validation**: Click validation buttons to check inputs

## Technical Notes

### System Requirements
- R 4.0+ recommended (tested with R 4.3.1 and R 4.5.0)
- Memory: 4GB+ RAM recommended for large simulations

### Required R Packages

SIACS uses two sets of packages. The app attempts to install missing packages automatically on first launch, but if you have a restricted package library or no internet access, install them manually first.

**GUI packages** (loaded when the Shiny app starts):

```r
install.packages(c("shiny", "jsonlite", "tidygeocoder", "dplyr", "DT",
                   "shinyFiles", "rhandsontable", "shinyjs", "shinyBS",
                   "readxl", "zoo", "processx", "plotly", "reshape2"))
```

**Engine packages** (loaded by the background simulation process; missing any of these will cause Run Queue to fail):

```r
install.packages(c("deSolve", "reshape", "ggplot2", "openxlsx",
                   "doParallel", "foreach", "rstudioapi"))
```

`deSolve` is the ODE solver and is the most common missing package on fresh R installations. Note that `reshape` (the original) and `reshape2` are different packages — both are required.

### Performance Considerations
- Large time series may require significant processing time
- Complex chemical mechanisms increase computational requirements
- Multiple simultaneous simulations may impact performance

---

## Version Information

This guide corresponds to SIACS version with the following modules:
- main_app.R (Main application interface)
- wizard_module.R (12-screen configuration wizard)
- advanced_module.R (Advanced file upload interface)
- shared.r (Validation and utility functions)

For the most up-to-date information, check the Help tab within the application or consult the README.md file in the SIACS_cran directory.
