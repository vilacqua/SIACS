# Building & releasing the SIACS Windows bundle

This produces a double-click-to-run SIACS bundle (Option A: bundled portable
R) and the release ZIP you attach to a GitHub Release on `vilacqua/SIACS`.

## Prerequisites (build machine only — not the end user)

- Windows 10/11
- PowerShell 5+ (built in)
- Internet access (to download R and packages at build time)
- ~2 GB free disk space
- Optional: [GitHub CLI](https://cli.github.com/) (`gh`) for uploading releases

## One-time: what's in `installer/`

| File | Role |
|---|---|
| `build_release.ps1` | Assembles the bundle and ZIP. Run this. |
| `launch_siacs.R`    | Bootstrap the bundled R runs at startup. |
| `SIACS.bat`         | The clickable launcher shipped to users. |
| `README-RUN.txt`    | End-user run instructions (shipped in the bundle). |
| `BUILD.md`          | This file (not shipped). |

## Build

From the repo root (`MayUpdate5`):

```powershell
powershell -ExecutionPolicy Bypass -File installer\build_release.ps1
```

Options:

```powershell
# Default: bundles the LATEST R release (auto-detected from CRAN)
... -File installer\build_release.ps1

# Pin a specific R version (tries current path, then CRAN old/ archive)
... -File installer\build_release.ps1 -RVersion 4.5.1

# Reuse an already-extracted R-portable (faster re-builds)
... -File installer\build_release.ps1 -SkipRDownload
```

Output lands in `dist\`:

```
dist\
  SIACS-windows\               <- the runnable bundle
    R-portable\
    library\
    app\
    SIACS.bat
    launch_siacs.R
    README-RUN.txt
  SIACS-windows-R<version>.zip <- attach THIS to the GitHub Release
```

> The exact ZIP name includes the bundled R version, e.g.
> `SIACS-windows-R4.5.1.zip`. Check the build output for the actual filename.

## Test before releasing

1. Open `dist\SIACS-windows\` and double-click `SIACS.bat`.
2. Confirm the browser opens SIACS and a simulation runs end to end.
3. Ideally test on a clean machine (or VM) with no R installed, to confirm the
   bundle is truly self-contained.

## Publish the release

### With GitHub CLI (recommended)

```powershell
gh auth login                      # once
gh release create v1.0.0 ^
  "dist\SIACS-windows-R4.5.1.zip" ^
  --repo vilacqua/SIACS ^
  --title "SIACS v1.0.0 (Windows, self-contained)" ^
  --notes "Double-click SIACS.bat after unzipping. No R/RStudio needed."
```

### Or via the web UI

1. Go to https://github.com/vilacqua/SIACS/releases/new
2. Create a tag (e.g. `v1.0.0`), title, and notes.
3. Drag the ZIP from `dist\` into the "Attach binaries" area.
4. Publish.

> Always attach the bundle as a **Release asset**, never commit it into the
> repo: the portable R + library is ~150–250 MB and exceeds GitHub's per-file
> and repo size guidance. Release assets allow files up to 2 GB.

## Notes

- The bundle is Windows-only (uses `Rscript.exe` and the prebuilt
  `tuv5.3.1.exe`). A Mac/Linux bundle would need a portable R for that OS and
  a shell-script launcher.
- TUV is shipped prebuilt inside `app\tuv5.3.1.exe\`; no Fortran compilation
  happens on the user's machine.
- Packages are pre-installed at build time into `library\`, so the bundle runs
  fully offline. The launcher only attempts downloads if a package is missing.
- `.gitignore` excludes `dist\` so build artifacts never get committed.
