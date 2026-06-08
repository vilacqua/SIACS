============================================================
 SIACS — How to run (Windows)
============================================================

SIACS is self-contained. You do NOT need to install R or RStudio.

1. Download the SIACS release ZIP from GitHub:
      https://github.com/vilacqua/SIACS  ->  Releases

2. Right-click the ZIP and choose "Extract All...".
   Extract to a LOCAL folder such as:
      C:\SIACS\
   Do NOT run it from inside a ZIP viewer, and avoid OneDrive /
   Documents / Desktop folders that may be protected by Windows
   "Controlled Folder Access" — those can block the app from
   writing its results.

3. Open the extracted folder and double-click:
      SIACS.bat

4. A black command window opens and then your default web browser
   opens with the SIACS interface.
   * Keep the black window open while you use SIACS.
   * Close that window when you are finished to stop the app.

------------------------------------------------------------
Where do my results go?
------------------------------------------------------------
SIACS writes its outputs, input snapshots, and log files to a
per-user workspace folder (shown in the startup messages in the
black window). This keeps everything writable even when SIACS
itself is installed in a protected location.

------------------------------------------------------------
First launch is slow / "installing packages"
------------------------------------------------------------
If the bundle was built without pre-installed packages, the first
launch will download them (needs internet). Normal builds include
all packages, so launch is immediate and works fully offline.

------------------------------------------------------------
Something went wrong
------------------------------------------------------------
If the app does not start, note the last lines in the black window
and send these files from your SIACS workspace folder:
   siacs_startup.log
   siacs_child_started.log
   siacs_child_steps.log
============================================================
