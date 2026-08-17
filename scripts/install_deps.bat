@echo off
rem Double-clickable entry point for install_deps.ps1.  All the logic lives in
rem the .ps1; this exists only so the script can be launched from Explorer and
rem so -ExecutionPolicy Bypass covers this one process instead of the machine
rem policy being changed.  %~dp0 keeps it working from any cwd; %* forwards the
rem skip switches (-SkipPfunit, -SkipNetcdf, -SkipHypre, -SkipYamlc).
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0install_deps.ps1" %*
