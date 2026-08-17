#Requires -Version 5.1
# install_deps.ps1 -- the Windows-native counterpart of install_deps.sh: builds
# pFUnit, the static NetCDF stack (zlib + HDF5 + netcdf-c + netcdf-fortran) and
# HYPRE from source with the MinGW gfortran/gcc toolchain, and re-applies the
# MinGW patch fortran-yaml-c needs.
#
# Everything is built STATIC on purpose.  funwave.exe must import nothing but
# msmpi.dll and the Windows system DLLs, so a single shared library anywhere in
# the chain fails the deployment gate.  MS-MPI itself stays dynamic (its import
# library is the supported entry point).
#
# Skip switches mirror install_deps.sh -- either the environment variable or the
# matching parameter:
#   SKIP_PFUNIT   -SkipPfunit     SKIP_NETCDF  -SkipNetcdf
#   SKIP_HYPRE    -SkipHypre      SKIP_YAMLC   -SkipYamlc
# PnetCDF has no Windows section at all; see the note near the end.
#
# ASCII only, deliberately: PowerShell 5.1 reads an unsigned .ps1 with the ANSI
# code page, so the box-drawing rules and em dashes of the shell script would
# come back mangled.

[CmdletBinding()]
param(
  [switch]$SkipPfunit,
  [switch]$SkipNetcdf,
  [switch]$SkipHypre,
  [switch]$SkipYamlc
)

# 'Continue', not 'Stop', and deliberately: under 'Stop' PS 5.1 turns ANY write
# to stderr by a native command into a terminating NativeCommandError, and git,
# cmake and ninja all report progress there -- `git clone` dies on its own
# "Cloning into ..." banner.  Exit codes decide instead; every native call is
# followed by Assert-LastExit, which throws.
$ErrorActionPreference = 'Continue'

# PS 5.1 redraws the progress bar on every chunk Invoke-WebRequest receives,
# which costs far more than the transfer itself on a multi-hundred-megabyte
# download.  Leave this off for the whole run; it is restored at the end.
$PrevProgressPreference = $ProgressPreference
$ProgressPreference = 'SilentlyContinue'

$ProjRoot = Split-Path -Parent $PSScriptRoot

if (-not $SkipPfunit) { $SkipPfunit = [bool]$env:SKIP_PFUNIT }
if (-not $SkipNetcdf) { $SkipNetcdf = [bool]$env:SKIP_NETCDF }
if (-not $SkipHypre) { $SkipHypre = [bool]$env:SKIP_HYPRE }
if (-not $SkipYamlc) { $SkipYamlc = [bool]$env:SKIP_YAMLC }

$Jobs = if ($env:NPROC) { $env:NPROC } elseif ($env:NUMBER_OF_PROCESSORS) { $env:NUMBER_OF_PROCESSORS } else { 4 }

# cmake wants forward slashes even on Windows; backslashes inside a -D value
# are read as escapes by some of these projects' own scripts
function ToCMakePath([string]$p) { return ($p -replace '\\', '/') }

# native tools do not raise on failure, so every step is checked by hand
function Assert-LastExit([string]$what) {
  if ($LASTEXITCODE -ne 0) { throw "$what failed (exit $LASTEXITCODE)" }
}

function Find-Tool([string]$name, [string[]]$candidates) {
  foreach ($c in $candidates) {
    if (Test-Path $c) { return $c }
  }
  $onPath = Get-Command $name -ErrorAction SilentlyContinue
  if ($onPath) { return $onPath.Source }
  return $null
}

function Clone-Once([string]$url, [string]$tag, [string]$dir) {
  if (Test-Path (Join-Path $dir '.git')) {
    Write-Output "Using existing clone: $dir"
    return
  }
  Write-Output "Cloning $url @ $tag ..."
  # git reports progress on stderr, which PowerShell renders as a red
  # NativeCommandError block for a clone that worked; stringifying the merged
  # records is no better, since the blank lines come back as
  # "System.Management.Automation.RemoteException".  Capture instead, and print
  # only when the exit code says it actually went wrong.
  $out = git clone --depth 1 --branch $tag $url $dir 2>&1
  if ($LASTEXITCODE -ne 0) { $out | ForEach-Object { "$_" } }
  Assert-LastExit "git clone $url"
}

try {

  # -- toolchain -------------------------------------------------------------
  # The mingw64 and MS-MPI bin directories are usually off PATH; put them in
  # front for this process only.  MINGW_BIN overrides a non-default install.

  $MinGWBin = if ($env:MINGW_BIN) { $env:MINGW_BIN } else { 'C:\Program Files\mingw64\bin' }
  $MpiBin = if ($env:MSMPI_BIN) { $env:MSMPI_BIN } else { 'C:\Program Files\Microsoft MPI\Bin' }
  $env:Path = "$MinGWBin;$MpiBin;" + $env:Path

  foreach ($tool in @('gfortran', 'gcc', 'cmake', 'ninja', 'git', 'python')) {
    if (-not (Get-Command $tool -ErrorAction SilentlyContinue)) {
      throw "$tool not found on PATH (looked for the toolchain under '$MinGWBin')"
    }
  }
  # index, not `| Select-Object -First 1`: that stops the pipeline, which kills
  # the child process and reports a failure for a command that succeeded
  Write-Output "toolchain: $((gfortran --version)[0])"

  # m4 is the one genuinely absent tool on Windows: gFTL hard-REQUIREs it and
  # neither the standalone mingw64 toolchain nor Git for Windows ships one.
  $M4 = Find-Tool 'm4' @(
    'C:\Program Files (x86)\GnuWin32\bin\m4.exe',
    'C:\Program Files\GnuWin32\bin\m4.exe')
  if (-not $M4 -and -not $SkipPfunit) {
    Write-Output "m4 not found -- installing GnuWin32 M4 ..."
    winget install --id GnuWin32.M4 --silent --accept-package-agreements --accept-source-agreements
    $M4 = Find-Tool 'm4' @(
      'C:\Program Files (x86)\GnuWin32\bin\m4.exe',
      'C:\Program Files\GnuWin32\bin\m4.exe')
  }
  if (-not $M4 -and -not $SkipPfunit) {
    throw "m4 is required by gFTL and could not be installed -- run 'winget install --id GnuWin32.M4' by hand"
  }

  # awk, by contrast, is present but invisible: Git for Windows carries gawk
  # just off PATH.  gFTL's vendored test tree find_program()s it REQUIRED, and
  # EXCLUDE_FROM_ALL does not skip configure -- so -DBUILD_TESTING=OFF is no
  # escape and the path has to be handed over explicitly.
  $AWK = Find-Tool 'awk' @(
    'C:\Program Files\Git\usr\bin\gawk.exe',
    'C:\Program Files\Git\usr\bin\awk.exe')
  if (-not $AWK -and -not $SkipPfunit) {
    throw "awk is required by gFTL -- expected Git for Windows at 'C:\Program Files\Git\usr\bin\gawk.exe'"
  }
  Write-Output "m4:  $M4"
  Write-Output "awk: $AWK"

  # -- pFUnit ----------------------------------------------------------------
  # SERIAL ONLY, and not by choice: pFUnit's own CMakeLists sets SKIP_MPI on
  # MinGW after the cache read, so find_package(MPI) never runs and no
  # libpfunit.a / pfunit.mod is produced.  Passing -DSKIP_MPI=NO changes
  # nothing.  The engine's CMake now skips the rank-count unit suites when the
  # install has no parallel umbrella, so the serial subset still builds and
  # runs; the MPI suites remain a Linux/macOS gate.
  #
  # Nothing here needs PYTHONUTF8, but the engine's unit build does, hence the
  # reminder printed at the end of this section: funitproc opens .pf sources
  # with the locale codec, cp1252 here, and several suites carry UTF-8 in their
  # comments.
  #
  # -D_WIN32 is not decoration.  pFUnit drops RegexFilter.F90 from the source
  # list on `if(NOT WIN32)` -- a CMake variable -- while FUnit.F90 still guards
  # `use pf_RegexFilter` on `#ifndef _WIN32`, a preprocessor macro that mingw
  # gcc defines but gfortran's -cpp does NOT.  The two guards therefore
  # disagree here and the build dies on a missing pf_regexfilter.mod.  Defining
  # it makes the Fortran side agree with the CMake side.

  if ($SkipPfunit) {
    Write-Output "SKIP_PFUNIT set -- skipping pFUnit build."
  }
  else {
    $PfSrc = Join-Path $ProjRoot 'extern\pfunit\src'
    $PfBuild = Join-Path $ProjRoot 'extern\pfunit\build'
    $PfInstall = Join-Path $ProjRoot 'extern\pfunit\installed'

    Write-Output "pFUnit source:  $PfSrc"
    Write-Output "pFUnit install: $PfInstall"

    if (-not (Test-Path (Join-Path $PfSrc '.git'))) {
      Write-Output "Cloning pFUnit..."
      $out = git clone --recursive https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git $PfSrc 2>&1
      if ($LASTEXITCODE -ne 0) { $out | ForEach-Object { "$_" } }
      Assert-LastExit 'git clone pFUnit'
    }

    Write-Output "Configuring pFUnit (serial)..."
    cmake -S (ToCMakePath $PfSrc) -B (ToCMakePath $PfBuild) -G Ninja `
      -DCMAKE_BUILD_TYPE=Release `
      -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ `
      -DSKIP_MPI=YES -DSKIP_OPENMP=YES -DSKIP_FHAMCREST=YES `
      -DBUILD_SHARED_LIBS=OFF `
      "-DCMAKE_Fortran_FLAGS=-D_WIN32" `
      "-DM4=$(ToCMakePath $M4)" `
      "-DAWK=$(ToCMakePath $AWK)" `
      "-DCMAKE_INSTALL_PREFIX=$(ToCMakePath $PfInstall)"
    Assert-LastExit 'pFUnit configure'

    Write-Output "Building and installing pFUnit..."
    cmake --build (ToCMakePath $PfBuild) -j $Jobs
    Assert-LastExit 'pFUnit build'
    cmake --install (ToCMakePath $PfBuild)
    Assert-LastExit 'pFUnit install'

    Write-Output "----------------------------------------------------"
    Write-Output "pFUnit installed to: $PfInstall  (SERIAL -- no pfunit.mod)"
    Write-Output "Set PYTHONUTF8=1 before building the unit tests."
    Write-Output "----------------------------------------------------"
  }

  # -- NetCDF stack ----------------------------------------------------------
  # netCDF-4/HDF5 is mandatory, not a preference: the output backend creates
  # every file with NF90_NETCDF4 and uses groups, and the only classic-format
  # writer in the tree is the PnetCDF one.  So the whole zlib -> HDF5 ->
  # netcdf-c -> netcdf-fortran chain has to be built static, into ONE shared
  # prefix, because netcdf-c exports its archive with an empty link interface
  # and no package config anywhere records the dependency chain.
  #
  # Each stage is skipped when its archive is already in the prefix, so a
  # re-run after a failure resumes rather than restarts.

  $NcRoot = Join-Path $ProjRoot 'extern\netcdf'
  $NcSrc = Join-Path $NcRoot 'src'
  $NcBuild = Join-Path $NcRoot 'build'
  $NcInstall = Join-Path $NcRoot 'installed'
  $PFX = ToCMakePath $NcInstall

  if ($SkipNetcdf) {
    Write-Output "SKIP_NETCDF set -- skipping the NetCDF stack."
  }
  else {
    New-Item -ItemType Directory -Force -Path $NcSrc | Out-Null
    Write-Output "NetCDF prefix: $NcInstall"

    Clone-Once 'https://github.com/madler/zlib.git' 'v1.3.1' (Join-Path $NcSrc 'zlib')
    Clone-Once 'https://github.com/HDFGroup/hdf5.git' 'hdf5_1.14.6' (Join-Path $NcSrc 'hdf5')
    Clone-Once 'https://github.com/Unidata/netcdf-c.git' 'v4.9.2' (Join-Path $NcSrc 'netcdf-c')
    Clone-Once 'https://github.com/Unidata/netcdf-fortran.git' 'v4.6.1' (Join-Path $NcSrc 'netcdf-fortran')

    # HDF5's exported targets name ZLIB::ZLIB but hdf5-config.cmake never does
    # find_dependency(ZLIB), so netcdf-c dies at generate time.  Inject the
    # imported target through CMAKE_PROJECT_INCLUDE rather than patching either
    # project.
    $ZPrep = Join-Path $NcRoot 'zprep.cmake'
    @(
      "# HDF5's exported targets reference ZLIB::ZLIB but its config package does no",
      '# find_dependency(ZLIB) -- define the imported target before FindHDF5 runs.',
      'find_package(ZLIB REQUIRED)'
    ) | Set-Content -Encoding ASCII $ZPrep

    # zlib.  BUILD_SHARED_LIBS=OFF does NOT stop zlib 1.3.1 emitting a DLL, so
    # the DLL and its import lib are deleted afterwards and the static archive
    # is given the name FindZLIB looks for.
    if (-not (Test-Path "$NcInstall\lib\libz.a")) {
      Write-Output "=== zlib"
      cmake -S "$(ToCMakePath (Join-Path $NcSrc 'zlib'))" -B "$(ToCMakePath (Join-Path $NcBuild 'zlib'))" -G Ninja `
        -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=gcc -DBUILD_SHARED_LIBS=OFF `
        "-DCMAKE_INSTALL_PREFIX=$PFX"
      Assert-LastExit 'zlib configure'
      cmake --build "$(ToCMakePath (Join-Path $NcBuild 'zlib'))" -j $Jobs
      Assert-LastExit 'zlib build'
      cmake --install "$(ToCMakePath (Join-Path $NcBuild 'zlib'))"
      Assert-LastExit 'zlib install'

      if (Test-Path "$NcInstall\lib\libzlibstatic.a") {
        Copy-Item "$NcInstall\lib\libzlibstatic.a" "$NcInstall\lib\libz.a" -Force
      }
      Remove-Item "$NcInstall\lib\libzlib.dll.a" -ErrorAction SilentlyContinue
      Remove-Item "$NcInstall\bin\libzlib.dll" -ErrorAction SilentlyContinue
      Remove-Item "$NcInstall\bin\zlib1.dll" -ErrorAction SilentlyContinue
    }

    # HDF5.  gcc 15 on mingw-w64 declares _Float16 but ships no FLT16_MAX, so
    # the half-float conversions do not compile -- turn the nonstandard feature
    # off rather than patching H5Tconv_*.c.
    if (-not (Test-Path "$NcInstall\lib\libhdf5.a")) {
      Write-Output "=== hdf5"
      cmake -S "$(ToCMakePath (Join-Path $NcSrc 'hdf5'))" -B "$(ToCMakePath (Join-Path $NcBuild 'hdf5'))" -G Ninja `
        -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ `
        -DBUILD_SHARED_LIBS=OFF -DBUILD_STATIC_LIBS=ON -DBUILD_TESTING=OFF `
        -DHDF5_BUILD_TOOLS=OFF -DHDF5_BUILD_EXAMPLES=OFF -DHDF5_BUILD_UTILS=OFF `
        -DHDF5_BUILD_CPP_LIB=OFF -DHDF5_BUILD_FORTRAN=OFF -DHDF5_BUILD_HL_LIB=ON `
        -DHDF5_ENABLE_Z_LIB_SUPPORT=ON -DHDF5_ENABLE_SZIP_SUPPORT=OFF `
        -DHDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16=OFF `
        -DHDF5_ALLOW_EXTERNAL_SUPPORT=NO `
        "-DZLIB_ROOT=$PFX" "-DZLIB_INCLUDE_DIR=$PFX/include" `
        "-DZLIB_LIBRARY=$PFX/lib/libz.a" `
        "-DCMAKE_PREFIX_PATH=$PFX" "-DCMAKE_INSTALL_PREFIX=$PFX"
      Assert-LastExit 'hdf5 configure'
      cmake --build "$(ToCMakePath (Join-Path $NcBuild 'hdf5'))" -j $Jobs
      Assert-LastExit 'hdf5 build'
      cmake --install "$(ToCMakePath (Join-Path $NcBuild 'hdf5'))"
      Assert-LastExit 'hdf5 install'
    }

    # netcdf-c.  NCstat() (libdispatch/dpathmgr.c) hands a struct stat* to
    # _wstat64(), which writes a struct _stat64 -- 48 bytes of caller buffer
    # for 56 bytes of write, a real overflow that gcc 15 rejects outright.
    # _FILE_OFFSET_BITS=64 makes struct stat 56 bytes with a field-for-field
    # identical layout, after which only the C type identity differs and
    # silencing the diagnostic is safe.  Both flags are needed: the -Wno- alone
    # would ship the overflow.
    if (-not (Test-Path "$NcInstall\lib\libnetcdf.a")) {
      Write-Output "=== netcdf-c"
      cmake -S "$(ToCMakePath (Join-Path $NcSrc 'netcdf-c'))" -B "$(ToCMakePath (Join-Path $NcBuild 'netcdf-c'))" -G Ninja `
        -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=gcc `
        "-DCMAKE_C_FLAGS=-D_FILE_OFFSET_BITS=64 -Wno-incompatible-pointer-types" `
        -DBUILD_SHARED_LIBS=OFF -DENABLE_NETCDF_4=ON -DENABLE_HDF5=ON `
        -DENABLE_DAP=OFF -DENABLE_DAP4=OFF -DENABLE_NCZARR=OFF `
        -DENABLE_BYTERANGE=OFF -DENABLE_PLUGINS=OFF `
        -DENABLE_TESTS=OFF -DENABLE_EXAMPLES=OFF -DBUILD_TESTING=OFF `
        -DBUILD_UTILITIES=ON -DHDF5_USE_STATIC_LIBRARIES=ON `
        "-DCMAKE_PROJECT_INCLUDE=$(ToCMakePath $ZPrep)" `
        "-DHDF5_ROOT=$PFX" "-DHDF5_DIR=$PFX/cmake" `
        "-DZLIB_ROOT=$PFX" "-DZLIB_INCLUDE_DIR=$PFX/include" `
        "-DZLIB_LIBRARY=$PFX/lib/libz.a" `
        "-DCMAKE_PREFIX_PATH=$PFX" "-DCMAKE_INSTALL_PREFIX=$PFX"
      Assert-LastExit 'netcdf-c configure'
      cmake --build "$(ToCMakePath (Join-Path $NcBuild 'netcdf-c'))" -j $Jobs
      Assert-LastExit 'netcdf-c build'
      cmake --install "$(ToCMakePath (Join-Path $NcBuild 'netcdf-c'))"
      Assert-LastExit 'netcdf-c install'
    }

    # netcdf-fortran.  Three things to route around: LINK_TO_SHARED_LIBS
    # defaults ON under WIN32 and compiles the C shim with -DDLL_NETCDF, so
    # every prototype comes back __declspec(dllimport) and the objects
    # reference __imp_nc_* that no static archive can satisfy; the
    # find_package(netCDF) branch feeds an imported TARGET NAME to
    # CHECK_LIBRARY_EXISTS so every probe answers "not found"; and the probes
    # read CMAKE_REQUIRED_INCLUDES from NETCDF_INCLUDE_DIR, which discovery
    # never sets.  Naming real paths skips all three.
    #
    # The examples switch is BUILD_EXAMPLES, not ENABLE_EXAMPLES -- get it
    # wrong and they build, then fail to link, because netcdf-c exports
    # libnetcdf.a with an empty link interface and nothing supplies the
    # HDF5/zlib chain to an example executable.  The library itself is already
    # linked by then, so a driver that does not check the exit code installs a
    # perfectly good prefix and calls it green.
    if (-not (Test-Path "$NcInstall\lib\libnetcdff.a")) {
      Write-Output "=== netcdf-fortran"
      cmake -S "$(ToCMakePath (Join-Path $NcSrc 'netcdf-fortran'))" -B "$(ToCMakePath (Join-Path $NcBuild 'netcdf-fortran'))" -G Ninja `
        -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=gcc `
        -DCMAKE_Fortran_COMPILER=gfortran `
        -DBUILD_SHARED_LIBS=OFF -DENABLE_TESTS=OFF -DBUILD_TESTING=OFF `
        -DBUILD_EXAMPLES=OFF -DLINK_TO_SHARED_LIBS=OFF `
        "-DnetCDF_LIBRARIES=$PFX/lib/libnetcdf.a" `
        "-DnetCDF_INCLUDE_DIR=$PFX/include" `
        "-DNETCDF_INCLUDE_DIR=$PFX/include" `
        "-DCMAKE_REQUIRED_LIBRARIES=$PFX/lib/libhdf5_hl.a;$PFX/lib/libhdf5.a;$PFX/lib/libz.a" `
        "-DCMAKE_PREFIX_PATH=$PFX" "-DCMAKE_INSTALL_PREFIX=$PFX"
      Assert-LastExit 'netcdf-fortran configure'
      cmake --build "$(ToCMakePath (Join-Path $NcBuild 'netcdf-fortran'))" -j $Jobs
      Assert-LastExit 'netcdf-fortran build'
      cmake --install "$(ToCMakePath (Join-Path $NcBuild 'netcdf-fortran'))"
      Assert-LastExit 'netcdf-fortran install'
    }

    # A DLL anywhere in the prefix means something built shared and the
    # deployment gate will fail later, in a much less obvious place.
    $dlls = Get-ChildItem $NcInstall -Recurse -Filter '*.dll' -ErrorAction SilentlyContinue
    if ($dlls) {
      Write-Output "WARNING: shared libraries in the NetCDF prefix -- the deployment gate will fail:"
      $dlls | ForEach-Object { Write-Output "  $($_.FullName)" }
    }

    Write-Output "----------------------------------------------------"
    Write-Output "NetCDF stack installed to: $NcInstall"
    Write-Output "Pass to CMake: -DNETCDF_FORTRAN_ROOT=$PFX"
    Write-Output "  and -DNETCDF_EXTRA_LIBS=$PFX/lib/libhdf5_hl.a;$PFX/lib/libhdf5.a;$PFX/lib/libz.a"
    Write-Output "----------------------------------------------------"
  }

  # -- PnetCDF ---------------------------------------------------------------
  # No Windows section.  PnetCDF buys parallel I/O across nodes, and Windows
  # runs are single-node local by scope; configure stubs it out cleanly and a
  # deck asking for it fails at output init with a proper message.  Build
  # FUNWAVE with -DUSE_PNETCDF=OFF.

  # -- fortran-yaml-c --------------------------------------------------------
  # The fork's CMakeLists runs FortranCInterface_VERIFY(), whose generated
  # project links with gcc and no -lgfortran, so it fails under MinGW even
  # though the mangling detection succeeds.  The skip has to be re-applied
  # after every refresh of the pinned copy -- without it a tests-enabled
  # configure fetches a pristine tree and dies on _gfortran_st_write.
  # Idempotent: safe to re-run.

  if ($SkipYamlc) {
    Write-Output "SKIP_YAMLC set -- skipping the fortran-yaml-c MinGW patch."
  }
  else {
    $YamlDir = if ($env:FUNWAVE_DEPS_DIR) {
      Join-Path $env:FUNWAVE_DEPS_DIR 'fortran-yaml-c'
    }
    else {
      Join-Path $ProjRoot '.deps\fortran-yaml-c'
    }

    if (-not (Test-Path (Join-Path $YamlDir 'CMakeLists.txt'))) {
      Write-Output "Cloning fortran-yaml-c into $YamlDir ..."
      New-Item -ItemType Directory -Force -Path (Split-Path -Parent $YamlDir) | Out-Null
      $out = git clone https://github.com/mayhl/fortran-yaml-c $YamlDir 2>&1
      if ($LASTEXITCODE -ne 0) { $out | ForEach-Object { "$_" } }
      Assert-LastExit 'git clone fortran-yaml-c'
    }

    $cml = Join-Path $YamlDir 'CMakeLists.txt'
    $txt = Get-Content $cml -Raw -ErrorAction Stop
    # the marker test has to come FIRST: the block this inserts contains an
    # indented FortranCInterface_VERIFY() of its own, which the pattern below
    # matches just as happily, so testing for the call first would wrap the
    # already-wrapped call again on every re-run
    if ($txt -match 'skip the link check there') {
      Write-Output "fortran-yaml-c: MinGW VERIFY skip already present."
    }
    elseif ($txt -match '(?m)^\s*FortranCInterface_VERIFY\(\)\s*$') {
      $patched = @"
# MinGW: the VERIFY link step drives gcc without -lgfortran and fails even
# though mangling detection succeeds; skip the link check there
if(NOT (WIN32 AND CMAKE_Fortran_COMPILER_ID STREQUAL "GNU"))
    FortranCInterface_VERIFY()
endif()
"@
      $txt = $txt -replace '(?m)^\s*FortranCInterface_VERIFY\(\)\s*$', $patched
      Set-Content -Path $cml -Value $txt -NoNewline -ErrorAction Stop
      Write-Output "fortran-yaml-c: MinGW VERIFY skip applied."
    }
    else {
      Write-Output "WARNING: FortranCInterface_VERIFY() not found in $cml -- upstream changed, re-check the patch."
    }

    Write-Output "----------------------------------------------------"
    Write-Output "fortran-yaml-c: $YamlDir"
    Write-Output "Pass to CMake: -DFUNWAVE_DEPS_DIR is read from the environment"
    Write-Output "----------------------------------------------------"
  }

  # -- HYPRE -----------------------------------------------------------------
  # Only the 3D (full_dispersion) target needs it.  CMake's FindMPI locates
  # MS-MPI unaided for C and links the MSVC import library directly -- GNU ld
  # consumes COFF import libraries fine for C, so the gendef/dlltool trick is
  # needed only for the Fortran entry points, which the engine's own build
  # generates.

  if ($SkipHypre) {
    Write-Output "SKIP_HYPRE set -- skipping HYPRE build."
  }
  else {
    $HySrc = Join-Path $ProjRoot 'extern\hypre\src'
    $HyBuild = Join-Path $ProjRoot 'extern\hypre\build'
    $HyInstall = Join-Path $ProjRoot 'extern\hypre\installed'

    Write-Output "HYPRE source:  $HySrc"
    Write-Output "HYPRE install: $HyInstall"

    Clone-Once 'https://github.com/hypre-space/hypre.git' 'v2.32.0' $HySrc

    Write-Output "Configuring HYPRE..."
    cmake -S "$(ToCMakePath (Join-Path $HySrc 'src'))" -B "$(ToCMakePath $HyBuild)" -G Ninja `
      -DCMAKE_BUILD_TYPE=Release `
      -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ `
      -DBUILD_SHARED_LIBS=OFF `
      -DHYPRE_ENABLE_SHARED=OFF `
      -DHYPRE_WITH_MPI=ON `
      "-DCMAKE_INSTALL_PREFIX=$(ToCMakePath $HyInstall)"
    Assert-LastExit 'HYPRE configure'

    Write-Output "Building and installing HYPRE..."
    cmake --build "$(ToCMakePath $HyBuild)" -j $Jobs
    Assert-LastExit 'HYPRE build'
    cmake --install "$(ToCMakePath $HyBuild)"
    Assert-LastExit 'HYPRE install'

    Write-Output "----------------------------------------------------"
    Write-Output "HYPRE installed to: $HyInstall"
    Write-Output "Pass to CMake: -DHYPRE_DIR=$(ToCMakePath $HyInstall)"
    Write-Output "----------------------------------------------------"
  }

  # -- what to run next ------------------------------------------------------
  # Quote every -D...=$var: PS 5.1 does not expand a variable inside an
  # unquoted argument that starts with '-' and contains '=', and it fails
  # silently -- the first zlib build here installed into a directory literally
  # named '$PFX'.

  Write-Output ""
  Write-Output "===================================================="
  Write-Output "Configure FUNWAVE with:"
  Write-Output ""
  Write-Output "  cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release ``"
  Write-Output "    -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ ``"
  Write-Output "    -DUSE_MSMPI_F08_SHIM=ON -DUSE_NETCDF=ON -DUSE_PNETCDF=OFF ``"
  Write-Output "    `"-DNETCDF_FORTRAN_ROOT=$PFX`" ``"
  Write-Output "    `"-DNETCDF_EXTRA_LIBS=$PFX/lib/libhdf5_hl.a;$PFX/lib/libhdf5.a;$PFX/lib/libz.a`" ``"
  Write-Output "    `"-DHYPRE_DIR=$(ToCMakePath (Join-Path $ProjRoot 'extern\hypre\installed'))`""
  Write-Output ""
  Write-Output "USE_STATIC_RUNTIME defaults ON here; leave it on or the exe"
  Write-Output "picks up libgfortran/libgcc DLLs and fails the deployment gate."
  Write-Output "===================================================="

}
finally {
  $ProgressPreference = $PrevProgressPreference
}
