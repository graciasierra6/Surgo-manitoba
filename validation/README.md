# Validation

`check_release.py` checks the actual shared files, input exclusions, source/reference
fingerprints and embedded HTML assets. `verify_reports.py` compares all 15 rebuilt
current outputs with the pre-restructure validated reports. Numerical checks are
also included in each country R script. `compare_analysis.R` independently compares
saved calculations/model results from two builds. Prior sensitivity/training
validation evidence is preserved in `analytical_checks.json`.

Reference updates require reviewed result changes; a reorganization must not replace
reference values merely to make a failing comparison pass. The previous layout and
historical reports remain in Git history. No source surveys or fitted models belong here.

`Rscript validation/test_methods.R` checks survey means, weighted coefficients and
CR2 against independent arithmetic/GLM calculations. `Rscript validation/test_training.R`
checks Sindh training statuses, encodings, conflicts and denominators on fixtures.

## User-interface checks

`Rscript validation/test_entrypoints.R` checks all seven single-report R entry points: definition-only sourcing, automatic installation settings, embedded dependency versions, native file-picker selection/cancellation support, pinned public resource checksums and quoting for spaced Windows arguments. The same scripts support CLI and interactive RStudio Source. `run_all.R` continues to rebuild the complete report/check set.

The current interfaces are exercised on macOS. Windows paths, executable naming, Python-launcher handling and PowerShell instructions are supported in code; a native Windows end-to-end execution is not implied by macOS tests.

The dual CLI/RStudio rebuild evidence is in [entrypoint_validation.json](entrypoint_validation.json). It records the full 15-document comparison, single-script and interactive Source checks, package-restore checks, cold residence-linkage comparisons, and platform limits. The published reports are preserved.
