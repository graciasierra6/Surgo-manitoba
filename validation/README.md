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
