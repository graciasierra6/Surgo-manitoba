# Reproduce reports from the documented inputs

The project has two geographic join modules and separate analyses using their
outputs. The complete rebuild entry point connects these existing modules; it
does not change scientific definitions or the source survey data.

## Requirements and inputs

1. Obtain the authorized complete input ZIP through the project's data-sharing
   arrangements. It is not distributed in this public repository. Keep the ZIP
   outside the repository and supply its location through `--input-zip` or
   `SURGO_MANITOBA_INPUT_ZIP`.
2. Use R 4.6.0 and restore the recorded packages with
   `Rscript R/install_dependencies.R`. `renv.lock` records the reference versions.
   An R version change or `--latest` package installation is a different environment
   and must be revalidated rather than assumed equivalent.
3. Install Python 3.9 or later and Pandoc. The reference build uses Pandoc 3.8.3.
   Supply its directory with `--pandoc-dir` or `RSTUDIO_PANDOC` if it is not on the
   executable search path. The spatial R package also needs compatible system
   libraries; installing its supported binary package is sufficient where available.

The installer creates an explicit project library beneath `renv/library/`, indexed
by R version and platform. It can copy already-installed packages only when their
versions match the lockfile, then restores any remaining versions and verifies the
final environment. No existing user library or cache is required: when exact
versions are unavailable locally, they are downloaded from the recorded repository.

Some pinned versions may require compilation. A first-time setup then needs a
compatible C/C++ and Fortran toolchain plus the spatial dependencies for `sf`/`s2`,
including OpenSSL, GDAL, GEOS, PROJ and UDUNITS. On macOS, Xcode tools alone do not
provide Fortran; use the compiler matching the R build. See the
[official R macOS build tools](https://mac.r-project.org/tools/) and
[sf installation instructions](https://r-spatial.github.io/sf/#installing).
Missing compiler/runtime or OpenSSL headers are software-prerequisite failures;
they are not resolved by changing the statistical package versions.

The reference complete ZIP contains 55 manifest-listed files, including the Senegal
health-district assignment evidence lookup and the auxiliary department crosswalk.
Every listed input is checked by size and SHA-256 before use. The calculation uses
the evidence lookup; the earlier 54-file bundle also contains that required file
but omits the auxiliary crosswalk. This audit uses the complete bundle.
Repackaging unchanged members may change the ZIP hash; member validation remains
essential. A different source wave requires a reviewed update of definitions and
reference assertions.

See the [complete ZIP inventory](docs/INPUTS.md) for all 55 input/reference filenames
and `bundle_manifest.json`, their roles, sizes and SHA-256 checksums. The
[machine-readable copy](docs/input_bundle_manifest.json) contains metadata only,
not survey records. Reproduction starts from the listed supplied inputs, including
the approved cleaned UoM summaries and archetypes; it does not independently refit
those upstream archetypes or recreate all upstream UoM cleaning.

## One complete build

Run from the repository root after installing dependencies:

```sh
Rscript R/rebuild_all.R --input-zip "$SURGO_MANITOBA_INPUT_ZIP"
```

Optional runtime parameters:

| Parameter | Default | Purpose |
|---|---|---|
| `--output-dir` | `output/reproduction/rebuilt` | New HTMLs and their validation record |
| `--work-dir` | `output/reproduction/work` | Joins, scores, models and calculation intermediates |
| `--cache-dir` | `data/reproduction_cache` | Manifest-verified extracted inputs |
| `--python` | `python3` | Python executable |
| `--pandoc-dir` | `RSTUDIO_PANDOC`, or executable search path | Pandoc directory |
| `--include-historical` | `true` | Include the three earlier report versions |
| `--verify` | `true` | Compare rebuilt output with validated references for the relevant code edition |
| `--resume-after` | `none` | `main` continues after seven already completed, verified country reports |

All paths are parameters resolved at runtime. Relative project/output paths
resolve from the repository root. Quoted input and project paths containing
spaces are supported. The runner refuses to overwrite the reference report
directory. With verification enabled, preflight also requires the recorded R and
package versions; use the documented installer before building. There is no
dependency on a previous developer's cache or fitted
model. Python stages explicitly use UTF-8 to preserve accented geography names
across locale defaults. The individual country entry points and Rmds remain available.

The sequence is: verify/extract inputs; prepare both geography joins; reaggregate
among confirmed FP providers; reproduce four administrative-area reports and three
residence-specific reports; generate diagnostics and coverage tables; reproduce
the earlier reports with their original source; compare all outputs.

If a build stops after all seven current country reports have finished, it can
continue with `--resume-after main`, using the same output, work and cache
parameters. This explicitly reuses completed calculations: the bundle and join
hash are rechecked, all seven reports must match their validated content references,
and each stored numerical-verification record must pass. Missing or mismatched
main results stop the resumed run. The default `none` always recalculates them.

## Current output inventory

Within the caller-selected output directory:

| Folder | HTML outputs |
|---|---|
| `reports/` | `Lagos_analysis_aggregate.html`, `Senegal_analysis_aggregate.html`, `Cote_dIvoire_analysis_aggregate.html`, `Sindh_analysis.html` |
| `reports/` | `Lagos_rural_urban_updated.html`, `Senegal_rural_urban_updated.html`, `CotedIvoire_rural_urban_updated.html` |
| `reports/` | `Demand_population_classification_check.html`, `Score_profile_examples.html`, `Score_profile_contrasts.html`, `Demand_pair_comparisons.html`, `Method_awareness_check_Senegal_Lagos.html`, `Data_checks.html` |
| `data_checks/` | `Surgo_UoM_checks.html`, `Composite score_comparisons.html` |

The repository publishes seven current country reports and the two `data_checks/`
reports. The six diagnostic HTMLs in the third table row and the three historical
HTMLs described below are generated-only outputs. They are not restored to the
published report folder. For these nine outputs only,
`tests/report_reference_hashes.json` supplies the exact SHA-256 from the validated
report set. Rebuilt files must match those hashes when no local reference HTML
exists. All nine published reports must retain their HTML references. This
preserves a complete verification target while distinguishing current published
results from archived or supplementary output.

`Data_checks.html` now has an explicit calculation source,
`analysis/build_data_checks.R`, and a presentation template. The observed numeric
cells in its base tables are recalculated from raw facilities and current analysis
frames. Its illustrative arithmetic examples and authored explanations are
presentation text, not estimated data. The Senegal and Côte d’Ivoire residence
sections are then independently regenerated by their existing update scripts.

The primary need definitions, final survey weights, FP-provider restrictions,
missing-value rules, geographic completions, flagged residence fallbacks, six
components, zero cutoffs and geographic CR2 model inference remain unchanged.
See country READMEs for measurement and linkage qualifications.

## Current published code and report edition

The current scripts use `survey::svymean()` for SURGO weighted means and
`survey::svyglm()` with `ids = ~1`, no strata or FPC, followed by the existing
administrative-area CR2 inference. Supply indices now run higher = greater
constraints, and each current report includes risk distributions and four map
rules: zero, +0.5, +1 and +1.5 final-composite SDs. The seven current country HTMLs and two data-check HTMLs have been updated to
this implementation. Presentations remain unchanged.

`tests/current_build_reference.json` records the validated rebuilt-content
signatures for this edition, current source hashes and published HTML hashes.
Previous reference hashes remain identified separately for provenance. Current
release manifests describe this rebuild; historical references retain their
original definitions and source revision. The default verifier selects the current-code signature
where present and the original reference comparison otherwise. It checks embedded
threshold-map documents as well as visible content, SVG labels, scripts and CSV
values. Labels and integer counts remain exact; floating-point script/CSV tokens
are normalized to seven significant digits for the stored signatures. The derived map document's runtime `source_sha256` is excluded from analytical signatures because
caption changes alter that digest; actual source files and published report bytes
have separate SHA-256 checks. Separate numerical comparisons use the stricter tolerances in
`tests/compare_analysis_update.R`.

Seven analysis rebuilds and all eight current supplementary reports were validated.
The primary weighted percentages, model populations, area flags, coefficients,
CR2 covariance, intervals and adjusted predictions match the earlier outputs to
numerical tolerance. Descriptive confidence intervals can change under the
explicit weights-only design; this occurs in Senegal, Côte d’Ivoire and Sindh.
These are intended variance-specification changes, not changes in percentages.
See [survey and scoring methods](docs/SURVEY_AND_SCORING.md).

## Historical outputs

`Lagos_analysis.html`, `Senegal_analysis.html` and `Cote_dIvoire_analysis.html`
are optional generated historical outputs preserving earlier definitions and
results. They are no longer published as current reports. Their matching source is pinned to public revision
`d2452b7f364f449ae6824db7a312a4217de8858b`; the downloaded source archive is checked
against a fixed SHA-256 in `tests/rebuild_historical_reports.py`. The source is
retrieved read-only through the public repository API; no sign-in is needed.
Its original joins can reuse the newly verified input bundle and checked geography,
but calculations are rerun. Historical source and models stay in working output.

## What is checked

The country workflows independently check original woman/field preservation,
geography matches, raw FP eligibility, valid facility means including genuine
zeros, demand coding and denominators, component standardization, score direction,
categories, model samples, weighted coefficients, uncertainty and final HTML.
The base joins also reconcile upstream aggregates and reference assignments.

`tests/verify_rebuilt_reports.py` compares the published and newly built HTMLs:
visible content, embedded downloadable tables, scripts containing map values,
figure counts and SVG figure labels. It also records exact SHA-256 agreement.
Floating-point values in scripts and downloadable tables permit only 1e-9 absolute
or 1e-7 relative roundoff; integer strings, geography identifiers, labels and
classifications must match exactly. The comparator's own tests check these rules.
Raster figure appearance is not a numerical equality test; its underlying reported
statistics are checked separately by the calculation and content checks. Fonts,
graphics libraries and Pandoc can change file bytes or layout across platforms.
Numerical equivalence and byte-identical rendering are therefore reported separately.
The generated-only supplementary and historical fingerprints require exact bytes when their
reference HTMLs are absent. A platform rendering difference in these outputs is
reported as a verification failure and requires review; it is not silently accepted.

The result is `reproduction_validation.json` inside the selected output directory.
It contains relative report names, hashes and comparison outcomes, not input paths
or respondent records. Source/release inspections are separate:

```sh
python3 tests/check_rural_urban_release.py
```

Passing reproduction checks establishes implementation consistency for the
documented input snapshot. It does not certify the sampling design, causal
interpretation, geography boundaries or precision of an area's composite score.

## What to upload

The release contains source, documentation, the nine current published HTMLs and
validation metadata. It contains no input datasets or fitted objects. Preserve the
repository-root structure, including `.Rprofile` and `.gitignore`; do not add an
extra outer directory. Current report filenames are retained. Presentations are
unchanged; Git history and earlier commit dates are preserved. Generated-only
diagnostics and historical HTMLs remain outside the published report folders.

Do not upload the input ZIP, extracted inputs, joins, fitted objects,
working calculation folders, local execution logs or respondent-level outputs.
The root ignore rules cover those working files. A clean upload must still be
inspected before sharing; ignore rules alone are not a privacy check.
