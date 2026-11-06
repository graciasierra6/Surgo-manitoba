# SURGO–Manitoba woman-level linkage

This project attaches University of Manitoba facility supply profiles to individual SURGO women in Lagos, Senegal, Côte d'Ivoire and Sindh. It preserves every original woman, source variable, survey weight and row order. The repository contains data preparation, joins and validation; research analyses and models are not included.

## Two reproducible modules

| Module | Aggregation and match | Documentation |
|---|---|---|
| `administrative_area` | Lagos LGA, Senegal/CDI department or Sindh district | [Administrative-area README](administrative_area/README.md) |
| `urban_rural` | The same administrative area plus urban/rural category | [Urban/rural README](urban_rural/README.md) |

Both entry points run end to end from the same checksummed input ZIP. The urban/rural module automatically calls common facility preparation in the administrative module; no prior manual run or intermediate file preparation is required. Each module has its own country configuration, entry point, documentation and output directory. The scripts live directly in these two modules; there is no additional project-name wrapper.

```text
.
├── README.md
├── .gitignore
├── administrative_area/
│   ├── README.md
│   └── R/
│       ├── run_join.R
│       └── configuration, preparation and validation scripts
└── urban_rural/
    ├── README.md
    ├── R/
    │   ├── run_join_urban_rural.R
    │   └── configuration, aggregation and linkage helpers
    └── tests/
        └── test_urban_rural.R
```

## Inputs and installation

Keep `SURGO_Manitoba_linkage_inputs.zip` outside the repository, for example in a locally synced OneDrive folder. Both modules verify its 54 source files against SHA-256 checksums. It includes the original surveys, cleaned facility summaries, archetypes, geographic crosswalks/lookups and reference exports. No Databricks connection or GitHub login is needed to run the joins. Data, caches, generated outputs and ZIPs are excluded from version control.

Requires R 4.1 or later and `dplyr` 1.1.1 or later. Install dependencies once:

```r
install.packages(c("haven", "dplyr", "readr", "jsonlite", "digest", "readxl"))
```

`readxl` is required only by the urban/rural module's Senegal lookup.

## Run from the repository root

Set the input ZIP location to your own external location; the value below is a placeholder, not a machine-specific path:

```sh
export SURGO_MANITOBA_INPUT_ZIP="/path/to/SURGO_Manitoba_linkage_inputs.zip"

Rscript administrative_area/R/run_join.R --geography-version both
Rscript urban_rural/R/run_join_urban_rural.R --geography-version both
```

Each command also accepts `--input-zip` instead of the environment variable and parameterized `--output-dir`, `--cache-dir` and `--country-config` arguments. Relative paths are resolved from the selected module's directory, not the shell's working directory. Paths containing spaces must be quoted. Each command can be run independently. A cache can be shared by giving both commands the same absolute `--cache-dir`.

The default geography version is `github`, which reproduces the author's reviewed crosswalk. The explicit `both` option above additionally writes the documented geographic completions/correction. Version alternatives must not be stacked together.

| Module | Default output root | Default cache |
|---|---|---|
| Administrative area | `administrative_area/output/` | `administrative_area/data/cache/` |
| Urban/rural | `urban_rural/output_urban_rural/` | `urban_rural/data/cache/` |

Within each output root, `datasets/` contains source-crosswalk DTA/RDS files and `checked_geography/` contains the optional completed-geography files. Geographic profiles, dictionaries and audit tables are also exported. Urban/rural preparation and reference reconciliation are in its own `admin_reference/` subdirectory. Original supply definitions, means, pooled ratios, totals and missing-data handling are retained; facility aggregation is unweighted. Added measures are archetype shares and FP counselor availability with valid-response coverage.

## Coverage and decisions

The following counts use the **completed geography** and retain every source woman:

| Geography | Source women | Administrative-area matches | Area + urban/rural matches |
|---|---:|---:|---:|
| Lagos | 4,555 | 4,555 | 4,555 |
| Senegal | 7,169 | 7,169 | 5,354 |
| Côte d'Ivoire | 10,859 | 10,703 | 10,703 |
| Sindh | 10,364 | 10,364 | Unavailable |

- **Senegal:** facility urban/rural is assigned by a health-district lookup, not a verified settlement classification for each facility. The 1,815 unmatched Rural women have no Rural-classified facility records in their departments. This does not establish that rural facilities are physically absent.
- **Côte d'Ivoire:** the same four departments also lack supply matches in the author's export. There are 153 women in those departments plus three records with missing geography. Adding urban/rural introduces no additional nonmatches.
- **Sindh:** facility residence is recorded, but the supplied women's files have no identifiable, documented residence classification. District-only matching is complete. The urban/rural module retains women and flags unavailable residence; an optional validated woman-residence lookup can be supplied later.
- **Unmatched policy:** keep every woman; set only the count of attached facility records to zero; leave unobserved supply percentages missing; record detailed reasons and administrative-area facility counts. An administrative-average fallback with an explicit resolution flag is documented as an alternative and is not applied.

The module READMEs contain the full country issues, crosswalk provenance, affected-department tables, variable definitions, output inventories, flags and alternatives. These are contextual area profiles, not links to a facility attended by a woman or proof of absent services.

## Validation

Every administrative run reproduces the supplied geography-level reference values and reconciles the source crosswalk, archetypes and like-for-like analytical populations. The supplied snapshot passes 69 administrative reference checks and 1,212 urban/rural checks, including original-row/value preservation, exact area/type matching, denominator and facility-count reconciliation, explicit missing-match handling and saved DTA round trips.

Run the deterministic linkage checks from the repository root:

```sh
Rscript urban_rural/tests/test_urban_rural.R
```

No source surveys or generated datasets are committed. Input/output/cache locations are parameters; no personal paths or credentials belong in repository content.
