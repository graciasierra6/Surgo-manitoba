# Administrative-area linkage

This project joins University of Manitoba facility supply profiles to every original SURGO woman's record in Lagos, Senegal, Côte d'Ivoire and Sindh. It performs data preparation, geographic linkage and validation only. It does not fit models, calculate research results or create a demand/supply constraint score.

## Inputs

Keep the supplied `SURGO_Manitoba_linkage_inputs.zip` outside this repository, for example in a locally synced OneDrive folder. It contains the original surveys, cleaned facility summaries, archetype assignments, geographic crosswalks, documented geographic lookup tables, and source outputs needed for validation. Original dataset bytes are retained and verified against SHA-256 checksums.

No Databricks connection or credentials are required. Input, output, cache and country-configuration locations are parameters; no personal paths are embedded in the scripts.

This module aggregates facilities to the shared administrative area: LGA in Lagos, department in Senegal and Côte d'Ivoire, and district in Sindh. It is the administrative-only component of the [project](../README.md). The [urban/rural module](../urban_rural/README.md) provides a separate finer option.

Run the commands below from `administrative_area/`. Alternatively, invoke `Rscript administrative_area/R/run_join.R` from the repository root. Relative input/output/cache/configuration paths are resolved from this module directory; absolute paths or the input-ZIP environment variable can be used for external storage.

## Run

Requires R 4.1 or later with `haven`, `dplyr` 1.1.1 or later, `readr`, `jsonlite` and `digest`.

```r
install.packages(c("haven", "dplyr", "readr", "jsonlite", "digest"))
```

From the repository folder:

```sh
Rscript R/run_join.R --input-zip "/path/to/OneDrive/SURGO_Manitoba_linkage_inputs.zip" --output-dir "/path/to/results"
```

The ZIP can instead be specified through the `SURGO_MANITOBA_INPUT_ZIP` environment variable. Paths containing spaces must be quoted. Relative paths are resolved from the repository root. The cache defaults to `data/cache`; override it with `--cache-dir`. Country file names, question fields, woman IDs, weight fields and join levels are centralized in `R/00_config.R`; override that file's location with `--country-config`.

## Geographic versions

The default `--geography-version github` reproduces the supplied GitHub crosswalk. Main woman-level files are written to `datasets/` under the configured output directory.

To also export the separately checked geographic assignments:

```sh
Rscript R/run_join.R --input-zip "/path/to/inputs.zip" --output-dir "/path/to/results" --geography-version both
```

The additional files are written to `checked_geography/`. Both versions retain the same source women and supply-variable definitions. They are alternatives and must not be stacked together.

| Geography | SURGO source | Records retained | Join geography |
|---|---|---:|---|
| Lagos | Main weighted sample | 4,555 | LGA |
| Senegal | Original women's file | 7,169 | Department |
| Côte d'Ivoire | Original women's file | 10,859 | Department |
| Sindh | Original weighted forms | 10,364 | District |

These are raw source-record totals, including records outside the author's analytical cohort. No woman is removed for consent, need, weight or missing supply. Analysts must later define their analytical population explicitly.

## Join steps

1. Verify the input bundle and extract/check cached files.
2. Match cleaned facility summaries, archetypes and new-client volumes by facility ID and geography of study (`fid` plus `loc`). Lagos uses the survey `key`; blank Sindh keys follow the documented facility-ID fallback.
3. Reconstruct geography from the granular survey and validate it against the supplied facility-geography lookup.
4. Apply the supplied reviewed crosswalk: Lagos LGA, Senegal department, Côte d'Ivoire department and Sindh district. There is no direct woman-to-attended-facility match.
5. Reproduce the original geographic supply indicators using facility means, pooled ratios, totals and category counts.
6. Add archetype shares and FP counselor availability.
7. Attach the one-row-per-area supply profile to individual SURGO women using a many-to-one left join. Retain all original columns, values and row order.
8. Validate the saved files, source archetype assignments, complete crosswalk, geographic coverage and like-for-like source analytical populations.

## Supply variables and additions

The 121 original supply columns retain their names and definitions. Their upstream coding is inherited; this project does not redefine the original offering, stockout or training indices.

- **Archetype shares:** counts divided by included facility records. Eighteen share columns cover the alternative k=3,4,5,6 solutions. Shares sum to one separately within each solution. Values are 0–1; multiply by 100 for percentages.
- **FP counselor availability:** Yes divided by Yes+No to Q702 in Lagos/Sindh or s7_2 in Senegal/CDI. Senegal's labeled No codes 0 and 2 are accepted. Other special/blank answers remain missing. Companion columns provide valid, missing, Yes/No counts and response coverage. This measures counselor/advisor availability, not observed counseling provision or quality.

Counselor responses are sparse in Senegal and Sindh. Reporting coverage must accompany the resulting percentages; missing answers are not evidence that a counselor is unavailable.

Matching fields use a `link_` prefix. `link_source_row` identifies the original source row; `link_has_supply` flags a matched geographic profile. Unmatched women remain with missing supply estimates. Their `n_facilities=0` means no included matched profile, not proof that no facilities exist locally.

## Proposed geographic completions and correction

Among areas already mapped by the author, our department/LGA targets agree for all 17 Lagos areas, 43 of 44 Senegal health districts, all 88 Côte d’Ivoire health districts and all 30 Sindh districts. Senegal has one existing-target conflict (Ziguinchor to Bignona); the other differences mainly complete previously unmatched source groups. The `checked_geography` folder and internal `audited_geography` label identify a proposed alternative, not a certification of every assignment. The default GitHub-crosswalk output remains unchanged.

- **Lagos:** recorded LGA/name mapping is unchanged. An inconsistency in the unused IBILE parent field does not affect the LGA join.
- **Senegal:** a documented health-district-to-department lookup adds supported matches and removes the Ziguinchor-to-Bignona allocation. Diakhao, Sangalkam and Yeumbeul retain time/boundary qualifications, recorded in linkage flags.
- **Côte d'Ivoire:** a companion health-district codebook restores raw s1_2 labels for codes 1–13 after checking correspondence for observed codes 14–113. This recovers Agboville, Sikensi and Tiassale; parent and subdistrict mappings remain explicit.
- **Sindh:** the fully populated raw `district` field supplies named districts for records whose `district_name` is blank. Explicit aliases resolve spelling/name variants. Existing source matches must agree.

Evidence, full source mappings and lookup tables are in the input bundle. Every facility's source and checked target, reason and qualification is exported. These are contextual geographic assignments; no facility-coordinate overlay or exact attended-facility linkage is available.

### Sindh: district fields and completion of missing assignments

The source pipeline constructs Sindh facility geography from `division` and `district_name`, within Sindh, before applying the reviewed crosswalk. The underlying facility file also contains a complete, labeled `district` field:

| Facility field | Source label | Meaning and completeness in the supplied snapshot |
|---|---|---|
| `division` | Health division as per district | Broader grouping containing several districts; populated for all 4,308 facilities. |
| `district_name` | district_name | Text district name; populated for 3,427 facilities and blank for 881. |
| `district` | Name of District | Numeric district code with district-name value labels; populated for all 4,308 facilities. |

The 881 records with blank `district_name` retain a division: 628 are in Karachi division and 253 in Hyderabad division. They remain unmatched in the source district crosswalk. Their labeled `district` values explicitly identify 11 districts: Karachi Central, Karachi East, Karachi Keemari, Karachi Korangi, Karachi Malir, Karachi South, Karachi West, Hyderabad, Matiari, Tando Allah Yar and TM Khan. No district is inferred from division alone.

For the checked alternative, the code reads the recorded `district` value labels, harmonizes documented name variants, and uses the corresponding SURGO `rb0c` district names as the final join labels. For example, `Karachi Central` maps to `KARACHI CENTRAL DISTRICT` and `TM Khan` to `TANDO MUHAMMAD KHAN`. Facility indicators are aggregated within district before attaching the profile to each woman's recorded `rb0c` district. Original source fields are preserved.

Two checks confirm continuity with the source approach: all 3,427 populated `district_name` values agree with the labeled `district` values after name normalization, and all 3,427 previously matched facility assignments agree with the source crosswalk. Reading `district` therefore gives the same completed assignments as retaining the existing division/district-name assignments and filling only the 881 blanks from `district`.

Both versions cover all 30 SURGO districts and all 10,364 original women. The GitHub-crosswalk version includes 3,427 facilities in district supply profiles; the checked alternative includes all 4,308. The additional facilities enter 11 districts that already had some matched facilities, so their supply summaries can change without changing any woman's district. The default remains the source-crosswalk version; use `--geography-version both` to export the completed alternative separately. Complete linkage coverage is not independent verification of every facility's physical location.

## Outputs

- `datasets/`: four woman-level DTA and RDS datasets using the GitHub crosswalk.
- `checked_geography/`: optional alternative woman-level files.
- `supply_profiles/`: geographic supply profiles for both mapping versions.
- `audit/`: source-row, denominator, geography, value-preservation and source-supply checks.
- `reconciliation/`: the exact source crosswalk, all-row comparison, expanded assignment flows, coverage/population comparisons, archetype checks and classification-label reconciliation.
- `dictionaries/`: original supply definitions and added-variable definitions.

RDS preserves original R classes and labels. Stata stores missing character values as empty strings; numeric values and source records are checked after DTA round-trip.

## Validation scope

For the supplied snapshot, the reference version reproduces all geographic supply values and overlapping populations when restricted to the same analytical women as the author. Raw row totals differ intentionally: the author's analytical files exclude 5 Lagos records, 3 Côte d'Ivoire records and 438 Sindh records. Three Côte d'Ivoire source records have blank woman IDs; they are retained using original row identity.

Archetype assignments are checked against the supplied pipeline file. The older curated output renumbers clusters for k=3–5 while retaining identical partitions; k=6 names/codes match. Thirteen legacy Sindh archetype records with blank facility IDs are reconciled through their unique survey IDs.

The default version is a faithful reproduction, not a certification of every upstream measurement assumption. Checked-geography differences are listed explicitly. Remaining boundary qualifications and incomplete counselor coverage prevent an unconditional substantive audit sign-off. No research-model validity or causal interpretation is assessed here.

## Repository contents

- `R/run_join.R`: parameterized entry point and input verification.
- `R/00_config.R`: source fields and geographic levels.
- `R/helpers.R`: reading, labels, keys and safe joins.
- `R/geography.R`: source geography and documented alternatives.
- `R/aggregate_supply.R`: geographic aggregation and parity checks.
- `R/04_join.R`: facility integration and woman-level exports.
- `R/05_validate.R`: complete crosswalk, classification and population reconciliation.

Data, ZIP files, cached inputs and generated outputs are excluded from version control. Store the input bundle separately in OneDrive; commit only project documentation, R scripts and `.gitignore`.
