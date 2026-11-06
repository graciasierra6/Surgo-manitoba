# Administrative area + urban/rural linkage

This module aggregates Manitoba facility supply measures by **administrative area and urban/rural category**, then attaches the corresponding profile to every original SURGO woman. Areas are Lagos LGAs, Senegal/CDI departments and Sindh districts. All source women, variables, weights and row order are retained. The module performs linkage and validation only.

## Inputs and dependencies

Use the supplied `SURGO_Manitoba_linkage_inputs.zip`, stored outside the repository. It includes source women and facilities, cleaned facility indicators, archetypes, reviewed geographic mappings, residence lookup and reference exports. Source files are verified against SHA-256 checksums.

This entry point runs end to end without a prior manual preparation step. It automatically calls common facility preparation in the sibling `administrative_area` source module and writes its own intermediate files to `admin_reference/` under this module's output directory. Keep that source module in the project; no pre-existing output dataset is required. Detailed administrative-crosswalk provenance is documented in the [administrative module](../administrative_area/README.md); this README documents the residence-specific aggregation and join.

Requires R 4.1 or later, `dplyr` 1.1.1 or later, and the packages below. `readxl` reads the Senegal residence lookup.

```r
install.packages(c("haven", "dplyr", "readr", "jsonlite", "digest", "readxl"))
```

## Run

From the `urban_rural/` module directory:

```sh
Rscript R/run_join_urban_rural.R --input-zip "/path/to/SURGO_Manitoba_linkage_inputs.zip" --geography-version both
```

From the repository root, use `Rscript urban_rural/R/run_join_urban_rural.R` with the same arguments. The input ZIP can instead be specified through `SURGO_MANITOBA_INPUT_ZIP`. Paths containing spaces must be quoted. Relative paths are resolved from the **urban_rural module directory**, regardless of the shell's working directory.

| Argument | Default | Purpose |
|---|---|---|
| `--input-zip` | `SURGO_MANITOBA_INPUT_ZIP` environment variable | External checksummed input bundle. |
| `--output-dir` | `output_urban_rural` | This module's datasets, profiles and audits. |
| `--cache-dir` | `data/cache` | Verified extracted inputs. |
| `--geography-version` | `github` | `github` exports source-crosswalk profiles; `both` also exports the documented completed/corrected geography. |
| `--country-config` | `R/00_config.R` | This module's source files, woman IDs, weights and administrative keys. |
| `--urban-rural-config` | `R/urban_rural_config.R` | Residence fields and classification sources. |
| `--administrative-module-dir` | `../administrative_area` | Location of the common preparation code. |
| `--sindh-residence-lookup` | Not supplied | Optional validated woman-residence lookup. |

`datasets/` uses the source crosswalk; `checked_geography/` contains the optional completed-geography files. These are alternative geographic versions and must not be stacked. Output and cache directories must differ. The script rejects an output directory already identified as another join component.

## Residence fields and qualifications

| Geography | SURGO woman field | Facility classification source | Qualification |
|---|---|---|---|
| Lagos | `settlement` | Raw `location`, checked against cleaned `rurb` | Recorded woman and facility categories. |
| Senegal | `FV_MILIEU` | Health district `s1_3` matched to `SenegalURlookup.xlsx`, checked against cleaned `rurb` | A **health-district proxy**, not each facility's recorded settlement type. |
| Côte d'Ivoire | `FV_MILIEU` | Raw `s1_5`, checked against cleaned `rurb` | Recorded woman and facility categories. |
| Sindh | Unavailable in supplied files | Raw `location`, checked against cleaned `rurb` | Facility profiles can be calculated; women cannot be assigned without a residence lookup. |

Accepted values are Rural/Urban labels, French `URBAIN`, and documented codes 1=Rural / 2=Urban. Blank, special and unrecognized values remain unclassified. No residence category is inferred from a district name, cluster-code digits or the predominant facility type.

Senegal's lookup assigns one category to an entire health district, so an Urban-classified district may contain facilities in rural settlements. `link_rurb_proxy` and `link_rurb_basis` identify this qualification. Nine Yeumbeul facility records have no lookup classification; they remain in the preparation audit but are excluded from residence-specific profiles.

## Aggregation and join

1. Verify and prepare the source facility summaries, geographic assignments, archetypes and counselor responses.
2. Read and harmonize the recorded woman residence field and the facility classification. Check raw/lookup classifications against cleaned `rurb`.
3. Group facilities by **country + selected administrative area + residence category**. Facilities without a usable area or residence category do not enter Rural/Urban cell denominators; their exclusion is counted in the audit.
4. Recalculate the 121 supply measures within each cell using the inherited definitions: facility means, pooled ratios, totals and counts. Add 18 archetype shares and counselor availability with valid-response counts/coverage. Facility aggregation is unweighted; woman survey weights are preserved for later analysis.
5. Attach each unique cell profile to women using both area and residence. Missing residence never matches missing facility residence. A Rural woman in department A receives its Rural-classified facility profile; an Urban woman receives its Urban profile.
6. Retain every source woman and verify the saved DTA/RDS outputs, exact matches, source values, facility counts and denominators.

The supply definitions are inherited; this module changes the aggregation grain, not the underlying offering, stockout or training coding. Archetype shares use all included facility records within each cell. Counselor availability uses Yes/(Yes+No) to Q702 in Lagos/Sindh or s7_2 in Senegal/CDI, with missing/special responses excluded from that indicator's denominator. It measures availability of an advisor/counselor, not counseling receipt or quality. Sparse counselor response coverage in Senegal and Sindh must accompany reported percentages.

## Missing matches: selected policy and alternative

| Option | Treatment | Status |
|---|---|---|
| **Strict area + residence match** | Retain the woman, set the count of attached facility records to **0**, keep unobserved supply estimates **missing**, and flag the reason. | **Implemented for all four geographies.** |
| **Broader administrative-average fallback** | Attach the area's overall supply profile when the exact cell is unavailable, with an explicit fallback/resolution flag. | **Not implemented.** Prepared broader profiles are available separately in this module's `admin_reference/` for a later decision. |

`n_facilities = 0` means no exact-match profile is attached. It does not establish physical absence of facilities, and also applies when a woman's residence is unknown. Do not replace missing percentages with zero: zero trained-provider availability would claim no trained staff, while zero stockouts would claim uninterrupted stock. An unmatched profile supports neither conclusion.

`link_has_supply` indicates an attached exact profile. `link_has_admin_supply` indicates whether a broader area profile exists without substituting it. `link_status` is the overall match status; `link_issue` records the detailed reason:

| `link_issue` | Meaning |
|---|---|
| `matched` | Exact area + residence profile attached. |
| `no_rural_classified_records` | Area has Urban-classified records but none classified Rural; a Rural woman remains unmatched. |
| `no_urban_classified_records` | Area has Rural-classified records but none classified Urban; an Urban woman remains unmatched. |
| `facility_type_unavailable` | Area has facility records, but none has a usable residence classification. |
| `no_area_facility_records` | No facility records are mapped to the woman's area under the selected crosswalk. |
| `woman_residence_unavailable` | Supplied woman dataset has no usable residence field. |
| `missing_woman_residence` | Residence is missing/unrecognized for this woman or absent from a supplied lookup. |
| `missing_woman_geography` | Woman's administrative geography is missing. |
| `no_area_type_profile` | Other absent exact profile; inspect the audit. |

`link_n_admin_fac`, `link_n_admin_rural`, `link_n_admin_urban` and `link_n_admin_unknown` show included facility counts in the woman's area. `link_single_facility` flags an attached profile based on one facility. Neither a classified cell nor its absence establishes an independent service catchment: women may travel across these boundaries.

## Residence-specific match results

All counts below are unweighted and all original women remain in the output. The first table uses completed geography.

| Geography | Women retained | Rural matched / Rural women | Urban matched / Urban women | Unmatched |
|---|---:|---:|---:|---:|
| Lagos | 4,555 | 222 / 222 | 4,333 / 4,333 | 0 |
| Senegal | 7,169 | 1,186 / 3,001 | 4,168 / 4,168 | 1,815 |
| Côte d'Ivoire | 10,859 | 4,498 / 4,621 | 6,205 / 6,235 | 156 |
| Sindh | 10,364 | Unavailable | Unavailable | 10,364 |

| Geography | Source-crosswalk area/type matches | Completed-geography area/type matches |
|---|---:|---:|
| Lagos | 4,555 | 4,555 |
| Senegal | 5,020 | 5,354 |
| Côte d'Ivoire | 10,556 | 10,703 |
| Sindh | 0 | 0 |

### Senegal: Rural women without Rural-classified facility profiles

In completed geography, 1,815 Rural women in the departments below have no Rural-classified facility profile. All 755 included facilities in these departments inherit Urban from the health-district lookup. This is a classification/coverage mismatch, not proof that rural facilities are physically absent.

| Department | Unmatched Rural women | Rural-classified facility records | Urban-classified facility records |
|---|---:|---:|---:|
| Birkelane | 124 | 0 | 24 |
| Dagana | 66 | 0 | 45 |
| Foundiougne | 96 | 0 | 61 |
| Goudomp | 73 | 0 | 23 |
| Kaffrine | 121 | 0 | 41 |
| Kanel | 99 | 0 | 25 |
| Kaolack | 110 | 0 | 73 |
| Kedougou | 26 | 0 | 26 |
| Kolda | 57 | 0 | 46 |
| Koumpentoum | 45 | 0 | 25 |
| Linguere | 48 | 0 | 51 |
| Mbacke | 253 | 0 | 64 |
| Nioro | 196 | 0 | 60 |
| Sedhiou | 102 | 0 | 30 |
| Thies | 235 | 0 | 85 |
| Tivaouane | 164 | 0 | 76 |
| **Total** | **1,815** | **0** | **755** |

These women receive `link_issue = no_rural_classified_records`, no attached profile and missing supply estimates. Review lookup provenance and obtain facility-specific residence classifications or validated geographic information before interpreting these as actual rural shortages. The issue audit covers both crosswalk versions; this table describes completed geography.

### Côte d'Ivoire: inherited geographic nonmatches

The completed residence-specific join leaves the following women unmatched because no facility profile is mapped to their department:

| Department | Rural women | Urban women | Total women |
|---|---:|---:|---:|
| Bonon | 29 | 30 | 59 |
| Facobly | 37 | 0 | 37 |
| Gohitafla | 23 | 0 | 23 |
| Taabo | 34 | 0 | 34 |
| **Total** | **123** | **30** | **153** |

Three additional women have missing administrative/residence information and remain flagged. The same four departments have zero matched facility records in the author's geographic export. These nonmatches predate residence stratification: adding urban/rural causes no additional nonmatches among women with a mapped area profile. Future review must distinguish survey coverage from different health-district/department catchments. Geographic-completion details belong to the administrative-crosswalk documentation; this module retains the selected mapping and flags the remaining gaps.

### Sindh: woman residence type unavailable

The facility `location` field records Rural for 2,409 facilities and Urban for 1,899. The supplied weighted SURGO file, `20250124_forms_wt.dta`, has 10,364 women and 1,359 variables, but no identifiable, documented woman residence classification. Names, available variable/category labels, dictionaries and the Pakistan questionnaire were reviewed. `RB0e` is described as Town/Village/Primary Sampling Unit and contains numeric identifiers; it is not a Rural/Urban category. `clusterCode` is also an identifier. Neither is recoded from its digits.

Without a validated residence lookup, all Sindh women remain with `link_issue = woman_residence_unavailable`, `link_has_supply = FALSE`, `n_facilities = 0` and missing stratified supply estimates. Their district-only preparation profiles remain available in this module's `admin_reference/`. This is incomplete residence linkage, not absence of facility data.

A future validated CSV may supply unique, nonmissing `col_id` and `rurb` fields, with Rural/Urban classifications linked to each woman or her documented sampling unit. A district's majority type or a facility's type is insufficient. The program checks IDs, classifications and coverage, records the lookup checksum and retains women absent from it as unmatched; source review must establish geographic validity.

```sh
Rscript R/run_join_urban_rural.R --input-zip "/path/to/SURGO_Manitoba_linkage_inputs.zip" --geography-version both --sindh-residence-lookup "/path/to/sindh_residence.csv"
```

## Outputs

All locations below are under this module's configured output directory.

- `datasets/*_women_manitoba_urban_rural.dta` and `.rds`: source-crosswalk woman-level files.
- `checked_geography/`: optional completed-geography woman-level files.
- `supply_profiles/`: one row per country, administrative area and classified residence category for each selected mapping version.
- `audit/urban_rural_source_coding.csv`: observed source codes/labels, harmonized categories and counts on both sides.
- `audit/facility_area_residence_assignments.csv`: facility IDs, administrative targets, residence category and classification basis.
- `audit/senegal_district_residence_lookup.csv`: proxy assignments and absent lookup matches.
- `audit/area_residence_coverage.csv`: women/facilities in each area/category, including missing classifications and geography.
- `audit/linkage_issue_summary.csv`: women by area/category, detailed issue and administrative-area facility counts.
- `audit/woman_level_merge_checks.csv` and `facility_residence_coverage.csv`: retained/matched women and exclusions from facility denominators.
- `audit/urban_rural_validation_checks.csv`: exact matching, preservation and denominator checks.
- `dictionaries/`: supply definitions qualified to this aggregation grain and linkage-field definitions.
- `run_parameters.json`: component, mapping version, input/lookup checksums, policy and limitations.
- `admin_reference/`: module-owned preparation outputs and reference reconciliation, generated automatically on this run.

RDS preserves source classes and labels. DTA round-trip checks allow Stata's representation of missing strings as empty strings and verify every saved column's values. Source data, caches and outputs are excluded from version control.

## Validation and module files

The supplied snapshot passes **1,212 residence-specific validation checks**. These cover raw/lookup agreement with cleaned residence fields, unique area/category profiles, exact two-field matching, missing types never matching, original women/values, zero attached-record counts for nonmatches, detailed issue flags, facility counts, counselor denominators, coverage-table totals and DTA round trips. The automatic preparation also passes its reference reconciliation. These checks validate technical linkage; they do not certify residence proxies, physical service access or causal effects.

From `urban_rural/`, run the deterministic helper checks:

```sh
Rscript tests/test_urban_rural.R
```

| File | Role |
|---|---|
| `R/run_join_urban_rural.R` | Parameterized entry point and automatic preparation dependency. |
| `R/00_config.R` | Module-local country/source configuration passed to preparation. |
| `R/urban_rural_config.R` | Woman/facility residence fields and proxy source. |
| `R/urban_rural_helpers.R` | Category normalization, exact keys, optional lookup and issue reasons. |
| `R/06_join_urban_rural.R` | Residence-specific aggregation, exports, dictionaries and audits. |
| `tests/test_urban_rural.R` | Coding, missing matches, duplicate rejection and issue-reason checks. |

The project overview is in the [root README](../README.md). Keep this module's documentation with its scripts; administrative-only execution and results are documented in that module rather than repeated here.
