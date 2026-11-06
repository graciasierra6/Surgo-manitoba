# Administrative area + urban/rural linkage

This separate module aggregates the same facility supply measures by administrative area **and** urban/rural category, then attaches the appropriate profile to each original SURGO woman. It performs linkage and validation only.

This module runs end to end from the input ZIP without a prior manual run of the other module. It automatically invokes the sibling `administrative_area` module for common facility preparation and reference checks, writes those intermediate outputs under its own `admin_reference/`, and then builds its own area/type profiles and datasets. Keep both source modules in the project. The dependency is source code, not a pre-existing output dataset.

Run the commands below from `urban_rural/`. From the repository root, invoke `Rscript urban_rural/R/run_join_urban_rural.R`. Relative CLI paths are resolved from this module directory. Its `R/00_config.R` and `R/urban_rural_config.R` are module-local; the country configuration is passed explicitly to the automatic preparation stage. To relocate the common source module, use `--administrative-module-dir`; its default is `../administrative_area`.

See the [project overview](../README.md) and [administrative module](../administrative_area/README.md) for the input inventory, original supply definitions and detailed geographic-crosswalk review.

## Run


Requires R 4.1 or later and `haven`, `dplyr` 1.1.1 or later, `readr`, `jsonlite`, `digest` and `readxl`. The last package reads the existing Senegal lookup in the supplied input ZIP:

```r
install.packages(c("haven", "dplyr", "readr", "jsonlite", "digest", "readxl"))
```

Run this component independently, using a different output directory:

```sh
Rscript R/run_join_urban_rural.R --input-zip "inputs.zip" --output-dir "output_urban_rural" --geography-version both
```

`inputs.zip` is a placeholder for the supplied input ZIP, whose location can also be set through `SURGO_MANITOBA_INPUT_ZIP`. The input ZIP and cache can be shared between both components. The second component first runs the existing preparation and reference checks in `output_urban_rural/admin_reference/`, then writes its own stratified profiles and woman-level datasets in the separate output root. The original geography-only output directory is not used. The default `--geography-version github` uses the supplied author crosswalk. `--geography-version both` additionally exports the documented completed/corrected geography. For these versions: `datasets/` reproduces the source crosswalk; `checked_geography/` applies the documented geographic completions/correction.

### Fields and qualifications

| Geography | SURGO residence type | Facility residence type | Qualification |
|---|---|---|---|
| Lagos | `settlement` | Raw `location`, checked against cleaned `rurb` | Recorded woman and facility classifications. |
| Senegal | `FV_MILIEU` | Raw health district `s1_3` matched to `SenegalURlookup.xlsx`, checked against cleaned `rurb` | **Health-district classification proxy**, not a recorded settlement classification for each facility. |
| Côte d'Ivoire | `FV_MILIEU` | Raw `s1_5`, checked against cleaned `rurb` | Recorded woman and facility classifications. |
| Sindh | Unavailable in the supplied SURGO files | Raw `location`, checked against cleaned `rurb` | Facility strata can be constructed, but women cannot be assigned to them without an additional validated residence lookup. |

Only the explicit Rural/Urban labels, French `URBAIN`, and documented codes 1=Rural / 2=Urban are harmonized. Other codes, missing and unrecognized values remain unclassified. Sindh `rb0e` and `clusterCode` contain cluster identifiers; neither is recoded to urban/rural from its digits. The current source files do not supply a validated cluster-to-residence mapping.

Senegal's lookup assigns one urban/rural label to an entire health district. Facilities in an urban-classified health district may still be situated in rural settlements. Therefore a department x assigned-health-district-type profile is only a proxy for the urban/rural service environment women experience. `link_rurb_proxy` and `link_rurb_basis` identify this qualification on every Senegal woman's row. The source-coding and lookup audit tables retain the assignments and unmatched health districts. Do not describe this as a verified settlement-level linkage.

### Aggregation and missing matches

Within each selected geographic version, facilities are grouped by **country + administrative geography + urban/rural**. The same 121 supply definitions, archetype counts/shares and counselor definitions are recalculated within each cell using unweighted facility aggregation. Indicator-specific valid denominators remain unchanged in logic. Woman survey weights are preserved and are not used as facility weights.

Each woman's recorded administrative geography and residence type are used together for a many-to-one left join. For example, a Rural woman in department A receives the supply profile of included Rural-classified facilities in department A. An Urban woman there receives the Urban profile.

All source women, variables and row order are retained. If the woman's residence type is missing, her administrative geography is missing, or the corresponding facility cell is absent, the stratified supply values remain missing. There is **no automatic administrative-only fallback**. `link_has_admin_supply` identifies whether a broader administrative profile exists without substituting that profile. Missing type never matches missing facility type. Facilities with missing type are retained in the source/audit files and administrative reference, but excluded from Rural/Urban profile denominators. Zero observed facilities in a cell means no included facility observations, not proof that no facilities exist there.

### Unmatched cases: two options and the implemented policy

| Option | Treatment | Current implementation |
|---|---|---|
| **1. Keep the strict area + residence join and flag gaps** | Retain every woman. Set the count of matched facility records (`n_facilities`) to **0** when no exact profile can be attached. Keep unobserved supply rates/percentages **missing**. Record the reason and administrative-area facility counts. | **Implemented for all four geographies.** No administrative-average substitution. |
| **2. Use the broader administrative-area profile as fallback** | For an unmatched woman, attach the department/LGA/district-only profile, with a separate resolution/fallback flag identifying that broader exposure. | **Not implemented.** The broader profiles are available separately in `admin_reference/` for a later decision. |

Option 1 preserves the meaning of the finer exposure and makes missing coverage visible. A zero in `n_facilities` means **zero records in an attached exact-match profile**, including cases where the woman's type is unknown. It does not mean there are physically no local facilities. Do not replace missing supply percentages with zero: zero trained-provider availability would claim no trained staff, whereas zero stockouts would claim uninterrupted stock. Neither conclusion follows from an unmatched profile.

`link_status` retains the overall match status; `link_issue` adds the detailed reason:

| `link_issue` | Meaning |
|---|---|
| `matched` | An exact administrative-area + residence profile was attached. |
| `no_rural_classified_records` | The area has included facility records classified Urban, but none classified Rural. The woman is Rural and remains unmatched. |
| `no_urban_classified_records` | The area has included facility records classified Rural, but none classified Urban. The woman is Urban and remains unmatched. |
| `facility_type_unavailable` | The area has included facilities, but none has a usable urban/rural classification. |
| `no_area_facility_records` | No facility records are matched to that administrative area under the selected crosswalk. |
| `woman_residence_unavailable` | The supplied woman dataset has no usable residence-type field; currently applies to Sindh without a validated lookup. |
| `missing_woman_residence` | Residence type is missing or unrecognized for this woman. |
| `missing_woman_geography` | The woman's administrative geography is missing. |
| `no_area_type_profile` | Catch-all for any other absent exact profile; inspect the coverage audit. |

The woman-level fields `link_n_admin_fac`, `link_n_admin_rural`, `link_n_admin_urban` and `link_n_admin_unknown` show included facility-record counts in her administrative area. They help distinguish a missing residence-specific match from an area without any matched facility records. `audit/linkage_issue_summary.csv` lists women by country, mapping version, area, residence, detailed issue and those facility counts. The coverage table also retains women with missing administrative geography.

### Senegal issue requiring later review

Under the completed-geography version, the following 1,815 Rural women have no Rural-classified facility records in their departments. There are 755 included facilities in those departments, all classified Urban through the health-district lookup. This is an observed classification/coverage mismatch, not verified absence of rural facilities.

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

For these women, `link_issue = no_rural_classified_records`, `link_has_supply = FALSE`, `n_facilities = 0`, and stratified supply estimates remain missing. Their department-only profiles remain available. Review the lookup's provenance and obtain facility-specific residence classifications or validated geographic information before interpreting these as actual rural service shortages. The issue summary covers both mapping versions; the table above describes the completed version only.

The same policy applies elsewhere: Lagos has complete exact-match coverage; in the completed Côte d'Ivoire mapping, 153 women in Bonon, Facobly, Gohitafla and Taabo lack a matched administrative facility profile, and three women have missing geographic/residence information. Sindh's missing woman residence type is flagged separately from facility coverage. No geography is silently assigned a broader average or zero-valued supply rate.

### Côte d'Ivoire: unmatched departments also present in the supplied GitHub export

The author's exported `exports/merged_supply_demand_department.csv` contains the same four departments below with **`n_facilities = 0`** and no matched supply profile. The source-export reconciliation confirms that our result agrees with his for these departments. These nonmatches predate the urban/rural extension.

| Department | SURGO women | Rural women | Urban women | Matched facilities in author's export | Matched facilities in our completed mapping |
|---|---:|---:|---:|---:|---:|
| Bonon | 59 | 29 | 30 | 0 | 0 |
| Facobly | 37 | 37 | 0 | 0 | 0 |
| Gohitafla | 23 | 23 | 0 | 0 | 0 |
| Taabo | 34 | 34 | 0 | 0 | 0 |
| **Total** | **153** | **123** | **30** | **0** | **0** |

An additional three raw SURGO records have missing administrative/residence information. They were excluded from the author's analytical export; our outputs retain them and flag `missing_woman_geography`. Thus the completed department-only join and completed department + urban/rural join both attach profiles to 10,703 of 10,859 source women, leaving the **same 156 original source rows** unmatched. Adding residence type introduces no additional Côte d'Ivoire nonmatches.

The unmodified source-crosswalk version has 303 unmatched raw women. The separately documented geographic completion recovers 147 women in Agboville (60), Sikensi (30) and Tiassale (57), leaving the 156 above. Agreement for the four remaining departments does not imply that the complete unmatched populations are identical between the source and completed crosswalk versions.

The unresolved issue is facility survey coverage or the health-district-to-department mapping, not evidence that these departments physically lack facilities. Future review should establish whether relevant facilities are absent from the survey or represented under different geographic catchments/names. Until reviewed, apply implemented option 1: retain women, flag the missing match, use zero for the matched-record count and missing for unobserved rates. Reconciliation evidence is in `admin_reference/reconciliation/all_area_overlap_population_comparison.csv` and the exact source export/crosswalk supplied inside the input bundle.

The coverage table includes Rural, Urban and missing-type rows for each area, making women-only and facilities-only cells visible. Every cell reports its included facility count; `link_single_facility` flags a woman whose attached profile rests on one facility. No claim is made that adding a residence stratum creates independent service catchments: women can obtain care across these boundaries.

### Sindh: district matching succeeds; woman residence type is unavailable

All 10,364 source women have a matched district profile across the 30 SURGO districts, in both administrative-only geography versions. In the completed mapping, the 881 facilities with blank `district_name` are resolved using their recorded `district` labels; all 4,308 facilities then contribute to district profiles. This recovered facility geography is separate from the missing woman urban/rural variable.

The facility `location` field records Rural for 2,409 facilities and Urban for 1,899. The supplied SURGO files provide the woman's district and cluster identifiers, but no usable woman urban/rural classification. In particular, `rb0e` and `clusterCode` are cluster codes. The script does not infer residence from code digits, district names or the predominant facility type. Although district x residence supply profiles can be calculated, a woman's correct profile cannot be selected without her residence type.

This was checked against every variable name, available variable label and category label in both supplied Sindh forms files, the exported dictionaries, and the Pakistan questionnaire text. The questionnaire describes `RB0e` as Town/Village/Primary Sampling Unit; the observed data contain numeric identifiers there, rather than residence categories. The weighted file has only four administrative variable labels and category labels for merge status, so the conclusion is that no identifiable, documented residence indicator is available in these supplied files. A future codebook or validated sampling-frame lookup may resolve this; the current code does not invent that mapping.

Without a supplied Sindh residence lookup, all 10,364 Sindh women are retained with `link_status = woman_residence_unavailable`, `link_has_supply = FALSE`, and missing stratified supply indicators. Their district-only profiles remain available in `admin_reference/`. This is an explicitly incomplete stratified linkage, not a completed Sindh urban/rural match.

An optional validated Sindh lookup can be supplied as a CSV containing unique `col_id` and `rurb` columns, with `rurb` equal to Rural or Urban. It must identify the residence of the woman or her documented survey cluster; a district's majority type or a facility's location is insufficient. The program checks unique/nonmissing IDs, accepted classifications and coverage, records its checksum, and retains women absent from the lookup as unmatched. Its geographic validity must be established before use.

```sh
Rscript R/run_join_urban_rural.R --input-zip "inputs.zip" --output-dir "output_urban_rural" --geography-version both --sindh-residence-lookup "sindh_residence.csv"
```

### Separate outputs and checks

- `datasets/*_women_manitoba_urban_rural.dta` and `.rds`: source-crosswalk, area/type woman-level files.
- `checked_geography/`: corresponding optional completed-geography files.
- `supply_profiles/`: one record per country, administrative area and classified residence type for each selected geographic version.
- `audit/urban_rural_source_coding.csv`: source fields, observed labels/codes, harmonized classifications and counts on each side.
- `audit/facility_area_residence_assignments.csv`: facility IDs, both administrative targets, residence category and classification basis.
- `audit/senegal_district_residence_lookup.csv`: health-district proxy assignments and missing lookup matches.
- `audit/area_residence_coverage.csv`: women and facilities in every area/type cell, including missing type.
- `audit/linkage_issue_summary.csv`: detailed unmatched reasons and administrative-area Rural/Urban/unknown facility counts for each area/type and mapping version.
- `audit/woman_level_merge_checks.csv` and `facility_residence_coverage.csv`: retained/matched records and exclusions from stratified facility denominators.
- `audit/urban_rural_validation_checks.csv`: source coding agreement, strict two-field matching, facility-count conservation, independent cell membership/mean and counselor-denominator checks, and saved DTA value preservation for every column.
- `dictionaries/`: supply definitions qualified to the new aggregation level and the new linkage fields.
- `run_parameters.json`: component, geographic version, input checksum, optional lookup checksum and limitations.

The automatic administrative preparation's full reference reconciliation remains in `admin_reference/`. The new stratified estimates are not expected to equal the author's administrative-only estimates, because the grouping and denominators differ.

### Verified coverage in the supplied snapshot

All source women are retained. Counts below are women with an exact area/type profile, using the recorded or qualified proxy classification described above. No Sindh residence lookup was supplied.

| Geography | Original women retained | Source-crosswalk area/type matches | Completed-geography area/type matches |
|---|---:|---:|---:|
| Lagos | 4,555 | 4,555 | 4,555 |
| Senegal | 7,169 | 5,020 | 5,354 |
| Côte d'Ivoire | 10,859 | 10,556 | 10,703 |
| Sindh | 10,364 | 0 | 0 |

The Senegal counts use the health-district proxy. Nine Yeumbeul facility records have no residence lookup classification; they remain in the administrative reference but are excluded from urban/rural profiles. In Côte d'Ivoire, three source women have missing administrative/residence information; all are retained. The coverage audits distinguish absent facility cells from missing woman classifications. The supplied-snapshot run passed 1,212 urban/rural validation checks in addition to the 69 administrative reference reconciliation checks.

Module source files are `R/run_join_urban_rural.R`, `R/00_config.R`, `R/urban_rural_config.R`, `R/urban_rural_helpers.R` and `R/06_join_urban_rural.R`. `--urban-rural-config` accepts a parameterized alternative configuration; it does not change the first component's configuration. Run `Rscript tests/test_urban_rural.R` for the deterministic missing-type, unmatched-stratum and duplicate-key checks. Commit the test script with the repository; keep generated files and datasets outside version control.
