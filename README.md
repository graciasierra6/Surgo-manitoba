# SURGO–Manitoba woman-level join

This project joins University of Manitoba facility supply profiles to every original SURGO woman's record in Lagos, Senegal, Côte d'Ivoire and Sindh. It performs data preparation, geographic linkage and validation only. It does not fit models, calculate research results or create a demand/supply constraint score.

## Inputs

Keep the supplied `SURGO_Manitoba_linkage_inputs.zip` outside this repository, for example in a locally synced OneDrive folder. It contains the original surveys, cleaned facility summaries, archetype assignments, geographic crosswalks, documented geographic lookup tables, and source outputs needed for validation. Original dataset bytes are retained and verified against SHA-256 checksums.

No Databricks connection or credentials are required. Input, output, cache and country-configuration locations are parameters; no personal paths are embedded in the scripts.

## Two separate join components

| Component | Entry point | Supply aggregation and woman-level match |
|---|---|---|
| Administrative geography | `R/run_join.R` | LGA / department / district |
| Administrative geography plus urban/rural | `R/run_join_urban_rural.R` | LGA / department / district **and** woman's urban/rural category |

The second component has its own entry point, configuration, join script, checks and outputs. It reuses the first component's verified facility preparation and geographic assignments. It does not replace the administrative-only datasets or alter their supply definitions.

The urban/rural component recalculates the existing supply indicators within each area/type cell and attaches that cell's profile to individual women. The implemented policy retains every woman, flags an unavailable match, sets only the matched-record count to zero and leaves unobserved supply estimates missing. A broader administrative-average fallback is documented as an alternative and is not applied. Review the country-specific qualifications before selecting the finer component for analysis, especially Senegal's facility classification proxy and Sindh's unavailable woman residence type.

Use the administrative run below for geography-only files, or the [separate urban/rural run](#separate-urbanrural-aggregation-and-join) for the additional component. The same input ZIP is used for both; keep their output directories separate.

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
- `R/run_join_urban_rural.R`: separate parameterized entry point; prepares and validates an administrative reference in its own output directory before running the finer join.
- `R/urban_rural_config.R`: woman/facility residence fields and the Senegal health-district proxy source; leaves Sindh woman residence explicitly unavailable.
- `R/urban_rural_helpers.R`: accepted residence labels, exact two-field keys, optional validated residence lookup and detailed unmatched reasons.
- `R/06_join_urban_rural.R`: area/type aggregation, woman-level exports, zero-count/missing-rate policy, coverage audits and validation.
- `tests/test_urban_rural.R`: deterministic coding, exact-match, missing-data, duplicate-key and issue-reason checks.

Data, ZIP files, cached inputs and generated outputs are excluded from version control. Store the input bundle separately in OneDrive; commit only this README, the R scripts, the tests and `.gitignore`.

## Separate urban/rural aggregation and join

Requires the packages above plus `readxl`, which reads the existing Senegal lookup in the supplied input ZIP:

```r
install.packages("readxl")
```

Run this component independently, using a different output directory:

```sh
Rscript R/run_join_urban_rural.R --input-zip "inputs.zip" --output-dir "output_urban_rural" --geography-version both
```

`inputs.zip` is a placeholder for the supplied input ZIP, whose location can also be set through `SURGO_MANITOBA_INPUT_ZIP`. The input ZIP and cache can be shared between both components. The second component first runs the existing preparation and reference checks in `output_urban_rural/admin_reference/`, then writes its own stratified profiles and woman-level datasets in the separate output root. The original geography-only output directory is not used. Both geography versions follow the same definitions as above: `datasets/` reproduces the source crosswalk; `checked_geography/` applies the documented geographic completions/correction.

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

The first component's full reference reconciliation remains in `admin_reference/`. The new stratified estimates are not expected to equal the author's administrative-only estimates, because the grouping and denominators differ.

### Verified coverage in the supplied snapshot

All source women are retained. Counts below are women with an exact area/type profile, using the recorded or qualified proxy classification described above. No Sindh residence lookup was supplied.

| Geography | Original women retained | Source-crosswalk area/type matches | Completed-geography area/type matches |
|---|---:|---:|---:|
| Lagos | 4,555 | 4,555 | 4,555 |
| Senegal | 7,169 | 5,020 | 5,354 |
| Côte d'Ivoire | 10,859 | 10,556 | 10,703 |
| Sindh | 10,364 | 0 | 0 |

The Senegal counts use the health-district proxy. Nine Yeumbeul facility records have no residence lookup classification; they remain in the administrative reference but are excluded from urban/rural profiles. In Côte d'Ivoire, three source women have missing administrative/residence information; all are retained. The coverage audits distinguish absent facility cells from missing woman classifications. The four-country run passed 1,212 urban/rural validation checks in addition to the original 69 reference reconciliation checks.

Additional source files are `R/run_join_urban_rural.R`, `R/urban_rural_config.R`, `R/urban_rural_helpers.R` and `R/06_join_urban_rural.R`. `--urban-rural-config` accepts a parameterized alternative configuration; it does not change the first component's configuration. Run `Rscript tests/test_urban_rural.R` for the deterministic missing-type, unmatched-stratum and duplicate-key checks. Commit the test script with the repository; keep generated files and datasets outside version control.
