# Input bundle inventory

For a clean list of exact filenames and ZIP members without checksums or run
details, use [Input ZIP filenames](DATASET_FILENAMES.md). This is the list to use
when matching files to Databricks sources later; no source links are assigned yet.

For a concise list of the 14 primary analytical inputs, required supporting files,
separate country commands and expected outputs, use the
[colleague run guide](RUN_GUIDE.md). The
[dataset catalog](dataset_catalog.json) records exact members, keys and pending
Databricks source mappings; the report scripts document their R dependencies and input contracts. This page remains the full checksum inventory.

The reproducible entry point accepts one authorized ZIP through `--input-zip` or
`SURGO_MANITOBA_INPUT_ZIP`. Obtain that ZIP through the project’s approved data-sharing
arrangements and keep it outside the repository. No source datasets are distributed
with this documentation or the build-only update.

## Reference bundle

- Reference filename: `SURGO_Manitoba_linkage_inputs_complete.zip` (the caller’s filename may differ).
- Contents: **55 manifest-listed files plus `bundle_manifest.json`: 56 ZIP members in total**.
- ZIP SHA-256: `f525ccffbefad4e3b00aea72e01a47e837d9ade2c6e3d5c15aad7d23cc439dd2`.
- The [machine-readable inventory](input_bundle_manifest.json) is a copy of the
  bundle manifest; it contains filenames, byte sizes and checksums only.
- Repackaging the same files can change the ZIP checksum. The pipeline verifies
  every listed file’s byte size and SHA-256, so the unchanged member checksums are
  the essential identity check. A different input wave must be reviewed and revalidated.
- The earlier 54-file ZIP already includes the required Senegal evidence lookup;
  it lacks only `lookups/senegal_health_district_department.csv`. The complete
  55-file bundle is the snapshot used for the full reproduction check.

Files retain the relative names shown below inside the ZIP. Do not flatten or rename
members. The manifest is read before extraction, and the join preparation validates
all listed members, including archived/reference files that are not selected for
current statistical calculations. The inventory is intentionally broader than the
minimum set directly read by one country’s analysis.

## How the inputs are used

1. **SURGO women:** select the original country women file, including the weighted
   Sindh file. Preserve all women, source fields and row order in the joins. Current
   need definitions use original FPN2 in Lagos, Senegal and Côte d’Ivoire, and the
   authorized Sindh derived-need source listed below.
2. **Manitoba facilities:** attach cleaned facility summaries, archetypes and service
   volumes by identifier; use the granular survey for eligibility, geography/residence
   checks and daily provision. The cleaned UoM measures and archetype assignments
   are supplied inputs. The build reproduces our joins, aggregations, scores, models
   and reports; it does not independently refit archetypes or recreate all upstream
   UoM cleaning from the original questionnaire.
3. **Geography:** reproduce the supplied reviewed crosswalk for reconciliation and
   the separately documented checked assignments for current analysis. Senegal’s
   facility Rural/Urban lookup is a health-district proxy, not GPS boundary validation.
4. **Analysis preparation:** filter confirmed FP providers and average valid facility
   responses by whole administrative area or area–residence cell, retaining genuine
   zero/FALSE. Match women to their own area and residence where available; flag a
   same-area whole-area fallback only if that exact cell has no eligible FP providers.
   An undefined component in an existing exact-cell profile remains undefined. If
   no whole-area profile exists, retain the woman and flag unmatched supply. Sindh
   uses districts only because the women’s source has no documented Rural/Urban field.
5. **Audit references:** compare reconstructed joins and upstream summaries using
   the exported reference tables and code. These files are not substituted for each
   woman’s responses or the newly aggregated FP-provider supply profiles.

The complete runner also uses the repository’s versioned map assets and package
lockfile. Earlier report/code versions remain in Git history. The current `run_all.R` builds
the seven country reports and eight current supplementary outputs; it does not
fetch historical code. Dependencies and Pandoc must also be installed.

## Complete ZIP membership

All paths below are relative paths **inside the external ZIP**, not local computer
locations. “Archived/reference” files are retained for source verification and
provenance; their presence does not mean they are executed as part of the current
statistical workflow.

| ZIP member | Purpose | Bytes | SHA-256 |
|---|---|---:|---|
| `curated/Facilities data with archetype assignments.csv` | Independent curated archetype reference for assignment reconciliation. | 5,142,980 | `7621cfe6caecba0594af3d2dbc56bbface90152de65ced42b20753ca500f9cac` |
| `curated/geo_crosswalk_draft.csv` | Archived draft crosswalk; retained for provenance, not the selected reviewed assignment. | 9,340 | `061e0a64a59fd3eeb8632f670912538fd7d39e57ef877a5118d9b9512399bbf5` |
| `curated/geo_surgo_levels.xlsx` | Archived geographic-level workbook; retained for provenance. | 17,129 | `708242f017ad91477bd2c7160d2d63152c68aad5e090626831023936dc0e32f1` |
| `exports/exports_manifest.csv` | Inventory of upstream exports; audit reference. | 628 | `f7dffb0ba98fae1f69a33fd67d0de72ed61e11bd72465d1c54dcc7b3595770eb` |
| `exports/merged_supply_demand_coverage.csv` | Upstream geographic coverage reference for linkage checks. | 302 | `81f8fd702faa372b49a0ee58460270f0b02c145336373d03a7f8caa6508594b2` |
| `exports/merged_supply_demand_department.csv` | Upstream merged area-level values used for independent reconciliation; not the final woman-level analysis input. | 306,462 | `0bf0456767484ee09943ac27cdf7a009cf3521472f5e05099ff72ab7cf9177d3` |
| `exports/merged_supply_demand_dictionary.csv` | Supply variable names, aggregation rules and dictionary metadata used by the join. | 57,755 | `dcc970b4e0c5282cd9878b006b53d3721b34511b6e001b37620afdb1a06152b5` |
| `exports/surgo_demand_department.csv` | Upstream weighted demand area summaries; audit/reference export. | 140,362 | `6f29ba24cd035a155092ba09becc6b091256c4085e80eeb4a6b961006f424953` |
| `exports/surgo_demand_qc_log.csv` | Upstream demand quality-control reference. | 18,354 | `f729193c0ccbdc3b2e33733ded5071fed75c13275ca4cd3ea6fddf2c654efb43` |
| `exports/surgo_demand_region.csv` | Upstream regional demand reference; not substituted for individual responses. | 18,067 | `8c844a0bd8adcac0f1bfe36ea232742b8ed355cf096d27bf6cf1ebadc0d84589` |
| `exports/surgo_geo_levels.csv` | Upstream geographic labels/levels reference. | 22,434 | `5eb64c2ba1c74f960fd4489a9c54bf0f840d89a8f55b48ece84116f1dcf541cf` |
| `exports/uom_facility_summary_diff.csv` | Upstream facility-summary reconciliation reference. | 74 | `c8620c0859621858831f18609ba5fe0d3a40ff587f5631cd7f77ec1a1d33f8e1` |
| `exports/uom_facility_summary_original.csv` | Archived original facility-summary export for comparison. | 2,648,316 | `a883d7a1f2ea2178da95504b8b0d212dfa2154f50fd99fd690dc5234917fef47` |
| `exports/uom_facility_summary_pipeline.csv` | Archived pipeline facility-summary export for comparison. | 2,603,727 | `bf9a89baea3a19d237b0fc61db4350cc497d8b3c4b6b6d274cd5e3171e3927da` |
| `exports/uom_surgo_geo_crosswalk.csv` | Earlier unreviewed crosswalk retained for reference. | 25,991 | `5b6b615323896592c1f58187d0d60575c40b2f6880a4bce64d9187ea839da120` |
| `exports/uom_surgo_geo_crosswalk_reviewed.csv` | Reviewed upstream facility-to-SURGO geography crosswalk; base-join and audit input. | 27,891 | `6f78409418dfd344cfeb1cc9c7054e7a34b502647a64b8924c3777e87725bf3f` |
| `inputs/SenegalURlookup.xlsx` | Senegal facility residence lookup by health district; a proxy with documented limitations. | 9,145 | `6f7a20f8942e667fdc9d0607b8af89f38a959d6e760c953331b933b3b1c830bb` |
| `inputs/surgo/cotedivoire/surgo_cotedivoire_women.dta` | Original Côte d’Ivoire women, geography, weights and survey responses. | 119,830,255 | `fc66af3fc880d8d6873dc06e3bcc064db4f8225fecf08424749eff97983d6bdc` |
| `inputs/surgo/lagos/surgo_lagos_women.csv` | Original Lagos main-sample women, geography, weights and survey responses. | 16,726,617 | `618d883dbf9f219052390e800bce9c17e6ae3fdc92e16fc17848c6ce70a90daf` |
| `inputs/surgo/senegal/surgo_senegal_women.dta` | Original Senegal women, geography, weights and survey responses. | 81,267,194 | `bf8604b1dfcc9c12f6241e02d7d4a30d5543e9db7ee4b60ac9445df435e3ea95` |
| `inputs/surgo/sindh/20250124_forms.dta` | Unweighted Sindh source retained for reference; the weighted file is selected for joins. | 42,547,391 | `8c1fa18212a5dc4d034fe1fccef3b5e0ffe7b10909676e256d96444b13fdcb5b` |
| `inputs/surgo/sindh/20250124_forms_wt.dta` | Selected original Sindh women file with normalized weights and district identifiers. | 43,017,253 | `84840d5883d49c18038d6afb3a75c4ca89e37847576bf958a437cbb20957eccb` |
| `inputs/uom/cdi/uom_cdi_facility_pooled.dta` | Granular Côte d’Ivoire facility survey: eligibility, identifiers, residence and method-service responses. | 130,217,757 | `7c1fe207452f10c78cd413cdd07222c55d9f804a7599e7ba60b2d5fb1e88e681` |
| `inputs/uom/lagos/uom_lagos_facility_pooled.dta` | Granular Lagos facility survey: eligibility, identifiers, residence and method-service responses. | 95,464,554 | `64565cda53f47b300dc133b86c03cb9f5aa8072dd02028c3fb01d5ea5064f62c` |
| `inputs/uom/senegal/uom_senegal_facility_pooled.dta` | Granular Senegal facility survey: eligibility, identifiers and method-service responses. | 36,543,860 | `93ee53f49124a6437062a43dcfcef1159077edc2161a92e63c00720cc3278476` |
| `inputs/uom/sindh/uom_sindh_facility_pooled.dta` | Granular Sindh facility survey: eligibility, identifiers, residence and method-service responses. | 239,919,794 | `a1f11eb3daf006a165e08ac9809aa31ff12b71258382b4e56754c92ec119c5be` |
| `lookups/cdi_health_district_codebook.csv` | Côte d’Ivoire district-code geography lookup for checked assignments. | 1,659 | `f158c1f85bf42049991147d90eb6c752eace3e9c5a41982e27eea740f6ba335d` |
| `lookups/geographic_sources.json` | Documented sources and limitations for geographic mappings. | 3,014 | `b4b87a09d13aa535c96a6dc97d1684229d97402829a4f259974ee058020782a0` |
| `lookups/senegal_health_district_department.csv` | Auxiliary Senegal crosswalk; present in the complete bundle, not used instead of the evidence lookup. | 106,738 | `327558f3d46da956f4c6f61ed26373b455eb436f28662b83a1882e9b4e1f44eb` |
| `lookups/senegal_health_district_department_evidence.csv` | Required checked Senegal district-to-département evidence lookup; includes qualified assignments. | 106,738 | `327558f3d46da956f4c6f61ed26373b455eb436f28662b83a1882e9b4e1f44eb` |
| `lookups/sindh_district_aliases.csv` | District-name aliases for checked Sindh matching. | 1,582 | `5ac31249cb8be02619e7a02d567652b368b8567cf2d68f733d9e92bbac2f4410` |
| `merge_pipeline/geo_bridge.csv` | Archived upstream geographic bridge for provenance. | 28,659 | `d915d0c77348f06b4aec57a42430faf26c7f5d0c4ec4d283093e691b1fc136fb` |
| `merge_pipeline/new_demand_vol_facility_crosswalked.csv` | Archived upstream crosswalked service-volume reference. | 1,797,333 | `2dc3bfb41dec5cb1279672a3dfdf9377e42628dee44cab8c68331eaee07b80a3` |
| `merge_pipeline/qc/e05_qc.csv` | Archived upstream export quality-control reference. | 2,527 | `0598ad7384a880516b5dc9088bb26c87f42541b087a8c370bd9148a82a2ea38c` |
| `merge_pipeline/qc/e05a_qc.csv` | Archived upstream export quality-control reference. | 117 | `71ba39650b32fa22d8e143e61356f09b821d82c19e3150d0bf921d78fcb7b94a` |
| `merge_pipeline/qc/m01_qc.csv` | Archived upstream geography-key quality-control reference. | 116 | `63e14091ffd0cb1b31033146b448f64c46b07b6b3f10ddf16753d2762ecdb76f` |
| `merge_pipeline/qc/m05_qc.csv` | Archived upstream expanded-geography quality-control reference. | 2,527 | `43399af6c3181b494614d4753a9e7a8a7318878659b8fdac43456366b6aadb05` |
| `merge_pipeline/uom_facility_geo.csv` | Exported upstream facility geography; compared with reconstructed assignments. | 752,949 | `d04e66cb53cd16c450d66e92fd89dc94f5e03448ef2094838a42c27fdf205243` |
| `merge_pipeline/uom_goes.csv` | Archived upstream geographic expansion reference. | 9,416 | `e03b22d27b4c3a1287785ceae978e4db4d5a0c4a9e58dfddb584a975a9bd4355` |
| `merge_pipeline/uom_summary_facility_crosswalked.csv` | Upstream crosswalked facility summary used to validate reconstructed join keys. | 3,073,855 | `52821349bcb1070dd4ea377d13f28d69d6d65964be376cd1575df9898c1f1e78` |
| `reference/upstream/DATA_DICTIONARY.md` | Upstream variable/aggregation documentation retained as reference. | 26,581 | `99f5eabf6340f9f7d035827ed329c029969beb83c7f1cf5e9a500a288bff37b1` |
| `reference/upstream/workflow_merge/m01_geography_keys.r` | Original geographic-key code reference; not a dependency on a remote workspace. | 4,130 | `f43df959a80d66d279866aa9cb096189481f9a8587e17ae3bab96f16b6392782` |
| `reference/upstream/workflow_merge/m04_apply_crosswalk_review.sql` | Original reviewed-crosswalk SQL reference. | 14,448 | `0d3f434a5a0113af627a6fb7a5d64af5b5423d2393123005c2f8ed521363b419` |
| `reference/upstream/workflow_merge/m05_expand_geographies.r` | Original geography-expansion code reference. | 9,532 | `6b1aefb1ed7262c8268f891f67b1e5845420d1671e944a7f2045b047dc6d5009` |
| `reference/upstream/workflow_merge/m06_build_merged_dataset.r` | Original aggregate merge code reference. | 21,948 | `6738ab18155ad5a41cc605e21ef1236cfe1cc6b8184336d0da8455a7d0351e58` |
| `reference/upstream/workflow_merge/m07_build_merged_dictionary.py` | Original merged-dictionary code reference. | 19,058 | `21695580ad9e920389caee34e9420efe9929d14b97f60656f258c5ebd1d0c0ec` |
| `surgo_pipeline/cotedivoire/wra_design_ready.rds` | Processed upstream Côte d’Ivoire women reference used for reconciliation. | 4,471,100 | `4e8334c09751c19a5d1b4be9bcab60207b99353497d07a119449742ea0625d29` |
| `surgo_pipeline/lagos/wra_design_ready.rds` | Processed upstream Lagos women reference used for reconciliation. | 1,924,547 | `f934a21a6df51a3fc915614c611ec581c85e01df2ebc075a9a4542843a917abb` |
| `surgo_pipeline/senegal/wra_design_ready.rds` | Processed upstream Senegal women reference used for reconciliation. | 2,997,250 | `2534db07f7a7a4fd29b6b2904113dd341eaf2586100a8bb0c2bf275f890945bd` |
| `surgo_pipeline/sindh/sindh_wra_design_ready.rds` | Authorized Sindh derived-need source used by the current analysis. | 3,417,507 | `01147a8dbb65ecb7170782ae198b77f22143cd3b8b20267de443e699fbd0a2d7` |
| `surgo_pipeline/sindh/wra_design_ready.rds` | Processed upstream Sindh women reference used for reconciliation. | 3,440,292 | `a792fe705f84da8582e114036de5ae1a4335384d0edb24e8da65b08210ea9939` |
| `uom_pipeline/Facilities data with archetype assignments.csv` | Selected facility archetype assignments attached by facility identifier. | 5,143,162 | `81db1390c3c24e4d6971b5407d74387b249e37c6e6700a32404b1dc26a72b0c3` |
| `uom_pipeline/new_demand_vol_facility.csv` | Cleaned facility service-volume measures used in base aggregation. | 1,104,270 | `baf2d373523e347bfd6fb600147f71a50dcfe5585c690cc8d81917098c89c059` |
| `uom_pipeline/uom_data.RData` | Archived cleaned UoM object retained in the source bundle; the CSV summary is selected by the current base join. | 14,075,948 | `b1b001b9cdd708fd428b37861c98f93c0dcf138038856846d2a80d47e116248f` |
| `uom_pipeline/uom_summary_facility.csv` | Selected cleaned facility-level summary indicators; joined to facility geography before aggregation. | 2,223,967 | `9268646689dbc278194803fd41919e6182624f2b41fa3b7122a289448be169cf` |
| `bundle_manifest.json` | Bundle schema, complete member inventory, byte sizes and SHA-256 checksums; read before extraction. Its own checksum is recorded here rather than recursively inside itself. | 10,466 | `e38184b834a195ad4a4636a226becc8770ec7e208a28058009f3d60595901e0d` |
