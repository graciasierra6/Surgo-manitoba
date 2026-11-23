# Input ZIP filenames

Exact names from `SURGO_Manitoba_linkage_inputs_complete.zip`. These are paths
**inside the ZIP**, not locations on a personal computer. Use the full ZIP member
to identify a file when adding a Databricks source link later: several filenames
occur in more than one folder. Source links have not been assigned here.

The ZIP contains **56 members**: 14 primary analytical inputs and 42 additional
supporting, reconciliation, reference and manifest files. Keep the complete bundle.

## Primary analytical inputs

| File name | Exact ZIP member |
|---|---|
| `surgo_lagos_women.csv` | `inputs/surgo/lagos/surgo_lagos_women.csv` |
| `surgo_senegal_women.dta` | `inputs/surgo/senegal/surgo_senegal_women.dta` |
| `surgo_cotedivoire_women.dta` | `inputs/surgo/cotedivoire/surgo_cotedivoire_women.dta` |
| `20250124_forms_wt.dta` | `inputs/surgo/sindh/20250124_forms_wt.dta` |
| `uom_lagos_facility_pooled.dta` | `inputs/uom/lagos/uom_lagos_facility_pooled.dta` |
| `uom_senegal_facility_pooled.dta` | `inputs/uom/senegal/uom_senegal_facility_pooled.dta` |
| `uom_cdi_facility_pooled.dta` | `inputs/uom/cdi/uom_cdi_facility_pooled.dta` |
| `uom_sindh_facility_pooled.dta` | `inputs/uom/sindh/uom_sindh_facility_pooled.dta` |
| `uom_summary_facility.csv` | `uom_pipeline/uom_summary_facility.csv` |
| `Facilities data with archetype assignments.csv` | `uom_pipeline/Facilities data with archetype assignments.csv` |
| `new_demand_vol_facility.csv` | `uom_pipeline/new_demand_vol_facility.csv` |
| `uom_surgo_geo_crosswalk_reviewed.csv` | `exports/uom_surgo_geo_crosswalk_reviewed.csv` |
| `SenegalURlookup.xlsx` | `inputs/SenegalURlookup.xlsx` |
| `sindh_wra_design_ready.rds` | `surgo_pipeline/sindh/sindh_wra_design_ready.rds` |

## Additional ZIP members

These names are listed separately to keep the selected inputs above easy to find.
They remain part of the validated input bundle.

### Bundle manifest

| File name | Exact ZIP member |
|---|---|
| `bundle_manifest.json` | `bundle_manifest.json` |

### curated

| File name | Exact ZIP member |
|---|---|
| `Facilities data with archetype assignments.csv` | `curated/Facilities data with archetype assignments.csv` |
| `geo_crosswalk_draft.csv` | `curated/geo_crosswalk_draft.csv` |
| `geo_surgo_levels.xlsx` | `curated/geo_surgo_levels.xlsx` |

### exports

| File name | Exact ZIP member |
|---|---|
| `exports_manifest.csv` | `exports/exports_manifest.csv` |
| `merged_supply_demand_coverage.csv` | `exports/merged_supply_demand_coverage.csv` |
| `merged_supply_demand_department.csv` | `exports/merged_supply_demand_department.csv` |
| `merged_supply_demand_dictionary.csv` | `exports/merged_supply_demand_dictionary.csv` |
| `surgo_demand_department.csv` | `exports/surgo_demand_department.csv` |
| `surgo_demand_qc_log.csv` | `exports/surgo_demand_qc_log.csv` |
| `surgo_demand_region.csv` | `exports/surgo_demand_region.csv` |
| `surgo_geo_levels.csv` | `exports/surgo_geo_levels.csv` |
| `uom_facility_summary_diff.csv` | `exports/uom_facility_summary_diff.csv` |
| `uom_facility_summary_original.csv` | `exports/uom_facility_summary_original.csv` |
| `uom_facility_summary_pipeline.csv` | `exports/uom_facility_summary_pipeline.csv` |
| `uom_surgo_geo_crosswalk.csv` | `exports/uom_surgo_geo_crosswalk.csv` |

### inputs

| File name | Exact ZIP member |
|---|---|
| `20250124_forms.dta` | `inputs/surgo/sindh/20250124_forms.dta` |

### lookups

| File name | Exact ZIP member |
|---|---|
| `cdi_health_district_codebook.csv` | `lookups/cdi_health_district_codebook.csv` |
| `geographic_sources.json` | `lookups/geographic_sources.json` |
| `senegal_health_district_department.csv` | `lookups/senegal_health_district_department.csv` |
| `senegal_health_district_department_evidence.csv` | `lookups/senegal_health_district_department_evidence.csv` |
| `sindh_district_aliases.csv` | `lookups/sindh_district_aliases.csv` |

### merge_pipeline

| File name | Exact ZIP member |
|---|---|
| `geo_bridge.csv` | `merge_pipeline/geo_bridge.csv` |
| `new_demand_vol_facility_crosswalked.csv` | `merge_pipeline/new_demand_vol_facility_crosswalked.csv` |
| `e05_qc.csv` | `merge_pipeline/qc/e05_qc.csv` |
| `e05a_qc.csv` | `merge_pipeline/qc/e05a_qc.csv` |
| `m01_qc.csv` | `merge_pipeline/qc/m01_qc.csv` |
| `m05_qc.csv` | `merge_pipeline/qc/m05_qc.csv` |
| `uom_facility_geo.csv` | `merge_pipeline/uom_facility_geo.csv` |
| `uom_goes.csv` | `merge_pipeline/uom_goes.csv` |
| `uom_summary_facility_crosswalked.csv` | `merge_pipeline/uom_summary_facility_crosswalked.csv` |

### reference

| File name | Exact ZIP member |
|---|---|
| `DATA_DICTIONARY.md` | `reference/upstream/DATA_DICTIONARY.md` |
| `m01_geography_keys.r` | `reference/upstream/workflow_merge/m01_geography_keys.r` |
| `m04_apply_crosswalk_review.sql` | `reference/upstream/workflow_merge/m04_apply_crosswalk_review.sql` |
| `m05_expand_geographies.r` | `reference/upstream/workflow_merge/m05_expand_geographies.r` |
| `m06_build_merged_dataset.r` | `reference/upstream/workflow_merge/m06_build_merged_dataset.r` |
| `m07_build_merged_dictionary.py` | `reference/upstream/workflow_merge/m07_build_merged_dictionary.py` |

### surgo_pipeline

| File name | Exact ZIP member |
|---|---|
| `wra_design_ready.rds` | `surgo_pipeline/cotedivoire/wra_design_ready.rds` |
| `wra_design_ready.rds` | `surgo_pipeline/lagos/wra_design_ready.rds` |
| `wra_design_ready.rds` | `surgo_pipeline/senegal/wra_design_ready.rds` |
| `wra_design_ready.rds` | `surgo_pipeline/sindh/wra_design_ready.rds` |

### uom_pipeline

| File name | Exact ZIP member |
|---|---|
| `uom_data.RData` | `uom_pipeline/uom_data.RData` |

For file purposes and checksums, see [the full input inventory](INPUTS.md).
For separate run commands and outputs, see [the colleague run guide](COLLEAGUE_RUN_GUIDE.md).
