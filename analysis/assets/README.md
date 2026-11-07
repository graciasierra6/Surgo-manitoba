# Report display and historical reference assets

These assets make report generation independent of earlier reports, private
temporary folders and live boundary services. `manifest.json` verifies SHA-256
checksums before each build. None assigns a facility to a woman or changes the
linkage crosswalk. There are no respondent records or facility point coordinates.

| Asset | Source and use |
|---|---|
| `lagos_map_template.html` | Archived Lagos display geometry/layout with aggregate facility-count dots and labels. Only map SVGs and CSS are retained; earlier analytic tables, models and JavaScript are removed. Current hover estimates and classifications are generated from the current analysis. |
| `boundaries/sindh_districts.geojson` | [WFP Pakistan district layer](https://atlas.geospatial.wfp.org/server/rest/services/Hosted/pak_bnd_adm3district_aha_usda/FeatureServer/0), 30 Sindh districts, WGS84 query with 0.002-degree display simplification; archived 6 November 2026. Explicit name aliases are checked by the country map script. |
| `boundaries/senegal_departments.geojson` | [Government of Senegal/OCHA HDX administrative boundaries](https://data.humdata.org/dataset/cod-ab-sen), archived 46-department layer. The map script checks names and documented aliases before display. |
| `boundaries/civ_departments_2018.geojson` and `civ_subprefectures_2018.geojson` | [CNTIG/OCHA HDX Côte d’Ivoire boundaries](https://data.humdata.org/dataset/cod-ab-civ), archived 108-department and 510-subprefecture layers. `cotedivoire/boundary_updates.csv` documents the three official 2020 splits used to construct 111 display units. |

The boundary files are public display snapshots. They can differ from survey-date
administrative boundaries and are not GPS validation of facility locations. The
Lagos template preserves the existing coverage-map layout; it is specific to the
checksummed source snapshot. Counts and coverage are reconciled against computed
inputs during report verification.
