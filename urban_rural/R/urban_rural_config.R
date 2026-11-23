# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: bundled R only; dependencies may be supplied by the caller.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Configuration.
# Inputs: Source-field definitions for the documented input snapshot; no credentials.
# Outputs: Country configuration objects used by the module entry.
# Source dataset names/roles: docs/COLLEAGUE_RUN_GUIDE.md (D01-D14 and supporting files).
# Use through its caller/module; this file is not an independent report entry point.
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# A separate component: these settings do not change the administrative join.
urban_rural_config <- list(
  lagos = list(woman_field = "settlement", facility_field = "location",
               facility_basis = "recorded_facility_location"),
  # One label per health district: a proxy, not each facility's settlement type.
  senegal = list(woman_field = "FV_MILIEU", facility_field = "s1_3",
                 facility_basis = "health_district_lookup_proxy",
                 facility_lookup = "inputs/SenegalURlookup.xlsx"),
  cotedivoire = list(woman_field = "FV_MILIEU", facility_field = "s1_5",
                     facility_basis = "recorded_facility_location"),
  # No usable source woman residence; rb0e is a cluster identifier, not this type.
  sindh = list(woman_field = NA_character_, facility_field = "location",
               facility_basis = "recorded_facility_location")
)
