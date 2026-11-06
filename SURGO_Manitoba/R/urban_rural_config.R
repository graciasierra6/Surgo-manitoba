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
