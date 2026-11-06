# Source fields and geography levels; edit here for a new source version.
country_config <- list(
  lagos = list(loc = "Lagos", facility = "lagos", raw_name = "uom_lagos_facility_pooled.dta",
               woman_file = "inputs/surgo/lagos/surgo_lagos_women.csv", woman_geo = "lga",
               woman_region = "state", join_level = "geo_level2", id = "uid", weight = "weight",
               counselor = "q702"),
  senegal = list(loc = "Senegal", facility = "senegal", raw_name = "uom_senegal_facility_pooled.dta",
                 woman_file = "inputs/surgo/senegal/surgo_senegal_women.dta", woman_geo = "RB0C",
                 woman_region = "RB0B", join_level = "geo_level2", id = "IDW", weight = "WEIGHT",
                 counselor = "s7_2"),
  cotedivoire = list(loc = "CDI", facility = "cdi", raw_name = "uom_cdi_facility_pooled.dta",
                     woman_file = "inputs/surgo/cotedivoire/surgo_cotedivoire_women.dta", woman_geo = "RB0C",
                     woman_region = "RB0B", join_level = "geo_level3", id = "IDW", weight = "WEIGHT",
                     counselor = "s7_2"),
  sindh = list(loc = "Sindh", facility = "sindh", raw_name = "uom_sindh_facility_pooled.dta",
                woman_file = "inputs/surgo/sindh/20250124_forms_wt.dta", woman_geo = "rb0c",
                woman_region = "rb0b", join_level = "geo_level2", id = "col_id", weight = "Normalizedweights",
                counselor = "q702")
)
