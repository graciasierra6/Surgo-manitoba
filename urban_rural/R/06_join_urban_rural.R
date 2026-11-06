# Run through R/run_join_urban_rural.R.
# Implemented: strict area/type match; unmatched record count = 0, rates = missing.
# Alternative (not implemented): attach broader administrative averages with a
# separate resolution/fallback flag. Never silently mix geographic resolutions.
# Senegal's district-level residence proxy can leave rural women unmatched even
# when the department has facilities; zero classified records is not proof of
# zero rural facilities. Sindh woman residence is unavailable in supplied files.
# Supplied CDI snapshot: Bonon/Facobly/Gohitafla/Taabo also have zero matched
# facilities in the author's geographic export (153 women); three additional
# raw women have missing geography and are absent from his analytical export.
# These nonmatches predate residence stratification; no special recoding is used.
# Inputs prepared by the separate entry point: verified facility summaries and
# administrative assignments, source women, residence configuration and lookup.
# Outputs are separate from the original component: profiles, DTA/RDS datasets,
# source coding, area/type coverage, detailed linkage reasons and checks.
for (folder in c("datasets", "supply_profiles", "audit", "dictionaries"))
  dir.create(file.path(out, folder), recursive = TRUE, showWarnings = FALSE)
fac <- readRDS(file.path(base_out, "audit/facility_linkage_inputs.rds"))
dictionary <- read_chars(file.path(input, "exports/merged_supply_demand_dictionary.csv"))
dictionary <- dictionary[dictionary$table_name == "merged_supply_demand_department", , drop = FALSE]
supply_names <- dictionary$column_name[dictionary$topic == "Supply"]
women <- list(); source_checks <- list(); coding <- list(); lookups <- list()
record_check <- function(country, mode, check, passed, detail = "") {
  source_checks[[length(source_checks) + 1L]] <<- data.frame(
    country = country, mode = mode, check = check, passed = isTRUE(passed), detail = detail)
  if (!isTRUE(passed)) stop(country, " / ", mode, ": ", check, " failed: ", detail)
}
coding_counts <- function(country, side, field, raw_value, residence, basis) {
  data.frame(country = country, side = side, source_field = field,
             raw_value = ifelse(is.na(raw_value) | trimws(raw_value) == "", "[missing]", raw_value),
             rurb_join = ifelse(is.na(residence), "[missing/unrecognized]", residence), basis = basis) %>%
    count(country, side, source_field, raw_value, rurb_join, basis, name = "records")
}
fac$rurb_join <- NA_character_
fac$rurb_basis <- NA_character_
for (country in names(country_config)) {
  cfg <- country_config[[country]]; rc <- rurb_config[[country]]
  if (is.null(rc)) stop("Missing urban/rural configuration: ", country)
  w <- read_women(file.path(input, cfg$woman_file))
  wi <- rep(NA_character_, nrow(w)); wr <- wi
  woman_basis <- "recorded_woman_residence"
  wf <- rc$woman_field
  if (!is.na(wf)) {
    field <- case_name(names(w), wf)
    wr <- lab(w[[field]]); wi <- rurb_value(w[[field]])
  } else if (country == "sindh" && nzchar(sindh_lookup)) {
    ext <- read_chars(sindh_lookup)
    result <- rurb_lookup(lab(w[[cfg$id]]), ext, cfg$id)
    wr <- result$raw_value; wi <- result$residence
    wf <- "external lookup: col_id -> rurb"; woman_basis <- "external_woman_residence_lookup"
    write_table(data.frame(col_id = ext[[cfg$id]], rurb = rurb_value(ext$rurb),
                           found_in_women = result$lookup_used),
                file.path(out, "audit/sindh_residence_lookup_coverage.csv"))
  } else {
    wf <- "[unavailable]"; woman_basis <- "woman_residence_unavailable"
  }
  women[[country]] <- list(data = w, residence = wi, residence_raw = wr,
                           residence_field = wf, residence_basis = woman_basis)
  coding[[length(coding) + 1L]] <- coding_counts(country, "women", wf, wr, wi, woman_basis)
  # Check the actual raw facility location, independently of the cleaned field.
  p <- file.path(input, "inputs/uom", cfg$facility, cfg$raw_name)
  meta <- haven::read_dta(p, n_max = 0)
  requested <- unique(c("key", "facility_id", "fid", "state_name", "IBILE_dist", "district_name",
                         "s1_1", "s1_2", "s1_3", "district", "division", rc$facility_field))
  wanted <- intersect(requested, names(meta))
  raw <- haven::read_dta(p, col_select = tidyselect::all_of(wanted))
  g <- source_geo(raw, country)
  if (country == "senegal") {
    lut <- readxl::read_xlsx(file.path(input, rc$facility_lookup), range = "A1:B79")
    assert_unique(data.frame(key = norm(lut$s1_3)), "key", "Senegal residence lookup")
    ix <- match(norm(lab(raw[[rc$facility_field]])), norm(lut$s1_3))
    rv <- as.character(lut$urbanrural[ix]); residence <- rurb_value(rv)
    lookups[[country]] <- data.frame(source_health_district = lab(raw[[rc$facility_field]]),
                                     lookup_code = rv, lookup_matched = !is.na(ix), rurb_join = residence) %>% distinct()
    write_table(lookups[[country]], file.path(out, "audit/senegal_district_residence_lookup.csv"))
  } else {
    rv <- lab(raw[[rc$facility_field]]); residence <- rurb_value(raw[[rc$facility_field]])
  }
  fi <- which(fac$country == country)
  ri <- match(fac$fid[fi], g$fid)
  record_check(country, "both", "raw_facility_ids_complete", !anyNA(ri))
  residence <- residence[ri]; rv <- rv[ri]
  inherited <- rurb_value(fac$rurb[fi])
  record_check(country, "both", "raw_or_lookup_matches_cleaned_rurb", all(rurb_same(residence, inherited)))
  fac$rurb_join[fi] <- residence; fac$rurb_basis[fi] <- rc$facility_basis
  coding_field <- if (country == "senegal") "SenegalURlookup.xlsx:urbanrural (via s1_3)" else rc$facility_field
  coding[[length(coding) + 1L]] <- coding_counts(country, "facilities", coding_field, rv, residence, rc$facility_basis)
}
write_table(bind_rows(coding), file.path(out, "audit/urban_rural_source_coding.csv"))
write_table(fac[c("country", "loc", "fid", "admin1", "admin2", "admin3", "geo_join_pipeline",
                  "geo_join_audited", "rurb_join", "rurb_basis")], file.path(out, "audit/facility_area_residence_assignments.csv"))

# Reaggregate within exact area/type cells for each selected crosswalk version.
modes <- if (run_options$geography_version == "both") c("pipeline_crosswalk", "audited_geography") else "pipeline_crosswalk"
coverage <- list(); merge_checks <- list(); facility_coverage <- list(); issues <- list()
for (mode in modes) {
  geo_col <- if (mode == "pipeline_crosswalk") "geo_join_pipeline" else "geo_join_audited"
  fac$.rurb_key <- rurb_key(fac[[geo_col]], fac$rurb_join)
  profile <- aggregate_supply(fac, dictionary, ".rurb_key")
  keys <- fac[!is.na(fac$.rurb_key), c("country", ".rurb_key", geo_col, "rurb_join")] %>% distinct()
  names(keys)[names(keys) == geo_col] <- "area_name"
  assert_unique(keys, c("country", ".rurb_key"), "Area/residence keys")
  profile <- left_join_safe(profile, keys, by = c("country", "geo_join" = ".rurb_key"), what = "Area/residence label")
  profile$geo_join <- profile$area_name; profile$area_name <- NULL
  profile$link_geo_mode <- mode
  assert_unique(profile, c("country", "geo_join", "rurb_join"), "Area/residence supply profiles")
  folder <- if (mode == "pipeline_crosswalk") "datasets" else "checked_geography"
  dir.create(file.path(out, folder), recursive = TRUE, showWarnings = FALSE)
  write_table(profile, file.path(out, "supply_profiles", paste0("supply_profiles_urban_rural_", mode, ".csv")))
  saveRDS(profile, file.path(out, "supply_profiles", paste0("supply_profiles_urban_rural_", mode, ".rds")))
  for (country in names(country_config)) {
    cfg <- country_config[[country]]; x <- women[[country]]; w <- x$data
    area <- toupper(trimws(lab(w[[case_name(names(w), cfg$woman_geo)]])))
    ar <- profile[profile$country == country, , drop = FALSE]
    fc <- fac[fac$country == country, , drop = FALSE]
    ix <- rurb_match(area, x$residence, ar)
    added <- ar[ix, setdiff(names(ar), c("country", "join_level", "geo_join", "rurb_join")), drop = FALSE]
    rownames(added) <- NULL
    if (length(intersect(tolower(names(w)), tolower(names(added))))) stop("Supply columns would overwrite original women fields")
    joined <- bind_cols(w, added)
    joined$link_source_row <- seq_len(nrow(w)); joined$link_country <- country
    joined$link_geo_level <- cfg$join_level; joined$link_geo_join <- area
    joined$link_rurb <- x$residence; joined$link_geo_mode <- mode
    joined$link_rurb_basis <- rurb_config[[country]]$facility_basis
    joined$link_woman_rurb_basis <- x$residence_basis
    joined$link_rurb_proxy <- rurb_config[[country]]$facility_basis == "health_district_lookup_proxy"
    joined$link_aggregation <- "administrative_area_urban_rural"
    joined$link_has_supply <- !is.na(ix)
    admin_areas <- unique(fc[[geo_col]][!is.na(fc[[geo_col]])])
    joined$link_has_admin_supply <- norm(area) %in% norm(admin_areas) & !is.na(norm(area))
    admin_counts <- fc[!is.na(fc[[geo_col]]), , drop = FALSE] %>%
      group_by(area_key = norm(.data[[geo_col]])) %>%
      summarise(n_admin = n(), n_rural = sum(rurb_join == "Rural", na.rm = TRUE),
                n_urban = sum(rurb_join == "Urban", na.rm = TRUE), n_unknown = sum(is.na(rurb_join)), .groups = "drop")
    ai <- match(norm(area), admin_counts$area_key)
    for (v in c("n_admin", "n_rural", "n_urban", "n_unknown")) {
      value <- admin_counts[[v]][ai]; value[is.na(value)] <- 0L
      target <- switch(v, n_admin = "link_n_admin_fac", n_rural = "link_n_admin_rural",
                       n_urban = "link_n_admin_urban", n_unknown = "link_n_admin_unknown")
      joined[[target]] <- value
    }
    joined$link_issue <- rurb_issue(area, x$residence, joined$link_has_supply,
      joined$link_n_admin_fac, joined$link_n_admin_rural, joined$link_n_admin_urban,
      woman_available = x$residence_basis != "woman_residence_unavailable")
    # A missing match describes data coverage. Do not turn missing rates into
    # zero availability, or replace them silently with broader area averages.
    joined$link_status <- ifelse(is.na(norm(area)), "missing_woman_geography",
      ifelse(is.na(x$residence), if (x$residence_basis == "woman_residence_unavailable") "woman_residence_unavailable" else "missing_woman_residence",
        ifelse(!is.na(ix), "matched", "no_facility_area_residence_match")))
    joined$n_facilities[is.na(joined$n_facilities)] <- 0L
    joined$link_single_facility <- joined$link_has_supply & joined$n_facilities == 1L
    joined$link_valid_weight <- is.finite(num(w[[cfg$weight]])) & num(w[[cfg$weight]]) > 0
    record_check(country, mode, "all_original_rows_preserved", nrow(joined) == nrow(w))
    record_check(country, mode, "all_original_columns_values_preserved",
      identical(names(joined)[seq_len(ncol(w))], names(w)) && all(vapply(names(w), function(v) identical(w[[v]], joined[[v]]), logical(1))))
    record_check(country, mode, "only_exact_area_residence_matches",
      all(is.na(ix) | (norm(area) == norm(ar$geo_join[ix]) & x$residence == ar$rurb_join[ix])))
    record_check(country, mode, "missing_residence_never_matches", all(!joined$link_has_supply[is.na(x$residence)]))
    record_check(country, mode, "unmatched_supply_values_missing", all(vapply(supply_names, function(v) all(is.na(joined[[v]][is.na(ix)])), logical(1))))
    record_check(country, mode, "unmatched_matched_record_count_zero", all(joined$n_facilities[!joined$link_has_supply] == 0L))
    record_check(country, mode, "administrative_type_counts_reconcile",
      all(joined$link_n_admin_fac == joined$link_n_admin_rural + joined$link_n_admin_urban + joined$link_n_admin_unknown))
    record_check(country, mode, "issue_flag_agrees_with_exact_match", all((joined$link_issue == "matched") == joined$link_has_supply))
    record_check(country, mode, "no_rural_flag_has_no_rural_records",
      all(joined$link_n_admin_rural[joined$link_issue == "no_rural_classified_records"] == 0L))
    record_check(country, mode, "no_urban_flag_has_no_urban_records",
      all(joined$link_n_admin_urban[joined$link_issue == "no_urban_classified_records"] == 0L))
    included <- !is.na(fc[[geo_col]]) & !is.na(fc$rurb_join)
    record_check(country, mode, "facility_count_conservation", sum(ar$n_facilities) == sum(included))
    # Independently check membership, means and pooled denominators from source facilities.
    for (j in seq_len(nrow(ar))) {
      zz <- fc[included & !is.na(fc[[geo_col]]) & norm(fc[[geo_col]]) == norm(ar$geo_join[j]) &
                 !is.na(fc$rurb_join) & fc$rurb_join == ar$rurb_join[j], , drop = FALSE]
      zz <- zz[!is.na(zz$fid), , drop = FALSE]
      eq <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-10))
      expected_free <- if (all(is.na(num(zz$p_free_prod)))) NA_real_ else mean(num(zz$p_free_prod), na.rm = TRUE)
      record_check(country, mode, paste0("cell_membership_and_mean_", j),
        nrow(zz) == ar$n_facilities[j] && eq(expected_free, ar$mean_p_free_prod[j]))
      record_check(country, mode, paste0("cell_counselor_denominator_", j),
        ar$n_fp_counselor_valid[j] == sum(!is.na(zz$counselor_available)) &&
        ar$n_fp_counselor_missing[j] + ar$n_fp_counselor_valid[j] == nrow(zz))
    }
    if (anyDuplicated(names(joined)) || any(nchar(names(joined)) > 32)) stop("Invalid Stata output names")
    for (v in supply_names) attr(joined[[v]], "label") <- substr(paste0("Area/residence: ", dictionary$definition[match(v, dictionary$column_name)]), 1, 80)
    attr(joined$link_rurb_proxy, "label") <- "Facility residence type assigned from health district, not individual location"
    fname <- paste0(country, "_women_manitoba_urban_rural")
    rds <- file.path(out, folder, paste0(fname, ".rds")); dta <- file.path(out, folder, paste0(fname, ".dta"))
    saveRDS(joined, rds, compress = "gzip"); haven::write_dta(joined, dta, version = 15)
    back <- haven::read_dta(dta)
    same <- vapply(names(joined), function(v) {
      a <- joined[[v]]; b <- back[[v]]
      if (is.character(a)) {a[is.na(a)] <- ""; b[is.na(b)] <- ""; identical(as.character(a), as.character(b))}
      else isTRUE(all.equal(as.numeric(a), as.numeric(b), check.attributes = FALSE, tolerance = 0))
    }, logical(1))
    record_check(country, mode, "dta_roundtrip_all_columns", nrow(back) == nrow(joined) && identical(names(back), names(joined)) && all(same))
    # Full area x residence grid shows women-only and facilities-only cells, including missing type.
    wg <- data.frame(geo_join = area, rurb_join = ifelse(is.na(x$residence), "[missing]", x$residence)) %>% count(geo_join, rurb_join, name = "women")
    fg <- data.frame(geo_join = fc[[geo_col]], rurb_join = ifelse(is.na(fc$rurb_join), "[missing]", fc$rurb_join)) %>%
      filter(!is.na(geo_join)) %>% count(geo_join, rurb_join, name = "facilities")
    wg$geo_join[is.na(norm(wg$geo_join))] <- "[missing]"
    wg <- wg %>% group_by(geo_join, rurb_join) %>% summarise(women = sum(women), .groups = "drop")
    all_areas <- sort(unique(c(wg$geo_join, fg$geo_join)))
    grid <- expand.grid(geo_join = all_areas, rurb_join = c("Rural", "Urban", "[missing]"), stringsAsFactors = FALSE)
    grid <- left_join(grid, wg, by = c("geo_join", "rurb_join")) %>% left_join(fg, by = c("geo_join", "rurb_join"))
    grid$women[is.na(grid$women)] <- 0L; grid$facilities[is.na(grid$facilities)] <- 0L
    grid$country <- country; grid$mode <- mode
    grid$exact_match_possible <- grid$rurb_join != "[missing]" & grid$geo_join != "[missing]" & grid$facilities > 0 & !is.na(norm(grid$geo_join))
    grid$facility_residence_basis <- rurb_config[[country]]$facility_basis
    coverage[[length(coverage) + 1L]] <- grid
    record_check(country, mode, "coverage_table_retains_all_women", sum(grid$women) == nrow(w))
    issues[[length(issues) + 1L]] <- data.frame(country = country, mode = mode, geo_join = area,
      rurb_join = x$residence, link_issue = joined$link_issue,
      link_n_admin_fac = joined$link_n_admin_fac, link_n_admin_rural = joined$link_n_admin_rural,
      link_n_admin_urban = joined$link_n_admin_urban, link_n_admin_unknown = joined$link_n_admin_unknown) %>%
      count(country, mode, geo_join, rurb_join, link_issue, link_n_admin_fac, link_n_admin_rural,
            link_n_admin_urban, link_n_admin_unknown, name = "women")
    facility_coverage[[length(facility_coverage) + 1L]] <- data.frame(country = country, mode = mode,
      source_facilities = nrow(fc), included_facilities = sum(included),
      unmatched_admin = sum(is.na(fc[[geo_col]])),
      missing_rurb_with_admin = sum(!is.na(fc[[geo_col]]) & is.na(fc$rurb_join)),
      all_missing_rurb = sum(is.na(fc$rurb_join)))
    merge_checks[[length(merge_checks) + 1L]] <- data.frame(country = country, mode = mode,
      source_women = nrow(w), final_women = nrow(joined), exact_matched_women = sum(joined$link_has_supply),
      unmatched_women = sum(!joined$link_has_supply), missing_residence = sum(is.na(x$residence)),
      no_matching_cell = sum(!joined$link_has_supply & !is.na(x$residence) & !is.na(norm(area))),
      admin_match_available = sum(joined$link_has_admin_supply),
      women_in_single_facility_cells = sum(joined$link_single_facility),
      available_supply_cells = nrow(ar), women_matched_supply_cells = length(unique(ix[!is.na(ix)])),
      original_variables_preserved = TRUE,
      facility_residence_basis = rurb_config[[country]]$facility_basis,
      woman_residence_basis = x$residence_basis)
    message(country, " / ", mode, ": ", sum(joined$link_has_supply), "/", nrow(joined), " exact area/residence matches")
  }
}
write_table(bind_rows(coverage), file.path(out, "audit/area_residence_coverage.csv"))
write_table(bind_rows(facility_coverage), file.path(out, "audit/facility_residence_coverage.csv"))
write_table(bind_rows(merge_checks), file.path(out, "audit/woman_level_merge_checks.csv"))
write_table(bind_rows(issues), file.path(out, "audit/linkage_issue_summary.csv"))
write_table(bind_rows(source_checks), file.path(out, "audit/urban_rural_validation_checks.csv"))
dd <- read_chars(file.path(base_out, "dictionaries/supply_variable_dictionary.csv"))
dd$definition <- paste0("Within administrative-area plus urban/rural cell. ", dd$definition)
write_table(dd, file.path(out, "dictionaries/supply_variable_dictionary.csv"))
meta <- data.frame(variable = c("link_rurb", "link_rurb_basis", "link_woman_rurb_basis", "link_rurb_proxy", "link_has_supply", "link_has_admin_supply", "link_status", "link_single_facility", "link_issue", "n_facilities", "link_n_admin_fac", "link_n_admin_rural", "link_n_admin_urban", "link_n_admin_unknown"),
 definition = c("Woman residence type: Rural/Urban; missing stays missing.",
 "Facility classification source: recorded location or Senegal health-district lookup proxy.",
 "Woman residence source: recorded survey value, supplied lookup or unavailable.",
 "TRUE for Senegal health-district-assigned residence type; not precise facility settlement.",
 "Exact administrative-area AND residence profile attached; no fallback.",
 "Administrative-only supply exists; this flag never fills missing stratified estimates.",
 "Exact-match status or reason for unmatched profile. All women retained.",
 "Matched cell contains only one facility; inspect coverage before analysis.",
 "Detailed match/coverage reason; no_rural/no_urban_classified_records describes observed labels, not verified absence of facilities.",
 "Count of facility records in the exact attached area/type profile; zero for no attached profile. Supply percentages remain missing when unmatched.",
 "Facility records in the matched administrative area, regardless of type; zero if no administrative match.",
 "Rural-classified facility records in the woman's administrative area; not a census of physically rural facilities.",
 "Urban-classified facility records in the woman's administrative area.",
 "Facility records in the woman's administrative area without a usable residence classification."))
write_table(meta, file.path(out, "dictionaries/linkage_variable_dictionary.csv"))
jsonlite::write_json(list(component = "administrative_area_urban_rural", geography_version = run_options$geography_version,
  input_bundle = basename(input_bundle), input_sha256 = digest::digest(file = input_bundle, algo = "sha256", serialize = FALSE),
  sindh_lookup = if (nzchar(sindh_lookup)) basename(sindh_lookup) else NULL,
  sindh_lookup_sha256 = if (nzchar(sindh_lookup)) digest::digest(file = sindh_lookup, algo = "sha256", serialize = FALSE) else NULL,
  fallback = "none", unmatched_policy = "matched facility count zero; unobserved supply rates missing; detailed issue flag",
  alternative_not_implemented = "broader administrative profile with explicit resolution/fallback flag",
  source_women_retained = TRUE, validation_checks = length(source_checks),
  limitations = c("Senegal facility residence is assigned by health-district lookup and is a proxy for facility settlement.",
    "Sindh source women have no usable residence type; absent a supplied validated lookup, stratified profiles remain missing.",
    "Women may obtain services across urban/rural or administrative boundaries; these remain contextual exposures.",
    "A cell with no included facility observations is not evidence that no facilities exist there.")),
  file.path(out, "run_parameters.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
