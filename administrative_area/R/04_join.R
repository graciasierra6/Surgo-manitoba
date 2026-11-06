# Join stage; run through R/run_join.R. Original survey columns are retained.
source(file.path(root, "R/helpers.R"))
source(file.path(root, "R/aggregate_supply.R"))
source(file.path(root, "R/geography.R"))
input <- Sys.getenv("LINKAGE_INPUT_DIR", file.path(root, "data/extracted"))
out <- Sys.getenv("LINKAGE_OUTPUT_DIR", file.path(root, "output"))
for (d in c("datasets", "supply_profiles", "audit", "dictionaries")) dir.create(file.path(out, d), recursive = TRUE, showWarnings = FALSE)

summary <- read_chars(file.path(input, "uom_pipeline/uom_summary_facility.csv"))
arch <- read_chars(file.path(input, "uom_pipeline/Facilities data with archetype assignments.csv"))
newv <- read_chars(file.path(input, "uom_pipeline/new_demand_vol_facility.csv"))
crosswalk <- read_chars(file.path(input, "exports/uom_surgo_geo_crosswalk_reviewed.csv"))
export_geo <- read_chars(file.path(input, "merge_pipeline/uom_facility_geo.csv"))
export_crosswalked <- read_chars(file.path(input, "merge_pipeline/uom_summary_facility_crosswalked.csv"))
reference <- read_chars(file.path(input, "exports/merged_supply_demand_department.csv"))
dictionary <- read_chars(file.path(input, "exports/merged_supply_demand_dictionary.csv"))
dictionary <- dictionary[dictionary$table_name == "merged_supply_demand_department", , drop = FALSE]
supply_names <- dictionary$column_name[dictionary$topic == "Supply"]
stopifnot(length(supply_names) > 0)
for (x in list(summary, arch, newv)) assert_unique(x, c("fid", "loc"), "Input facility table")

facilities <- list(); women <- list(); counselor_audit <- list(); id_audit <- list()
for (country in names(country_config)) {
  cfg <- country_config[[country]]
  message("Reading ", country)
  w <- read_women(file.path(input, cfg$woman_file))
  women[[country]] <- w
  p <- file.path(input, "inputs/uom", cfg$facility, cfg$raw_name)
  meta <- haven::read_dta(p, n_max = 0)
  desired <- unique(c("key", "facility_id", "fid", "state_name", "IBILE_dist", "district_name", "district_code",
                      "s1_1", "s1_2", "s1_3", "district", "division", cfg$counselor))
  wanted <- unique(unlist(lapply(desired, function(z) {
    if (z %in% names(meta)) z else names(meta)[tolower(names(meta)) == tolower(z)][1]
  })))
  wanted <- wanted[!is.na(wanted)]
  raw <- haven::read_dta(p, col_select = tidyselect::all_of(wanted))
  raw_g <- source_geo(raw, country)
  # Add the agreed counselor availability item, never assuming blank means No.
  cv <- num(raw[[case_name(names(raw), cfg$counselor)]])
  yes_codes <- 1
  no_codes <- if (country == "senegal") c(0, 2) else 2
  counsel <- ifelse(cv %in% yes_codes, 1, ifelse(cv %in% no_codes, 0, NA_real_))
  detail <- data.frame(fid = raw_g$fid, loc = cfg$loc, counselor_available = counsel, counselor_raw_code = cv)
  assert_unique(detail, c("fid", "loc"), paste(country, "counselor IDs"))
  f <- summary[summary$loc == cfg$loc, , drop = FALSE]
  f <- left_join_safe(f, arch[, c("fid", "loc", "pp_avail", "ftyp2", "pubpriv", "cluster_k3", "cluster_k4", "cluster_k5", "cluster_k6",
                                  "dmnd_ret_t", "dmnd_ret_i", "dmnd_ret_s", "dmnd_ret_l")], c("fid", "loc"), "archetype join")
  f <- left_join_safe(f, newv[, c("fid", "loc", "new_dmnd_vol", "t_new_larc", "t_new_inj", "t_new_sa", "t_new_steril")], c("fid", "loc"), "new volumes")
  f <- left_join_safe(f, detail, c("fid", "loc"), "counselor join")
  f <- attach_geography_audit(f, raw, country, w, crosswalk, export_geo, root, input)
  # Independently verify reconstructed upstream match keys against downloaded m05 output.
  ex <- export_crosswalked[export_crosswalked$loc == cfg$loc, , drop = FALSE]
  ix <- match(f$fid, ex$fid)
  expected <- toupper(trimws(ex$surgo_value[ix]))
  expected[ex$match_type[ix] == "no_match" | is.na(expected)] <- NA_character_
  same <- (is.na(expected) & is.na(f$geo_join_pipeline)) | (!is.na(expected) & !is.na(f$geo_join_pipeline) & expected == f$geo_join_pipeline)
  stopifnot(all(same), nrow(f) == nrow(raw), setequal(f$fid, raw_g$fid))
  facilities[[country]] <- f
  counselor_audit[[country]] <- data.frame(country = country, source_variable = cfg$counselor,
    question = paste(attr(raw[[case_name(names(raw), cfg$counselor)]], "label"), collapse = " "),
    yes_codes = "1", no_codes = paste(no_codes, collapse = ","), facilities = nrow(f),
    yes = sum(counsel == 1, na.rm = TRUE), no = sum(counsel == 0, na.rm = TRUE),
    missing_invalid = sum(is.na(counsel)), raw_missing = sum(is.na(cv)),
    raw_special_nonresponse = sum(!is.na(cv) & !cv %in% c(yes_codes, no_codes)))
  id_audit[[country]] <- data.frame(country = country, raw_facilities = nrow(raw), summary_facilities = nrow(f),
    geography_key_reproduced = all(same), counselor_matched = sum(f$fid %in% detail$fid),
    original_women = nrow(w), original_women_variables = ncol(w))
}
fac <- bind_rows(facilities)
write_table(bind_rows(counselor_audit), file.path(out, "audit/counselor_item_coverage.csv"))
write_table(bind_rows(id_audit), file.path(out, "audit/source_row_counts.csv"))
geo_fields <- c("country", "loc", "fid", "admin1", "admin2", "admin3", "source_geo_field", "source_geo_name", "corrected_source_geo_name",
                "assigned_join_area", "geo_join_pipeline", "geo_join_audited", "crosswalk_method", "crosswalk_status", "crosswalk_note",
                "geo_changed", "geo_review_required", "geo_assignment_basis", "geo_evidence", "counselor_raw_code", "counselor_available")
write_table(fac[geo_fields], file.path(out, "audit/facility_geography_audit.csv"))
geo_comparison <- fac %>% group_by(country, admin1, admin2, admin3, assigned_join_area, geo_join_pipeline, geo_join_audited,
                                  geo_changed, geo_review_required, geo_assignment_basis) %>% summarise(facilities = n(), .groups = "drop")
write_table(geo_comparison, file.path(out, "audit/geographic_assignment_comparison.csv"))

message("Reproducing upstream geographic supply measures")
pipeline <- aggregate_supply(fac, dictionary, "geo_join_pipeline")
parity <- check_upstream_parity(pipeline, reference, supply_names)
write_table(parity, file.path(out, "audit/upstream_supply_parity.csv"))
if (any(!parity$passed)) {
  print(head(parity[!parity$passed, ], 25))
  stop("Upstream supply parity failed; see audit/upstream_supply_parity.csv")
}
message("All ", nrow(parity), " upstream supply comparisons passed")
audited <- aggregate_supply(fac, dictionary, "geo_join_audited")
write_table(pipeline, file.path(out, "supply_profiles/supply_profiles_pipeline_crosswalk.csv"))
write_table(audited, file.path(out, "supply_profiles/supply_profiles_audited_geography.csv"))
saveRDS(pipeline, file.path(out, "supply_profiles/supply_profiles_pipeline_crosswalk.rds"))
saveRDS(audited, file.path(out, "supply_profiles/supply_profiles_audited_geography.rds"))

# Preserve all SURGO variables and all original records, including records missing weights.
merge_log <- list()
export_modes <- strsplit(Sys.getenv("LINKAGE_EXPORT_MODES", "pipeline_crosswalk"), ",", fixed=TRUE)[[1]]
for (mode in export_modes) {
  a <- if (mode == "pipeline_crosswalk") pipeline else audited
  for (country in names(country_config)) {
    cfg <- country_config[[country]]; w <- women[[country]]
    target <- toupper(trimws(lab(w[[case_name(names(w), cfg$woman_geo)]])))
    ar <- a[a$country == country, , drop = FALSE]
    lookup <- match(norm(target), norm(ar$geo_join))
    if (anyDuplicated(norm(ar$geo_join))) stop(country, ": ambiguous area profiles")
    added <- ar[lookup, setdiff(names(ar), c("country", "join_level", "geo_join")), drop = FALSE]
    rownames(added) <- NULL
    collision <- intersect(tolower(names(w)), tolower(names(added)))
    if (length(collision)) stop("Original SURGO columns would be overwritten: ", paste(collision, collapse = ", "))
    joined <- bind_cols(w, added)
    joined$link_source_row <- seq_len(nrow(w))
    joined$link_country <- country
    joined$link_geo_level <- cfg$join_level
    joined$link_geo_join <- target
    joined$link_has_supply <- !is.na(lookup)
    joined$link_geo_mode <- mode
    joined$link_status <- ifelse(is.na(target) | target == "", "missing_woman_geography",
                                  ifelse(!is.na(lookup), "matched", "no_matched_facility_profile"))
    joined$n_facilities[is.na(joined$n_facilities)] <- 0
    weight_col <- case_name(names(w), cfg$weight)
    joined$link_valid_weight <- is.finite(num(w[[weight_col]])) & num(w[[weight_col]]) > 0
    stopifnot(nrow(joined) == nrow(w), identical(names(joined)[seq_len(ncol(w))], names(w)),
              all(vapply(names(w), function(v) identical(joined[[v]], w[[v]]), logical(1))))
    if (anyDuplicated(names(joined)) || any(nchar(names(joined)) > 32)) stop("Invalid/overlong output column names")
    for (v in supply_names) {
      d <- dictionary$definition[match(v, dictionary$column_name)]
      attr(joined[[v]], "label") <- substr(d, 1, 80)
    }
    for (k in 3:6) for (j in seq_len(k)) {
      v <- paste0("share_cluster_k", k, "_", j)
      attr(joined[[v]], "label") <- paste0("Share of matched facilities in cluster ", j, " of k=", k, " solution (0-1)")
    }
    attr(joined$share_fp_counselor, "label") <- "Share of reporting facilities with FP counselor available (0-1)"
    folder <- if (mode == "pipeline_crosswalk") "datasets" else "checked_geography"
    dir.create(file.path(out, folder), recursive=TRUE, showWarnings=FALSE)
    fname <- paste0(country, "_women_manitoba")
    saveRDS(joined, file.path(out, folder, paste0(fname, ".rds")), compress = "gzip")
    haven::write_dta(joined, file.path(out, folder, paste0(fname, ".dta")), version = 15)
    # Read back the actual deliverable to verify row count, field order and source values.
    back <- haven::read_dta(file.path(out, folder, paste0(fname, ".dta")))
    same_values <- vapply(names(w), function(v) {
      x <- w[[v]]; y <- back[[v]]
      # Stata represents missing strings as ""; RDS retains the original R NA.
      if (is.character(x)) {
        x <- as.character(x); y <- as.character(y)
        x[is.na(x)] <- ""; y[is.na(y)] <- ""
        identical(x, y)
      } else
        isTRUE(all.equal(as.numeric(x), as.numeric(y), check.attributes = FALSE, tolerance = 0))
    }, logical(1))
    if (!all(same_values) || nrow(back) != nrow(w)) stop("DTA round-trip altered source data: ", country)
    merge_log[[length(merge_log) + 1]] <- data.frame(country = country, mode = mode,
      original_women = nrow(w), final_women = nrow(joined), original_variables = ncol(w),
      final_variables = ncol(joined), matched_women = sum(joined$link_has_supply),
      unmatched_women = sum(!joined$link_has_supply), original_variables_preserved = all(same_values),
      supply_areas = nrow(ar), invalid_or_missing_weights_preserved = sum(!joined$link_valid_weight))
    message(country, " / ", mode, ": ", nrow(joined), " women; ", sum(joined$link_has_supply), " matched")
    rm(back, joined); gc(verbose = FALSE)
  }
}
write_table(bind_rows(merge_log), file.path(out, "audit/woman_level_merge_checks.csv"))
coverage <- bind_rows(lapply(names(country_config), function(c) {
  z <- fac[fac$country == c, ]
  data.frame(country = c, facilities = nrow(z), pipeline_matched = sum(!is.na(z$geo_join_pipeline)),
             audited_matched = sum(!is.na(z$geo_join_audited)), changed_assignments = sum(z$geo_changed),
             qualified_assignments = sum(z$geo_review_required))
}))
write_table(coverage, file.path(out, "audit/facility_match_coverage.csv"))
saveRDS(fac, file.path(out, "audit/facility_linkage_inputs.rds"), compress = "gzip")

# Dictionary: retain upstream definitions and explain the newly added families.
dd <- dictionary[dictionary$topic == "Supply", c("column_name", "topic", "source", "definition", "is_inferred")]
extra <- list()
for (k in 3:6) for (j in seq_len(k)) extra[[length(extra) + 1]] <- data.frame(
  column_name = paste0("share_cluster_k", k, "_", j), topic = "Supply addition", source = "archetype file (d01)",
  definition = paste0("n_cluster_k", k, "_", j, " / n_facilities; proportion 0-1, multiply by100 for percent. Denominator is all geographically included facility records."),
  is_inferred = "False")
for (v in c("share_fp_counselor", "n_fp_counselor_yes", "n_fp_counselor_no", "n_fp_counselor_valid", "n_fp_counselor_missing", "coverage_fp_counselor")) {
  def <- switch(v, share_fp_counselor = "Yes / (Yes+No) to FP counselor/advisor availability; Lagos/Sindh Q702, Senegal/CDI s7_2. No FP-provision filter; unknown/special responses excluded, never zero-filled. Proportion 0-1, not counseling quality or receipt.",
                n_fp_counselor_yes = "Included facilities reporting counselor available.",
                n_fp_counselor_no = "Included facilities reporting counselor unavailable.",
                n_fp_counselor_valid = "Yes+No reporting facilities: denominator of share_fp_counselor.",
                n_fp_counselor_missing = "Included facility records without a valid counselor availability answer.",
                coverage_fp_counselor = "n_fp_counselor_valid / n_facilities (0-1).")
  extra[[length(extra) + 1]] <- data.frame(column_name = v, topic = "Supply addition", source = "Granular facility survey", definition = def, is_inferred = "False")
}
write_table(bind_rows(dd, bind_rows(extra)), file.path(out, "dictionaries/supply_variable_dictionary.csv"))
jsonlite::write_json(list(lagos_source = "Main sample,4555 records; user selected; no oversample appended",
  primary_geography = "pipeline_crosswalk; optional checked_geography exports documented alternatives",
  original_supply_columns = length(supply_names), archetype_share_columns = 18,
  counselor_question = "Counselor/advisor availability, not observed counseling receipt or quality",
  women_merge = "Many women to one geographic profile; all source rows/variables retained; no consent,need,weight or match filters",
  facility_aggregation = "Replicates upstream definitions, missing-value handling and unweighted aggregation; no upstream facility recoding changes",
  parity = list(comparisons = nrow(parity), passed = sum(parity$passed)),
  limitations = c("District-to-department crosswalks are contextual assignments; no facility GPS overlay was available.",
                  "Senegal Diakhao,Sangalkam,Yeumbeul retain boundary/time qualifications; flags are attached.",
                  "Original facility missing-answer coding is inherited for the121 reference measures; reproducing it is not a new validation of those definitions.",
                  "No individual outcomes,need flags or survey design identifiers were redefined.")),
  file.path(out, "audit/build_summary.json"), auto_unbox = TRUE, pretty = TRUE)
message("Build complete: ", out)
