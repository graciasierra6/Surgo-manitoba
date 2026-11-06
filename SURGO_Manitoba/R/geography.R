# Reconstruct the upstream facility geography, then document supported geographic
# corrections separately. No fuzzy matching or coordinate inference is used.

attach_geography_audit <- function(fac, raw, country, women, crosswalk, source_geography, project_root, input_dir) {
  cfg <- country_config[[country]]
  g <- source_geo(raw, country)
  assert_unique(g, c("fid", "loc"), paste(country, "raw facility geography"))
  exported <- source_geography[source_geography$loc == cfg$loc, , drop = FALSE]
  ii <- match(g$fid, exported$fid)
  if (anyNA(ii)) stop(country, ": raw facility IDs missing from exported geography")
  for (v in c("admin1", "admin2", "admin3")) {
    a <- g[[v]]; b <- exported[[v]][ii]
    a[is.na(a) | a == ""] <- "<NA>"; b[is.na(b) | b == ""] <- "<NA>"
    if (!identical(a, b)) stop(country, ": raw/exported ", v, " differs")
  }
  fac <- left_join_safe(fac, g[, c("fid", "loc", "admin1", "admin2", "admin3")],
                         by = c("fid", "loc"), what = paste(country, "raw geography"))
  # Map each facility through all three geographic levels, not LGA/district name alone.
  cw <- crosswalk[crosswalk$loc == cfg$loc, , drop = FALSE]
  composite <- function(df, prefix = "") {
    apply(df[paste0(prefix, c("admin1", "admin2", "admin3"))], 1,
          function(v) paste(ifelse(is.na(v) | v == "", "<NA>", v), collapse = "\r"))
  }
  kk <- composite(cw, "uom_")
  if (anyDuplicated(kk)) stop(country, ": duplicate full crosswalk keys")
  ci <- match(composite(fac), kk)
  if (anyNA(ci)) stop(country, ": facility geography absent from reviewed crosswalk")
  fac$crosswalk_method <- cw$match_method[ci]
  fac$crosswalk_status <- cw$status[ci]
  fac$crosswalk_note <- cw$notes[ci]
  candidate <- if (cfg$join_level == "geo_level3") cw$surgo_geo_level3[ci] else cw$surgo_geo_level2[ci]
  unmatched <- is.na(candidate) | candidate == "" | cw$match_method[ci] == "none" | cw$status[ci] == "unmappable"
  fac$geo_join_pipeline <- toupper(trimws(candidate))
  fac$geo_join_pipeline[unmatched] <- NA_character_
  fac$country <- country
  fac$join_level <- cfg$join_level
  fac$geo_rollup <- cw$match_method[ci] %in% "parent-rollup" |
    (!is.na(cw$rollup_basis[ci]) & startsWith(cw$rollup_basis[ci], "parent-rollup"))
  fac$geo_rollup_pipeline <- fac$geo_rollup

  targets <- sort(unique(toupper(trimws(lab(women[[case_name(names(women), cfg$woman_geo)]])))))
  targets <- targets[!is.na(targets) & targets != ""]
  nk <- norm(targets)
  if (anyDuplicated(nk)) stop(country, ": ambiguous normalized SURGO geography labels")
  canonical <- function(x) targets[match(norm(x), nk)]
  fac$geo_join_audited <- fac$geo_join_pipeline
  fac$assigned_join_area <- if (country == "senegal") NA_character_ else fac$admin3
  fac$geo_assignment_basis <- "Reviewed upstream geographic crosswalk"
  fac$geo_evidence <- "reference/upstream/workflow_merge/m04_apply_crosswalk_review.sql and exported reviewed crosswalk (inside input bundle)"
  fac$geo_review_required <- !unmatched & fac$crosswalk_status %in% "needs_review"
  fac$source_geo_field <- if (country == "senegal") "s1_3" else if (country == "lagos") "district_name" else if (country == "sindh") "district_name" else "s1_2"
  fac$source_geo_name <- if (country == "senegal") fac$admin2 else fac$admin3
  fac$corrected_source_geo_name <- fac$source_geo_name

  if (country == "senegal") {
    evidence <- read_chars(file.path(input_dir, "lookups/senegal_health_district_department_evidence.csv"))
    ei <- match(norm(fac$admin2), norm(evidence[["Health district"]]))
    if (anyNA(ei)) stop("Senegal: district absent from documented department lookup")
    dept <- evidence[["Department-checked"]][ei]
    dept <- trimws(sub(" —.*$", "", dept))
    dept <- trimws(sub(" \\(.*$", "", dept))
    fac$assigned_join_area <- dept
    fac$geo_join_audited <- canonical(dept)
    fac$geo_assignment_basis <- "Health district to administrative department from documented official-source review"
    fac$geo_evidence <- evidence[["Source of data checks*"]][ei]
    # These rows have time/boundary qualifications in the supplied source review.
    fac$geo_review_required <- norm(fac$admin2) %in% norm(c("diakhao", "sangalkam", "yeumbeul"))
    fac$geo_rollup <- FALSE
  }
  if (country == "sindh") {
    district_field <- case_name(names(raw), "district")
    raw_district <- lab(raw[[district_field]])
    ri <- match(fac$fid, g$fid)
    district <- raw_district[ri]
    aliases <- read_chars(file.path(input_dir, "lookups/sindh_district_aliases.csv"))
    key <- norm(district)
    ai <- match(key, aliases$source_normalized)
    key[!is.na(ai)] <- aliases$target_normalized[ai[!is.na(ai)]]
    fac$geo_join_audited <- targets[match(key, nk)]
    if (anyNA(fac$geo_join_audited)) stop("Sindh: raw named district could not be mapped to a SURGO district")
    fac$assigned_join_area <- fac$geo_join_audited
    fac$corrected_source_geo_name <- district
    fac$source_geo_field <- "district (Name of District), preferred to incomplete district_name"
    fac$geo_assignment_basis <- "Explicit named district in original facility survey; explicit aliases to SURGO names"
    fac$geo_evidence <- "Raw district value labels; lookups/sindh_district_aliases.csv; government district naming reference in lookups/geographic_sources.json"
    fac$geo_review_required <- FALSE
    fac$geo_rollup <- FALSE
    same_existing <- is.na(fac$geo_join_pipeline) | norm(fac$geo_join_pipeline) == norm(fac$geo_join_audited)
    if (!all(same_existing)) stop("Sindh: named district conflicts with an existing matched assignment")
  }
  if (country == "cotedivoire") {
    codebook <- read_chars(file.path(input_dir, "lookups/cdi_health_district_codebook.csv"))
    raw_code <- num(raw[[case_name(names(raw), "s1_2")]])
    codes <- raw_code[match(fac$fid, g$fid)]
    restored <- codebook$label[match(codes, num(codebook$code))]
    if (anyNA(restored)) stop("CDI: code absent from companion health-district codebook")
    # The raw facility labels 1-13 are overwritten by administrative-district labels.
    # For codes 14 onward the companion codebook must agree before restoring 1-13.
    known <- codes >= 14
    if (!all(norm(restored[known]) == norm(fac$admin3[known]))) stop("CDI companion district-code correspondence failed")
    fac$corrected_source_geo_name <- restored
    fac$assigned_join_area <- restored
    # Retain existing supported matches; add only unambiguous district/department name matches.
    new_target <- canonical(restored)
    fac$geo_join_audited <- ifelse(!is.na(new_target), new_target, fac$geo_join_pipeline)
    abidjan <- codes %in% 1:10
    fac$geo_join_audited[abidjan] <- canonical("ABIDJAN")
    fac$assigned_join_area[abidjan] <- "ABIDJAN"
    subdistrict <- c("KORHOGO1" = "KORHOGO", "KORHOGO2" = "KORHOGO",
                     "GAGNOA1" = "GAGNOA", "GAGNOA2" = "GAGNOA",
                     "BOUAKENORDEST" = "BOUAKE", "BOUAKENORDOUEST" = "BOUAKE", "BOUAKESUD" = "BOUAKE",
                     "TOULEUPLEU" = "TOULEPLEU")
    si <- match(norm(restored), names(subdistrict))
    fac$assigned_join_area[!is.na(si)] <- unname(subdistrict[si[!is.na(si)]])
    fac$geo_join_audited[!is.na(si)] <- canonical(fac$assigned_join_area[!is.na(si)])
    fac$assigned_join_area[!is.na(fac$geo_join_audited)] <- fac$geo_join_audited[!is.na(fac$geo_join_audited)]
    fac$geo_assignment_basis[codes <= 13] <- "Restore mislabeled health-district codes 1-13 using companion codebook, checked against ministry district names"
    fac$geo_evidence[codes <= 13] <- "lookups/cdi_health_district_codebook.csv; ministry RASS district list in lookups/geographic_sources.json; codes 14-113 agree between raw and companion labels"
    # Health district to department remains a contextual crosswalk; no facility GPS overlay.
    fac$geo_review_required <- FALSE
    fac$geo_rollup <- abidjan
  }
  if (country == "lagos") {
    # Source LGA is the merge key. IBILE labels are retained unchanged and audited separately.
    fac$assigned_join_area <- fac$admin3
    fac$geo_review_required <- FALSE
    fac$geo_rollup <- FALSE
  }
  fac$geo_changed <- (is.na(fac$geo_join_pipeline) != is.na(fac$geo_join_audited)) |
    (!is.na(fac$geo_join_pipeline) & !is.na(fac$geo_join_audited) & norm(fac$geo_join_pipeline) != norm(fac$geo_join_audited))
  fac$geo_changed[is.na(fac$geo_changed)] <- FALSE
  fac
}
