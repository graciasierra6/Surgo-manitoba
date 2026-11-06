# Only explicit rural/urban labels or documented codes 1/2 are accepted.
rurb_value <- function(x) {
  value <- toupper(trimws(lab(x)))
  value <- gsub("[[:space:]]+", " ", value)
  ifelse(value %in% c("RURAL", "1"), "Rural",
         ifelse(value %in% c("URBAN", "URBAIN", "2"), "Urban", NA_character_))
}

# Build no key when either residence or administrative identity is unavailable.
rurb_key <- function(area, residence) {
  a <- norm(area)
  r <- rurb_value(residence)
  ifelse(is.na(a) | is.na(r), NA_character_, paste(a, r, sep = "|"))
}

# A lookup must be unique on BOTH fields; missing never matches missing.
rurb_match <- function(area, residence, profiles) {
  key <- rurb_key(profiles$geo_join, profiles$rurb_join)
  if (anyNA(key) || anyDuplicated(key)) stop("Invalid or duplicate area/residence profiles")
  match(rurb_key(area, residence), key)
}

rurb_same <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

# Optional externally validated woman residence lookup. Validate IDs and codes;
# geographic correctness of the supplied classifications requires source review.
rurb_lookup <- function(woman_ids, lookup, id_field = "col_id") {
  if (!all(c(id_field, "rurb") %in% names(lookup))) stop("Residence lookup requires ID and rurb columns")
  ids <- as.character(lookup[[id_field]])
  if (any(is.na(ids) | trimws(ids) == "") || anyDuplicated(ids)) stop("Residence lookup IDs must be nonmissing and unique")
  residence <- rurb_value(lookup$rurb)
  if (anyNA(residence)) stop("Residence lookup has a missing or unrecognized residence classification")
  index <- match(as.character(woman_ids), ids)
  list(residence = residence[index], raw_value = as.character(lookup$rurb[index]),
       lookup_used = ids %in% as.character(woman_ids))
}

# These are linkage/data-coverage reasons, not proof of absent services.
rurb_issue <- function(area, residence, matched, n_admin, n_rural, n_urban,
                       woman_available = TRUE) {
  r <- rurb_value(residence)
  issue <- rep("no_area_type_profile", length(area))
  issue[!matched & n_admin == 0] <- "no_area_facility_records"
  issue[!matched & n_admin > 0 & n_rural + n_urban == 0] <- "facility_type_unavailable"
  issue[!matched & n_admin > 0 & n_rural == 0 & n_urban > 0 & !is.na(r) & r == "Rural"] <- "no_rural_classified_records"
  issue[!matched & n_admin > 0 & n_urban == 0 & n_rural > 0 & !is.na(r) & r == "Urban"] <- "no_urban_classified_records"
  issue[matched] <- "matched"
  issue[is.na(r)] <- if (woman_available) "missing_woman_residence" else "woman_residence_unavailable"
  issue[is.na(norm(area))] <- "missing_woman_geography"
  issue
}
