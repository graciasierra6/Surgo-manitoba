# Shared ingestion and geographic linkage. Inputs are runtime parameters.
# Supply aggregation here preserves upstream parity. Report preparation applies FP eligibility.
load_linkage_definitions <- function(target) {
  evalq({
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


    suppressPackageStartupMessages({
      library(dplyr)
      library(haven)
      library(readr)
      library(jsonlite)
    })

    read_chars <- function(path) {
      as.data.frame(readr::read_csv(path, col_types = cols(.default = col_character()),
                                   na = c("", "NA", "NaN"), show_col_types = FALSE,
                                   name_repair = "minimal"), check.names = FALSE)
    }

    write_table <- function(x, path) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      readr::write_csv(x, path, na = "")
    }

    num <- function(x) suppressWarnings(as.numeric(x))
    lab <- function(x) {
      if (inherits(x, "haven_labelled")) as.character(haven::as_factor(x)) else as.character(x)
    }
    norm <- function(x) {
      x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
      x <- gsub("[^A-Z0-9]", "", toupper(trimws(x)))
      x[is.na(x) | x %in% c("", "NA")] <- NA_character_
      x
    }
    bool <- function(x) {
      s <- tolower(trimws(as.character(x)))
      ifelse(s %in% c("true", "1"), 1, ifelse(s %in% c("false", "0"), 0, NA_real_))
    }
    mean_sql <- function(x) {
      x <- num(x)
      if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
    }
    sum_sql <- function(x) {
      x <- num(x)
      if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
    }
    ratio <- function(a, b) if (is.na(b) || b == 0) NA_real_ else a / b

    assert_unique <- function(x, cols, what) {
      k <- do.call(paste, c(lapply(x[cols], function(z) ifelse(is.na(z), "<NA>", z)), sep = "\r"))
      if (anyDuplicated(k)) stop(what, " is not unique on ", paste(cols, collapse = ", "))
      invisible(TRUE)
    }

    left_join_safe <- function(x, y, by, what) {
      assert_unique(y, unname(by), paste(what, "lookup"))
      n <- nrow(x)
      out <- left_join(x, y, by = by, relationship = "many-to-one", na_matches = "never")
      if (nrow(out) != n) stop(what, " changed row count")
      out
    }


    read_women <- function(path) {
      if (grepl("\\.dta$", path, ignore.case = TRUE)) haven::read_dta(path) else
        readr::read_csv(path, show_col_types = FALSE, guess_max = 20000,
                       name_repair = "minimal", na = c("", "NA"))
    }

    case_name <- function(names, wanted) {
      exact <- match(wanted, names)
      if (!is.na(exact)) return(names[exact])
      j <- which(tolower(names) == tolower(wanted))
      if (length(j) != 1) stop("Cannot uniquely resolve field ", wanted)
      names[j]
    }

    source_geo <- function(raw, country) {
      cfg <- country_config[[country]]
      get <- function(v) lab(raw[[case_name(names(raw), v)]])
      if (country == "lagos") {
        out <- data.frame(fid = get("key"), admin1 = get("state_name"),
                           admin2 = get("IBILE_dist"), admin3 = get("district_name"))
      } else if (country == "senegal") {
        out <- data.frame(fid = get("fid"), admin1 = get("s1_1"), admin2 = get("s1_3"), admin3 = NA_character_)
      } else if (country == "cotedivoire") {
        out <- data.frame(fid = get("fid"), admin1 = get("district"), admin2 = get("s1_1"), admin3 = get("s1_2"))
      } else {
        fid <- get("key")
        empty <- is.na(fid) | trimws(fid) == ""
        fid[empty] <- get("facility_id")[empty]
        out <- data.frame(fid = fid, admin1 = "Sindh", admin2 = get("division"), admin3 = get("district_name"))
      }
      out$loc <- cfg$loc
      out$fid <- trimws(out$fid)
      out
    }


    # Reproduce the supply columns in upstream workflow_merge/m06, using its saved
    # facility summaries and classifications. Facility measures are not redefined.

    aggregate_supply <- function(fac, dictionary, geo_col = "geo_join_pipeline") {
      supply_names <- dictionary$column_name[dictionary$topic == "Supply"]
      out <- list()
      category_map <- c(n_flvl_ = "flvl", n_ftyp_ = "ftyp", n_rurb_ = "rurb",
                        n_mpop_ = "mpop_prdct", n_mp_in_stock_ = "mp_in_stock",
                        n_mp_stockout_ = "mp_stockout", n_pubpriv_ = "pubpriv", n_ftyp2_ = "ftyp2")
      clean_val <- function(x) {
        x[is.na(x)] <- "NA"
        gsub("^_+|_+$", "", tolower(gsub("[^A-Za-z0-9]+", "_", as.character(x))))
      }
      flag_map <- c(share_offer_1plus_larc = "one_larc_offer", share_offer_3plus_larc = "three_larc_offer",
                    share_offer_larc_and_3_sa = "one_larc_3_sa_offer", share_stocked_1plus_larc = "one_larc_stockin",
                    share_stocked_3plus_larc = "three_larc_stockin", share_stocked_larc_and_3_sa = "one_larc_3_sa_stockin")
      mean_map <- c(mean_facility_any_stockout = "p_any_stockout", mean_p_free_prod = "p_free_prod",
                    mean_avg_duration = "avg_duration", mean_mp_stockout_dur = "mp_stockout_dur",
                    mean_p_train = "p_train", mean_mdn_skill_lvl = "mdn_skill_lvl",
                    mean_n_unique_prod = "n_unique_prod", mean_n_srvc_offr = "n_srvc_offr", mean_pp_avail = "pp_avail")
      sum_map <- c(vol_total = "dmnd_vol", vol_larc = "t_larc", vol_inj = "t_inj", vol_short_acting = "t_sa",
                   vol_steril = "t_steril", vol_new_client_total = "new_dmnd_vol", vol_new_larc = "t_new_larc",
                   vol_new_inj = "t_new_inj", vol_new_short_acting = "t_new_sa", vol_new_steril = "t_new_steril",
                   sum_t_in_stock = "t_in_stock", sum_t_out_of_stock = "t_out_of_stock",
                   sum_t_free_prod = "t_free_prod", sum_t_not_free_prod = "t_not_free_prod",
                   vol_returning_total = "dmnd_ret_t", vol_returning_inj = "dmnd_ret_i",
                   vol_returning_short_acting = "dmnd_ret_s", vol_returning_larc = "dmnd_ret_l")
      countries <- unique(fac$country)
      for (country in countries) {
        fc <- fac[fac$country == country & !is.na(fac[[geo_col]]) & fac[[geo_col]] != "", , drop = FALSE]
        for (g in sort(unique(fc[[geo_col]]))) {
          z <- fc[fc[[geo_col]] == g, , drop = FALSE]
          r <- list(country = country, join_level = unique(z$join_level), geo_join = g, n_facilities = nrow(z))
          for (v in names(flag_map)) {
            # Spark AVG(decimal 1.0 / 0.0) in the reference export has five decimal places.
            # A trailing zero may be omitted in the downloaded CSV.
            r[[v]] <- round(mean_sql(bool(z[[flag_map[[v]]]])), 5)
          }
          for (v in names(mean_map)) r[[v]] <- mean_sql(z[[mean_map[[v]]]])
          for (v in names(sum_map)) r[[v]] <- sum_sql(z[[sum_map[[v]]]])
          r$stockout_rate_pooled <- ratio(sum_sql(z$t_out_of_stock),
                                          sum_sql(num(z$t_in_stock) + num(z$t_out_of_stock)))
          r$share_products_free <- ratio(sum_sql(z$t_free_prod),
                                        sum_sql(num(z$t_free_prod) + num(z$t_not_free_prod)))
          h <- !is.na(num(z$n_tot)) & !is.na(num(z$n_train))
          r$n_hcp_total <- sum_sql(z$n_tot[h])
          r$n_hcp_trained <- sum_sql(z$n_train[h])
          r$share_providers_trained <- ratio(r$n_hcp_trained, r$n_hcp_total)
          r$n_facilities_reporting_volume <- sum(!is.na(num(z$dmnd_vol)))
          for (v in c("p_free_prod", "mp_stockout_dur", "p_train", "mdn_skill_lvl")) {
            r[[paste0("n_reporting_", v)]] <- sum(!is.na(num(z[[v]])))
          }
          for (prefix in names(category_map)) {
            source <- category_map[[prefix]]
            source_values <- clean_val(z[[source]])
            wanted <- supply_names[startsWith(supply_names, prefix)]
            for (v in wanted) r[[v]] <- sum(source_values == substring(v, nchar(prefix) + 1))
          }
          for (k in 3:6) {
            cluster <- num(z[[paste0("cluster_k", k)]])
            for (j in seq_len(k)) r[[paste0("n_cluster_k", k, "_", j)]] <- sum(cluster == j, na.rm = TRUE)
          }
          # Two requested additions: archetype shares and FP counselor availability.
          for (k in 3:6) for (j in seq_len(k)) {
            r[[paste0("share_cluster_k", k, "_", j)]] <- r[[paste0("n_cluster_k", k, "_", j)]] / nrow(z)
          }
          valid <- !is.na(z$counselor_available)
          r$n_fp_counselor_yes <- sum(z$counselor_available == 1, na.rm = TRUE)
          r$n_fp_counselor_no <- sum(z$counselor_available == 0, na.rm = TRUE)
          r$n_fp_counselor_valid <- sum(valid)
          r$n_fp_counselor_missing <- sum(!valid)
          r$share_fp_counselor <- if (any(valid)) mean(z$counselor_available[valid]) else NA_real_
          r$coverage_fp_counselor <- mean(valid)
          review <- if (geo_col == "geo_join_pipeline") z$crosswalk_status %in% "needs_review" else z$geo_review_required %in% TRUE
          r$link_n_geo_review <- sum(review)
          r$link_share_geo_review <- mean(review)
          r$link_n_geo_changed <- sum(z$geo_changed %in% TRUE)
          r$link_geo_mode <- if (geo_col == "geo_join_pipeline") "pipeline_crosswalk" else "audited_geography"
          rollup <- if (geo_col == "geo_join_pipeline") z$geo_rollup_pipeline else z$geo_rollup
          r$link_n_rollup <- sum(rollup %in% TRUE)
          r$link_rollup_dominated <- r$link_n_rollup > nrow(z) / 2
          missing <- setdiff(supply_names, names(r))
          if (length(missing)) stop("Unimplemented upstream supply columns: ", paste(missing, collapse = ", "))
          out[[length(out) + 1]] <- as.data.frame(r, stringsAsFactors = FALSE)
        }
      }
      ans <- bind_rows(out)
      for (c in unique(ans$country)) for (v in c("share_offer_3plus_larc", "share_stocked_3plus_larc")) {
        z <- ans$country == c
        if (all(is.na(ans[[v]][z]) | ans[[v]][z] == 0)) ans[[v]][z] <- NA_real_
      }
      assert_unique(ans, c("country", "join_level", "geo_join"), "Aggregated supply")
      for (k in 3:6) {
        cs <- paste0("n_cluster_k", k, "_", seq_len(k))
        ss <- paste0("share_cluster_k", k, "_", seq_len(k))
        stopifnot(all(rowSums(ans[cs]) == ans$n_facilities), all(abs(rowSums(ans[ss]) - 1) < 1e-12))
      }
      ans
    }

    check_upstream_parity <- function(aggregated, reference, supply_names) {
      rows <- list()
      for (i in seq_len(nrow(reference))) {
        rr <- reference[i, , drop = FALSE]
        z <- aggregated[aggregated$country == rr$country & aggregated$geo_join == rr$geo_join, , drop = FALSE]
        for (v in c("n_facilities", supply_names)) {
          actual <- if (nrow(z)) num(z[[v]][1]) else if (v == "n_facilities") 0 else NA_real_
          expected <- num(rr[[v]])
          same <- (is.na(actual) && is.na(expected)) ||
            (!is.na(actual) && !is.na(expected) && abs(actual - expected) <= 1e-9 * max(1, abs(expected)))
          rows[[length(rows) + 1]] <- data.frame(country = rr$country, geo_join = rr$geo_join, variable = v,
                                                 expected = expected, reproduced = actual, passed = same)
        }
      }
      bind_rows(rows)
    }


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

  }, envir=target)
}
verify_input_bundle <- function(input_zip, cache_dir) {
  stopifnot(file.exists(input_zip))
  members <- utils::unzip(input_zip,list=TRUE)$Name
  if(any(grepl("^/|^[A-Za-z]:|\\\\",members)) ||
     any(vapply(strsplit(members,"/",fixed=TRUE),function(x)any(x==".."),logical(1))))stop("Unsafe ZIP member")
  if(!"bundle_manifest.json" %in% members)stop("Input ZIP lacks bundle_manifest.json")
  dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
  utils::unzip(input_zip,files="bundle_manifest.json",exdir=cache_dir)
  manifest<-jsonlite::read_json(file.path(cache_dir,"bundle_manifest.json"),simplifyVector=TRUE)
  for(i in seq_len(nrow(manifest$files))) {
    f<-manifest$files[i,];p<-file.path(cache_dir,f$member)
    valid<-file.exists(p) && file.info(p)$size==f$bytes && digest::digest(file=p,algo="sha256")==f$sha256
    if(!f$member %in% members)stop("Missing input ZIP member: ",f$member)
    if(!valid)utils::unzip(input_zip,files=f$member,exdir=cache_dir)
    stopifnot(file.exists(p),file.info(p)$size==f$bytes,digest::digest(file=p,algo="sha256")==f$sha256)
  }
  manifest
}
build_administrative_join <- function(root,input_zip,out,input) {
  message("Verifying inputs and reconstructing administrative joins")
  manifest<-verify_input_bundle(input_zip,input)
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  jsonlite::write_json(list(input_bundle=basename(input_zip),input_sha256=digest::digest(file=input_zip,algo="sha256"),
    geography_version="both",keep_all_source_women=TRUE,lagos_sample="main",country_config="Documented source configuration",input_file_count=nrow(manifest$files)),
    file.path(out,"run_parameters.json"),auto_unbox=TRUE,pretty=TRUE)
  e<-new.env(parent=globalenv());load_linkage_definitions(e)
  e$root<-root;e$input<-input;e$out<-out
  evalq({
    # Join stage; run through R/run_join.R. Original survey columns are retained.
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
    export_modes <- c("pipeline_crosswalk", "audited_geography")
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

  },e)
  evalq({
    # Join/reference validation only; no models or substantive analysis.
    dest <- file.path(out, "reconciliation")
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)

    fac <- readRDS(file.path(out, "audit/facility_linkage_inputs.rds"))
    crosswalk <- read_chars(file.path(input, "exports/uom_surgo_geo_crosswalk_reviewed.csv"))
    reference <- read_chars(file.path(input, "exports/merged_supply_demand_department.csv"))
    source_coverage <- read_chars(file.path(input, "exports/merged_supply_demand_coverage.csv"))
    arch <- read_chars(file.path(input, "uom_pipeline/Facilities data with archetype assignments.csv"))
    curated <- read_chars(file.path(input, "curated/Facilities data with archetype assignments.csv"))
    dictionary <- read_chars(file.path(input, "exports/merged_supply_demand_dictionary.csv"))
    sup_names <- dictionary$column_name[dictionary$table_name == "merged_supply_demand_department" & dictionary$topic == "Supply"]
    pipeline <- readRDS(file.path(out, "supply_profiles/supply_profiles_pipeline_crosswalk.rds"))
    audited <- readRDS(file.path(out, "supply_profiles/supply_profiles_audited_geography.rds"))
    tests <- list()
    check <- function(name, ok, detail = "") {
      tests[[length(tests) + 1]] <<- data.frame(check = name, passed = isTRUE(ok), detail = detail)
      if (!isTRUE(ok)) warning("CHECK FAILED: ", name, ": ", detail)
    }
    same_num <- function(a, b) {
      a <- num(a); b <- num(b)
      (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a-b) <= 1e-9 * pmax(1, abs(b)))
    }
    source_key <- function(x, prefix="") apply(x[paste0(prefix,c("admin1","admin2","admin3"))],1,
        function(z)paste(ifelse(is.na(z)|z=="","<NA>",z),collapse="\r"))
    cw_keys <- paste(crosswalk$loc, source_key(crosswalk,"uom_"),sep="\r")
    fi <- match(paste(fac$loc,source_key(fac),sep="\r"),cw_keys)
    check("Every facility maps to one source-crosswalk row",!anyNA(fi)&&!anyDuplicated(cw_keys))
    fac$source_crosswalk_row <- fi

    # The exact source file is retained separately, unchanged byte for byte.
    file.copy(file.path(input,"exports/uom_surgo_geo_crosswalk_reviewed.csv"),
              file.path(dest,"his_reviewed_crosswalk_exact.csv"),overwrite=TRUE)
    summary_rows <- list(); flow_rows <- list()
    for(i in seq_len(nrow(crosswalk))){
      x <- crosswalk[i,,drop=FALSE]; z <- fac[fac$source_crosswalk_row==i,,drop=FALSE]
      cfg_name <- names(country_config)[vapply(country_config,function(y)y$loc==x$loc,logical(1))]
      target <- if(country_config[[cfg_name]]$join_level=="geo_level3")x$surgo_geo_level3 else x$surgo_geo_level2
      if(x$match_method%in%c("none")||x$status%in%"unmappable"||is.na(target)||target=="")target<-NA_character_
      ref_targets <- unique(z$geo_join_pipeline)
      equal_ref <- all((is.na(z$geo_join_pipeline)&is.na(target))|(!is.na(z$geo_join_pipeline)&!is.na(target)&z$geo_join_pipeline==toupper(target)))
      count_ok <- nrow(z)==num(x$n_facilities)
      actual_targets <- sort(unique(z$geo_join_audited[!is.na(z$geo_join_audited)]))
      physical <- sort(unique(z$assigned_join_area[!is.na(z$assigned_join_area)]))
      restored <- sum(!is.na(z$corrected_source_geo_name)&!is.na(z$source_geo_name)&norm(z$corrected_source_geo_name)!=norm(z$source_geo_name))
      r<-x
      r$source_crosswalk_id<-sprintf("CW%04d",i)
      r$our_reference_facilities<-nrow(z)
      r$our_reference_target<-if(length(ref_targets)&&!all(is.na(ref_targets)))paste(sort(unique(na.omit(ref_targets))),collapse="; ")else NA_character_
      r$reference_count_matches<-count_ok;r$reference_target_matches<-equal_ref
      r$our_source_names_after_label_check<-paste(sort(unique(na.omit(z$corrected_source_geo_name))),collapse="; ")
      r$our_assigned_join_areas<-paste(physical,collapse="; ")
      r$our_audited_surgo_targets<-paste(actual_targets,collapse="; ")
      r$our_audited_target_count<-length(actual_targets)
      r$our_audited_matched_facilities<-sum(!is.na(z$geo_join_audited))
      r$our_changed_assignments<-sum(z$geo_changed)
      r$our_restored_source_labels<-restored
      r$our_qualified_assignments<-sum(z$geo_review_required)
      r$unused_parent_field_issue<-x$loc=="Lagos" & norm(x$uom_admin3)%in%norm(c("Agege","Alimosho","Ifako-Ijaiye")) & norm(x$uom_admin2)!="IKEJA"
      r$assignment_basis<-paste(sort(unique(z$geo_assignment_basis)),collapse=" | ")
      r$review_result<-if(!count_ok||!equal_ref)"FAIL_REFERENCE_REPRODUCTION" else if(any(z$geo_review_required))"QUALIFIED_GEOGRAPHY" else if(any(z$geo_changed))"DOCUMENTED_TARGET_DIFFERENCE" else if(restored>0)"LABEL_CORRECTION_SAME_TARGET" else if(r$unused_parent_field_issue)"REFERENCE_MATCH_UNUSED_PARENT_ISSUE" else "REFERENCE_MATCH"
      summary_rows[[i]]<-r
      split_key<-paste(ifelse(is.na(z$geo_join_audited),"<NO_SURGO_TARGET>",z$geo_join_audited),
                       ifelse(is.na(z$assigned_join_area),"<UNKNOWN>",z$assigned_join_area),sep="\r")
      for(k in unique(split_key)){
        s<-z[split_key==k,,drop=FALSE]
        flow_rows[[length(flow_rows)+1]]<-data.frame(source_crosswalk_id=sprintf("CW%04d",i),loc=x$loc,
          uom_admin1=x$uom_admin1,uom_admin2=x$uom_admin2,uom_admin3=x$uom_admin3,
          his_surgo_target=target,our_audited_target=s$geo_join_audited[1],
          our_assigned_join_area=s$assigned_join_area[1],facility_records=nrow(s),
          changed_records=sum(s$geo_changed),qualified_records=sum(s$geo_review_required),
          basis=paste(sort(unique(s$geo_assignment_basis)),collapse=" | "))
      }
    }
    cw_summary<-bind_rows(summary_rows);flows<-bind_rows(flow_rows)
    check("All source crosswalk facility counts reproduced",all(cw_summary$reference_count_matches))
    check("All source crosswalk targets reproduced",all(cw_summary$reference_target_matches))
    check("Expanded crosswalk flows sum to all facility records",sum(flows$facility_records)==nrow(fac))
    write_table(cw_summary,file.path(dest,"complete_crosswalk_comparison.csv"))
    write_table(flows,file.path(dest,"expanded_crosswalk_assignment_flows.csv"))

    # Facility classifications: imported assignments must match by facility ID, not row position.
    key<-function(x)paste(x$loc,x$fid,sep="\r")
    ai<-match(key(fac),key(arch));oi<-match(key(fac),key(curated))
    curated_match_method<-rep("fid_and_loc",nrow(fac))
    missing_curated<-which(is.na(oi))
    if(length(missing_curated)){
      # The supplied legacy file has13 blank Sindh fids; the README documents sid recovery.
      sid_key<-paste(curated$loc,curated$sid,sep="\r")
      for(j in missing_curated){
        hit<-which(sid_key==paste(fac$loc[j],fac$sid[j],sep="\r") & (is.na(curated$fid)|curated$fid==""))
        if(length(hit)==1){oi[j]<-hit;curated_match_method[j]<-"unique_sid_for_legacy_blank_fid"}
      }
    }
    check("All facility IDs present in supplied archetype outputs",!anyNA(ai)&&!anyNA(oi))
    write_table(data.frame(country=fac$country,fid=fac$fid,sid=fac$sid,curated_match_method=curated_match_method),
                file.path(dest,"archetype_identity_reconciliation.csv"))
    classification_cells<-list();classification_summary<-list();permutation<-list()
    ari<-function(a,b){
      t<-table(a,b);n<-sum(t);choose2<-function(z)z*(z-1)/2
      if(n<2)return(1)
      expected<-sum(choose2(rowSums(t)))*sum(choose2(colSums(t)))/choose2(n)
      denom<-(sum(choose2(rowSums(t)))+sum(choose2(colSums(t))))/2-expected
      if(abs(denom)<1e-14)return(1)
      (sum(choose2(t))-expected)/denom
    }
    for(country in names(country_config)){
      take<-fac$country==country
      for(k in 3:6){
        v<-paste0("cluster_k",k);ours<-num(fac[[v]][take]);his<-num(arch[[v]][ai[take]]);old<-num(curated[[v]][oi[take]])
        ok<-same_num(ours,his)
        classification_summary[[length(classification_summary)+1]]<-data.frame(country=country,solution=k,facilities=sum(take),
          exact_pipeline_matches=sum(ok),exact_pipeline_match_pct=100*mean(ok),curated_code_matches=sum(same_num(ours,old)),
          adjusted_rand_vs_curated=ari(ours,old))
        classification_cells[[length(classification_cells)+1]]<-data.frame(country=country,fid=fac$fid[take],solution=k,
          our_cluster=ours,his_pipeline_cluster=his,his_curated_cluster=old,exact_pipeline_match=ok)
        pp<-as.data.frame(table(pipeline_cluster=his,curated_cluster=old));pp<-pp[pp$Freq>0,,drop=FALSE]
        pp$country<-country;pp$solution<-k;permutation[[length(permutation)+1]]<-pp
        check(paste(country,k,"archetype assignment parity"),all(ok))
        check(paste(country,k,"archetype partition parity with curated"),abs(ari(ours,old)-1)<1e-12)
      }
      for(v in c("pubpriv","ftyp2"))check(paste(country,v,"classification parity"),
        all((is.na(fac[[v]][take])&is.na(arch[[v]][ai[take]]))|(!is.na(fac[[v]][take])&!is.na(arch[[v]][ai[take]])&fac[[v]][take]==arch[[v]][ai[take]])))
    }
    write_table(bind_rows(classification_cells),file.path(dest,"facility_archetype_assignment_comparison.csv"))
    write_table(bind_rows(classification_summary),file.path(dest,"archetype_classification_summary.csv"))
    write_table(bind_rows(permutation),file.path(dest,"curated_pipeline_cluster_label_mapping.csv"))
    labels<-arch[,c("loc","cluster_k6","NamesC")]%>%mutate(NamesC=gsub("[\r\n]+"," ",NamesC))%>%group_by(loc,cluster_k6,NamesC)%>%summarise(facilities=n(),.groups="drop")
    write_table(labels,file.path(dest,"six_archetype_label_lookup.csv"))

    # Compare the same analytical women/need flags as his source RDS, not raw totals.
    area_rows<-list();cohort_rows<-list();coverage_rows<-list();woman_parity<-list()
    for(country in names(country_config)){
      cfg<-country_config[[country]]
      original<-read_women(file.path(input,cfg$woman_file))
      his<-readRDS(file.path(input,"surgo_pipeline",country,"wra_design_ready.rds"))
      oid<-case_name(names(original),cfg$id);hid<-case_name(names(his),cfg$id)
      ids<-as.character(original[[oid]]);hids<-as.character(his[[hid]])
      present_ids<-!is.na(ids)&ids!=""
      present_hids<-!is.na(hids)&hids!=""
      check(paste(country,"nonmissing woman IDs unique; analytic IDs complete"),
            !anyDuplicated(ids[present_ids])&&!anyDuplicated(hids)&&all(present_hids))
      hi<-match(ids,hids)
      check(paste(country,"every source analytic woman exists in raw data"),all(hids%in%ids))
      in_his<-!is.na(hi)
      need<-rep(NA_real_,nrow(original));need[in_his]<-num(his$need_n[hi[in_his]])
      geo<-toupper(trimws(lab(original[[case_name(names(original),cfg$woman_geo)]])))
      hg<-toupper(trimws(as.character(his$.dept)))
      check(paste(country,"analytic woman geography unchanged"),all(norm(geo[in_his])==norm(hg[hi[in_his]])))
      w<-num(original[[case_name(names(original),cfg$weight)]])
      check(paste(country,"analytic weights unchanged"),all(same_num(w[in_his],his$.w[hi[in_his]])))
      c<-readRDS(file.path(out,"datasets",paste0(country,"_women_manitoba.rds")))
      rr<-reference[reference$country==country,,drop=FALSE]
      ri<-match(norm(geo),norm(rr$geo_join))
      ncmp<-0;ndiff<-0
      for(v in c("n_facilities",sup_names)){
        expected<-num(rr[[v]][ri]);if(v=="n_facilities")expected[is.na(ri)]<-0
        same<-same_num(c[[v]],expected)
        ncmp<-ncmp+length(same);ndiff<-ndiff+sum(!same)
      }
      woman_parity[[country]]<-data.frame(country=country,women=nrow(original),supply_columns=length(sup_names)+1,
        comparisons=ncmp,mismatches=ndiff)
      check(paste(country,"every reference woman's supply values agree with source geography"),ndiff==0)
      consent_field<-case_name(names(original),"irb1")
      cons<-num(original[[consent_field]])==1;cons[is.na(cons)]<-FALSE
      valid<-is.finite(w)&w>0
      cohort_rows[[country]]<-data.frame(country=country,raw_women=nrow(original),his_analytic_women=nrow(his),
        original_women_retained_in_ours=nrow(c),women_outside_his_analysis=sum(!in_his),
        missing_or_nonpositive_weight=sum(!valid),nonconsenting_or_unknown=sum(!cons),
        excluded_with_valid_weight=sum(!in_his&valid),raw_missing_woman_ID=sum(!present_ids),missing_analytic_ids= sum(!hids%in%ids))
      for(i in seq_len(nrow(rr))){
        r<-rr[i,,drop=FALSE];z<-norm(geo)==norm(r$geo_join);z[is.na(z)]<-FALSE
        hc<-z&in_his
        p<-pipeline[pipeline$country==country&norm(pipeline$geo_join)==norm(r$geo_join),,drop=FALSE]
        aa<-audited[audited$country==country&norm(audited$geo_join)==norm(r$geo_join),,drop=FALSE]
        ours_n<-if(nrow(p))p$n_facilities else 0;aud_n<-if(nrow(aa))aa$n_facilities else 0
        area_rows[[length(area_rows)+1]]<-data.frame(country=country,geo_join=r$geo_join,
          his_facilities=num(r$n_facilities),our_reference_facilities=ours_n,our_audited_facilities=aud_n,
          his_analytic_women=num(r$n_sample),our_same_cohort_women=sum(hc),our_all_raw_women=sum(z),
          his_women_in_need=num(r$n_in_need),our_same_cohort_need=sum(need[hc]==1,na.rm=TRUE),
          his_has_supply=bool(r$has_supply)==1,our_reference_has_supply=nrow(p)>0,our_audited_has_supply=nrow(aa)>0,
          his_binding_constraint=r$binding_constraint)
      }
      cov<-source_coverage[source_coverage$country==country,,drop=FALSE]
      x<-bind_rows(area_rows);x<-x[x$country==country,,drop=FALSE]
      total_fac<-sum(fac$country==country)
      hfac<-sum(x$his_facilities);p_fac<-sum(x$our_reference_facilities);a_fac<-sum(x$our_audited_facilities)
      sample<-sum(x$his_analytic_women);nd<-sum(x$his_women_in_need)
      coverage_rows[[country]]<-data.frame(country=country,source_facility_total=num(cov$n_facilities_total),our_facility_total=total_fac,
        his_matched_facilities=hfac,our_reference_matched_facilities=p_fac,our_audited_matched_facilities=a_fac,
        his_facility_match_pct=num(cov$pct_matched),our_reference_facility_match_pct=100*p_fac/total_fac,our_audited_facility_match_pct=100*a_fac/total_fac,
        his_geographies=nrow(x),his_geographies_with_supply=sum(x$his_has_supply),our_reference_geographies_with_supply=sum(x$our_reference_has_supply),
        our_audited_geographies_with_supply=sum(x$our_audited_has_supply),his_analytic_women=sample,
        his_analytic_women_with_supply=sum(x$his_analytic_women[x$his_has_supply]),
        our_reference_same_cohort_with_supply=sum(x$our_same_cohort_women[x$our_reference_has_supply]),
        our_audited_same_cohort_with_supply=sum(x$our_same_cohort_women[x$our_audited_has_supply]),
        his_analytic_need=nd,his_need_with_supply=sum(x$his_women_in_need[x$his_has_supply]),
        our_reference_same_need_with_supply=sum(x$our_same_cohort_need[x$our_reference_has_supply]),
        our_audited_same_need_with_supply=sum(x$our_same_cohort_need[x$our_audited_has_supply]))
    }
    areas<-bind_rows(area_rows);coverage<-bind_rows(coverage_rows);cohorts<-bind_rows(cohort_rows)
    check("All source area facility counts match reference",all(areas$his_facilities==areas$our_reference_facilities))
    check("All source area analytic population counts reproduced",all(areas$his_analytic_women==areas$our_same_cohort_women))
    check("All source area need counts reproduced",all(areas$his_women_in_need==areas$our_same_cohort_need))
    check("All source supply/demand overlap classifications reproduced",all(areas$his_has_supply==areas$our_reference_has_supply))
    write_table(areas,file.path(dest,"all_area_overlap_population_comparison.csv"))
    write_table(coverage,file.path(dest,"country_overlap_population_comparison.csv"))
    write_table(cohorts,file.path(dest,"raw_versus_his_analytic_population.csv"))
    write_table(bind_rows(woman_parity),file.path(dest,"woman_supply_reference_parity.csv"))
    checks<-bind_rows(tests)
    write_table(checks,file.path(dest,"reconciliation_checks.csv"))
    result<-list(checks=nrow(checks),passed=sum(checks$passed),failed=sum(!checks$passed),
      source_crosswalk_rows=nrow(crosswalk),comparison_crosswalk_rows=nrow(cw_summary),expanded_flow_rows=nrow(flows),
      source_geographic_rows=nrow(areas),facility_archetype_comparisons=nrow(bind_rows(classification_cells)),
      woman_supply_cell_comparisons=sum(bind_rows(woman_parity)$comparisons),
      limitations=c("Imported archetype assignments are checked against source outputs; clustering was not independently refit.",
                    "Source binding_constraint is a SURGO cascade classification,not the same construct as an LGA supply/demand score classification.",
                    "Proposed completions/corrections are listed separately; the reference mapping reproduces the source exactly.",
                    "This is a technical reconciliation,not an external certification of survey measurement or facility boundary membership."))
    jsonlite::write_json(result,file.path(dest,"reconciliation_summary.json"),auto_unbox=TRUE,pretty=TRUE)
    print(result)
    if(any(!checks$passed))stop("Reconciliation has failed checks; inspect reconciliation_checks.csv")

  },e)
  invisible(out)
}
build_residence_join <- function(root,input_zip,out,input) {
  base_out<-file.path(out,"admin_reference")
  build_administrative_join(root,input_zip,base_out,input)
  e<-new.env(parent=globalenv());load_linkage_definitions(e)
  e$root<-root;e$out<-out;e$input<-input;e$base_out<-base_out
  e$rurb_config<-e$urban_rural_config;e$sindh_lookup<-"";e$input_bundle<-input_zip
  e$run_options<-list(geography_version="both")
  evalq({
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

  },e)
  invisible(out)
}
