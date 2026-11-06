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
