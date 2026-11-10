# Report-specific supply definitions; historical country definitions are unchanged.
base_senegal_domains <- senegal_domains
senegal_domains <- function() {
  a <- base_senegal_domains()
  a$S <- c("offering", "free", "training", "daily_provision", "larc_stock", "stockout_burden")
  a$SL <- c("Short + long-acting offering", "Free-product provision", "Trained-provider share",
            "Daily provision of offered modern methods", "At least one LARC in stock", "Stockout burden")
  a
}

senegal_supply <- function(x) {
  map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod",
    training = "mean_p_train", daily_provision = "mean_daily_modern_provision",
    larc_stock = "share_stocked_1plus_larc", stockout_burden = "mean_facility_any_stockout")
  missing <- setdiff(unname(map), names(x))
  if (length(missing)) stop("Missing updated supply fields: ", paste(missing, collapse = ", "))
  as.data.frame(lapply(map, function(k) as.numeric(x[[k]])))
}

# Raw burden remains visible in tables. Reverse it only for favorable scoring/PCA.
senegal_favorable_supply <- function(x) {
  x <- as.data.frame(x)
  if ("stockout_burden" %in% names(x)) x$stockout_burden <- 1 - x$stockout_burden
  x
}

# Seven core modern categories: pills, injectables, male/female condoms, ECP, IUD,
# implants. Raw numeric codes avoid collapsing frequency labels through offering.
senegal_frequency <- function(raw) {
  fields <- paste0("s5_7", letters[1:7])
  stopifnot(all(fields %in% names(raw)))
  m <- vapply(raw[fields], as.numeric, numeric(nrow(raw)))
  valid <- m >= 1 & m <= 5
  offered <- m >= 1 & m <= 4
  n_offered <- rowSums(offered, na.rm = TRUE)
  n_daily <- rowSums(m == 1, na.rm = TRUE)
  n_weekly <- rowSums(m >= 1 & m <= 2, na.rm = TRUE)
  daily <- ifelse(n_offered > 0, n_daily / n_offered, NA_real_)
  weekly <- ifelse(n_offered > 0, n_weekly / n_offered, NA_real_)
  data.frame(daily_provision = daily, at_least_weekly_provision = weekly,
    frequency_daily_categories = n_daily, frequency_offered_categories = n_offered,
    frequency_valid_categories = rowSums(valid, na.rm = TRUE),
    frequency_unknown_categories = 7L - rowSums(valid, na.rm = TRUE))
}
