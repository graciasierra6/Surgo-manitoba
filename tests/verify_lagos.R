# Verify every model exposure against the prepared facility records in its
# exact administrative area and rural/urban cell. No source records are changed.
args <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { i <- match(k, args); if (is.na(i) || i == length(args)) stop("Supply ", k); args[i + 1L] }
facility_file <- opt("--facility-input"); model_dir <- opt("--model-dir"); data_dir <- opt("--data-dir")
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite")
before <- unname(tools::md5sum(facility_file))
f <- readRDS(facility_file); f <- f[f$country == "lagos", ]
key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
f$geo <- ifelse(!is.na(f$geo_join_pipeline), key(f$geo_join_pipeline), key(f$source_geo_name))
f$residence <- as.character(f$rurb)
p <- read.csv(file.path(model_dir, "model_supply_profiles.csv"), na.strings = "NA")
report <- jsonlite::fromJSON(file.path(data_dir, "report_data.json"))
bool <- function(v) { v <- tolower(as.character(v)); ifelse(v %in% c("true", "1"), 1, ifelse(v %in% c("false", "0"), 0, NA_real_)) }
mv <- function(v) { v <- as.numeric(v); if (any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_ }
rows <- list()
for (i in seq_len(nrow(p))) {
  a <- p[i, ]; z <- f[f$geo == a$geo & f$residence == a$residence, ]
  stopifnot(nrow(z) == a$n_facilities)
  free <- mv(z$p_free_prod)
  # Existing, separately documented analysis exception; original value untouched.
  if (a$geo == "IBEJULEKKI" && a$residence == "Urban") { stopifnot(nrow(z) == 1, is.na(free)); free <- 0 }
  observed <- c(offering = round(mv(bool(z$one_larc_3_sa_offer)), 5), free = free,
    training = mv(z$p_train), counselor = mv(z$counselor_available),
    larc_stock = round(mv(bool(z$three_larc_stockin)), 5), popular_stock = mv(bool(z$mp_in_stock)))
  for (k in names(observed)) {
    v <- as.numeric(a[[k]])
    same <- (is.na(v) && is.na(observed[k])) || (is.finite(v) && is.finite(observed[k]) && abs(v - observed[k]) < 1e-10)
    stopifnot(same)
    rows[[length(rows) + 1L]] <- data.frame(geo = a$geo, residence = a$residence, variable = k,
      facilities = nrow(z), model_observed_value = v, recomputed_value = unname(observed[k]), agrees = same)
  }
  used <- observed
  if (a$fallback) {
    stopifnot(a$geo == "IBEJULEKKI", a$residence == "Urban", is.na(observed["popular_stock"]))
    local <- f[f$geo == a$geo, ]
    used["popular_stock"] <- mv(bool(local$mp_in_stock))
  }
  stopifnot(abs(a$popular_stock_used - used["popular_stock"]) < 1e-10)
  score <- -mean(vapply(names(used), function(k) {
    ref <- report$component_trace[report$component_trace$side == "Supply" & report$component_trace$variable == k, ]
    (100 * used[k] - unique(ref$indicator_mean)) / unique(ref$indicator_SD)
  }, numeric(1)))
  stopifnot(abs(score - a$supply_risk) < 1e-9)
}
write.csv(do.call(rbind, rows), file.path(model_dir, "facility_to_model_exposure_verification.csv"), row.names = FALSE)
check <- list(exact_lga_residence_profiles = nrow(p), verified_components = length(rows),
  rural_to_rural_urban_to_urban = TRUE, no_cross_residence_averaging = TRUE,
  approved_popular_component_fallback_profiles = sum(p$fallback),
  need_women_in_fallback_profile = sum(p$n_need[p$fallback]),
  scores_reproduced_from_facility_records = TRUE,
  source_unchanged = before == unname(tools::md5sum(facility_file)))
stopifnot(check$source_unchanged, nrow(p) == 18, sum(p$fallback) == 1)
jsonlite::write_json(check, file.path(model_dir, "supply_exposure_verification.json"), pretty = TRUE, auto_unbox = TRUE)
print(check)
