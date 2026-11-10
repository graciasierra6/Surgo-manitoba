# Compare the current PB2A indicator with the prior three-item definition.
# Inputs are caller-supplied; only aggregate statistics are written.
args <- commandArgs(trailingOnly = TRUE)
arg <- function(name) {
  i <- match(name, args)
  if (is.na(i) || i == length(args)) stop("Supply ", name)
  args[i + 1L]
}
join_dir <- arg("--join-dir")
need_file <- arg("--sindh-need-file")
out_dir <- arg("--output-dir")
file_arg <- grep("^--file=", commandArgs(), value = TRUE)
here <- dirname(sub("^--file=", "", file_arg[1]))
spec <- list(
  lagos = list(file = "lagos/lagos_definitions.R", fun = "lagos_derive_women", upper = FALSE),
  senegal = list(file = "senegal/senegal_definitions.R", fun = "senegal_definitions", upper = TRUE),
  sindh = list(file = "sindh/sindh_definitions.R", fun = "sindh_definitions", upper = FALSE),
  cotedivoire = list(file = "cotedivoire/cdi_definitions.R", fun = "cdi_definitions", upper = TRUE)
)
inputs <- c(file.path(join_dir, paste0(names(spec), "_women_manitoba.rds")), need_file)
if (!all(file.exists(inputs))) stop("Input files are missing")
before <- unname(tools::md5sum(inputs))
results <- list()
for (country in names(spec)) {
  s <- spec[[country]]
  env <- new.env(parent = globalenv())
  sys.source(file.path(here, s$file), envir = env)
  x <- readRDS(file.path(join_dir, paste0(country, "_women_manitoba.rds")))
  if (country == "lagos") x$link_rurb <- as.character(x$settlement)
  if (country %in% c("senegal", "cotedivoire")) {
    x$link_rurb <- ifelse(as.numeric(x$FV_MILIEU) == 1, "Rural",
      ifelse(as.numeric(x$FV_MILIEU) == 2, "Urban", NA_character_))
  }
  d <- if (country == "sindh") env[[s$fun]](x, need_file) else env[[s$fun]](x)
  fields <- if (s$upper) paste0("PB2", LETTERS[1:3]) else paste0("pb2", letters[1:3])
  flags <- vapply(fields, function(k) {
    v <- as.numeric(x[[k]])
    ifelse(v %in% 1:4, as.numeric(v %in% 1:2), NA_real_)
  }, numeric(nrow(x)))
  prior <- ifelse(rowSums(flags == 1, na.rm = TRUE) > 0, 1,
    ifelse(rowSums(is.na(flags)) > 0, NA_real_, 0))
  current <- flags[, 1]
  stopifnot(isTRUE(all.equal(d$health, current, check.attributes = FALSE)))
  use <- d$need %in% TRUE & is.finite(d$weight) & d$weight > 0
  w <- d$weight[use]
  old_missing <- is.na(prior[use]); new_missing <- is.na(current[use])
  domains <- c("agency", "stigma", "health", "fertility", "confidence", "opposes")
  previous_domains <- d[domains]
  previous_domains$health <- prior
  old_any <- !complete.cases(previous_domains[use, ])
  new_any <- !complete.cases(d[use, domains])
  results[[country]] <- data.frame(
    geography = country, n_in_need = sum(use),
    prior_missing_n = sum(old_missing),
    prior_missing_weighted_pct = 100 * weighted.mean(old_missing, w),
    current_missing_n = sum(new_missing),
    current_missing_weighted_pct = 100 * weighted.mean(new_missing, w),
    prior_any_domain_missing_n = sum(old_any),
    prior_any_domain_missing_weighted_pct = 100 * weighted.mean(old_any, w),
    current_any_domain_missing_n = sum(new_any),
    current_any_domain_missing_weighted_pct = 100 * weighted.mean(new_any, w),
    reduction_pp = 100 * (weighted.mean(old_missing, w) - weighted.mean(new_missing, w)),
    prior_missing_now_valid_n = sum(old_missing & !new_missing),
    prior_valid_now_missing_n = sum(!old_missing & new_missing),
    prior_concern_now_no_concern_n = sum(prior[use] %in% 1 & current[use] %in% 0),
    prior_concern_weighted_pct = 100 * weighted.mean(prior[use][!old_missing], w[!old_missing]),
    current_concern_weighted_pct = 100 * weighted.mean(current[use][!new_missing], w[!new_missing])
  )
}
stopifnot(identical(before, unname(tools::md5sum(inputs))))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out <- do.call(rbind, results)
rownames(out) <- NULL
write.csv(out, file.path(out_dir, "health_beliefs_definition_comparison.csv"), row.names = FALSE, na = "")
cat("Verified PB2A primary coding in all four country functions; source inputs unchanged.\n")
print(out, row.names = FALSE, digits = 6)
