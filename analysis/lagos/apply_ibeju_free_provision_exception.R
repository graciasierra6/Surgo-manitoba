# Apply one documented profile-level exception to an existing Lagos join.
# No facility-wide recoding or other geographic change is performed.
# Required arguments: --input FILE --output-dir FOLDER
args <- commandArgs(trailingOnly = TRUE)
opt <- function(key) {
  i <- match(key, args)
  if (!is.na(i) && i < length(args)) return(args[i + 1L])
  hit <- grep(paste0("^", key, "="), args, value = TRUE)
  if (length(hit)) return(sub(paste0("^", key, "="), "", hit[1L]))
  stop("Supply ", key)
}
input <- opt("--input"); out <- opt("--output-dir")
for (pkg in c("haven", "jsonlite"))
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Install ", pkg)
before_hash <- unname(tools::md5sum(input))
x <- readRDS(input)
needed <- c("link_geo_join", "link_rurb", "mean_p_free_prod", "n_facilities", "fpn2")
stopifnot(all(needed %in% names(x)))
area <- toupper(gsub("[^A-Za-z0-9]", "", as.character(x$link_geo_join)))
urban <- toupper(trimws(as.character(x$link_rurb))) %in% "URBAN"
target <- area %in% "IBEJULEKKI" & urban
stopifnot(any(target), all(is.na(x$mean_p_free_prod[target])),
          all(as.numeric(x$n_facilities[target]) == 1))
y <- x
y$mean_p_free_prod[target] <- 0
other <- setdiff(names(x), "mean_p_free_prod")
stopifnot(identical(names(x), names(y)), nrow(x) == nrow(y),
          all(vapply(other, function(k) identical(x[[k]], y[[k]]), logical(1))),
          identical(x$mean_p_free_prod[!target], y$mean_p_free_prod[!target]),
          all(y$mean_p_free_prod[target] == 0))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
output_name <- "lagos_women_manitoba_urban_rural.rds"
output_path <- file.path(out, output_name)
stopifnot(normalizePath(input) != normalizePath(output_path, mustWork = FALSE))
saveRDS(y, output_path, compress = "gzip")
haven::write_dta(y, file.path(out, "lagos_women_manitoba_urban_rural.dta"), version = 15)
back <- readRDS(output_path)
stopifnot(identical(y, back))
stata <- haven::read_dta(file.path(out, "lagos_women_manitoba_urban_rural.dta"))
stopifnot(nrow(stata) == nrow(y), identical(names(stata), names(y)))
for (k in names(y)) {
  a <- y[[k]]; b <- stata[[k]]
  if (is.character(a)) {
    a[is.na(a)] <- ""; b[is.na(b)] <- ""
    stopifnot(identical(as.character(a), as.character(b)))
  } else stopifnot(isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 0)))
}
audit <- data.frame(area = "IBEJU/LEKKI", residence = "Urban", variable = "mean_p_free_prod",
                    original_value = NA_real_, revised_value = 0,
                    source_facility_records = 1, women_affected = sum(target),
                    fpn2_1_women_affected = sum(target & as.numeric(x$fpn2) %in% 1),
                    profiles_changed = 1, stringsAsFactors = FALSE)
write.csv(audit, file.path(out, "case_specific_change.csv"), row.names = FALSE, na = "Missing")
note <- c(
  "# Ibeju-Lekki Urban: free-provision exception",
  "",
  "Decision: change only mean_p_free_prod for the Ibeju-Lekki Urban profile from missing to 0. Apply the contextual value to every woman assigned to that profile, irrespective of FPN2, so the geographic supply characteristic remains consistent.",
  "",
  "Reason: the profile contains one facility. Its raw Q506 answer is No (does not provide FP services). The cleaned free and not-free product counts are both zero, so the original within-facility ratio is 0/0 and the geographic mean is missing. The analysis uses zero as a case-specific indication of no free FP provision at this recorded facility, rather than excluding the area's women for this variable.",
  "",
  "Interpretation: this is an explicit local analysis exception, not a newly observed product proportion or an unchanged UoM calculation. It does not establish absence of free FP services throughout the LGA or at unobserved facilities. The Urban profile is based on only one facility; the 45 Rural facilities are not substituted.",
  "",
  "Scope: no other LGA/residence profile is recoded. No global Q506 recoding is applied. The facility-level p_free_prod, pooled share_products_free, original reporting counts, other supply variables, all SURGO values, weights, eligibility flags, row order and geographic assignments remain unchanged. Original n_reporting_p_free_prod remains zero because the exception is not an additional observed facility response.",
  "",
  "The reference joined input is retained unchanged. case_specific_change.csv records the counts, and case_specific_change.json records the verification checks. The RDS and DTA in this folder are the analysis copies with this exception applied."
)
writeLines(note, file.path(out, "DECISION.md"))
stopifnot(identical(before_hash, unname(tools::md5sum(input))))
jsonlite::write_json(list(input_file = basename(input), input_md5 = before_hash,
  output_file = output_name, original_input_unchanged = TRUE,
  profiles_changed = 1, women_changed = sum(target),
  fpn2_1_women_changed = sum(target & as.numeric(x$fpn2) %in% 1),
  sole_changed_variable = "mean_p_free_prod", all_other_columns_identical = TRUE,
  non_target_values_identical = TRUE, row_order_preserved = TRUE,
  rds_roundtrip_exact = TRUE, dta_roundtrip_all_fields_verified = TRUE,
  rule = "Ibeju-Lekki Urban only: missing mean_p_free_prod set to 0 as an explicit no-provision exception; no general facility recoding."),
  file.path(out, "case_specific_change.json"), auto_unbox = TRUE, pretty = TRUE)
cat("Applied one profile exception to", sum(target), "women, including",
    sum(target & as.numeric(x$fpn2) %in% 1), "with supplied FPN2 = 1. All other values preserved.\n")
