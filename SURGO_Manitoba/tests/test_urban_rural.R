# Deterministic checks for strict area/type linkage and missing-value handling.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
script <- gsub("~+~", " ", script, fixed = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
lab <- function(x) if (inherits(x, "haven_labelled")) as.character(haven::as_factor(x)) else as.character(x)
norm <- function(x) {x <- gsub("[^A-Z0-9]", "", toupper(trimws(x))); x[is.na(x) | x == ""] <- NA_character_; x}
source(file.path(root, "R/urban_rural_helpers.R"))
stopifnot(identical(rurb_value(c(" rural ", "URBAIN", "Urban", "1", "2", "3", "", NA)),
                    c("Rural", "Urban", "Urban", "Rural", "Urban", NA_character_, NA_character_, NA_character_)))
p <- data.frame(geo_join = c("Area A", "Area A", "Area B"), rurb_join = c("Rural", "Urban", "Urban"),
                value = c(0.2, 0.8, 0.6))
i <- rurb_match(c("AREA A", "area a", "Area B", "Area B", "Area A", NA),
                 c("Rural", "Urban", "Urban", "Rural", NA, "Urban"), p)
stopifnot(identical(i, c(1L, 2L, 3L, NA_integer_, NA_integer_, NA_integer_)))
stopifnot(identical(p$value[i[1:3]], c(0.2, 0.8, 0.6)))
stopifnot(inherits(try(rurb_match("Area A", "Rural", rbind(p, p[1, ])), silent = TRUE), "try-error"))
p$rurb_join[1] <- NA_character_
stopifnot(inherits(try(rurb_match("Area A", "Rural", p), silent = TRUE), "try-error"))
lookup <- data.frame(col_id = c("c", "a", "extra"), rurb = c("Urban", "Rural", "Urban"))
x <- rurb_lookup(c("a", "b", "c"), lookup)
stopifnot(identical(x$residence, c("Rural", NA_character_, "Urban")),
          identical(x$lookup_used, c(TRUE, TRUE, FALSE)))
stopifnot(inherits(try(rurb_lookup("a", rbind(lookup, lookup[1, ])), silent = TRUE), "try-error"))
lookup$rurb[1] <- "Unknown"
stopifnot(inherits(try(rurb_lookup("a", lookup), silent = TRUE), "try-error"))
reason <- rurb_issue(c("A", "A", "B", "C", "D", "A", NA),
  c("Rural", "Urban", "Urban", "Rural", "Rural", NA, "Urban"),
  c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  c(24, 24, 3, 5, 0, 24, 0), c(0, 0, 3, 0, 0, 0, 0), c(24, 24, 0, 0, 0, 24, 0))
stopifnot(identical(reason, c("no_rural_classified_records", "matched", "no_urban_classified_records",
  "facility_type_unavailable", "no_area_facility_records", "missing_woman_residence", "missing_woman_geography")))
stopifnot(identical(rurb_issue("A", NA_character_, FALSE, 24, 0, 24, woman_available = FALSE),
                    "woman_residence_unavailable"))
cat("PASS: coding, exact linkage, missing data, partial lookup, duplicate rejection and detailed coverage reasons.\n")
