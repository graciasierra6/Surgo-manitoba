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

config_path <- Sys.getenv("LINKAGE_COUNTRY_CONFIG", file.path(root, "R/00_config.R"))
country_config <- source(config_path, local = new.env(parent = baseenv()))$value

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
