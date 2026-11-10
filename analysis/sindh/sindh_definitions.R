# Country-specific coding from the supplied Pakistan questionnaire and need file.
sindh_definitions <- function(x, need_file) {
  nv <- function(k) { if (!k %in% names(x)) stop("Missing Sindh field: ", k); as.numeric(x[[k]]) }
  key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
  flag <- function(k, pos, valid) { v <- nv(k); ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_) }
  any_flag <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1,
    ifelse(rowSums(is.na(m)) > 0, NA_real_, 0))
  ref <- readRDS(need_file)
  stopifnot(!anyDuplicated(x$col_id), !anyDuplicated(ref$col_id), "fpn2_derived" %in% names(ref))
  ii <- match(x$col_id, ref$col_id)
  d <- data.frame(row = seq_len(nrow(x)), id = x$col_id, geo = key(x$link_geo_join),
    district = as.character(x$link_geo_join), weight = nv("Normalizedweights"),
    consent = nv("irb1") %in% 1, psu = as.character(x$clusterCode))
  d$positive <- d$consent & is.finite(d$weight) & d$weight > 0
  d$need_code <- ref$fpn2_derived[ii]
  d$need <- d$positive & d$need_code %in% 1
  # Verify the authorized upstream derivation independently; retain its SA1 policy.
  exclude <- nv("sf0b") %in% 1 | nv("sf0c") %in% 1 |
    nv("sf1a") %in% 3 | nv("sf1b") %in% 3 | nv("sf1c") %in% 3 |
    nv("sa0") %in% 2 | nv("sa1") %in% c(5, 6, 95, 97) |
    nv("pc0") %in% 1 | nv("sf2") %in% 1 | nv("pc18") %in% 1 | nv("pc19") %in% 2
  stopifnot(all(!is.na(ii[d$positive])), all(d$need_code[d$positive] == as.integer(!exclude[d$positive])),
    sum(d$need) == 3274, all(nv("fpn2") == 95),
    identical(as.numeric(x$rb0e), readBin(writeBin(as.double(x$clusterCode), raw(), size = 4),
      what = "double", n = nrow(x), size = 4)))
  d$agency <- flag("hs7c", 1, 1:2)
  d$stigma <- any_flag(cbind(flag("pb7b", 1:2, 1:4), flag("pb7c", 1:2, 1:4)))
  # Primary health/side-effect belief: perceived health problems, PB2A only.
  d$health <- flag("pb2a", 1:2, 1:4)
  mp <- sapply(paste0("mp4_", 1:9), nv)
  selected <- rowSums(mp[, 1:8, drop = FALSE] == 1, na.rm = TRUE) > 0
  none <- mp[, 9] %in% 1
  coherent <- apply(mp, 1, function(v) all(v %in% 0:1)) & (selected | none) & !(selected & none)
  d$fertility <- ifelse(coherent, as.numeric(rowSums(mp[, 1:6, drop = FALSE] == 1, na.rm = TRUE) > 0), NA_real_)
  d$confidence <- any_flag(cbind(flag("mc5a", 3:4, 1:4), flag("mc5b", 3:4, 1:4)))
  d$opposes <- flag("fp1", 3:4, 1:4)
  prompts <- sapply(paste0("cu5", letters[1:4]), nv)
  current <- ifelse(nv("cu1") %in% c(1, 3) | rowSums(prompts == 1, na.rm = TRUE) > 0, 1,
    ifelse(nv("cu1") %in% 2 & rowSums(prompts == 2, na.rm = TRUE) == 4, 0, NA_real_))
  d$user <- current %in% 1
  modern <- rowSums(sapply(paste0("cu4_", 1:8), function(k) nv(k) %in% 1)) > 0 |
    nv("cu5c") %in% 1 | nv("cu5d") %in% 1 | (nv("cu4_11") %in% 1 & nv("cu4_lam") %in% 1)
  traditional <- rowSums(sapply(paste0("cu4_", c(9,10,12:15)), function(k) nv(k) %in% 1)) > 0 |
    nv("cu5a") %in% 1 | nv("cu5b") %in% 1
  d$modern <- d$user & modern
  d$traditional <- d$user & !modern & traditional
  d$y_modern <- ifelse(current %in% 0, 0, ifelse(d$modern, 1, ifelse(d$traditional, 0, NA_real_)))
  d$intender <- current %in% 0 & nv("itu1") %in% 1:2
  d$no_intent <- current %in% 0 & nv("itu1") %in% c(3, 4, 99)
  d$unresolved <- !(d$user | d$intender | d$no_intent)
  d$wanted_start <- d$user & nv("cub8") %in% 1
  d$other_start <- d$user & nv("cub8") %in% 2
  d$modern_other_start <- d$modern & nv("cub8") %in% 2
  future_modern <- rowSums(sapply(paste0("its2_", 1:8), function(k) nv(k) %in% 1)) > 0
  d$traditional_modern_switch <- d$traditional & nv("itc2") %in% 1:2 & future_modern
  age <- nv("rb2"); d$age10 <- ifelse(age >= 15 & age <= 49, (age - 30) / 10, NA_real_)
  d$age10_sq <- d$age10^2
  # Pakistan education categories differ from the Nigeria instrument.
  edu <- ifelse(nv("rb3") %in% 1:4, "Primary or less",
    ifelse(nv("rb3") %in% 5:7, "Middle / secondary", ifelse(nv("rb3") %in% 8:9, "Tertiary / professional", NA_character_)))
  d$education <- factor(edu, levels = c("Primary or less", "Middle / secondary", "Tertiary / professional"))
  goal <- rep("Uncertain or unstated", nrow(x))
  goal[nv("sf2") %in% 2] <- "In 1-3 years"; goal[nv("sf2") %in% 3] <- "After 3 years"
  goal[nv("sf1a") %in% 2 | nv("sf1b") %in% 2 | nv("sf1c") %in% 2] <- "No more children"
  d$fertility_goal <- factor(goal, levels = c("In 1-3 years", "After 3 years", "No more children", "Uncertain or unstated"))
  d$partnered <- flag("r1", 1, 1:2)
  # Named cluster is present; do not invent urban/rural or sampling strata.
  stopifnot(!anyNA(d$psu[d$positive]), all(nzchar(d$psu[d$positive])),
    all(vapply(split(d$geo[d$positive], d$psu[d$positive]), function(v) length(unique(v)) == 1, logical(1))))
  d
}
sindh_domains <- function() list(
  D = c("agency", "stigma", "health", "fertility", "confidence", "opposes"),
  DL = c("Healthcare permission barriers", "Anticipated social stigma", "Health / side-effect beliefs", "Future-fertility concerns", "Low method knowledge", "Opposes family planning"),
  S = c("offering", "free", "training", "counselor", "larc_stock", "popular_stock"),
  SL = c("Short + long-acting offering", "Free-product provision", "Trained-provider share", "FP counselor availability", "At least one LARC in stock", "Most-popular product in stock"))
sindh_supply <- function(x) {
  map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod", training = "mean_p_train",
    counselor = "share_fp_counselor", larc_stock = "share_stocked_1plus_larc")
  a <- as.data.frame(lapply(map, function(k) as.numeric(x[[k]])))
  yes <- as.numeric(x$n_mp_in_stock_true); no <- as.numeric(x$n_mp_in_stock_false)
  a$popular_stock <- ifelse(yes + no > 0, yes / (yes + no), NA_real_)
  a
}
