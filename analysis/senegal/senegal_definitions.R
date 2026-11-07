# Shared Senegal coding. Original SURGO FPN2 is retained without additions.
senegal_key <- function(v) {
  v <- iconv(as.character(v), to = "ASCII//TRANSLIT")
  v <- toupper(gsub("[^A-Za-z0-9]", "", v))
  v[is.na(v) | v == ""] <- NA_character_
  v
}
senegal_domains <- function() list(
  D = c("agency", "stigma", "health", "fertility", "confidence", "opposes"),
  DL = c("Healthcare permission barriers", "Anticipated social stigma", "Health / side-effect beliefs",
         "Future-fertility concerns", "Low method knowledge", "Opposes family planning"),
  S = c("offering", "free", "training", "counselor", "larc_stock", "popular_stock"),
  SL = c("Short + long-acting offering", "Free-product provision", "Trained-provider share",
         "FP counselor availability", "At least one LARC in stock", "Most-popular product in stock"))
senegal_supply <- function(x) {
  map <- c(offering = "share_offer_larc_and_3_sa", free = "mean_p_free_prod",
    training = "mean_p_train", counselor = "share_fp_counselor", larc_stock = "share_stocked_1plus_larc")
  a <- as.data.frame(lapply(map, function(k) as.numeric(x[[k]])))
  yes <- as.numeric(x$n_mp_in_stock_true); no <- as.numeric(x$n_mp_in_stock_false)
  a$popular_stock <- ifelse(yes + no > 0, yes / (yes + no), NA_real_)
  a
}
senegal_definitions <- function(x) {
  nv <- function(k) { if (!k %in% names(x)) stop("Missing Senegal field: ", k); as.numeric(x[[k]]) }
  flag <- function(k, pos, valid) { v <- nv(k); ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_) }
  any_flag <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1,
    ifelse(rowSums(is.na(m)) > 0, NA_real_, 0))
  stopifnot(!anyDuplicated(x$IDW))
  d <- data.frame(row = seq_len(nrow(x)), id = as.character(x$IDW), geo = senegal_key(x$link_geo_join),
    district = as.character(x$link_geo_join), weight = nv("WEIGHT"), consent = nv("IRB1") %in% 1,
    psu = as.character(x$RB0H), residence = as.character(x$link_rurb))
  d$positive <- d$consent & is.finite(d$weight) & d$weight > 0
  d$need_code <- nv("FPN2"); d$need <- d$positive & d$need_code %in% 1
  d$agency <- flag("HS7C", 1, 1:2)
  d$stigma <- any_flag(cbind(flag("PB7B", 1:2, 1:4), flag("PB7C", 1:2, 1:4)))
  d$health <- any_flag(sapply(paste0("PB2", LETTERS[1:3]), flag, pos = 1:2, valid = 1:4))
  mp <- sapply(paste0("MP4_", letters[1:9]), nv)
  selected <- rowSums(mp[, 1:8, drop = FALSE] == 1, na.rm = TRUE) > 0
  none <- mp[, 9] %in% 1
  coherent <- apply(mp, 1, function(v) all(v %in% 0:1)) & nv("MP4_99") %in% 0 &
    (selected | none) & !(selected & none)
  d$fertility <- ifelse(coherent, as.numeric(rowSums(mp[, 1:6, drop = FALSE] == 1, na.rm = TRUE) > 0), NA_real_)
  d$confidence <- any_flag(cbind(flag("MC5A", 3:4, 1:4), flag("MC5B", 3:4, 1:4)))
  d$opposes <- flag("FP1", 3:4, 1:4)
  prompts <- sapply(paste0("CU5", LETTERS[1:4]), nv)
  current <- ifelse(nv("CU1") %in% c(1, 3) | rowSums(prompts == 1, na.rm = TRUE) > 0, 1,
    ifelse(nv("CU1") %in% 2 & rowSums(prompts == 2, na.rm = TRUE) == 4, 0, NA_real_))
  d$user <- current %in% 1
  modern <- rowSums(sapply(paste0("CU4_", 1:8), function(k) nv(k) %in% 1)) > 0 |
    nv("CU5C") %in% 1 | nv("CU5D") %in% 1 | (nv("CU4_11") %in% 1 & nv("CU4_lam") %in% 1)
  traditional <- rowSums(sapply(paste0("CU4_", c(9, 10, 12:15)), function(k) nv(k) %in% 1)) > 0 |
    nv("CU5A") %in% 1 | nv("CU5B") %in% 1
  d$modern <- d$user & modern; d$traditional <- d$user & !modern & traditional
  d$y_modern <- ifelse(current %in% 0, 0, ifelse(d$modern, 1, ifelse(d$traditional, 0, NA_real_)))
  d$intender <- current %in% 0 & nv("ITU1") %in% 1:2
  d$no_intent <- current %in% 0 & nv("ITU1") %in% c(3, 4, 99)
  d$unresolved <- !(d$user | d$intender | d$no_intent)
  d$wanted_start <- d$user & nv("CUB8") %in% 1
  d$other_start <- d$user & nv("CUB8") %in% 2
  d$modern_other_start <- d$modern & nv("CUB8") %in% 2
  future_modern <- rowSums(sapply(paste0("ITS2_", 1:8), function(k) nv(k) %in% 1)) > 0
  d$traditional_modern_switch <- d$traditional & nv("ITC2") %in% 1:2 & future_modern
  age <- nv("RB2"); d$age10 <- ifelse(age >= 15 & age <= 49, (age - 30) / 10, NA_real_)
  d$age10_sq <- d$age10^2
  edu <- ifelse(nv("RB3") %in% 1:4, "Primary or less",
    ifelse(nv("RB3") %in% 5:6, "Middle / secondary",
      ifelse(nv("RB3") %in% 7:8, "Tertiary / professional", NA_character_)))
  d$education <- factor(edu, levels = c("Primary or less", "Middle / secondary", "Tertiary / professional"))
  d$partnered <- flag("R1", 1:3, 1:4)
  goal <- rep("Uncertain or unstated", nrow(x))
  goal[nv("SF2") %in% 4] <- "After marriage"; goal[nv("SF2") %in% 3] <- "After 3 years"
  goal[nv("SF2") %in% 2] <- "In 1-3 years"
  goal[nv("SF1A") %in% 2 | nv("SF1B") %in% 2 | nv("SF1C") %in% 2] <- "No more children"
  d$fertility_goal <- factor(goal, levels = c("In 1-3 years", "After 3 years", "No more children", "After marriage", "Uncertain or unstated"))
  d$urban <- ifelse(nv("FV_MILIEU") %in% 1:2, as.numeric(nv("FV_MILIEU") == 2), NA_real_)
  stopifnot(all(d$residence == ifelse(d$urban == 1, "Urban", "Rural")),
    !anyNA(d$psu[d$positive]), all(nzchar(d$psu[d$positive])),
    all(vapply(split(d$geo[d$positive], d$psu[d$positive]), function(v) length(unique(v)) == 1, logical(1))))
  d
}
