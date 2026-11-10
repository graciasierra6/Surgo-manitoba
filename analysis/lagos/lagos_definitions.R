# Shared SURGO definitions for the Lagos FPN2 analysis and outcome models.
# Input is an existing woman-level join; original columns and need status persist.
lagos_derive_women <- function(x) {
  nv <- function(k) suppressWarnings(as.numeric(x[[k]]))
  key <- function(v) toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
  flag <- function(k, pos, valid) { v <- nv(k); ifelse(v %in% valid, as.numeric(v %in% pos), NA_real_) }
  any_concern <- function(m) ifelse(rowSums(m == 1, na.rm = TRUE) > 0, 1,
    ifelse(rowSums(is.na(m)) > 0, NA_real_, 0))
  d <- data.frame(row = seq_len(nrow(x)), geo = key(x$link_geo_join), weight = nv("weight"),
                  need = nv("fpn2") %in% 1, lga = as.character(x$link_geo_join))
  d$agency <- flag("hs7c", 1, 1:2)
  d$stigma <- any_concern(cbind(flag("pb7b", 1:2, 1:4), flag("pb7c", 1:2, 1:4)))
  # Primary health/side-effect belief: perceived health problems, PB2A only.
  d$health <- flag("pb2a", 1:2, 1:4)
  mp <- sapply(paste0("mp4_", letters[1:9]), nv)
  selected <- rowSums(mp[, 1:8, drop = FALSE] == 1, na.rm = TRUE) > 0
  none <- mp[, 9] %in% 1
  coherent <- apply(mp, 1, function(v) all(v %in% 0:1)) & nv("mp4_99") %in% 0 & (selected | none) & !(selected & none)
  d$fertility <- ifelse(coherent, as.numeric(rowSums(mp[, 1:6, drop = FALSE] == 1, na.rm = TRUE) > 0), NA_real_)
  d$confidence <- any_concern(cbind(flag("mc5a", 3:4, 1:4), flag("mc5b", 3:4, 1:4)))
  d$opposes <- flag("fp1", 3:4, 1:4)
  prompts <- sapply(paste0("cu5", letters[1:4]), nv)
  self_use <- nv("cu1") %in% c(1, 3)
  current <- ifelse(self_use | rowSums(prompts == 1, na.rm = TRUE) > 0, 1,
                    ifelse(nv("cu1") %in% 2 & rowSums(prompts == 2, na.rm = TRUE) == 4, 0, NA_real_))
  d$user <- current %in% 1
  modern <- rowSums(sapply(paste0("cu4_", 1:8), function(k) nv(k) %in% 1)) > 0 |
    nv("cu5c") %in% 1 | nv("cu5d") %in% 1 | (nv("cu4_11") %in% 1 & nv("cu4_lam") %in% 1)
  traditional <- rowSums(sapply(paste0("cu4_", c(9, 10, 12:15)), function(k) nv(k) %in% 1)) > 0 | nv("cu5a") %in% 1 | nv("cu5b") %in% 1
  d$modern <- d$user & modern
  d$traditional <- d$user & !modern & traditional
  d$intender <- current %in% 0 & nv("itu1") %in% 1:2
  d$no_intent <- current %in% 0 & nv("itu1") %in% c(3, 4, 99)
  d$unresolved <- !(d$user | d$intender | d$no_intent)
  d$wanted_start <- d$user & nv("cub8") %in% 1
  d$other_start <- d$user & nv("cub8") %in% 2
  modern_reference <- tolower(as.character(x$cu12)) %in% c("male condom", "injectables", "daily pills", "iud", "implant", "female sterilization", "male sterilization", "emergency pill", "lactational amenorrhea")
  d$modern_other_start <- d$modern & modern_reference & nv("cub8") %in% 2
  modern_target <- rowSums(sapply(paste0("its2_", 1:8), function(k) nv(k) %in% 1)) > 0
  d$traditional_modern_switch <- d$traditional & nv("itc2") %in% 1:2 & modern_target
  # Modern-use models require a known method classification. Current users
  # without a confirmed modern or traditional classification remain unresolved.
  d$y_modern <- ifelse(current %in% 0, 0,
    ifelse(d$modern, 1, ifelse(d$traditional, 0, NA_real_)))
  d$residence <- as.character(x$link_rurb)
  d$age10 <- (nv("current_age") - 30) / 10
  d$age10_sq <- d$age10^2
  edu <- ifelse(nv("rb3") %in% c(1, 2, 7), "Primary or less",
    ifelse(nv("rb3") %in% 3:4, "Secondary", ifelse(nv("rb3") %in% 5:6, "Post-secondary", NA_character_)))
  d$education <- factor(edu, levels = c("Primary or less", "Secondary", "Post-secondary"))
  d$partnered <- ifelse(nv("r1") %in% 1:4, as.numeric(nv("r1") %in% 1:3), NA_real_)
  goal <- rep("Uncertain or unstated", nrow(x))
  goal[nv("sf2") %in% 4] <- "After marriage"
  goal[nv("sf2") %in% 3] <- "After 3 years"
  goal[nv("sf2") %in% 2] <- "In 1-3 years"
  goal[nv("sf1a") %in% 2 | nv("sf1b") %in% 2] <- "No more children"
  d$fertility_goal <- factor(goal, levels = c("In 1-3 years", "After 3 years", "No more children", "After marriage", "Uncertain or unstated"))
  d$urban <- ifelse(d$residence %in% c("Urban", "Rural"), as.numeric(d$residence == "Urban"), NA_real_)
  d
}
