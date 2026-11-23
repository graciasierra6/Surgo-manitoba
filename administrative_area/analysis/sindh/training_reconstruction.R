# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: bundled R only; dependencies may be supplied by the caller.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Country definitions helper.
# Inputs: Country raw response objects supplied by caller; Sindh need helper also reads D14.
# Outputs: Recoded values and definition/helper functions for the caller.
# Source context via country pipeline: D04,D08,D09,D10,D11,D12,D14; exact dataset names in
#   docs/COLLEAGUE_RUN_GUIDE.md.
# Use through its caller/module; this file is not an independent report entry point.
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Reconstruct the selected Sindh training topics from both survey encodings.
sindh_training_fields <- function(fields) {
  grep("^q406(_[a-hyz])?_[0-9]+$", fields, value = TRUE)
}

sindh_training <- function(raw) {
  selected <- c("a", "c", "d", "g", "h")
  topics <- c(letters[1:8], "y", "z")
  slots <- sort(unique(sub("^q406_([a-hyz]_)?", "",
                           sindh_training_fields(names(raw)))))
  n <- nrow(raw)
  counts <- as.data.frame(setNames(rep(list(integer(n)), 12L),
    c("n_known", "n_trained", "n_dont_know", "n_conflict", "n_malformed",
      "n_unresolved_recorded", "n_text_recovered", "n_original_known", "n_original_trained",
      "n_overlap_staff", "n_compared_topics", "n_topic_disagreements")))
  for (slot in slots) {
    get_binary <- function(topic) {
      nm <- paste0("q406_", topic, "_", slot)
      if (nm %in% names(raw)) as.numeric(raw[[nm]]) else rep(NA_real_, n)
    }
    binary <- vapply(topics, get_binary, numeric(n))
    m <- binary[, selected, drop = FALSE]
    original_known <- rowSums(!is.na(m) & (m == 0 | m == 1)) > 0
    original_yes <- rowSums(m == 1, na.rm = TRUE) > 0
    counts$n_original_known <- counts$n_original_known + original_known
    counts$n_original_trained <- counts$n_original_trained + original_yes

    nm <- paste0("q406_", slot)
    text <- if (nm %in% names(raw)) as.character(raw[[nm]]) else rep("", n)
    text[is.na(text)] <- ""
    text <- toupper(trimws(gsub("[[:space:]]+", " ", text)))
    has_text <- nzchar(text)
    syntax <- has_text & grepl("^[A-HYZ]( [A-HYZ])*$", text)
    tokens <- vapply(toupper(topics), function(k)
      grepl(paste0("(^| )", k, "( |$)"), text), logical(n))
    colnames(tokens) <- topics
    # A known No in a complete binary set is valid. A partial all-No set is
    # insufficient to rule out training in the remaining included topics.
    binary_known <- original_yes | rowSums(!is.na(m) & (m == 0 | m == 1)) == length(selected)
    explicit_dk <- tokens[, "y"] | binary[, "y"] %in% 1
    overlap <- syntax & !explicit_dk & original_known
    compared <- !is.na(m) & (m == 0 | m == 1)
    counts$n_overlap_staff <- counts$n_overlap_staff + overlap
    counts$n_compared_topics <- counts$n_compared_topics + overlap * rowSums(compared)
    counts$n_topic_disagreements <- counts$n_topic_disagreements + overlap *
      rowSums(compared & (m != tokens[, selected, drop = FALSE]), na.rm = TRUE)
    explicit_none <- tokens[, "z"] | binary[, "z"] %in% 1
    substantive <- rowSums(tokens[, letters[1:8], drop = FALSE]) > 0 |
      rowSums(binary[, letters[1:8], drop = FALSE] == 1, na.rm = TRUE) > 0
    # Where both encodings are supplied, each recorded checkbox must agree
    # with the corresponding text selection, including None and Don't know.
    mismatch <- rowSums((!is.na(binary) & (binary == 0 | binary == 1)) &
      (binary != tokens), na.rm = TRUE) > 0
    conflict <- ((explicit_dk | explicit_none) & substantive) |
      (explicit_dk & explicit_none) | (syntax & mismatch)
    invalid_binary <- rowSums(!is.na(binary) & !(binary == 0 | binary == 1)) > 0
    malformed <- (has_text & !syntax) | invalid_binary
    known <- !explicit_dk & !conflict & !malformed &
      (syntax | binary_known | explicit_none)
    yes <- original_yes | rowSums(tokens[, selected, drop = FALSE]) > 0
    recorded <- has_text | rowSums(!is.na(binary)) > 0
    counts$n_known <- counts$n_known + known
    counts$n_trained <- counts$n_trained + (known & yes)
    counts$n_dont_know <- counts$n_dont_know + explicit_dk
    counts$n_conflict <- counts$n_conflict + conflict
    counts$n_malformed <- counts$n_malformed + malformed
    counts$n_unresolved_recorded <- counts$n_unresolved_recorded + (recorded & !known)
    counts$n_text_recovered <- counts$n_text_recovered + (known & !original_known & syntax)
  }
  counts$proportion <- ifelse(counts$n_known > 0, counts$n_trained / counts$n_known, NA_real_)
  counts$original_rebuild <- ifelse(counts$n_original_known > 0,
    counts$n_original_trained / counts$n_original_known, NA_real_)
  counts
}

# Revise an in-memory analysis copy, preserving the supplied cleaned inputs.
apply_sindh_training <- function(facilities, raw, raw_index) {
  training <- sindh_training(raw[raw_index, , drop = FALSE])
  original <- as.numeric(facilities$p_train)
  stopifnot(identical(is.finite(original), is.finite(training$original_rebuild)),
    max(abs(original - training$original_rebuild), na.rm = TRUE) < 1e-12)
  facilities$training_original_p_train <- original
  facilities$training_original_n_tot <- facilities$n_tot
  facilities$training_original_n_train <- facilities$n_train
  facilities$p_train <- training$proportion
  facilities$n_tot <- ifelse(training$n_known > 0, training$n_known, NA_real_)
  facilities$n_train <- ifelse(training$n_known > 0, training$n_trained, NA_real_)
  for (k in names(training)) facilities[[paste0("training_", k)]] <- training[[k]]
  facilities
}
