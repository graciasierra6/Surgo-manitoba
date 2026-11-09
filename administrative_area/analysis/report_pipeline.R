# Shared administrative-area report preparation. The caller defines root.
source(file.path(root, "analysis/common/pipeline.R"), local = TRUE)

administrative_join_dir <- function(path) {
  candidates <- c(path, file.path(path, "admin_reference"))
  ok <- vapply(candidates, function(p) file.exists(file.path(p,
    "checked_geography/lagos_women_manitoba.rds")) &&
    file.exists(file.path(p, "audit/facility_linkage_inputs.rds")), logical(1))
  if (!any(ok)) stop("Supply a verified administrative-area join with checked geography and facility audit inputs")
  normalizePath(candidates[which(ok)[1]], mustWork = TRUE)
}

prepare_aggregate_inputs <- function(root, input_zip = "", work_dir,
                                     cache_dir, join_dir = "") {
  work_dir <- project_path(work_dir, root)
  cache_dir <- project_path(cache_dir, root)
  if (nzchar(join_dir)) join_dir <- project_path(join_dir, root)
  if (!nzchar(input_zip)) {
    if (!nzchar(join_dir)) stop("Supply --input-zip, SURGO_MANITOBA_INPUT_ZIP, or an existing --join-dir")
    return(list(join_dir = administrative_join_dir(join_dir), cache_dir = cache_dir,
                bundle_sha256 = NA_character_))
  }
  input_zip <- project_path(input_zip, root)
  if (!file.exists(input_zip)) stop("Input bundle not found")
  bundle_sha <- digest::digest(file = input_zip, algo = "sha256")
  if (!nzchar(join_dir)) {
    join_dir <- file.path(work_dir, "linkage")
    run_stage(file.path(root, "administrative_area/R/run_join.R"),
      c("--input-zip", input_zip, "--output-dir", join_dir,
        "--cache-dir", cache_dir, "--geography-version", "both"))
  }
  admin <- administrative_join_dir(join_dir)
  parameters <- file.path(admin, "run_parameters.json")
  if (!file.exists(parameters)) stop("Join output lacks input-verification parameters")
  settings <- jsonlite::read_json(parameters, simplifyVector = TRUE)
  if (settings$geography_version != "both" || settings$input_sha256 != bundle_sha)
    stop("Existing administrative join must contain checked geography and match the supplied bundle")
  members <- utils::unzip(input_zip, list = TRUE)$Name
  if (any(grepl("^/|^[A-Za-z]:|\\\\", members)) ||
      any(vapply(strsplit(members, "/", fixed = TRUE), function(x) any(x == ".."), logical(1))))
    stop("Unsafe input-bundle member")
  if (!"bundle_manifest.json" %in% members) stop("Input bundle lacks its manifest")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(input_zip, files = "bundle_manifest.json", exdir = cache_dir)
  manifest <- jsonlite::read_json(file.path(cache_dir, "bundle_manifest.json"), simplifyVector = TRUE)
  for (i in seq_len(nrow(manifest$files))) {
    item <- manifest$files[i, ]; target <- file.path(cache_dir, item$member)
    if (!item$member %in% members) stop("Manifest member missing from input bundle")
    valid <- file.exists(target) && file.info(target)$size == item$bytes &&
      digest::digest(file = target, algo = "sha256") == item$sha256
    if (!valid) utils::unzip(input_zip, files = item$member, exdir = cache_dir)
    if (!file.exists(target) || file.info(target)$size != item$bytes ||
        digest::digest(file = target, algo = "sha256") != item$sha256)
      stop("Input-integrity check failed: ", item$member)
  }
  list(join_dir = admin, cache_dir = cache_dir, bundle_sha256 = bundle_sha)
}
