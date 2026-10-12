#' Full EXIOBASE multi-region LCA factor loader (Task #23 of LCA-DEEPENING).
#'
#' @description
#' Replaces the WF+ZA cache-only EXIOBASE path with full 49-region
#' support via pymrio. The pymrio Python package is heavy (~300 MB
#' EXIOBASE data per release) and only installed on `wright`, so this
#' bridge mirrors `global_lca_service.R`'s SSH-call pattern: `wright`
#' extracts the per-region per-sector factor table + writes a parquet
#' result, then emburdenlca reads the parquet locally.
#'
#' Results are cached under `~/.cache/emburdendata/exiobase/` keyed by
#' (region, indicator_set) so a repeat call is instant.
#'
#' Indicator set exposed to R (17 total): co2, co2_bio, co2e (CO2+CH4+N2O
#' GWP100), ch4, n2o, nox, sox, pm25, co, nmvoc, water_blue, water_green,
#' land_crop, land_pasture, land_total, metal_use, fossil_use.
#'
#' @param region EXIOBASE 2-letter region. Available: AT, AU, BE, BG,
#'   BR, CA, CH, CN, CY, CZ, DE, DK, EE, ES, FI, FR, GB, GR, HR, HU,
#'   ID, IE, IN, IT, JP, KR, LT, LU, LV, MT, MX, NL, NO, PL, PT, RO,
#'   RU, SE, SI, SK, TR, TW, US, WA, WE, WF, WL, WM, ZA.
#' @param indicators Character vector of indicator names (see list
#'   above). Default is `c("co2e","nox","sox","pm25","water_blue",
#'   "land_total","metal_use")`.
#' @param host SSH host that has pymrio installed; default `"wright"`.
#' @param force_refresh TRUE to bypass the local parquet cache.
#' @return A tibble with columns `region, sector_code, <indicator>…`.
#'   163 rows per region (EXIOBASE sector count).
#' @export
#' @examples
#' \dontrun{
#' za <- load_exiobase_region_factors("ZA")
#' us <- load_exiobase_region_factors("US",
#'            indicators = c("co2e", "water_blue"))
#' }
load_exiobase_region_factors <- function(
  region,
  indicators = c("co2e", "nox", "sox", "pm25",
                  "water_blue", "land_total", "metal_use"),
  host = "wright",
  force_refresh = FALSE
) {
  stopifnot(is.character(region), length(region) == 1, nchar(region) == 2)
  stopifnot(is.character(indicators), length(indicators) >= 1)

  cache_dir <- path.expand("~/.cache/emburdendata/exiobase")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  # Cache key hashes the indicator set so a wider pull doesn't stomp a
  # narrower cached result.
  key <- substr(digest::digest(list(region, sort(indicators))), 1, 8)
  cache_path <- file.path(cache_dir, sprintf("factors_%s_%s.parquet",
                                                region, key))
  if (!force_refresh && file.exists(cache_path)) {
    return(arrow::read_parquet(cache_path))
  }

  # Remote extraction. pymrio + EXIOBASE live on `host`; the extractor
  # script lives at ~/Documents/apps/projects/lca/scripts/.
  remote_tmp <- sprintf("/tmp/exiobase_factors_%s_%s.parquet", region, key)
  venv_py <- "~/Documents/apps/projects/lca/.venv/bin/python3"
  script  <- "~/Documents/apps/projects/lca/scripts/extract_exiobase_factors.py"
  cmd <- sprintf(
    "ssh %s '%s %s --region %s --out %s --indicators %s' 2>&1",
    host, venv_py, script, region, remote_tmp,
    paste(indicators, collapse = ",")
  )
  message("Running EXIOBASE extraction on ", host, " for region ", region,
          " (indicators: ", paste(indicators, collapse = ", "), ")...")
  out <- system(cmd, intern = TRUE)
  if (any(grepl("ERROR", out))) {
    stop("EXIOBASE extraction failed on ", host, ":\n",
         paste(out, collapse = "\n"))
  }
  # Pull the parquet back + cache.
  rc <- system(sprintf("scp -q %s:%s %s", host, remote_tmp, cache_path))
  if (rc != 0 || !file.exists(cache_path)) {
    stop("Failed to scp result parquet from ", host, ":", remote_tmp)
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Install the `arrow` R package to read parquet: ",
         "install.packages('arrow').")
  }
  arrow::read_parquet(cache_path)
}

#' List EXIOBASE regions available via `load_exiobase_region_factors`.
#'
#' @param host SSH host; default `"wright"`.
#' @return A character vector of 49 region codes.
#' @export
list_exiobase_regions <- function(host = "wright") {
  venv_py <- "~/Documents/apps/projects/lca/.venv/bin/python3"
  script  <- "~/Documents/apps/projects/lca/scripts/extract_exiobase_factors.py"
  out <- system(sprintf("ssh %s '%s %s --list-regions'",
                         host, venv_py, script),
                intern = TRUE)
  jsonlite::fromJSON(paste(out, collapse = "\n"))
}

#' List EXIOBASE indicators exposed by `load_exiobase_region_factors`.
#'
#' @param host SSH host; default `"wright"`.
#' @return A character vector of indicator names.
#' @export
list_exiobase_indicators <- function(host = "wright") {
  venv_py <- "~/Documents/apps/projects/lca/.venv/bin/python3"
  script  <- "~/Documents/apps/projects/lca/scripts/extract_exiobase_factors.py"
  out <- system(sprintf("ssh %s '%s %s --list-indicators'",
                         host, venv_py, script),
                intern = TRUE)
  jsonlite::fromJSON(paste(out, collapse = "\n"))
}
