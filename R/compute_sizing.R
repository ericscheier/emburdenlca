#' Compute workload → storage headroom sizing (Tasks #11 + #16 of the
#' LCA-DEEPENING backlog).
#'
#' @description
#' Different data-center compute workloads imply different storage
#' needs for grid integration — training is highly flexible (shiftable
#' into curtailment hours), inference must-run near-flat, batch is
#' maximally batchable, crypto is near-flat but can shed on economic
#' triggers. Wright's `btm_generation.py` hardcodes a 4-hour duration
#' for every scenario; this helper exposes a per-workload sizing table
#' + a scaling function that returns required BESS MWh for a load.
#'
#' @section Taxonomy columns:
#' - `workload_type`: training / inference / batch / crypto.
#' - `load_factor_mean`: mean load as a fraction of nameplate MW.
#' - `load_factor_p90`: 90th-percentile hourly load (duck-curve relevance).
#' - `flexibility_score`: 0-1 scalar — fraction of annual energy that
#'   can be shifted ±4 hours without penalty.
#' - `must_run_hours_per_year`: hours the workload cannot shed.
#' - `storage_hours_per_mw`: hours of BESS per nameplate MW required
#'   to flatten the demand on the grid during peaks.
#' - `ramp_rate_mw_per_min`: typical ramp when the workload transitions.
#'
#' @export
load_workload_taxonomy <- function() {
  path <- system.file("extdata", "workload_storage_sizing.csv",
                      package = "emburdenlca")
  if (!nzchar(path) || !file.exists(path)) {
    guess <- file.path("inst", "extdata", "workload_storage_sizing.csv")
    if (file.exists(guess)) path <- guess
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("workload_storage_sizing.csv not found; install emburdenlca ",
         "or run from its repo root.")
  }
  stopifnot(requireNamespace("readr", quietly = TRUE))
  readr::read_csv(path, show_col_types = FALSE)
}

#' Compute the required BESS MWh for a given compute workload.
#'
#' @param workload_type One of `"training"`, `"inference"`, `"batch"`,
#'   or `"crypto"`.
#' @param load_mw Nameplate load of the compute cluster in MW.
#' @return A one-row tibble with `workload_type, load_mw,
#'   storage_mwh, storage_hours, flexibility_score`.
#' @export
#' @examples
#' \dontrun{
#' compute_storage_sizing("inference", load_mw = 300)
#' compute_storage_sizing("training",  load_mw = 300)
#' }
compute_storage_sizing <- function(workload_type, load_mw) {
  stopifnot(is.numeric(load_mw), load_mw > 0)
  tax <- load_workload_taxonomy()
  row <- tax[tax$workload_type == workload_type, ]
  if (nrow(row) == 0) {
    stop(sprintf("Unknown workload_type '%s'. Available: %s",
                 workload_type,
                 paste(tax$workload_type, collapse = ", ")))
  }
  data.frame(
    workload_type     = workload_type,
    load_mw           = load_mw,
    storage_mwh       = load_mw * row$storage_hours_per_mw,
    storage_hours     = row$storage_hours_per_mw,
    flexibility_score = row$flexibility_score,
    load_factor_mean  = row$load_factor_mean,
    must_run_hours    = row$must_run_hours_per_year
  )
}

#' Compute storage sizing × NERC region (task #17 coupling).
#'
#' @description
#' NERC regions with steeper duck curves (CAISO, SPP in the renewable
#' middle-day peak) need more storage per MW of inference load than
#' regions with flat baseload (SERC, RFC) — the compute's "must-run"
#' demand coincides with the renewable-ramp or dispatch tightness.
#' This multiplies the base sizing (from `compute_storage_sizing()`)
#' by a NERC-region duck-curve multiplier derived from the eGRID
#' renewable_pct × a renewable-amplifier.
#'
#' @param workload_type Workload taxonomy name.
#' @param load_mw Nameplate MW.
#' @param nerc_region NERC region code (FRCC, MRO, NPCC, RFC, SERC,
#'   SPP, TRE, WECC).
#' @param year Numeric year for the grid-mix lookup; defaults to the
#'   latest in the eGRID panel.
#' @return A one-row data.frame extending `compute_storage_sizing()`'s
#'   return with `nerc_region, duck_multiplier, storage_mwh_nerc,
#'   storage_hours_nerc`.
#' @export
compute_storage_sizing_by_nerc <- function(workload_type, load_mw,
                                             nerc_region, year = NULL) {
  base <- compute_storage_sizing(workload_type, load_mw)
  # Grab the renewable share for this region/year from the regional
  # grid-factor loader. Multiplier scales 1.0 (zero-renewable) to 2.0
  # (fully-renewable) linearly in the renewable share.
  if (!exists("load_nerc_grid_factors", mode = "function")) {
    source(file.path("R", "nerc_regional_factors.R"))
  }
  tbl <- load_nerc_grid_factors()
  if (is.null(year)) year <- max(tbl$year, na.rm = TRUE)
  row <- tbl[tbl$nerc_region == nerc_region & tbl$year == year, ]
  if (nrow(row) == 0) {
    warning(sprintf(
      "No grid-mix data for (region=%s, year=%s); using default 1.0 multiplier",
      nerc_region, year))
    renewable_pct <- 0.0
  } else {
    renewable_pct <- as.numeric(row$renewable_pct)
  }
  duck <- 1.0 + renewable_pct  # 0% ren → 1.0; 50% ren → 1.5; 100% → 2.0.
  base$nerc_region         <- nerc_region
  base$duck_multiplier     <- duck
  base$storage_mwh_nerc    <- base$storage_mwh * duck
  base$storage_hours_nerc  <- base$storage_hours * duck
  base
}
