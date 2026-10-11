#' HEII bridge for CCC (Task #18 of the LCA-DEEPENING backlog).
#'
#' @description
#' The combined adequacy + burden metric
#' `household_emergy_insecurity()` lives in `emburdensynth::` but the
#' CCC equity constraint on wright wants to call it from an
#' emburdenlca-rooted dependency chain. This file is the thin
#' wrapper that re-exports the metric at `emburdenlca::compute_heii_for_ccc()`
#' and returns the subset of fields the solver needs
#' (`heii_sej`, `heii_norm`, binding-constraint flag).
#'
#' Pure delegation. No reimplementation. If `emburdensynth` is not
#' installed, this errors with an install instruction.
#'
#' @param iso3 ISO3 country code.
#' @param year Numeric year (default 2020 matches emburdensynth).
#' @param kwh_delivered Household electricity delivered (kWh/yr).
#' @param kwh_required Household electricity need (kWh/yr) — from
#'   adequacy modeling.
#' @param spend_usd Household energy spend (USD/yr).
#' @param income_usd Household income (USD/yr).
#' @param heii_threshold Binding threshold on `heii_norm` (fractional,
#'   default 0.5). The returned `binding` flag is TRUE when
#'   `heii_norm > heii_threshold`; CCC's solver drops any MW envelope
#'   that produces a binding HEII in the policy decile.
#' @param ... Passed through to `household_emergy_insecurity()`.
#' @return A named list with `heii_sej`, `heii_norm`, `heii_esi`,
#'   `heii_product`, and `binding` (logical).
#' @export
#' @examples
#' \dontrun{
#' r <- compute_heii_for_ccc("USA", 2020,
#'                            kwh_delivered = 7000,
#'                            kwh_required = 10000,
#'                            spend_usd = 2200,
#'                            income_usd = 55000)
#' r$binding
#' }
compute_heii_for_ccc <- function(iso3, year = 2020,
                                   kwh_delivered, kwh_required,
                                   spend_usd, income_usd,
                                   heii_threshold = 0.5,
                                   ...) {
  if (!requireNamespace("emburdensynth", quietly = TRUE)) {
    stop("emburdensynth is required for compute_heii_for_ccc(). ",
         "Install with: ",
         "remotes::install_local('~/Documents/apps/emburdensynth').")
  }
  res <- emburdensynth::household_emergy_insecurity(
    iso3 = iso3, year = year,
    kwh_delivered = kwh_delivered, kwh_required = kwh_required,
    spend_usd = spend_usd, income_usd = income_usd,
    ...
  )
  binding <- is.finite(res$heii_norm) && res$heii_norm > heii_threshold
  list(
    heii_sej = res$heii_sej,
    heii_norm = res$heii_norm,
    heii_esi = res$heii_esi,
    heii_product = res$heii_product,
    binding = binding,
    threshold = heii_threshold
  )
}
