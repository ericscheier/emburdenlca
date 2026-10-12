#' BESS storage LCA factors (Task #7 of the LCA-DEEPENING backlog).
#'
#' @description
#' Per-kWh-capacity LCA factors across 17 ReCiPe 2016 midpoints for
#' LFP, NMC, and sodium-ion batteries. Values mirror wright's
#' `src/dc_e2c3/modules/m25_storage.py:48-105` DEFAULT_RECIPE_FACTORS
#' dict (synced 2026-10-11), so emburdenlca and DC-E2C3 agree on
#' every chemistry's factor without a round-trip over SSH.
#'
#' Factors are **per kWh of installed capacity** (not per kWh of
#' energy throughput). Round-trip efficiencies: LFP 0.92, NMC 0.93,
#' sodium-ion 0.88 — exposed as `rte` in the returned tibble.
#'
#' Future chemistries (pumped-hydro, hydrogen, flow batteries) are
#' tracked under tasks #8-#10 of the same backlog and will append to
#' `inst/extdata/bess_recipe_factors.csv` + add rows to the loader's
#' validation list.
#'
#' Chemistries supported, with representative source citations:
#'   - `"lfp"`, `"nmc"`, `"sodium_ion"`: wright's DEFAULT_RECIPE_FACTORS
#'     (Jasper 2022 + Peters 2024). RTE 0.92 / 0.93 / 0.88.
#'   - `"pumped_hydro"`: NREL 2024 PHS LCA review. RTE 0.80. Dominant
#'     impacts in metal_depletion (penstock + turbine steel) and
#'     land occupation (reservoir); low toxicity + no critical minerals.
#'   - `"hydrogen"`: PEM electrolysis + compression + PEM fuel-cell
#'     round-trip, IEA Future of Hydrogen 2024 + Argonne GREET 2024.
#'     RTE 0.35. Platinum + iridium drive metal_depletion; electrolysis
#'     water intensity ~9 L / kg H2 drives water_depletion.
#'   - `"vanadium_flow"`, `"zinc_bromide"`: DOE Long-Duration Storage
#'     Shot 2024 LCA report. RTE 0.70 / 0.65. Vanadium flow carries
#'     the highest metal_depletion (580 kg Fe-eq/kWh — vanadium is
#'     critical); zinc-bromide avoids vanadium but shifts intensity
#'     into ecotoxicity (bromide chemistry).
#'
#' @param chemistry Character; one of `"lfp"`, `"nmc"`, `"sodium_ion"`,
#'   `"pumped_hydro"`, `"hydrogen"`, `"vanadium_flow"`, `"zinc_bromide"`,
#'   or `"all"` to return every chemistry.
#' @return A tibble with columns `chemistry, rte,
#'   climate_change_kg_co2_eq, ozone_depletion_kg_cfc11_eq,
#'   human_toxicity_kg_14dcb_eq, photochemical_oxidant_kg_nmvoc_eq,
#'   particulate_matter_kg_pm25_eq, ionising_radiation_kbq_u235_eq,
#'   terrestrial_acidification_kg_so2_eq,
#'   freshwater_eutrophication_kg_p_eq,
#'   terrestrial_ecotoxicity_kg_14dcb_eq,
#'   freshwater_ecotoxicity_kg_14dcb_eq,
#'   marine_ecotoxicity_kg_14dcb_eq, agricultural_land_m2yr,
#'   urban_land_m2yr, natural_land_transformation_m2,
#'   metal_depletion_kg_fe_eq, fossil_depletion_kg_oil_eq,
#'   water_depletion_m3`.
#' @export
#' @examples
#' \dontrun{
#' lfp_factors <- load_bess_lca_factors("lfp")
#' all_factors <- load_bess_lca_factors("all")
#' }
load_bess_lca_factors <- function(chemistry = c("lfp", "nmc",
                                                   "sodium_ion",
                                                   "pumped_hydro",
                                                   "hydrogen",
                                                   "vanadium_flow",
                                                   "zinc_bromide",
                                                   "all")) {
  chemistry <- match.arg(chemistry)
  path <- system.file("extdata", "bess_recipe_factors.csv",
                      package = "emburdenlca")
  if (!nzchar(path) || !file.exists(path)) {
    # Dev-mode fallback (package not installed): look relative to cwd.
    guess <- file.path("inst", "extdata", "bess_recipe_factors.csv")
    if (file.exists(guess)) {
      path <- guess
    } else {
      path <- ""  # trigger the error below
    }
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("bess_recipe_factors.csv not found at ", path,
         ". Install emburdenlca properly or run from its repo root.")
  }
  stopifnot(requireNamespace("readr", quietly = TRUE))
  tbl <- readr::read_csv(path, show_col_types = FALSE)
  if (chemistry == "all") return(tbl)
  out <- tbl[tbl$chemistry == chemistry, ]
  if (nrow(out) == 0) {
    stop("Unknown chemistry '", chemistry,
         "'. Available: ", paste(tbl$chemistry, collapse = ", "))
  }
  out
}

#' Scale BESS LCA factors to an installed capacity + lifetime.
#'
#' @param chemistry Chemistry name (see `load_bess_lca_factors`).
#' @param capacity_kwh Installed BESS capacity in kWh.
#' @param lifetime_years Lifetime for amortization (default 15).
#' @return A one-row tibble with the amortized factors per year.
#' @export
amortize_bess_lca <- function(chemistry, capacity_kwh,
                                lifetime_years = 15) {
  stopifnot(is.numeric(capacity_kwh), capacity_kwh > 0,
            is.numeric(lifetime_years), lifetime_years > 0)
  base <- load_bess_lca_factors(chemistry)
  numeric_cols <- setdiff(names(base), c("chemistry", "rte"))
  base[, numeric_cols] <- base[, numeric_cols] * capacity_kwh / lifetime_years
  base$capacity_kwh <- capacity_kwh
  base$lifetime_years <- lifetime_years
  base
}

# Internal helper: `%||%` for R <4.4 compat.
`%||%` <- function(a, b) if (is.null(a)) b else a
