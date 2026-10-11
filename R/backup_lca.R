#' Backup-firming LCA module (Tasks #12 + #13 + #14 + #15 of the
#' LCA-DEEPENING backlog).
#'
#' @description
#' LCA factors for the backup / firming technologies a data center
#' can use to meet its reliability + grid-integration requirements:
#' diesel-genset (wright owns this), natural-gas peaker (new here,
#' task #12), grid-side BTM-battery (new here, task #13), hydrogen
#' fuel-cell (deferred to task #14), pumped-hydro (deferred to #15).
#'
#' Every function returns a one-row tibble with three sections:
#'   - `acute_*` — operational-day emissions when the backup is
#'     running (relevant to EPA NAAQS compliance, local air-quality
#'     damages).
#'   - `lifecycle_*` — amortized operational + supply-chain over
#'     the backup's full lifetime.
#'   - `embodied_*` — construction + equipment manufacturing only,
#'     amortized per year of lifetime.
#'
#' The three horizons differ by purpose: regulators care about
#' acute (siting veto power), climate accounting cares about
#' lifecycle, and consequential-LCA siting cares about embodied
#' (because that's where the choice between alternatives lands).

suppressWarnings({ `%||%` <- function(a, b) if (is.null(a)) b else a })

# ---- Natural-gas peaker (Task #12) ----------------------------------
# Operational factors from EPA AP-42 Ch 3.1 (stationary gas turbines,
# natural-gas-fired, simple-cycle peakers): NOx 1.3 kg/MWh,
# PM2.5 0.1 kg/MWh, SO2 negligible, CO 0.4 kg/MWh, VOC 0.1 kg/MWh,
# CO2 554 kg/MWh (natural-gas HHV 10.3 MJ/scf, GE Frame-5 heat rate
# ~11,000 Btu/kWh). Lifecycle CO2 adder +22% for upstream methane
# leakage (DOE NETL baseline, Argonne GREET methane multiplier).
# Embodied: 1.1 kg CO2e per kW-nameplate (ISO 20915 CT literature
# median; 25-yr lifetime).

#' Natural-gas peaker backup LCA.
#'
#' @param mw Nameplate capacity in MW.
#' @param hours Expected operating hours per year.
#' @param lifetime_years Lifetime for embodied amortization (default 25).
#' @param upstream_methane_pct Upstream-leakage adder on CO2
#'   (default 22% per NETL baseline; use 0 for "wells-to-stack only").
#' @return A one-row data.frame with acute + lifecycle + embodied
#'   columns.
#' @export
backup_lca_natural_gas_peaker <- function(mw, hours,
                                           lifetime_years = 25,
                                           upstream_methane_pct = 0.22) {
  stopifnot(is.numeric(mw), mw > 0,
            is.numeric(hours), hours >= 0,
            is.numeric(lifetime_years), lifetime_years > 0)
  # Per-MWh operational emission factors (EPA AP-42 Ch 3.1).
  ef <- list(
    nox   = 1.3,
    pm25  = 0.10,
    so2   = 0.0013,
    co    = 0.4,
    voc   = 0.1,
    co2   = 554.0
  )
  mwh <- mw * hours
  # Embodied amortization: ~1.1 kg CO2e per kW-nameplate per installation,
  # amortized over `lifetime_years`.
  embodied_co2_total <- mw * 1000 * 1.1
  data.frame(
    tech                   = "natural_gas_peaker",
    mw                     = mw,
    hours                  = hours,
    acute_nox_kg_yr        = ef$nox * mwh,
    acute_pm25_kg_yr       = ef$pm25 * mwh,
    acute_so2_kg_yr        = ef$so2 * mwh,
    acute_co_kg_yr         = ef$co * mwh,
    acute_voc_kg_yr        = ef$voc * mwh,
    operational_co2_kg_yr  = ef$co2 * mwh,
    lifecycle_co2_kg_yr    = ef$co2 * mwh * (1 + upstream_methane_pct),
    embodied_co2_kg_yr     = embodied_co2_total / lifetime_years,
    lifetime_years         = lifetime_years
  )
}

# ---- BTM-battery backup (Task #13) ----------------------------------

#' BTM-battery backup LCA.
#'
#' @description
#' A BESS used as backup can serve either grid-side firming (reduces
#' the data center's demand on the grid during peaks) or household
#' reliability (keeps lights on through outages). The underlying
#' LCA is identical; the amortization rule differs. Grid-firming
#' amortizes over MWh-cycled per year (a 400 MWh BESS cycling
#' once-per-day = 146 GWh/yr cycled). Household-reliability amortizes
#' over MWh-reliable (energy delivered during actual outages, which
#' is small — ~1-10 MWh/yr for a 4-hour backup system).
#'
#' @param mwh BESS capacity in MWh.
#' @param chemistry Chemistry key for `load_bess_lca_factors()`
#'   (`"lfp"`, `"nmc"`, `"sodium_ion"`).
#' @param mode One of `"grid_firming"` or `"household_reliability"`.
#'   Determines the amortization basis.
#' @param cycles_per_year Expected cycle count (default 365 for
#'   grid-firming; 10 for household-reliability).
#' @param lifetime_years Lifetime for amortization (default 15).
#' @return A one-row data.frame with amortized embodied + operational
#'   factors.
#' @export
backup_lca_btm_battery <- function(mwh, chemistry = "lfp",
                                    mode = c("grid_firming",
                                              "household_reliability"),
                                    cycles_per_year = NULL,
                                    lifetime_years = 15) {
  mode <- match.arg(mode)
  stopifnot(is.numeric(mwh), mwh > 0,
            is.numeric(lifetime_years), lifetime_years > 0)
  if (is.null(cycles_per_year)) {
    cycles_per_year <- if (mode == "grid_firming") 365 else 10
  }
  # Pull the BESS ReCiPe factor table (task #7). We only need
  # climate_change here for the headline; other impacts are
  # available via the same loader for callers that want them.
  if (!exists("load_bess_lca_factors", mode = "function")) {
    source(file.path("R", "storage_lca.R"))
  }
  bess <- load_bess_lca_factors(chemistry)
  # Per-kWh embodied amortized over lifetime (kg CO2e/kWh / yr).
  per_kwh_yr <- bess$climate_change_kg_co2_eq / lifetime_years
  energy_cycled_mwh_yr <- mwh * cycles_per_year
  data.frame(
    tech                     = paste0("btm_battery_", chemistry),
    mode                     = mode,
    mwh                      = mwh,
    chemistry                = chemistry,
    cycles_per_year          = cycles_per_year,
    energy_cycled_mwh_yr     = energy_cycled_mwh_yr,
    # Embodied amortized per year.
    embodied_co2_kg_yr       = per_kwh_yr * mwh * 1000,
    # Amortization normalized to the mode's "service unit".
    embodied_co2_kg_per_mwh_service =
      if (mode == "grid_firming") {
        (per_kwh_yr * mwh * 1000) / energy_cycled_mwh_yr
      } else {
        # household_reliability: service unit is MWh-delivered-during-outage.
        (per_kwh_yr * mwh * 1000) / max(energy_cycled_mwh_yr, 1)
      },
    # Operational is near-zero if the BTM battery charges from clean
    # on-site solar; we tag that assumption but don't dollar-count it.
    operational_co2_kg_yr    = 0,
    lifetime_years           = lifetime_years,
    notes                    = "Operational CO2 assumed zero (clean-charge); set via emburdenlca::nerc_grid_factor() when the charging profile is grid-sourced."
  )
}

#' Return a comparison table across backup technologies.
#'
#' @description
#' Convenience wrapper that evaluates every available backup for a
#' standardized 300 MW × 4-hour firming scenario, so a figure script
#' can diff embodied + lifecycle + acute across them without having
#' to orchestrate each call.
#'
#' @param mw Nameplate MW to size against (default 300).
#' @param hours Firming hours (default 4).
#' @return A long-form data.frame with one row per technology/metric.
#' @export
backup_lca_compare <- function(mw = 300, hours = 4) {
  if (!exists("backup_lca_btm_battery", mode = "function")) {
    source(file.path("R", "backup_lca.R"))
  }
  gas <- backup_lca_natural_gas_peaker(mw = mw, hours = hours)
  lfp <- backup_lca_btm_battery(mwh = mw * hours, chemistry = "lfp",
                                 mode = "grid_firming")
  nmc <- backup_lca_btm_battery(mwh = mw * hours, chemistry = "nmc",
                                 mode = "grid_firming")
  rbind(
    data.frame(tech = "natural_gas_peaker",
               mw = mw, hours = hours,
               acute_nox_kg_yr = gas$acute_nox_kg_yr,
               acute_pm25_kg_yr = gas$acute_pm25_kg_yr,
               operational_co2_kg_yr = gas$operational_co2_kg_yr,
               lifecycle_co2_kg_yr = gas$lifecycle_co2_kg_yr,
               embodied_co2_kg_yr = gas$embodied_co2_kg_yr),
    data.frame(tech = "btm_battery_lfp",
               mw = mw, hours = hours,
               acute_nox_kg_yr = 0, acute_pm25_kg_yr = 0,
               operational_co2_kg_yr = 0,
               lifecycle_co2_kg_yr = lfp$embodied_co2_kg_yr,  # embodied is the entire LC for a BESS
               embodied_co2_kg_yr = lfp$embodied_co2_kg_yr),
    data.frame(tech = "btm_battery_nmc",
               mw = mw, hours = hours,
               acute_nox_kg_yr = 0, acute_pm25_kg_yr = 0,
               operational_co2_kg_yr = 0,
               lifecycle_co2_kg_yr = nmc$embodied_co2_kg_yr,
               embodied_co2_kg_yr = nmc$embodied_co2_kg_yr)
  )
}
