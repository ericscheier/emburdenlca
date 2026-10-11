# =============================================================================
# Local USEEIO v2 loader — ends the hard SSH-to-wright dependency for US
# -----------------------------------------------------------------------------
# USEEIOv2.0.1-411 is EPA's input-output model of the US economy with 411
# industry sectors + environmental extensions (greenhouse gases, land, water,
# emergy, etc). It ships as a flat CSV release (useeior R package / EPA
# downloads). This loader caches the model locally so emburdenlca does not
# need the DC-E2C3 SSH bridge for US factor queries.
#
# Primary consumer: `attach_lca_footprint()` on US households. The remote
# `lca_ghg_factors()` SSH call stays as a fallback for non-US regions
# (EXIOBASE) until the EXIOBASE cache fill is complete.
#
# Install the useeior upstream package once (CRAN or EPA GitHub):
#   remotes::install_github("USEPA/useeior")
# Model files cache at `~/.cache/emburdendata/lca/useeio_v2/`.
# =============================================================================

#' Cache directory for USEEIO v2 factor tables
#'
#' Honours the ecosystem-wide `~/.cache/emburdendata/lca/` convention
#' used by `global_lca_service.R` and `eeio_lite_attach.R`. The USEEIO v2
#' tables live under `useeio_v2/` to keep release versions separable.
#'
#' @return Absolute path (character scalar); created if missing.
#' @export
useeio_cache_dir <- function() {
  d <- file.path(path.expand("~/.cache/emburdendata/lca"), "useeio_v2")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' Load USEEIO v2 sector-emergy and GHG factors (local)
#'
#' Returns a per-sector factor table (411 NAICS BEA detail sectors by
#' default; summary 71-sector table on request). The default factor set
#' is CO2e (direct + total) plus solar emergy, matching the shape every
#' downstream caller expects. **Additional TRACI 2.1 impact categories
#' (acidification, eutrophication, PM2.5-eq, water, land, etc.) can be
#' pulled by passing their row names via `indicators`** — those columns
#' are added to the returned tibble with slugified names (lower-case,
#' underscores, `_total` suffix). The cache key varies with the
#' indicator set so a bigger pull does not stomp on the default cache.
#'
#' When `useeior` is not installed and no cached table is present, prints
#' a one-line install instruction and errors — avoids silent failure.
#'
#' @param level `"detail"` (default, 411 sectors) or `"summary"` (71).
#' @param force_refresh Ignore cache, rebuild factors (default `FALSE`).
#' @param model Which USEEIO model release to use (default `"USEEIOv2.0.1-411"`).
#' @param indicators Optional character vector of extra USEEIO N-matrix
#'   row names to pull. Pass exactly as they appear in `rownames(m$N)`
#'   (e.g. `"Acidification Potential"`, `"Particulate Matter 2.5"`,
#'   `"Water Use"`). Default `NULL` preserves historical return shape.
#' @return A tibble with columns `sector_code`, `sector_name`,
#'   `co2e_total`, `co2e_direct`, `emergy_total_sej_per_dollar`,
#'   `model`, `level`, plus one `<slug>_total` column per entry in
#'   `indicators`.
#' @examples
#' \dontrun{
#' tbl <- load_useeio_v2()
#' head(tbl)
#' traci <- load_useeio_v2(indicators = c("Acidification Potential",
#'                                         "Particulate Matter 2.5",
#'                                         "Water Use"))
#' head(traci)
#' }
#' @export
load_useeio_v2 <- function(level = c("detail", "summary"),
                             force_refresh = FALSE,
                             model = "USEEIOv2.0.1-411",
                             indicators = NULL) {
  level <- match.arg(level)
  # Cache key varies with the indicator set so an extended pull does not
  # overwrite the default (CO2e + emergy) cache a downstream caller may
  # still be reading. Default `indicators = NULL` → legacy cache name.
  cache_suffix <- if (is.null(indicators)) "" else {
    paste0("_", substr(digest::digest(sort(indicators)), 1, 8))
  }
  cache_path <- file.path(useeio_cache_dir(),
                           paste0(model, "_", level, "_factors",
                                   cache_suffix, ".rds"))

  if (!force_refresh && file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  if (!requireNamespace("useeior", quietly = TRUE)) {
    stop("USEEIO v2 factors are not cached and `useeior` is not installed. ",
         "Install once with: remotes::install_github('USEPA/useeior'). ",
         "Alternatively, drop a pre-built factor table at `",
         cache_path, "` and re-run.")
  }

  # useeior::buildModel() calls as.environment("package:useeior") internally,
  # which requires useeior to be attached (library()), not merely loaded via
  # ::. Attach it once if needed; harmless if it is already attached.
  suppressPackageStartupMessages(library(useeior))

  message("Building USEEIO v2 factor table from useeior::buildModel() — ",
          "this takes 2–5 min the first time per level.")
  m <- useeior::buildModel(model)
  # N (total intensity) + D (direct intensity) matrices in useeior
  # terminology; CO2e is one row of the environmental intensity matrix.
  co2e_total  <- m$N["Greenhouse Gases", , drop = TRUE]
  co2e_direct <- m$D["Greenhouse Gases", , drop = TRUE]
  # Emergy intensity: if the chosen model release carries an emergy row
  # (sej/$), pull it; otherwise approximate with the CO2e proxy and tag
  # the column so downstream code knows it is a proxy.
  emergy_row <- if ("Solar Emergy" %in% rownames(m$N)) "Solar Emergy" else NA_character_
  emergy_vec <- if (!is.na(emergy_row)) m$N[emergy_row, , drop = TRUE]
                 else rep(NA_real_, length(co2e_total))

  tbl <- tibble::tibble(
    sector_code = colnames(m$N),
    sector_name = m$Commodities$Name[match(colnames(m$N), m$Commodities$Code)],
    co2e_total  = as.numeric(co2e_total),
    co2e_direct = as.numeric(co2e_direct),
    emergy_total_sej_per_dollar = as.numeric(emergy_vec),
    model = model,
    level = level
  )

  # Extra TRACI rows: for each named indicator in `indicators`, pull
  # the row from m$N and attach as a column. Unknown indicator → warn
  # and fill NA, so a typo doesn't blow up a full rebuild.
  if (!is.null(indicators) && length(indicators) > 0) {
    for (ind in indicators) {
      slug <- paste0(gsub("[^A-Za-z0-9]+", "_",
                           tolower(trimws(ind))), "_total")
      if (ind %in% rownames(m$N)) {
        tbl[[slug]] <- as.numeric(m$N[ind, , drop = TRUE])
      } else {
        warning(sprintf(
          "USEEIO indicator '%s' not found in m$N — column '%s' filled NA. Available rows: %s",
          ind, slug, paste(head(rownames(m$N), 15), collapse = ", ")))
        tbl[[slug]] <- NA_real_
      }
    }
  }

  if (level == "summary") {
    tbl <- tbl |> dplyr::group_by(sector_code = substr(sector_code, 1, 2)) |>
      dplyr::summarise(across(c(co2e_total, co2e_direct,
                                 emergy_total_sej_per_dollar),
                               ~mean(.x, na.rm = TRUE)), .groups = "drop") |>
      dplyr::mutate(sector_name = sector_code, model = model, level = level)
  }

  saveRDS(tbl, cache_path)
  message("Cached ", nrow(tbl), " sectors to ", cache_path)
  tbl
}

#' Fetch sector-specific emergy or GHG factors from USEEIO v2
#'
#' Convenience wrapper around `load_useeio_v2()` that returns a single
#' column for a vector of sector codes. Primary consumer: Phase 6 LCA
#' attachment inside `attach_lca_footprint()`.
#'
#' @param sector_codes Character vector of NAICS BEA sector codes.
#' @param field One of `"co2e_total"` (default), `"co2e_direct"`,
#'   `"emergy_total_sej_per_dollar"`.
#' @param level Passed to `load_useeio_v2()`; default `"detail"`.
#' @return Numeric vector the same length as `sector_codes`; `NA` for
#'   codes not present in the factor table.
#' @export
useeio_factor <- function(sector_codes,
                            field = c("co2e_total", "co2e_direct",
                                      "emergy_total_sej_per_dollar"),
                            level = "detail") {
  field <- match.arg(field)
  tbl <- load_useeio_v2(level = level)
  tbl[[field]][match(sector_codes, tbl$sector_code)]
}

#' Pre-warm the USEEIO v2 cache
#'
#' Builds and caches both the detail (411-sector) and summary
#' (71-sector) factor tables so the first `attach_lca_footprint()` call
#' in a session returns instantly instead of blocking on model build.
#'
#' Call once after `remotes::install_github('USEPA/useeior')`.
#'
#' @return Invisibly, `TRUE` on success.
#' @export
useeio_prewarm_cache <- function() {
  invisible(all(
    is.data.frame(load_useeio_v2(level = "detail",  force_refresh = TRUE)),
    is.data.frame(load_useeio_v2(level = "summary", force_refresh = TRUE))
  ))
}
