#' ──────────────────────────────────────────────────────────────────────────────
#' prototypes/shared/paths.R
#'
#' find_path() resolves a repo-relative path (e.g. "app_data/water_supplies.csv",
#' "calc_vulnerability_index.R", "shared/methods.R") from wherever a prototype
#' is running:
#'
#'   - locally, from prototypes/<app>/      -> ../../app_data/..., ../shared/...
#'   - locally, from the repo root          -> app_data/..., prototypes/shared/...
#'   - deployed, from a flat staged bundle  -> app_data/..., shared/...
#'     (deploy_prototype.R copies app.R, calc_vulnerability_index.R, shared/
#'     and the needed app_data/ files into one folder)
#'
#' The first candidate that exists wins. An error names every place it looked.
#' ──────────────────────────────────────────────────────────────────────────────

find_path <- function(rel) {
  roots <- c(".", "..", "../..", "prototypes")
  candidates <- unique(normalizePath(file.path(roots, rel), mustWork = FALSE))
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    stop("[prototypes] Could not find '", rel, "' from ", getwd(),
         ". Looked in:\n  ", paste(candidates, collapse = "\n  "), call. = FALSE)
  }
  hit[[1]]
}
