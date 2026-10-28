#' ──────────────────────────────────────────────────────────────────────────────
#' prototypes/shared/methods.R
#'
#' Method layer shared by the prototype apps. A "method" is a spec (a plain
#' list made by method_spec()) that run_method() turns into scores, ranks and a
#' priority classification for every water supply.
#'
#' Two groupings:
#'
#'   overall   The V1 Overall Index, exactly as calc_vulnerability_index()
#'             computes it (Euclidean distance across all 14 factors, top 25%
#'             = priority). run_method() calls calc_vulnerability_index()
#'             directly, so this is V1 by construction.
#'
#'   pathways  Impact Pathway Indices. Indicators are assigned to named
#'             pathways (Water Supply, Water Demand, Drought, Wildfire, Coastal
#'             Impacts by default, from the Phase 1 Revisions Notes table).
#'             Inside a pathway, the exposure indicators and the sensitivity
#'             indicators are each combined the same way V1 builds a factor
#'             (Euclidean distance of the normalized indicators, re-normalized
#'             0-1), then the two are combined into a pathway score. A supply
#'             is High in a pathway when its pathway score is in the top 25%
#'             of the comparison group (the same rule as V1's hazard flags).
#'             Supplies are ranked by how many pathways they are High in.
#'
#' Normalized indicators (norm_*) always come from calc_vulnerability_index(),
#' so normalization, inversion and model-agreement weighting are V1's. The
#' pathway engine reuses euclid_factor(), minmax_norm() and pct_rank() from
#' calc_vulnerability_index.R; that file must be sourced first.
#'
#' Parity: a pathway table that reproduces V1's factor grouping (see
#' v1_factor_table()) gives pathway component scores identical to V1's
#' factor_* columns, and its Euclidean total equals VULNERABILITY. This is
#' asserted in prototypes/tests/test-parity.R.
#' ──────────────────────────────────────────────────────────────────────────────

# ── Indicator catalog ────────────────────────────────────────────────────────
# One row per indicator a pathway can use. Water supply indicators are
# "virtual": they resolve to the runoff or the precipitation version by source
# type, exactly as V1's Water Supply factor does (rainwater -> precipitation,
# everything else -> runoff). `ocean_zero` marks indicators V1 sets to zero for
# ocean supplies (water supply and demand).
#
# `v1_factor` is the V1 factor the indicator belongs to, used to rebuild V1's
# grouping for the parity test and for display.

INDICATOR_CATALOG <- data.frame(
  indicator = c(
    "ws_change", "ws_timing", "ws_trend",
    "nearby_use_change", "auc_change_exp", "competition", "auc_current",
    "visitation_trend",
    "drought_change", "drought_trend",
    "fire_change", "wildfire_hazard",
    "slr", "storm_surge", "swi", "inundation_current",
    "flood_risk",
    "temp_change", "temp_trend",
    "depth_to_water"
  ),
  label = c(
    "% Change in Runoff/Precip", "Change in Runoff/Precip Timing",
    "Historical Trend in Runoff/Precip",
    "Change in Nearby Water Use", "Change in Supply-Demand Mismatch",
    "Nearby Water Use (Competition)", "Current Supply-Demand Mismatch",
    "Historical Visitation Trend",
    "Change in SPEI (Drought)", "Historical Trend in SPEI",
    "Change in Wildfire Likelihood", "Wildfire Hazard Potential",
    "Inundation by SLR", "Inundation by Storm Surge", "Saltwater Intrusion",
    "Current Inundation",
    "Current Flood Risk",
    "Change in Mean Annual Temperature", "Historical Temperature Trend",
    "Depth to Water Table"
  ),
  component = c(
    "Exposure", "Exposure", "Sensitivity",
    "Exposure", "Exposure", "Sensitivity", "Sensitivity",
    "Sensitivity",
    "Exposure", "Sensitivity",
    "Exposure", "Sensitivity",
    "Exposure", "Exposure", "Exposure", "Sensitivity",
    "Sensitivity",
    "Exposure", "Sensitivity",
    "Sensitivity"
  ),
  runoff_col = c(
    "norm_exp_runoff_change", "norm_exp_runoff_timing_change", "norm_sen_runoff_trend",
    "norm_exp_nearby_use_change", "norm_exp_auc_change", "norm_sen_competition",
    "norm_sen_auc_change",
    "norm_sen_visitation_trend",
    "norm_exp_drought_change", "norm_sen_drought_trend",
    "norm_exp_fire_prob_change", "norm_sen_wildfire_hazard",
    "norm_exp_inundation_slr", "norm_exp_storm_surge", "norm_exp_swi",
    "norm_sen_inundation_current",
    "norm_sen_flood_risk",
    "norm_exp_temp_change", "norm_sen_temp_trend",
    "norm_sen_source_type"
  ),
  precip_col = c(
    "norm_exp_precip_change", "norm_exp_precip_timing_change", "norm_sen_precip_trend",
    rep(NA_character_, 17)
  ),
  ocean_zero = c(
    TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE,
    TRUE,
    rep(FALSE, 12)
  ),
  v1_factor = c(
    "Water Supply", "Water Supply", "Water Supply",
    "Demand", "Demand", "Demand", "Demand", "Demand",
    "Drought", "Drought",
    "Wildfire", "Wildfire",
    "Coastal (SLR)", "Coastal (SLR)", "Coastal (SLR)", "Coastal (SLR)",
    "Flood",
    "Temperature", "Temperature",
    "Infrastructure (Source Type)"
  ),
  stringsAsFactors = FALSE
)

indicator_label <- function(id) {
  INDICATOR_CATALOG$label[match(id, INDICATOR_CATALOG$indicator)]
}

indicator_component <- function(id) {
  INDICATOR_CATALOG$component[match(id, INDICATOR_CATALOG$indicator)]
}

# ── Pathway tables ───────────────────────────────────────────────────────────
# A pathway table has one row per (pathway, indicator) assignment:
#   pathway    pathway name (free text; rows with the same name form a pathway)
#   indicator  an INDICATOR_CATALOG id. The same indicator may appear in more
#              than one pathway ("duplicated").
#   weight     multiplier on the normalized indicator before aggregation
#              (1 = V1). Weights act relative to the other indicators in the
#              same pathway component, because each component is re-normalized
#              0-1 afterwards (as V1 factors are).
# The component (Exposure / Sensitivity) is fixed by the indicator.

#' Default Impact Pathways, from the "Categorical Indices (Impact Pathways?)"
#' table in private_docs/Revisions_Notes.docx (Phase 1 revisions, Oct 2026).
#' Historical Visitation Trend is in Water Demand sensitivity, as in V1 (the
#' table omitted it by mistake; confirmed by Caitlin, Oct 28 2026).
#'
#' Not assigned by that table, so left out of the default (add them in the
#' explorer's Pathway Builder, or use pathway_presets()):
#'   - Temperature (orphaned; Tyler suggests the Drought pathway)
#'   - Current Flood Risk (orphaned; could join Coastal as "Coastal/Flooding")
#'   - Depth to Water Table (listed as a "Weighting Variable", use not defined)
# TODO: confirm the default grouping with NPS (Tyler, Koren) before it is
# quoted anywhere.
default_pathway_table <- function() {
  data.frame(
    pathway = c(
      rep("Water Supply", 3),
      rep("Water Demand", 5),
      rep("Drought", 2),
      rep("Wildfire", 2),
      rep("Coastal Impacts", 4)
    ),
    indicator = c(
      "ws_change", "ws_timing", "ws_trend",
      "nearby_use_change", "auc_change_exp", "competition", "auc_current",
      "visitation_trend",
      "drought_change", "drought_trend",
      "fire_change", "wildfire_hazard",
      "slr", "storm_surge", "swi", "inundation_current"
    ),
    weight = 1,
    stringsAsFactors = FALSE
  )
}

#' V1's factor grouping expressed as a pathway table: one "pathway" per V1
#' factor, all 20 indicators, unit weights. Used by the parity test; with
#' euclidean aggregation the pathway components equal V1's factor_* columns.
v1_factor_table <- function() {
  cat <- INDICATOR_CATALOG
  data.frame(pathway = cat$v1_factor, indicator = cat$indicator, weight = 1,
             stringsAsFactors = FALSE)
}

#' Named alternative pathway tables for quick comparisons in the explorer.
pathway_presets <- function() {
  d <- default_pathway_table()
  add <- function(tbl, pathway, indicator, weight = 1) {
    rbind(tbl, data.frame(pathway = pathway, indicator = indicator,
                          weight = weight, stringsAsFactors = FALSE))
  }
  coastal_flood <- add(d, "Coastal Impacts", "flood_risk")
  coastal_flood$pathway[coastal_flood$pathway == "Coastal Impacts"] <- "Coastal/Flooding"
  depth_all <- d
  for (p in unique(d$pathway)) depth_all <- add(depth_all, p, "depth_to_water")

  list(
    "Revisions Notes default" = d,
    "Default + Temperature in Drought" =
      add(add(d, "Drought", "temp_change"), "Drought", "temp_trend"),
    "Default + Flood in Coastal (Coastal/Flooding)" = coastal_flood,
    "Default + Depth to Water in every pathway" = depth_all,
    "All orphaned indicators added" =
      add(add(add(coastal_flood, "Drought", "temp_change"), "Drought", "temp_trend"),
          "Water Supply", "depth_to_water"),
    "V1 factors as pathways (8 pathways)" = v1_factor_table()
  )
}

#' Check and tidy a pathway table. Drops blank / unknown rows with a message.
clean_pathway_table <- function(tbl) {
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
  tbl$pathway   <- trimws(as.character(tbl$pathway))
  tbl$indicator <- as.character(tbl$indicator)
  tbl$weight    <- suppressWarnings(as.numeric(tbl$weight))
  tbl$weight[is.na(tbl$weight)] <- 1
  tbl$weight    <- pmax(tbl$weight, 0)
  bad <- is.na(tbl$pathway) | tbl$pathway == "" |
    !tbl$indicator %in% INDICATOR_CATALOG$indicator
  if (any(bad)) message("[methods] Dropping ", sum(bad), " pathway row(s) with ",
                        "no pathway name or an unknown indicator.")
  tbl <- tbl[!bad, c("pathway", "indicator", "weight"), drop = FALSE]
  # The same indicator twice in one pathway would double-count it silently.
  tbl[!duplicated(tbl[, c("pathway", "indicator")]), , drop = FALSE]
}

# ── Method specs ─────────────────────────────────────────────────────────────

#' Build a method spec.
#'
#' @param grouping        "overall" (V1 index) or "pathways"
#' @param pathways        pathway table (see default_pathway_table())
#' @param component_weights data frame (pathway, w_exp, w_sen) multiplying each
#'                        pathway's exposure / sensitivity score before they
#'                        are combined. Missing pathways get 1 / 1.
#' @param factor_agg      how indicators combine inside a pathway component:
#'                        "euclidean" (V1), "mean" or "max"; re-normalized 0-1
#' @param pathway_agg     how exposure and sensitivity combine into a pathway
#'                        score: "euclidean" (V1 style), "mean" or "max"
#' @param high_pct        percentile (0-1) at or above which a pathway score is
#'                        High (V1 flags and priority use 0.75)
#' @param min_high        a supply is Priority under pathways when it is High in
#'                        at least this many pathways
#' @param rank_rule       how pathway results become one ranking:
#'                        "count_then_mean" (number of High pathways, ties
#'                        broken by mean pathway percentile), "count" (ties
#'                        kept), "max_pct" (highest pathway percentile),
#'                        "mean_pct" (mean pathway percentile)
#' @param label           display name
method_spec <- function(grouping = c("overall", "pathways"),
                        pathways = default_pathway_table(),
                        component_weights = NULL,
                        factor_agg = c("euclidean", "mean", "max"),
                        pathway_agg = c("euclidean", "mean", "max"),
                        high_pct = 0.75,
                        min_high = 1,
                        rank_rule = c("count_then_mean", "count", "max_pct", "mean_pct"),
                        label = NULL) {
  grouping    <- match.arg(grouping)
  factor_agg  <- match.arg(factor_agg)
  pathway_agg <- match.arg(pathway_agg)
  rank_rule   <- match.arg(rank_rule)
  if (is.null(label)) label <- if (grouping == "overall") "Overall Index" else "Pathway Indices"
  structure(list(
    grouping = grouping,
    pathways = if (grouping == "pathways") clean_pathway_table(pathways) else NULL,
    component_weights = component_weights,
    factor_agg = factor_agg,
    pathway_agg = pathway_agg,
    high_pct = high_pct,
    min_high = min_high,
    rank_rule = rank_rule,
    label = label
  ), class = "wsva_method")
}

v1_method <- function(label = "Overall Index") {
  method_spec("overall", label = label)
}

pathways_method <- function(pathways = default_pathway_table(), ...,
                            label = "Pathway Indices") {
  method_spec("pathways", pathways = pathways, ..., label = label)
}

# ── Pathway engine ───────────────────────────────────────────────────────────

#' The supply's water-supply mode, matching V1's branch selection.
supply_mode <- function(dat) {
  st <- dat$source_type
  ifelse(!is.na(st) & st == "ocean", "ocean",
         ifelse(!is.na(st) & st == "rainwater", "precip", "runoff"))
}

#' Normalized indicator vector for one catalog indicator under one mode.
indicator_vector <- function(base, indicator, mode) {
  row <- INDICATOR_CATALOG[INDICATOR_CATALOG$indicator == indicator, ]
  if (mode == "ocean" && row$ocean_zero) return(rep(0, nrow(base)))
  col <- if (mode == "precip" && !is.na(row$precip_col)) row$precip_col else row$runoff_col
  if (!col %in% names(base)) return(rep(NA_real_, nrow(base)))
  base[[col]]
}

#' Combine already-normalized indicator vectors into a 0-1 factor score.
#' "euclidean" is V1's euclid_factor(); the others keep its NA convention (a
#' row with every indicator missing is NA) and re-normalize the same way.
agg_factor <- function(vecs, how = "euclidean") {
  if (how == "euclidean") return(do.call(euclid_factor, vecs))
  mat    <- do.call(cbind, vecs)
  all_na <- rowSums(!is.na(mat)) == 0
  out <- switch(how,
    mean = rowMeans(mat, na.rm = TRUE),
    max  = suppressWarnings(apply(mat, 1, max, na.rm = TRUE))
  )
  out[all_na] <- NA_real_
  minmax_norm(out)
}

#' Score one pathway component (all its exposure, or all its sensitivity,
#' indicators). The score is computed across all supplies once per water
#' supply mode (runoff / precip / ocean) and each supply takes its own mode's
#' value. That is how V1 builds its Water Supply factor (runoff and precip
#' branches each normalized over all supplies, then picked by source type) and
#' how it zeroes water supply and demand for ocean supplies.
component_score <- function(base, rows, mode, how = "euclidean") {
  n <- nrow(base)
  if (nrow(rows) == 0) return(rep(NA_real_, n))
  out <- rep(NA_real_, n)
  for (m in unique(mode)) {
    vecs <- lapply(seq_len(nrow(rows)), function(i) {
      indicator_vector(base, rows$indicator[i], m) * rows$weight[i]
    })
    s <- agg_factor(vecs, how)
    out[mode == m] <- s[mode == m]
  }
  out
}

#' Combine a supply's exposure and sensitivity scores into a pathway score.
#' NA components are skipped; both NA gives NA.
combine_components <- function(e, s, how = "euclidean") {
  mat    <- cbind(e, s)
  all_na <- rowSums(!is.na(mat)) == 0
  out <- switch(how,
    euclidean = sqrt(rowSums(mat^2, na.rm = TRUE)),
    mean      = rowMeans(mat, na.rm = TRUE),
    max       = suppressWarnings(apply(mat, 1, max, na.rm = TRUE))
  )
  out[all_na] <- NA_real_
  out
}

#' Top-share flag with V1's flag_cut() semantics: >= the given quantile of the
#' supplies that have a value; missing is FALSE (unassessed, not safe, but not
#' a positive flag either).
high_flag <- function(x, p = 0.75) {
  if (all(is.na(x))) return(rep(FALSE, length(x)))
  thr <- stats::quantile(x, p, na.rm = TRUE)
  dplyr::coalesce(x >= thr, FALSE)
}

#' Pathway keys used in column names (path_<key>_score etc.).
pathway_key <- function(name) {
  k <- tolower(gsub("[^A-Za-z0-9]+", "_", name))
  k <- gsub("^_|_$", "", k)
  make.unique(k, sep = "_")
}

#' Add pathway columns to a scored data frame.
#'
#' Adds, per pathway: path_<key>_exp, path_<key>_sen, path_<key>_score,
#' path_<key>_pct (percentile 0-100 in the comparison group), path_<key>_high.
#' Plus n_high (number of High pathways), n_pathways_scored, mean_path_pct and
#' max_path_pct. attr(out, "pathways") is a data frame of name / key.
add_pathways <- function(base, spec) {
  tbl  <- spec$pathways
  mode <- supply_mode(base)
  names_p <- unique(tbl$pathway)
  keys    <- pathway_key(names_p)
  cw <- spec$component_weights

  for (i in seq_along(names_p)) {
    rows <- tbl[tbl$pathway == names_p[i], , drop = FALSE]
    comp <- indicator_component(rows$indicator)
    e <- component_score(base, rows[comp == "Exposure", , drop = FALSE], mode, spec$factor_agg)
    s <- component_score(base, rows[comp == "Sensitivity", , drop = FALSE], mode, spec$factor_agg)
    w_e <- 1; w_s <- 1
    if (!is.null(cw) && names_p[i] %in% cw$pathway) {
      w_e <- cw$w_exp[match(names_p[i], cw$pathway)]
      w_s <- cw$w_sen[match(names_p[i], cw$pathway)]
    }
    score <- combine_components(e * w_e, s * w_s, spec$pathway_agg)
    k <- keys[i]
    base[[paste0("path_", k, "_exp")]]   <- e
    base[[paste0("path_", k, "_sen")]]   <- s
    base[[paste0("path_", k, "_score")]] <- score
    base[[paste0("path_", k, "_pct")]]   <- pct_rank(score)
    base[[paste0("path_", k, "_high")]]  <- high_flag(score, spec$high_pct)
  }

  high_m <- as.matrix(base[, paste0("path_", keys, "_high"), drop = FALSE])
  pct_m  <- as.matrix(base[, paste0("path_", keys, "_pct"),  drop = FALSE])
  base$n_high            <- rowSums(high_m)
  base$n_pathways_scored <- rowSums(!is.na(pct_m))
  # A supply with no pathway data at all ranks at the bottom, not as missing.
  base$mean_path_pct <- dplyr::coalesce(rowMeans(pct_m, na.rm = TRUE), 0)
  base$max_path_pct  <- dplyr::coalesce(
    suppressWarnings(apply(pct_m, 1, max, na.rm = TRUE)), 0)
  base$max_path_pct[!is.finite(base$max_path_pct)] <- 0

  attr(base, "pathways") <- data.frame(name = names_p, key = keys,
                                       stringsAsFactors = FALSE)
  base
}

# ── run_method() ─────────────────────────────────────────────────────────────

#' Score every supply under a method.
#'
#' Always returns calc_vulnerability_index()'s columns plus a common set every
#' comparison uses:
#'   m_score     score used for ranking (higher = more vulnerable)
#'   m_rank      percentile rank of m_score in the comparison group (0-100)
#'   m_priority  TRUE for Priority supplies (overall: V1 priority_group;
#'               pathways: High in at least min_high pathways)
#'
#' @param dat  the app's scoring input (water_supplies joined to
#'             final_indicators), already subset to the comparison group
#' @param base optional precomputed calc_vulnerability_index(dat), to avoid
#'             recomputing it for several methods on the same group
run_method <- function(dat, spec, base = NULL) {
  if (is.null(base)) base <- calc_vulnerability_index(dat)
  if (spec$grouping == "overall") {
    base$m_score    <- base$VULNERABILITY
    base$m_priority <- dplyr::coalesce(base$priority_group, FALSE)
  } else {
    base <- add_pathways(base, spec)
    base$m_score <- switch(spec$rank_rule,
      count_then_mean = base$n_high + base$mean_path_pct / 1000,
      count           = base$n_high,
      max_pct         = base$max_path_pct,
      mean_pct        = base$mean_path_pct
    )
    base$m_priority <- base$n_high >= spec$min_high
  }
  base$m_rank <- pct_rank(base$m_score)
  attr(base, "method") <- spec
  base
}

#' run_method() inside a comparison group (national, a region, or a state).
#'
#' Mirrors the V1 app's geo_recalculated_data(): the group is rescored from
#' scratch, and if it is too small or scoring fails the national scores are
#' used instead. attr(out, "group_label") says which actually happened, and
#' every explorer output shows it.
run_method_grouped <- function(dat, spec, group = c("National", "Region", "State"),
                               value = NULL, min_n = 20, base = NULL) {
  group <- match.arg(group)
  if (group == "National" || is.null(value) || value == "") {
    out <- run_method(dat, spec, base)
    attr(out, "group_label") <- "National"
    return(out)
  }
  col <- if (group == "Region") "region" else "state"
  sub <- dat[!is.na(dat[[col]]) & dat[[col]] == value, , drop = FALSE]
  out <- if (nrow(sub) >= min_n) {
    tryCatch(run_method(sub, spec), error = function(e) {
      message("[methods] Recalculation failed for ", value, ": ", conditionMessage(e))
      NULL
    })
  } else NULL
  if (is.null(out)) {
    nat <- run_method(dat, spec, base)
    out <- nat[!is.na(nat[[col]]) & nat[[col]] == value, , drop = FALSE]
    attr(out, "method") <- spec
    attr(out, "pathways") <- attr(nat, "pathways")
    attr(out, "group_label") <- paste0(value, " (national scores; group has ",
                                       nrow(sub), " supplies, fewer than ", min_n,
                                       " needed to recalculate)")
  } else {
    attr(out, "group_label") <- paste0(value, " (recalculated within ", tolower(group), ")")
  }
  out
}

# ── Park summaries ───────────────────────────────────────────────────────────

#' One row per park.
#'
#' @param agg "max" (the park's most vulnerable supply), "mean" (mean across
#'   its supplies) or "any" (pathways only: number of distinct pathways in
#'   which ANY of the park's supplies is High; overall falls back to max).
park_summary <- function(res, agg = c("max", "mean", "any")) {
  agg <- match.arg(agg)
  pw  <- attr(res, "pathways")
  is_path <- !is.null(pw) && "n_high" %in% names(res)
  df <- sf::st_drop_geometry(res)
  if (inherits(df, "sf")) df <- as.data.frame(df)

  # Group on the unit code alone: a code can carry more than one park_name in
  # water_supplies.csv (e.g. GLAC), which would otherwise split one park in two.
  grp <- dplyr::group_by(df, park_unit)
  out <- dplyr::summarise(grp,
    park_name    = dplyr::first(park_name),
    n_supplies   = dplyr::n(),
    n_priority   = sum(m_priority, na.rm = TRUE),
    park_score   = if (agg == "mean") mean(m_score, na.rm = TRUE) else max(m_score, na.rm = TRUE),
    .groups = "drop")

  if (is_path) {
    high_cols <- paste0("path_", pw$key, "_high")
    pct_cols  <- paste0("path_", pw$key, "_pct")
    anyhigh <- dplyr::summarise(grp, dplyr::across(dplyr::all_of(high_cols), ~ any(.x, na.rm = TRUE)),
                                n_high_max = max(n_high), .groups = "drop")
    maxpct <- dplyr::summarise(grp, dplyr::across(dplyr::all_of(pct_cols),
                               ~ suppressWarnings(max(.x, na.rm = TRUE))), .groups = "drop")
    out$n_high_any <- rowSums(as.matrix(anyhigh[, high_cols]))
    out$n_high_max <- anyhigh$n_high_max
    for (h in high_cols) out[[h]] <- anyhigh[[h]]
    if (agg == "any") {
      pm <- as.matrix(maxpct[, pct_cols]); pm[!is.finite(pm)] <- NA
      out$park_score <- out$n_high_any +
        dplyr::coalesce(rowMeans(pm, na.rm = TRUE), 0) / 1000
    }
    out$park_n_high <- if (agg == "any") out$n_high_any else out$n_high_max
  }
  out$park_score[!is.finite(out$park_score)] <- NA_real_
  out$park_rank <- pct_rank(out$park_score)
  out
}

# ── Comparison metrics ───────────────────────────────────────────────────────

#' Ordinal position (1 = most vulnerable), ties averaged.
position <- function(score) rank(-score, ties.method = "average", na.last = "keep")

#' Share of the top-N under A that is also in the top-N under B. Ties at the
#' cut are included, so a tie-heavy ranking (e.g. counts) can put more than N
#' units in its top set.
top_n_overlap <- function(id, score_a, score_b, n) {
  in_top <- function(s) {
    if (all(is.na(s))) return(rep(FALSE, length(s)))
    n <- min(n, sum(!is.na(s)))
    thr <- sort(s, decreasing = TRUE)[n]
    !is.na(s) & s >= thr
  }
  a <- in_top(score_a); b <- in_top(score_b)
  list(in_a = a, in_b = b, n_a = sum(a), n_b = sum(b), shared = sum(a & b),
       overlap = sum(a & b) / max(1, min(sum(a), sum(b))),
       jaccard = sum(a & b) / max(1, sum(a | b)))
}

#' Compare two run_method() results on the same supplies (or two
#' park_summary() results on the same parks).
#'
#' @param a,b   results with an id column and m_score/m_priority (supplies) or
#'              park_score (parks)
#' @param unit  "supply" or "park"
#' @param top_n size of the top set for the overlap
#' @return list: rho (Spearman), n, top (top_n_overlap()), crosstab of
#'   priority A x B (supplies only), and `joined`, a data frame of both
#'   ranks with the position shift (positive = more vulnerable under B)
compare_methods <- function(a, b, unit = c("supply", "park"), top_n = 50) {
  unit <- match.arg(unit)
  if (unit == "supply") {
    da <- sf::st_drop_geometry(a); db <- sf::st_drop_geometry(b)
    col_or_na <- function(d, c) if (c %in% names(d)) d[[c]] else rep(NA_character_, nrow(d))
    j <- data.frame(id = da$wsd_source_id, park_unit = col_or_na(da, "park_unit"),
                    park_name = col_or_na(da, "park_name"), score_a = da$m_score,
                    pri_a = da$m_priority, stringsAsFactors = FALSE)
    j <- merge(j, data.frame(id = db$wsd_source_id, score_b = db$m_score,
                             pri_b = db$m_priority), by = "id")
  } else {
    j <- data.frame(id = a$park_unit, park_unit = a$park_unit, park_name = a$park_name,
                    score_a = a$park_score, stringsAsFactors = FALSE)
    j <- merge(j, data.frame(id = b$park_unit, score_b = b$park_score), by = "id")
  }
  j$pos_a <- position(j$score_a)
  j$pos_b <- position(j$score_b)
  j$pct_a <- pct_rank(j$score_a)
  j$pct_b <- pct_rank(j$score_b)
  j$shift <- j$pos_a - j$pos_b
  top <- top_n_overlap(j$id, j$score_a, j$score_b, top_n)
  j$top_a <- top$in_a; j$top_b <- top$in_b
  ok  <- stats::complete.cases(j$score_a, j$score_b)
  rho <- if (sum(ok) > 2) suppressWarnings(stats::cor(j$score_a[ok], j$score_b[ok],
                                                      method = "spearman")) else NA_real_
  xt <- if (unit == "supply") table(`Method A priority` = j$pri_a,
                                    `Method B priority` = j$pri_b) else NULL
  list(rho = rho, n = sum(ok), top = top, crosstab = xt, joined = j)
}

#' Per-pathway flag agreement between two pathway results with shared pathway
#' names (e.g. default vs custom). Pathways present in only one are listed
#' with NA agreement.
compare_pathway_flags <- function(a, b) {
  pa <- attr(a, "pathways"); pb <- attr(b, "pathways")
  da <- sf::st_drop_geometry(a); db <- sf::st_drop_geometry(b)
  db <- db[match(da$wsd_source_id, db$wsd_source_id), ]
  nm <- union(pa$name, pb$name)
  do.call(rbind, lapply(nm, function(p) {
    ka <- pa$key[match(p, pa$name)]; kb <- pb$key[match(p, pb$name)]
    ha <- if (!is.na(ka)) da[[paste0("path_", ka, "_high")]] else NULL
    hb <- if (!is.na(kb)) db[[paste0("path_", kb, "_high")]] else NULL
    sa <- if (!is.na(ka)) da[[paste0("path_", ka, "_score")]] else NULL
    sb <- if (!is.na(kb)) db[[paste0("path_", kb, "_score")]] else NULL
    both <- !is.null(ha) && !is.null(hb)
    ok <- if (both) stats::complete.cases(sa, sb) else FALSE
    data.frame(
      pathway = p,
      in_a = !is.na(ka), in_b = !is.na(kb),
      high_a = if (!is.null(ha)) sum(ha) else NA,
      high_b = if (!is.null(hb)) sum(hb) else NA,
      gained = if (both) sum(!ha & hb) else NA,
      lost   = if (both) sum(ha & !hb) else NA,
      rho    = if (both && sum(ok) > 2) suppressWarnings(
                 stats::cor(sa[ok], sb[ok], method = "spearman")) else NA_real_,
      stringsAsFactors = FALSE)
  }))
}

# ── Decision sensitivity ─────────────────────────────────────────────────────

#' Leave-one-indicator-out: drop each indicator from a pathway table in turn
#' and compare the result with the full table. Shows which assignments the
#' pathway results actually depend on.
drop_one_indicator <- function(dat, spec, base = NULL, top_n = 50) {
  if (is.null(base)) base <- calc_vulnerability_index(dat)
  full <- run_method(dat, spec, base)
  inds <- unique(spec$pathways$indicator)
  do.call(rbind, lapply(inds, function(ind) {
    s2 <- spec
    s2$pathways <- spec$pathways[spec$pathways$indicator != ind, , drop = FALSE]
    if (nrow(s2$pathways) == 0) return(NULL)
    r <- run_method(dat, s2, base)
    cm <- compare_methods(full, r, "supply", top_n)
    data.frame(indicator = ind, label = indicator_label(ind),
               pathways = paste(unique(spec$pathways$pathway[spec$pathways$indicator == ind]),
                                collapse = ", "),
               rho = cm$rho, top_overlap = cm$top$overlap,
               priority_flips = sum(cm$joined$pri_a != cm$joined$pri_b, na.rm = TRUE),
               stringsAsFactors = FALSE)
  }))
}

#' Random weight perturbation: draw every indicator weight (and each pathway's
#' exposure / sensitivity weight) from U(1 - spread, 1 + spread) and compare
#' with the unperturbed spec. Gives a reference for how much ranks move under
#' weighting choices nobody would argue about, to judge a deliberate change
#' against.
perturb_weights <- function(dat, spec, base = NULL, n_draws = 100, spread = 0.5,
                            top_n = 50, seed = 1, progress = NULL) {
  if (is.null(base)) base <- calc_vulnerability_index(dat)
  ref <- run_method(dat, spec, base)
  set.seed(seed)
  pw <- unique(spec$pathways$pathway)
  do.call(rbind, lapply(seq_len(n_draws), function(i) {
    if (!is.null(progress)) progress(i / n_draws)
    s2 <- spec
    s2$pathways$weight <- spec$pathways$weight *
      stats::runif(nrow(spec$pathways), 1 - spread, 1 + spread)
    s2$component_weights <- data.frame(
      pathway = pw,
      w_exp = stats::runif(length(pw), 1 - spread, 1 + spread),
      w_sen = stats::runif(length(pw), 1 - spread, 1 + spread))
    r  <- run_method(dat, s2, base)
    cm <- compare_methods(ref, r, "supply", top_n)
    data.frame(draw = i, rho = cm$rho, top_overlap = cm$top$overlap,
               priority_flips = sum(cm$joined$pri_a != cm$joined$pri_b, na.rm = TRUE))
  }))
}
