# Parity between the prototype method layer and V1.
#
# Run from the repo root:
#   Rscript -e 'testthat::test_file("prototypes/tests/test-parity.R")'
#
# 1. run_method(dat, v1_method()) reproduces calc_vulnerability_index(dat).
# 2. The pathway engine, given V1's own factor grouping (v1_factor_table()),
#    reproduces every V1 factor_* column and VULNERABILITY. This is what makes
#    the Overall vs Pathways comparison a comparison of grouping decisions
#    rather than of two different implementations.
# 3. Comparing V1 with itself gives rho = 1 and no priority flips.

library(testthat)
suppressPackageStartupMessages(library(dplyr))

# test_file() runs from this file's folder; find_path() resolves either way.
paths_r <- c("prototypes/shared/paths.R", "../shared/paths.R")
source(paths_r[file.exists(paths_r)][1])
source(find_path("calc_vulnerability_index.R"))
source(find_path("tests/testthat/helper-fixtures.R"))
source(find_path("shared/methods.R"))

V1_FACTORS <- c(
  "Water Supply" = "water_supply", "Demand" = "demand", "Drought" = "drought",
  "Wildfire" = "wildfire", "Coastal (SLR)" = "slr", "Temperature" = "temp",
  "Flood" = "flood", "Infrastructure (Source Type)" = "infrastructure"
)

real_input <- function() {
  ws <- readr::read_csv(find_path("app_data/water_supplies.csv"), show_col_types = FALSE)
  fi <- readr::read_csv(find_path("app_data/final_indicators.csv"), show_col_types = FALSE)
  dplyr::left_join(ws, fi, by = "wsd_source_id")
}

fake_inputs <- list(
  groundwater = make_fake_supplies(n = 12, seed = 3),
  mixed_types = make_fake_supplies(
    n = 16, seed = 7,
    source_type = rep(c("river/stream", "rainwater", "ocean", "spring"), 4)),
  missing_coastal = make_fake_supplies(
    n = 10, seed = 11,
    na_cols = c("exp_inundation_slr", "exp_swi", "exp_storm_surge",
                "sen_inundation_current", "sen_flood_risk"))
)

check_parity <- function(dat) {
  v1  <- calc_vulnerability_index(dat)
  out <- run_method(dat, v1_method())

  # 1. Overall method is V1
  for (col in c("EXPOSURE", "SENSITIVITY", "VULNERABILITY", "priority_group",
                "vulnerability_quartile")) {
    expect_identical(out[[col]], v1[[col]], info = col)
  }
  expect_equal(out$m_score, v1$VULNERABILITY)
  expect_identical(out$m_priority, dplyr::coalesce(v1$priority_group, FALSE))

  # 2. Pathway engine with V1 grouping reproduces V1 factors and total
  pw  <- run_method(dat, pathways_method(v1_factor_table()))
  key <- attr(pw, "pathways")
  for (nm in names(V1_FACTORS)) {
    k <- key$key[key$name == nm]
    f <- V1_FACTORS[[nm]]
    for (comp in c("exp", "sen")) {
      v1_col <- paste0("factor_", comp, "_", f)
      if (!v1_col %in% names(v1)) next
      expect_equal(pw[[paste0("path_", k, "_", comp)]], v1[[v1_col]],
                   tolerance = 1e-12, info = paste(nm, comp))
    }
  }
  score_m <- as.matrix(pw[, paste0("path_", key$key, "_score")])
  total   <- sqrt(rowSums(score_m^2, na.rm = TRUE))
  expect_equal(total, v1$VULNERABILITY, tolerance = 1e-12)

  # 3. V1 vs V1
  cm <- compare_methods(out, run_method(dat, v1_method()), "supply", top_n = 5)
  expect_equal(cm$rho, 1)
  expect_equal(sum(cm$joined$pri_a != cm$joined$pri_b), 0)
  expect_equal(cm$top$overlap, 1)
}

for (nm in names(fake_inputs)) {
  test_that(paste("parity on make_fake_supplies():", nm), {
    check_parity(fake_inputs[[nm]])
  })
}

test_that("parity on the real app input", {
  skip_if(inherits(try(find_path("app_data/final_indicators.csv"), silent = TRUE), "try-error"),
          "app_data not found")
  check_parity(real_input())
})

test_that("default pathways run and behave sensibly on the real input", {
  skip_if(inherits(try(find_path("app_data/final_indicators.csv"), silent = TRUE), "try-error"),
          "app_data not found")
  dat <- real_input()
  res <- run_method(dat, pathways_method())
  pw  <- attr(res, "pathways")
  expect_setequal(pw$name, c("Water Supply", "Water Demand", "Drought",
                             "Wildfire", "Coastal Impacts"))
  expect_true(all(res$n_high >= 0 & res$n_high <= nrow(pw)))
  # Each pathway flags roughly the top 25% of the supplies it can score
  for (k in pw$key) {
    s <- res[[paste0("path_", k, "_score")]]
    share <- sum(res[[paste0("path_", k, "_high")]]) / sum(!is.na(s))
    expect_gt(share, 0.2); expect_lt(share, 0.35)
  }
  # Default pathway components that keep V1's factor membership equal V1
  base <- calc_vulnerability_index(dat)
  expect_equal(res$path_water_demand_sen, base$factor_sen_demand, tolerance = 1e-12)
  expect_equal(res$path_water_demand_exp, base$factor_exp_demand, tolerance = 1e-12)
  expect_equal(res$path_water_supply_exp, base$factor_exp_water_supply, tolerance = 1e-12)
  # Ocean supplies score zero on water supply and demand, as in V1
  ocean <- res$source_type == "ocean"
  expect_true(all(res$path_water_supply_score[ocean] == 0))
  expect_true(all(res$path_water_demand_score[ocean] == 0))
})

test_that("unit weights are V1, and weights change results", {
  dat <- fake_inputs$mixed_types
  t1 <- default_pathway_table()
  t2 <- t1; t2$weight[t2$indicator == "ws_change"] <- 3
  r1 <- run_method(dat, pathways_method(t1))
  r2 <- run_method(dat, pathways_method(t2))
  expect_false(isTRUE(all.equal(r1$path_water_supply_exp, r2$path_water_supply_exp)))
  expect_equal(r1$path_wildfire_score, r2$path_wildfire_score)
})

test_that("duplicated indicators and new pathways are supported", {
  dat <- fake_inputs$groundwater
  t <- rbind(default_pathway_table(),
             data.frame(pathway = c("Drought", "Heat"),
                        indicator = c("ws_change", "temp_change"), weight = 1))
  res <- run_method(dat, pathways_method(t))
  expect_true("Heat" %in% attr(res, "pathways")$name)
  expect_true(all(c("path_heat_score", "path_drought_score") %in% names(res)))
})

test_that("comparison group recalculation falls back to national when small", {
  dat <- fake_inputs$mixed_types
  dat$region <- rep(c("A", "B"), length.out = nrow(dat))
  r <- run_method_grouped(dat, v1_method(), "Region", "A", min_n = 100)
  expect_match(attr(r, "group_label"), "national scores")
  expect_true(all(r$region == "A"))
  r2 <- run_method_grouped(dat, v1_method(), "Region", "A", min_n = 2)
  expect_match(attr(r2, "group_label"), "recalculated")
})
