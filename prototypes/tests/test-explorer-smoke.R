# Smoke tests for prototypes/methods_explorer.
#
# Run from the repo root:
#   Rscript -e 'testthat::test_file("prototypes/tests/test-explorer-smoke.R")'
#
# The testServer() block needs no browser. The shinytest2 block needs
# shinytest2 and Chrome and is skipped without them.

library(testthat)

app_dir <- local({
  d <- c("prototypes/methods_explorer", "../methods_explorer")
  normalizePath(d[dir.exists(d)][1])
})

test_that("server computes every comparison and reacts to pathway edits", {
  app <- shiny::shinyAppDir(app_dir)
  shiny::testServer(app, {
    session$setInputs(group = "National", unit = "supply", park_agg = "max",
                      top_n = 50, high_pct = 75, min_high = 1,
                      rank_rule = "count_then_mean", factor_agg = "euclidean",
                      pathway_agg = "euclidean", preset = "Revisions Notes default")

    # Overall vs default pathways
    expect_gt(cmp_od()$rho, 0); expect_lt(cmp_od()$rho, 1)
    expect_equal(nrow(res_overall()), nrow(res_default()))
    expect_match(glabel(), "National")

    # Custom starts equal to default
    session$elapse(500)
    expect_equal(cmp_dc()$rho, 1)

    # Move an indicator into a new pathway via a cell edit
    t <- custom_tbl()
    row <- which(t$indicator == "wildfire_hazard")
    session$setInputs(path_table_cell_edit = data.frame(row = row, col = 0, value = "Fire Risk"))
    expect_true("Fire Risk" %in% custom_tbl()$pathway)
    session$elapse(500)
    expect_lt(cmp_dc()$rho, 1)

    # Parks and a recalculated region
    session$setInputs(unit = "park", group = "Region", region = "Intermountain Region")
    expect_match(glabel(), "recalculated|national scores")
    expect_gt(nrow(cmp_od()$joined), 1)
  })
})

test_that("app launches in a browser (shinytest2)", {
  skip_if_not_installed("shinytest2")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
          "Chrome not available")
  drv <- shinytest2::AppDriver$new(app_dir, name = "methods-explorer", load_timeout = 60000)
  on.exit(drv$stop(), add = TRUE)
  drv$wait_for_idle(timeout = 30000)
  expect_match(drv$get_text("#ctx1"), "Comparison group")
  expect_match(drv$get_text("#boxes1"), "RANK CORRELATION|Rank correlation")
})
