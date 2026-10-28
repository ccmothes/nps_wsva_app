# prototypes/

Prototype apps for the next round of WSVA method decisions. Nothing in this
folder is part of the production app (`rapid_app`, served at
<https://apps.gis.colostate.edu/WSVA_tool/>): `deploy.R` publishes only its
`APP_FILES`, `tests/testthat/test-deploy-manifest.R` scans only the root
`app.R`, and `prototypes` is listed in `.rscignore`.

The V1 app at the repo root (`app.R`, `calc_vulnerability_index.R`,
`deploy.R`, `manifest.json`, `tests/`) is not modified by anything here.

## Contents

- `shared/paths.R`: `find_path()`, which resolves `app_data/`, `shared/` and
  `calc_vulnerability_index.R` from `prototypes/<app>/`, from the repo root, or
  from a flat deployed bundle.
- `shared/methods.R`: the method layer. `method_spec()`, `v1_method()`,
  `pathways_method()`, `run_method()`, `run_method_grouped()`,
  `compare_methods()`, `park_summary()`, plus the indicator catalog, the default
  pathway table and the decision sensitivity checks (`drop_one_indicator()`,
  `perturb_weights()`). Reuses `minmax_norm()`, `pct_rank()` and
  `euclid_factor()` from `calc_vulnerability_index.R`.
- `methods_explorer/app.R`: the Methods Explorer (below).
- `pathways_app/`: not started yet.
- `tests/test-parity.R`: the method layer reproduces V1 exactly.
- `tests/test-explorer-smoke.R`: server-level smoke test (no browser needed)
  plus a shinytest2 launch test (skipped without shinytest2 and Chrome).
- `deploy_prototype.R`: stages and publishes a prototype under its own
  shinyapps.io name. Refuses `rapid_app`.

## Methods Explorer

Compares the V1 **Overall Index** with **Pathway Indices**, then lets the
pathway grouping and weights be edited to see how much the results depend on
those decisions.

- **Overall vs Pathways**: Spearman rank correlation, top-N overlap, Priority
  counts, side-by-side maps (Overall Priority vs supplies sized by number of
  High pathways), overall quartile x number of High pathways, a rank
  scatter, what each pathway flags beyond Overall Priority, and the top-N table.
- **Pathway Builder**: edit the pathway table (move an indicator by renaming
  its pathway, duplicate it into another pathway, change weights, delete it,
  or start from a preset), plus exposure / sensitivity weights per pathway.
  Compares Overall, Default pathways and Custom pathways pairwise, shows
  per-pathway flag changes and the biggest movers, and runs two reference
  checks: random weight perturbation of the default pathways and
  leave-one-indicator-out on the custom pathways.
- **Park Lookup**: any park, its supplies under all three methods.
- Sidebar: comparison group (National, Region, State; rescored within the
  group, with a national fallback under 20 supplies), unit (supplies or
  parks), top-N, the High percentile, the number of High pathways that makes
  a supply Priority, the ranking rule, and aggregation choices. Every output
  says which comparison group it uses.

Default pathways follow the Impact Pathways table in the Phase 1 Revisions
Notes (Water Supply, Water Demand, Drought, Wildfire, Coastal Impacts), with
Historical Visitation Trend in Water Demand sensitivity as in V1 (the table
omitted it). Temperature, Current Flood Risk and Depth to Water Table are not in
any default pathway; presets add each of them back.

## Running

From the repo root in R:

```r
install.packages(c("shiny", "bslib", "DT", "plotly", "leaflet", "dplyr",
                   "readr", "sf", "testthat", "shinytest2", "rsconnect"))
shiny::runApp("prototypes/methods_explorer")
```

Tests, from the repo root:

```bash
Rscript -e 'testthat::test_file("prototypes/tests/test-parity.R")'
Rscript -e 'testthat::test_file("prototypes/tests/test-explorer-smoke.R")'
```

## Deploying a prototype

Never use `deploy.R` for prototypes.

```bash
Rscript prototypes/deploy_prototype.R methods_explorer stage
Rscript prototypes/deploy_prototype.R methods_explorer deploy wsva_methods_explorer
```

`stage` builds the flat bundle in a temp folder without uploading. `deploy`
publishes it under the given name on the `geocentroid` account.
