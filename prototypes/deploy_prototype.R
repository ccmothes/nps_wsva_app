#' ──────────────────────────────────────────────────────────────────────────────
#' prototypes/deploy_prototype.R
#'
#' Stage and (optionally) publish a prototype app under its OWN shinyapps.io
#' name. Never touches the production app.
#'
#' From the repo root:
#'   Rscript prototypes/deploy_prototype.R methods_explorer stage
#'       # builds the flat bundle in a temp dir and prints its path; no upload
#'   Rscript prototypes/deploy_prototype.R methods_explorer deploy wsva_methods_explorer
#'       # stages, then publishes as `wsva_methods_explorer`
#'
#' The staged bundle is flat:
#'   app.R                       (prototypes/<app>/app.R)
#'   calc_vulnerability_index.R  (V1 scoring, unchanged)
#'   shared/                     (paths.R, methods.R)
#'   app_data/..., www/...       (the entries of deploy.R's APP_FILES the
#'                                app needs; see PROTOTYPE_DATA)
#' shared/paths.R's find_path() resolves all of these from the bundle root.
#'
#' Safety: refuses the production app name (deploy.R's SHINYAPPS_APP,
#' "rapid_app"). deploy.R itself is only read, never run.
#' ──────────────────────────────────────────────────────────────────────────────

# Data each prototype reads. NULL = every app_data/ and www entry of APP_FILES.
PROTOTYPE_DATA <- list(
  methods_explorer = c("app_data/water_supplies.csv", "app_data/final_indicators.csv")
)

repo_root <- function() {
  if (file.exists("deploy.R") && dir.exists("prototypes")) return(normalizePath("."))
  if (file.exists("../deploy.R")) return(normalizePath(".."))
  stop("Run from the repo root (or prototypes/).", call. = FALSE)
}

#' APP_FILES and SHINYAPPS_* from deploy.R, read without running its main block
#' (sourcing sets sys.nframe() > 0, so deploy.R's command dispatch is skipped).
read_deploy_config <- function(root) {
  env <- new.env()
  sys.source(file.path(root, "deploy.R"), envir = env)
  list(app_files = env$APP_FILES, account = env$SHINYAPPS_ACCOUNT,
       prod_app = env$SHINYAPPS_APP)
}

stage_prototype <- function(app, out_dir = file.path(tempdir(), paste0("wsva_", app))) {
  root <- repo_root()
  cfg  <- read_deploy_config(root)
  app_r <- file.path(root, "prototypes", app, "app.R")
  if (!file.exists(app_r)) stop("No prototype app at ", app_r, call. = FALSE)

  data <- PROTOTYPE_DATA[[app]]
  if (is.null(data)) data <- grep("^(app_data/|www$)", cfg$app_files, value = TRUE)
  missing <- setdiff(data, cfg$app_files)
  if (length(missing)) {
    stop("Not in deploy.R APP_FILES: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  unlink(out_dir, recursive = TRUE)
  dir.create(file.path(out_dir, "shared"), recursive = TRUE)
  file.copy(app_r, file.path(out_dir, "app.R"))
  file.copy(file.path(root, "calc_vulnerability_index.R"), out_dir)
  file.copy(list.files(file.path(root, "prototypes", "shared"), full.names = TRUE),
            file.path(out_dir, "shared"))
  for (f in data) {
    src <- file.path(root, f)
    dir.create(dirname(file.path(out_dir, f)), recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(src)) {
      file.copy(src, dirname(file.path(out_dir, f)), recursive = TRUE)
    } else {
      file.copy(src, file.path(out_dir, f))
    }
  }
  message("Staged ", app, " in ", out_dir)
  invisible(out_dir)
}

deploy_prototype <- function(app, app_name, account = NULL) {
  root <- repo_root()
  cfg  <- read_deploy_config(root)
  if (missing(app_name) || !nzchar(app_name)) stop("Give a shinyapps.io app name.", call. = FALSE)
  if (identical(tolower(app_name), tolower(cfg$prod_app))) {
    stop("Refusing to deploy a prototype as '", app_name,
         "': that is the production app. Use deploy.R for production.", call. = FALSE)
  }
  dir <- stage_prototype(app)
  rsconnect::deployApp(
    appDir = dir, appName = app_name,
    account = account %||% cfg$account, server = "shinyapps.io",
    launch.browser = FALSE
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2 && args[2] == "stage") {
    stage_prototype(args[1])
  } else if (length(args) >= 3 && args[2] == "deploy") {
    deploy_prototype(args[1], args[3])
  } else {
    stop("Usage: Rscript prototypes/deploy_prototype.R <app> stage\n",
         "       Rscript prototypes/deploy_prototype.R <app> deploy <shinyapps_name>",
         call. = FALSE)
  }
}
