#' ──────────────────────────────────────────────────────────────────────────────
#' prototypes/methods_explorer/app.R
#'
#' WSVA Methods Explorer (prototype). Compares the V1 Overall Index with Impact
#' Pathway Indices, and lets the pathway grouping and weights be edited to see
#' how much the rankings and the findings depend on those decisions.
#'
#' Run from the repo root:   shiny::runApp("prototypes/methods_explorer")
#' Deploy (never deploy.R):  Rscript prototypes/deploy_prototype.R methods_explorer <name>
#'
#' Scoring lives in prototypes/shared/methods.R (parity with V1 is asserted in
#' prototypes/tests/test-parity.R). This file is UI only.
#' ──────────────────────────────────────────────────────────────────────────────

# install.packages(c("shiny", "bslib", "DT", "plotly", "leaflet", "dplyr",
#                    "readr", "sf"))
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(leaflet)
library(dplyr)

local({
  p <- c("shared/paths.R", "../shared/paths.R", "prototypes/shared/paths.R")
  source(p[file.exists(p)][1])
})
source(find_path("calc_vulnerability_index.R"))
source(find_path("shared/methods.R"))

COL_EXP  <- "#5B8C6E"
COL_SEN  <- "#7C6FAD"
COL_HIGH <- "#9B2226"
COL_NONE <- "#A3ACB3"

# ── Data ─────────────────────────────────────────────────────────────────────
# Same scoring input as the V1 app (app.R): water_supplies joined to
# final_indicators. Kept as a plain data frame; maps use the coordinates.
water_supplies <- readr::read_csv(find_path("app_data/water_supplies.csv"),
                                  show_col_types = FALSE) %>%
  select(wsd_source_id, park_unit, park_name, region, state, water_system_name,
         source_type, source_longitude, source_latitude)
final_indicators <- readr::read_csv(find_path("app_data/final_indicators.csv"),
                                    show_col_types = FALSE)
combined_raw <- left_join(water_supplies, final_indicators, by = "wsd_source_id")
NAT_BASE <- calc_vulnerability_index(combined_raw)

REGIONS <- sort(unique(na.omit(combined_raw$region)))
STATES  <- sort(unique(na.omit(combined_raw$state)))
PARKS   <- combined_raw %>% distinct(park_unit, .keep_all = TRUE) %>%
  select(park_unit, park_name) %>% arrange(park_name)
PARK_CHOICES <- setNames(PARKS$park_unit, paste0(PARKS$park_name, " (", PARKS$park_unit, ")"))
IND_CHOICES <- setNames(INDICATOR_CATALOG$indicator,
                        paste0(INDICATOR_CATALOG$label, " [", INDICATOR_CATALOG$component, "]"))
PRESETS <- pathway_presets()

RANK_RULES <- c(
  "# High pathways, ties broken by mean pathway percentile" = "count_then_mean",
  "# High pathways only (ties kept)" = "count",
  "Highest pathway percentile" = "max_pct",
  "Mean pathway percentile" = "mean_pct"
)

# ── Helpers ──────────────────────────────────────────────────────────────────

#' Names of the pathways a row is High in, as one string per row.
high_names <- function(df, pw) {
  if (is.null(pw) || nrow(df) == 0) return(character(nrow(df)))
  m <- as.matrix(df[, paste0("path_", pw$key, "_high"), drop = FALSE])
  apply(m, 1, function(r) paste(pw$name[which(r %in% TRUE)], collapse = ", "))
}

#' Rank comparison at the chosen unit. Returns the compare_methods() result
#' plus per-unit pathway info for B (and C when given).
unit_compare <- function(a, b, unit, park_agg, top_n) {
  if (unit == "supply") {
    cm <- compare_methods(a, b, "supply", top_n)
    pw <- attr(b, "pathways")
    info <- data.frame(id = b$wsd_source_id, system = b$water_system_name,
                       source_type = b$source_type,
                       n_high = if (!is.null(pw)) b$n_high else NA,
                       high_list = high_names(b, pw),
                       quart_a = a$vulnerability_quartile[match(b$wsd_source_id, a$wsd_source_id)],
                       lng = b$source_longitude, lat = b$source_latitude,
                       stringsAsFactors = FALSE)
  } else {
    pa <- park_summary(a, if (park_agg == "any") "max" else park_agg)
    pb <- park_summary(b, park_agg)
    cm <- compare_methods(pa, pb, "park", top_n)
    pw <- attr(b, "pathways")
    coords <- b %>% group_by(park_unit) %>%
      summarise(lng = mean(source_longitude, na.rm = TRUE),
                lat = mean(source_latitude, na.rm = TRUE), .groups = "drop")
    info <- data.frame(id = pb$park_unit, system = paste(pb$n_supplies, "supplies"),
                       source_type = NA_character_,
                       n_high = if (!is.null(pw)) pb$park_n_high else NA,
                       high_list = high_names(pb, pw),
                       quart_a = pmin(4, pmax(1, ceiling(pa$park_rank[match(pb$park_unit, pa$park_unit)] / 25))),
                       stringsAsFactors = FALSE)
    info <- left_join(info, coords, by = c("id" = "park_unit"))
    cm$joined$pri_a <- pa$n_priority[match(cm$joined$id, pa$park_unit)] > 0
  }
  cm$joined <- left_join(cm$joined, info, by = "id")
  cm
}

fmt_rho <- function(x) ifelse(is.na(x), "n/a", formatC(x, format = "f", digits = 3))
fmt_pct <- function(x) ifelse(is.na(x), "n/a", paste0(round(100 * x), "%"))

rho_words <- function(r) {
  if (is.na(r)) return("")
  if (r >= 0.95) "nearly identical ordering"
  else if (r >= 0.85) "similar ordering, some reshuffling"
  else if (r >= 0.7) "moderately different ordering"
  else "substantially different ordering"
}

vbox <- function(title, value, sub = NULL, color = "#334") {
  div(class = "vbox",
      div(class = "vbox-title", title),
      div(class = "vbox-value", style = paste0("color:", color), value),
      if (!is.null(sub)) div(class = "vbox-sub", sub))
}

empty_plot <- function(msg) {
  plotly_empty(type = "scatter", mode = "markers") %>%
    layout(title = list(text = msg, font = list(size = 13)))
}

#' Heatmap of a two-way table of counts.
count_heatmap <- function(x, y, xlab, ylab, title) {
  tb <- as.data.frame(table(x = factor(x), y = factor(y)))
  # Annotations on a category axis are placed by 0-based category index;
  # numeric-looking labels ("1", "2") would otherwise land one cell off.
  ax <- as.integer(tb$x) - 1; ay <- as.integer(tb$y) - 1
  tb$x <- as.character(tb$x); tb$y <- as.character(tb$y)
  tb$label <- tb$Freq
  plot_ly(tb, x = ~x, y = ~y, z = ~Freq, type = "heatmap",
          colors = colorRamp(c("#F4F1EE", COL_HIGH)), showscale = FALSE,
          hovertemplate = paste0(xlab, ": %{x}<br>", ylab, ": %{y}<br>Count: %{z}<extra></extra>")) %>%
    add_annotations(x = ax, y = ay, text = tb$label, showarrow = FALSE,
                    font = list(color = ifelse(tb$Freq > max(tb$Freq) * 0.55, "white", "#222"))) %>%
    layout(title = list(text = title, font = list(size = 13)),
           xaxis = list(title = xlab, type = "category"),
           yaxis = list(title = ylab, type = "category"))
}

#' Scatter of percentiles under two methods.
rank_scatter <- function(j, xlab, ylab, title, color_by = NULL) {
  j <- j[stats::complete.cases(j$pct_a, j$pct_b), ]
  j$hover <- paste0(j$park_name, "<br>", j$system,
                    "<br>", xlab, " pct: ", round(j$pct_a, 1),
                    "<br>", ylab, " pct: ", round(j$pct_b, 1),
                    if (!is.null(color_by)) paste0("<br># High pathways: ", j[[color_by]]) else "")
  p <- if (!is.null(color_by)) {
    plot_ly(j, x = ~pct_a, y = ~pct_b, type = "scatter", mode = "markers",
            color = ~factor(j[[color_by]]),
            colors = colorRampPalette(c("#C9CED2", "#E3A07A", COL_HIGH))(max(1, length(unique(j[[color_by]])))),
            text = ~hover, hoverinfo = "text", marker = list(size = 6, opacity = 0.75))
  } else {
    plot_ly(j, x = ~pct_a, y = ~pct_b, type = "scatter", mode = "markers",
            text = ~hover, hoverinfo = "text",
            marker = list(size = 6, opacity = 0.7, color = COL_SEN))
  }
  p %>% add_segments(x = 0, y = 0, xend = 100, yend = 100, inherit = FALSE,
                     line = list(color = "#888", dash = "dot"), showlegend = FALSE,
                     hoverinfo = "skip") %>%
    layout(title = list(text = title, font = list(size = 13)),
           xaxis = list(title = paste(xlab, "percentile"), range = c(0, 101)),
           yaxis = list(title = paste(ylab, "percentile"), range = c(0, 101)),
           legend = list(title = list(text = "# High")))
}

# ── UI ───────────────────────────────────────────────────────────────────────

css <- "
.vbox { background:#fff; border:1px solid #e3e3e3; border-radius:8px; padding:10px 14px; height:100%; }
.vbox-title { font-size:.8rem; color:#555; text-transform:uppercase; letter-spacing:.03em; }
.vbox-value { font-size:1.6rem; font-weight:600; line-height:1.2; }
.vbox-sub { font-size:.8rem; color:#666; }
.ctx { background:#F4F1EE; border-left:4px solid #5B8C6E; padding:6px 12px; margin-bottom:10px; font-size:.9rem; }
.note { font-size:.85rem; color:#555; }
.card-header { font-weight:600; }
"

sidebar_ui <- sidebar(
  width = 300,
  h6("Comparison group"),
  radioButtons("group", NULL, c("National", "Region", "State"), inline = TRUE),
  conditionalPanel("input.group == 'Region'",
                   selectInput("region", NULL, REGIONS)),
  conditionalPanel("input.group == 'State'",
                   selectInput("state", NULL, STATES)),
  hr(),
  h6("Compare"),
  radioButtons("unit", NULL, c("Water supplies" = "supply", "Parks" = "park"), inline = TRUE),
  conditionalPanel("input.unit == 'park'",
    selectInput("park_agg", "Park summary",
                c("Most vulnerable supply (max)" = "max",
                  "Mean across supplies" = "mean",
                  "Pathways High at any supply (union)" = "any"))),
  sliderInput("top_n", "Top-N for overlap", min = 10, max = 300, value = 50, step = 5, ticks = FALSE),
  hr(),
  h6("Pathway rules"),
  sliderInput("high_pct", "High = pathway score at or above percentile",
              min = 50, max = 95, value = 75, step = 5, post = "th"),
  sliderInput("min_high", "Pathway Priority = High in at least",
              min = 1, max = 8, value = 1, step = 1, post = " pathway(s)"),
  selectInput("rank_rule", "Ranking under pathways", RANK_RULES),
  accordion(open = FALSE,
    accordion_panel("Aggregation",
      selectInput("factor_agg", "Indicators within a pathway component",
                  c("Euclidean (V1)" = "euclidean", "Mean" = "mean", "Max" = "max")),
      selectInput("pathway_agg", "Exposure + Sensitivity into pathway score",
                  c("Euclidean (V1 style)" = "euclidean", "Mean" = "mean", "Max" = "max"))
    )
  ),
  div(class = "note", "Pathway rules apply to both the default and the custom pathways, so the Builder tab isolates the effect of grouping and weights.")
)

tab_overview <- nav_panel(
  "Overall vs Pathways",
  uiOutput("ctx1"),
  radioButtons("tab1_version", "Pathways version compared with the Overall Index",
               c("Default pathways" = "default",
                 "Custom pathways (from the Pathway Builder)" = "custom"),
               inline = TRUE),
  uiOutput("boxes1"),
  layout_columns(col_widths = c(6, 6),
    card(full_screen = TRUE, card_header(textOutput("map_a_title", inline = TRUE)),
         leafletOutput("map_a", height = 380)),
    card(full_screen = TRUE, card_header(textOutput("map_b_title", inline = TRUE)),
         leafletOutput("map_b", height = 380))
  ),
  layout_columns(col_widths = c(6, 6),
    card(full_screen = TRUE, card_header("Overall quartile vs number of High pathways"),
         plotlyOutput("xtab1", height = 340),
         div(class = "note", "Rows: how many pathways the unit is High in. Columns: its quartile under the Overall Index (4 = top 25%, the V1 Priority group).")),
    card(full_screen = TRUE, card_header("Rank agreement"),
         plotlyOutput("scatter1", height = 340),
         div(class = "note", "Points on the dotted line rank the same under both methods. Above the line = ranked as more vulnerable under pathways."),
         uiOutput("rank_note"))
  ),
  layout_columns(col_widths = c(5, 7),
    card(full_screen = TRUE, card_header("What each pathway flags, relative to Overall Priority"),
         plotlyOutput("pathway_bars", height = 360),
         div(class = "note", "Water supplies High in each pathway, split by whether the Overall Index already calls them Priority. The light part is what the pathway surfaces that the overall index does not.")),
    card(full_screen = TRUE, card_header(textOutput("top_title", inline = TRUE)),
         DTOutput("top_table"))
  )
)

tab_builder <- nav_panel(
  "Pathway Builder",
  uiOutput("ctx2"),
  layout_columns(col_widths = c(5, 7),
    div(
      card(
        card_header("Custom pathways"),
        layout_columns(col_widths = c(8, 4),
          selectInput("preset", "Start from", names(PRESETS), width = "100%"),
          div(style = "padding-top:32px;", actionButton("load_preset", "Load", class = "btn-sm"),
              actionButton("reset", "Reset", class = "btn-sm"))),
        div(class = "note", "Double-click a Pathway cell to move the indicator (type an existing or a new pathway name) or a Weight cell to change it, then click outside the cell (or press Tab) to apply. Select rows and Delete to remove them."),
        DTOutput("path_table"),
        actionButton("delete_rows", "Delete selected", class = "btn-sm btn-outline-danger"),
        hr(),
        strong("Add or duplicate an indicator"),
        layout_columns(col_widths = c(12, 7, 5),
          selectInput("add_ind", "Indicator", IND_CHOICES, width = "100%"),
          selectizeInput("add_path", "Pathway", choices = NULL,
                         options = list(create = TRUE, placeholder = "pick or type a new name")),
          numericInput("add_w", "Weight", 1, min = 0, step = 0.25)),
        actionButton("add_row", "Add", class = "btn-sm btn-primary"),
        uiOutput("unassigned")
      ),
      card(
        card_header("Exposure / Sensitivity weight per pathway"),
        div(class = "note", "Multiplies each pathway's exposure and sensitivity score before they are combined. Indicator weights act only relative to other indicators in the same component, so this is the way to weight a single-indicator component."),
        DTOutput("cw_table")
      )
    ),
    div(
      uiOutput("boxes2"),
      card(full_screen = TRUE, card_header("Pairwise agreement between the three methods"),
           tableOutput("matrix_table"),
           div(class = "note", "Spearman rank correlation (top) and share of the Top-N shared (bottom), at the selected unit.")),
      layout_columns(col_widths = c(6, 6),
        card(full_screen = TRUE, card_header("# High pathways: default vs custom"),
             plotlyOutput("xtab2", height = 320)),
        card(full_screen = TRUE, card_header("Rank agreement: default vs custom"),
             plotlyOutput("scatter2", height = 320))
      ),
      card(full_screen = TRUE, card_header("Per-pathway changes (default vs custom, water supplies)"),
           DTOutput("pathway_changes"),
           div(class = "note", "Pathways are matched by name. Gained / lost = supplies that become / stop being High in that pathway.")),
      card(full_screen = TRUE, card_header(textOutput("movers_title", inline = TRUE)),
           DTOutput("movers"))
    )
  ),
  card(
    card_header("How big is this change? Decision sensitivity"),
    div(class = "note",
        "Reference 1: randomly perturb every indicator weight and every exposure / sensitivity weight of the DEFAULT pathways by up to the chosen spread and compare with the unperturbed default. That shows how much rankings move under weighting choices that are arbitrary. A custom configuration whose agreement with the default falls inside that band is not a meaningful change. ",
        "Reference 2: drop each indicator of the CUSTOM pathways in turn."),
    layout_columns(col_widths = c(3, 3, 3, 3),
      numericInput("n_draws", "Random draws", 100, min = 20, max = 1000, step = 20),
      sliderInput("spread", "Weight spread (+/-)", min = 0.1, max = 0.9, value = 0.5, step = 0.1),
      div(style = "padding-top:30px;", actionButton("run_sens", "Run checks", class = "btn-primary")),
      div()
    ),
    layout_columns(col_widths = c(6, 6),
      plotlyOutput("perturb_plot", height = 320),
      DTOutput("drop_table"))
  )
)

tab_park <- nav_panel(
  "Park Lookup",
  uiOutput("ctx3"),
  selectizeInput("park", "Park", PARK_CHOICES, width = "500px"),
  uiOutput("park_summary"),
  card(full_screen = TRUE, card_header("Water supplies at this park"),
       DTOutput("park_table"),
       div(class = "note", "Percentiles are within the comparison group. High pathways are listed for the default and the custom pathways.")),
  card(full_screen = TRUE, card_header("Pathway percentiles by supply (default pathways)"),
       plotlyOutput("park_bars", height = 360))
)

tab_about <- nav_panel(
  "About",
  card(card_body(
    h4("What this compares"),
    p(strong("Overall Index (V1)."), " The current method: every indicator is normalized, combined into 14 factors, and the factors are combined into one Euclidean vulnerability score. Priority = top 25% of the comparison group. Computed by the same calc_vulnerability_index() the live app uses."),
    p(strong("Pathway Indices."), " Indicators are grouped into impact pathways. Within a pathway, exposure indicators and sensitivity indicators are each combined the way V1 builds a factor (Euclidean distance, re-normalized 0 to 1), and the two are combined into a pathway score. A supply is High in a pathway when its pathway score is in the top 25% (adjustable) of the supplies in the comparison group that have a score for that pathway. Supplies with no data for a pathway (for example inland supplies and Coastal Impacts) are not High in it. Supplies are ranked by how many pathways they are High in."),
    p(strong("Default pathways"), " come from the Impact Pathways table in the Phase 1 Revisions Notes: Water Supply, Water Demand, Drought, Wildfire, Coastal Impacts, with Historical Visitation Trend in Water Demand sensitivity as in V1. Not in any default pathway: Temperature (exposure and sensitivity), Current Flood Risk, and Depth to Water Table (listed in the notes as a weighting variable, with its use not yet defined). The presets add each of these back."),
    p(strong("Same building blocks."), " Both methods use V1's normalized indicators, model agreement weighting, and source type rules (rainwater uses precipitation, other types use runoff, ocean supplies score zero on water supply and demand). A parity test confirms the pathway engine reproduces every V1 factor and the V1 score exactly when given V1's own grouping, so differences here come from the grouping and categorization decisions, not from implementation."),
    h4("Reading the comparison"),
    tags$ul(
      tags$li("Spearman rho: rank correlation, 1 = identical ordering."),
      tags$li("Top-N overlap: share of the N most vulnerable units under one method that are also in the top N under the other. Ties at the cut are included, so a count-based ranking can put more than N units in its top set."),
      tags$li("Parks: each method's park score is its most vulnerable supply (or mean across supplies, or for pathways the number of pathways High at any supply)."),
      tags$li("Comparison group: Region and State rescore everything within the group, as in the V1 app. Groups under 20 supplies fall back to national scores, and the label says so.")
    ),
    h4("Open items"),
    tags$ul(
      tags$li("TODO: default pathway grouping pending NPS sign-off."),
      tags$li("TODO: how the Depth to Water Table weighting variable should act (multiplier on every pathway? groundwater systems only?)."),
      tags$li("TODO: absolute (raw-unit) thresholds as an alternative to the top 25% relative cut."),
      tags$li("TODO: alternative normalizations (percentile, log) for skewed indicators such as depth to water.")
    )
  ))
)

ui <- page_navbar(
  title = "WSVA Methods Explorer (prototype)",
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = COL_EXP),
  header = tags$head(tags$style(HTML(css))),
  sidebar = sidebar_ui,
  tab_overview, tab_builder, tab_park, tab_about
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Custom pathway state ------------------------------------------------------
  custom_tbl <- reactiveVal(default_pathway_table())
  cw_tbl     <- reactiveVal(data.frame(pathway = unique(default_pathway_table()$pathway),
                                       w_exp = 1, w_sen = 1, stringsAsFactors = FALSE))

  # Keep the exposure / sensitivity weights in step with the pathway names
  observeEvent(custom_tbl(), {
    p  <- unique(custom_tbl()$pathway)
    cw <- cw_tbl()
    cw <- cw[cw$pathway %in% p, , drop = FALSE]
    new <- setdiff(p, cw$pathway)
    if (length(new)) cw <- rbind(cw, data.frame(pathway = new, w_exp = 1, w_sen = 1))
    cw <- cw[match(p, cw$pathway), , drop = FALSE]
    if (!identical(cw, cw_tbl())) cw_tbl(cw)
    cur <- isolate(input$add_path)
    updateSelectizeInput(session, "add_path", choices = p,
                         selected = if (length(cur) && cur %in% p) cur else p[1],
                         server = FALSE)
  })

  observeEvent(input$load_preset, {
    custom_tbl(PRESETS[[input$preset]])
    cw_tbl(data.frame(pathway = character(0), w_exp = numeric(0), w_sen = numeric(0)))
  })
  observeEvent(input$reset, {
    custom_tbl(default_pathway_table())
    cw_tbl(data.frame(pathway = character(0), w_exp = numeric(0), w_sen = numeric(0)))
    updateSelectInput(session, "preset", selected = names(PRESETS)[1])
  })
  observeEvent(input$add_row, {
    req(input$add_ind, nzchar(input$add_path %||% ""))
    t <- rbind(custom_tbl(), data.frame(pathway = input$add_path, indicator = input$add_ind,
                                        weight = input$add_w %||% 1))
    custom_tbl(clean_pathway_table(t))
  })
  observeEvent(input$delete_rows, {
    s <- input$path_table_rows_selected
    req(length(s) > 0)
    custom_tbl(custom_tbl()[-s, , drop = FALSE])
  })
  observeEvent(input$path_table_cell_edit, {
    e <- input$path_table_cell_edit
    t <- custom_tbl()
    # Displayed columns: Pathway (0), Indicator (1), Component (2), Weight (3)
    if (e$col == 0) t$pathway[e$row] <- trimws(e$value)
    if (e$col == 3) t$weight[e$row] <- suppressWarnings(as.numeric(e$value))
    custom_tbl(clean_pathway_table(t))
  })
  observeEvent(input$cw_table_cell_edit, {
    e <- input$cw_table_cell_edit
    cw <- cw_tbl()
    v <- suppressWarnings(as.numeric(e$value)); if (is.na(v)) v <- 1
    if (e$col == 1) cw$w_exp[e$row] <- max(0, v)
    if (e$col == 2) cw$w_sen[e$row] <- max(0, v)
    cw_tbl(cw)
  })

  output$path_table <- renderDT({
    t <- custom_tbl()
    datatable(data.frame(Pathway = t$pathway, Indicator = indicator_label(t$indicator),
                         Component = indicator_component(t$indicator), Weight = t$weight),
              rownames = FALSE, selection = "multiple",
              editable = list(target = "cell", disable = list(columns = c(1, 2))),
              options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = "360px")) %>%
      formatStyle("Component", color = styleEqual(c("Exposure", "Sensitivity"), c(COL_EXP, COL_SEN)),
                  fontWeight = "bold")
  })
  output$cw_table <- renderDT({
    cw <- cw_tbl()
    datatable(data.frame(Pathway = cw$pathway, Exposure = cw$w_exp, Sensitivity = cw$w_sen),
              rownames = FALSE, selection = "none",
              editable = list(target = "cell", disable = list(columns = 0)),
              options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  output$unassigned <- renderUI({
    miss <- setdiff(INDICATOR_CATALOG$indicator, custom_tbl()$indicator)
    if (!length(miss)) return(NULL)
    div(class = "note", style = "margin-top:8px;",
        strong("Not in any custom pathway: "), paste(indicator_label(miss), collapse = "; "))
  })

  # Specs -----------------------------------------------------------------------
  rules <- reactive(list(
    factor_agg = input$factor_agg, pathway_agg = input$pathway_agg,
    high_pct = input$high_pct / 100, min_high = input$min_high,
    rank_rule = input$rank_rule))

  spec_default <- reactive({
    r <- rules()
    pathways_method(default_pathway_table(), factor_agg = r$factor_agg,
                    pathway_agg = r$pathway_agg, high_pct = r$high_pct,
                    min_high = r$min_high, rank_rule = r$rank_rule,
                    label = "Default pathways")
  })
  spec_custom_raw <- reactive({
    r <- rules()
    req(nrow(custom_tbl()) > 0)
    pathways_method(custom_tbl(), component_weights = cw_tbl(),
                    factor_agg = r$factor_agg, pathway_agg = r$pathway_agg,
                    high_pct = r$high_pct, min_high = r$min_high,
                    rank_rule = r$rank_rule, label = "Custom pathways")
  })
  spec_custom <- debounce(spec_custom_raw, 400)

  # Scoring in the comparison group --------------------------------------------
  group_value <- reactive(switch(input$group, Region = input$region, State = input$state, NULL))

  score <- function(spec) {
    run_method_grouped(combined_raw, spec, input$group, group_value(), base = NAT_BASE)
  }
  res_overall <- reactive(score(v1_method()))
  res_default <- reactive(score(spec_default()))
  res_custom  <- reactive(score(spec_custom()))

  glabel <- reactive(attr(res_overall(), "group_label"))
  unit_label <- reactive(if (input$unit == "supply") "water supplies" else "parks")
  park_agg <- reactive(if (input$unit == "park") input$park_agg else "max")

  ctx <- function() {
    renderUI(div(class = "ctx",
      strong("Comparison group: "), glabel(), HTML(" &nbsp;|&nbsp; "),
      strong("Unit: "), unit_label(), HTML(" &nbsp;|&nbsp; "),
      strong("Top-N: "), input$top_n, HTML(" &nbsp;|&nbsp; "),
      strong("High: "), paste0(">= ", input$high_pct, "th pct per pathway"), HTML(" &nbsp;|&nbsp; "),
      strong("Pathway Priority: "), paste0(">= ", input$min_high, " High")))
  }
  output$ctx1 <- ctx(); output$ctx2 <- ctx(); output$ctx3 <- ctx()

  cmp_od <- reactive(unit_compare(res_overall(), res_default(), input$unit, park_agg(), input$top_n))
  cmp_oc <- reactive(unit_compare(res_overall(), res_custom(), input$unit, park_agg(), input$top_n))
  cmp_dc <- reactive(unit_compare(res_default(), res_custom(), input$unit, park_agg(), input$top_n))

  # The Overall vs Pathways tab shows either pathway version; the Builder tab
  # always compares both, so it keeps using cmp_od() / cmp_oc() directly.
  tab1_custom <- reactive(identical(input$tab1_version, "custom"))
  res_tab1 <- reactive(if (tab1_custom()) res_custom() else res_default())
  cmp_tab1 <- reactive(if (tab1_custom()) cmp_oc() else cmp_od())
  tab1_name <- reactive(if (tab1_custom()) "Custom pathways" else "Default pathways")

  # Tab 1: Overall vs Pathways -------------------------------------------------
  output$boxes1 <- renderUI({
    cm <- cmp_tab1(); o <- res_overall(); d <- res_tab1()
    n_pri_o <- sum(o$m_priority); n_pri_d <- sum(d$m_priority)
    pri_flag <- mean(d$n_high[o$m_priority] >= 1)
    layout_columns(col_widths = c(3, 3, 3, 3),
      vbox(paste("Rank correlation,", unit_label()), fmt_rho(cm$rho), rho_words(cm$rho)),
      vbox(paste0("Top-", input$top_n, " overlap"), fmt_pct(cm$top$overlap),
           paste0(cm$top$shared, " shared (", cm$top$n_a, " overall, ", cm$top$n_b, " ", tolower(tab1_name()), " in top set)")),
      vbox("Priority supplies", paste0(n_pri_o, " vs ", n_pri_d),
           paste0("Overall top 25% vs High in >= ", input$min_high, " pathway(s), of ", nrow(o)),
           COL_HIGH),
      vbox("Overall Priority also High in a pathway", fmt_pct(pri_flag),
           paste0(sum(d$n_high == 0 & !o$m_priority), " supplies High in no pathway and not Priority")))
  })

  # National opens on the lower 48; a region or state fits its own supplies.
  map_base <- function() {
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas)
    if (input$group == "National") m <- setView(m, -98, 39, 3)
    m
  }
  output$map_a_title <- renderText(paste0("Overall Index: Priority ", unit_label(), " (", glabel(), ")"))
  output$map_b_title <- renderText(paste0(tab1_name(), ": sized by # High pathways (", glabel(), ")"))

  output$map_a <- renderLeaflet({
    j <- cmp_tab1()$joined %>% filter(!is.na(lng), !is.na(lat))
    j$pri <- dplyr::coalesce(j$pri_a, FALSE)
    j <- arrange(j, pri)
    lab <- if (input$unit == "supply") "Priority (top 25%)" else "Park with a Priority supply"
    map_base() %>%
      addCircleMarkers(data = j, lng = ~lng, lat = ~lat,
                       radius = ~ifelse(pri, 6, 3.5), stroke = FALSE,
                       fillColor = ~ifelse(pri, COL_HIGH, COL_NONE), fillOpacity = 0.8,
                       label = ~paste0(park_name, ": ", system, " | overall pct ", round(pct_a, 1))) %>%
      addLegend("bottomright", colors = c(COL_HIGH, COL_NONE), labels = c(lab, "Other"), opacity = 0.8)
  })
  output$map_b <- renderLeaflet({
    j <- cmp_tab1()$joined %>% filter(!is.na(lng), !is.na(lat)) %>% arrange(n_high)
    mx <- max(5, j$n_high, na.rm = TRUE)
    pal <- colorNumeric(c("#C9CED2", "#E3A07A", COL_HIGH), domain = c(0, mx))
    map_base() %>%
      addCircleMarkers(data = j, lng = ~lng, lat = ~lat,
                       radius = ~3 + 2.2 * n_high, stroke = FALSE,
                       fillColor = ~pal(n_high), fillOpacity = 0.8,
                       label = ~paste0(park_name, ": ", system, " | High in ", n_high,
                                       ifelse(nzchar(high_list), paste0(" (", high_list, ")"), ""))) %>%
      addLegend("bottomright", pal = pal, values = 0:mx, title = "# High pathways", opacity = 0.8)
  })

  output$xtab1 <- renderPlotly({
    j <- cmp_tab1()$joined
    count_heatmap(j$quart_a, j$n_high, "Overall quartile", "# High pathways",
                  paste0(tools::toTitleCase(unit_label()), ", ", tab1_name(), ", ", glabel()))
  })
  output$scatter1 <- renderPlotly({
    rank_scatter(cmp_tab1()$joined, "Overall", "Pathways",
                 paste0("rho = ", fmt_rho(cmp_tab1()$rho), ", ", tab1_name(), ", ", glabel()),
                 color_by = "n_high")
  })
  output$pathway_bars <- renderPlotly({
    o <- res_overall(); d <- res_tab1(); pw <- attr(d, "pathways")
    df <- do.call(rbind, lapply(seq_len(nrow(pw)), function(i) {
      h <- d[[paste0("path_", pw$key[i], "_high")]]
      data.frame(pathway = pw$name[i],
                 group = c("Also Overall Priority", "Not Overall Priority"),
                 n = c(sum(h & o$m_priority), sum(h & !o$m_priority)))
    }))
    df$pathway <- factor(df$pathway, levels = rev(pw$name))
    plot_ly(df, y = ~pathway, x = ~n, color = ~group, type = "bar", orientation = "h",
            colors = c(COL_HIGH, "#E8B4A8")) %>%
      layout(barmode = "stack", title = list(text = paste0("Water supplies, ", tab1_name(), ", ", glabel()), font = list(size = 13)),
             xaxis = list(title = "Supplies High in pathway"), yaxis = list(title = ""),
             legend = list(orientation = "h", y = -0.2))
  })

  output$top_title <- renderText(paste0("Top-", input$top_n, " ", unit_label(),
                                        " under the Overall Index or ", tolower(tab1_name()), " (", glabel(), ")"))
  output$top_table <- renderDT({
    j <- cmp_tab1()$joined %>% filter(top_a | top_b) %>%
      mutate(In_top = case_when(top_a & top_b ~ "Both", top_a ~ "Overall only", TRUE ~ "Pathways only")) %>%
      arrange(pmin(pos_a, pos_b))
    out <- data.frame(Park = j$park_name, Unit = j$system,
                      `Overall position` = round(j$pos_a, 1), `Pathways position` = round(j$pos_b, 1),
                      `# High` = j$n_high, `High pathways` = j$high_list, `In top-N` = j$In_top,
                      check.names = FALSE)
    datatable(out, rownames = FALSE, filter = "top",
              options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatStyle("In top-N", backgroundColor = styleEqual(
        c("Both", "Overall only", "Pathways only"), c("#E7EFE9", "#F3E3E3", "#ECE9F5")))
  })

  output$rank_note <- renderUI({
    how <- switch(input$rank_rule,
      count_then_mean = paste0(
        "Each supply's pathway ranking score is its number of High pathways plus a ",
        "tiebreaker: the mean of its pathway percentiles divided by 1,000 (so the count ",
        "always dominates). A pathway percentile is the supply's percentile rank on that ",
        "pathway's score among the supplies in the comparison group that have a score for it; ",
        "a pathway with no data is left out of the mean."),
      count = "Each supply's pathway ranking score is its number of High pathways. Ties are kept, so the percentiles fall in a few bands.",
      max_pct = "Each supply's pathway ranking score is its highest pathway percentile.",
      mean_pct = "Each supply's pathway ranking score is the mean of its pathway percentiles (pathways with no data left out).")
    unit <- if (input$unit == "park") paste0(
      " For parks, the park score is ",
      switch(input$park_agg, max = "the highest supply score at the park",
             mean = "the mean of its supplies' scores",
             any = "the number of pathways High at any of its supplies, plus the same tiebreaker"),
      ".") else ""
    div(class = "note", style = "margin-top:6px;",
        strong("Pathways percentile: "), how, unit,
        " The axis value is the percentile rank (0 to 100, ties averaged) of that score among all ",
        unit_label(), " in the comparison group. The Overall percentile is the same thing for the ",
        "V1 VULNERABILITY score.")
  })

  # Tab 2: Builder results ------------------------------------------------------
  output$boxes2 <- renderUI({
    a <- cmp_dc(); b <- cmp_oc(); c0 <- cmp_od()
    layout_columns(col_widths = c(4, 4, 4),
      vbox("Custom vs Default pathways", fmt_rho(a$rho),
           paste0("rho; top-", input$top_n, " overlap ", fmt_pct(a$top$overlap)), COL_SEN),
      vbox("Custom vs Overall Index", fmt_rho(b$rho),
           paste0("rho; top-", input$top_n, " overlap ", fmt_pct(b$top$overlap)), COL_EXP),
      vbox("Default vs Overall (reference)", fmt_rho(c0$rho),
           paste0("rho; top-", input$top_n, " overlap ", fmt_pct(c0$top$overlap))))
  })
  output$matrix_table <- renderTable({
    m <- c("Overall Index", "Default pathways", "Custom pathways")
    r <- matrix(1, 3, 3, dimnames = list(m, m)); o <- r
    r[1, 2] <- r[2, 1] <- cmp_od()$rho; o[1, 2] <- o[2, 1] <- cmp_od()$top$overlap
    r[1, 3] <- r[3, 1] <- cmp_oc()$rho; o[1, 3] <- o[3, 1] <- cmp_oc()$top$overlap
    r[2, 3] <- r[3, 2] <- cmp_dc()$rho; o[2, 3] <- o[3, 2] <- cmp_dc()$top$overlap
    out <- rbind(
      data.frame(Metric = paste("Spearman rho,", unit_label()), Method = m,
                 apply(r, 2, fmt_rho), check.names = FALSE),
      data.frame(Metric = paste0("Top-", input$top_n, " overlap"), Method = m,
                 apply(o, 2, fmt_pct), check.names = FALSE))
    out$Metric[duplicated(out$Metric)] <- ""
    out
  }, striped = TRUE, bordered = FALSE, caption = NULL)

  output$xtab2 <- renderPlotly({
    j <- cmp_dc()$joined
    j$n_high_a <- cmp_od()$joined$n_high[match(j$id, cmp_od()$joined$id)]
    count_heatmap(j$n_high_a, j$n_high, "# High, default", "# High, custom",
                  paste0(tools::toTitleCase(unit_label()), ", ", glabel()))
  })
  output$scatter2 <- renderPlotly({
    rank_scatter(cmp_dc()$joined, "Default", "Custom",
                 paste0("rho = ", fmt_rho(cmp_dc()$rho), ", ", glabel()), color_by = "n_high")
  })
  output$pathway_changes <- renderDT({
    x <- compare_pathway_flags(res_default(), res_custom())
    out <- data.frame(Pathway = x$pathway,
                      Status = ifelse(x$in_a & x$in_b, "In both",
                               ifelse(x$in_a, "Default only", "Custom only")),
                      `High, default` = x$high_a, `High, custom` = x$high_b,
                      Gained = x$gained, Lost = x$lost,
                      `Score rho` = round(x$rho, 3), check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(dom = "t", paging = FALSE),
              caption = paste("Comparison group:", glabel()))
  })
  output$movers_title <- renderText(paste0("Biggest rank changes, default to custom (", unit_label(), ", ", glabel(), ")"))
  output$movers <- renderDT({
    j <- cmp_dc()$joined
    j$high_default <- cmp_od()$joined$high_list[match(j$id, cmp_od()$joined$id)]
    j <- j[order(-abs(j$shift)), ][seq_len(min(200, nrow(j))), ]
    out <- data.frame(Park = j$park_name, Unit = j$system,
                      `Default position` = round(j$pos_a, 1), `Custom position` = round(j$pos_b, 1),
                      `Positions moved up` = round(j$shift, 1),
                      `High, default` = j$high_default, `High, custom` = j$high_list,
                      check.names = FALSE)
    datatable(out, rownames = FALSE, filter = "top", options = list(pageLength = 8, scrollX = TRUE))
  })

  # Decision sensitivity ----------------------------------------------------------
  sens <- eventReactive(input$run_sens, {
    g <- input$group; v <- group_value()
    dat <- if (g == "National" || is.null(v)) combined_raw else
      combined_raw[combined_raw[[if (g == "Region") "region" else "state"]] %in% v, ]
    if (nrow(dat) < 20) dat <- combined_raw
    base <- calc_vulnerability_index(dat)
    withProgress(message = "Random weight draws", value = 0, {
      pert <- perturb_weights(dat, spec_default(), base, n_draws = input$n_draws,
                              spread = input$spread, top_n = input$top_n,
                              progress = function(p) setProgress(p))
    })
    drop <- drop_one_indicator(dat, spec_custom(), base, top_n = input$top_n)
    list(pert = pert, drop = drop, label = if (nrow(dat) == nrow(combined_raw)) "National" else glabel())
  })
  output$perturb_plot <- renderPlotly({
    if (input$run_sens == 0) return(empty_plot("Click 'Run checks'"))
    s <- sens()
    obs <- compare_methods(res_default(), res_custom(), "supply", input$top_n)$rho
    q <- stats::quantile(s$pert$rho, c(0.05, 0.5))
    plot_ly(s$pert, x = ~rho, type = "histogram", nbinsx = 30,
            marker = list(color = "#C9CED2"), name = "Random weights") %>%
      add_segments(x = obs, xend = obs, y = 0, yend = max(5, nrow(s$pert) / 6),
                   line = list(color = COL_HIGH, width = 3), name = "Custom vs default") %>%
      layout(title = list(text = paste0("Supplies, ", s$label, ": median rho ", fmt_rho(q[2]),
                                        ", 5th pct ", fmt_rho(q[1]), "; custom ", fmt_rho(obs)),
                          font = list(size = 12)),
             xaxis = list(title = "Spearman rho vs default pathways"), yaxis = list(title = "Draws"),
             legend = list(orientation = "h", y = -0.25))
  })
  output$drop_table <- renderDT({
    if (input$run_sens == 0) return(datatable(data.frame(Note = "Click 'Run checks'"), rownames = FALSE, options = list(dom = "t")))
    d <- sens()$drop
    out <- data.frame(`Dropped indicator` = d$label, Pathways = d$pathways,
                      rho = round(d$rho, 3), `Top-N overlap` = fmt_pct(d$top_overlap),
                      `Priority flips` = d$priority_flips, check.names = FALSE)
    datatable(out[order(out$rho), ], rownames = FALSE,
              caption = paste0("Custom pathways, each indicator dropped in turn (supplies, ", sens()$label, ")"),
              options = list(dom = "tp", pageLength = 8))
  })

  # Tab 3: Park lookup ----------------------------------------------------------
  park_rows <- reactive({
    req(input$park)
    o <- res_overall(); d <- res_default(); cu <- res_custom()
    keep <- which(o$park_unit == input$park)
    validate(need(length(keep) > 0, "This park has no supplies in the current comparison group."))
    oo <- o[keep, ]
    dd <- d[match(oo$wsd_source_id, d$wsd_source_id), ]
    cc <- cu[match(oo$wsd_source_id, cu$wsd_source_id), ]
    list(o = oo, d = dd, c = cc, pw = attr(d, "pathways"), pwc = attr(cu, "pathways"))
  })
  output$park_summary <- renderUI({
    pr <- park_rows()
    pos <- function(r) {
      ps <- park_summary(r, park_agg())
      p <- position(ps$park_score)[ps$park_unit == input$park]
      paste0(round(p), " of ", nrow(ps))
    }
    layout_columns(col_widths = c(4, 4, 4),
      vbox("Overall Index park position", pos(res_overall()),
           paste(sum(pr$o$m_priority), "of", nrow(pr$o), "supplies Priority"), COL_HIGH),
      vbox("Default pathways park position", pos(res_default()),
           paste("High pathways at any supply:", length(unique(unlist(strsplit(high_names(pr$d, pr$pw), ", ")))))),
      vbox("Custom pathways park position", pos(res_custom()),
           paste("High pathways at any supply:", length(unique(unlist(strsplit(high_names(pr$c, pr$pwc), ", ")))))))
  })
  output$park_table <- renderDT({
    pr <- park_rows()
    out <- data.frame(System = pr$o$water_system_name, `Source type` = pr$o$source_type,
                      `Overall pct` = round(pr$o$m_rank, 1),
                      `Overall Priority` = ifelse(pr$o$m_priority, "Yes", ""),
                      `# High, default` = pr$d$n_high, `High, default` = high_names(pr$d, pr$pw),
                      `# High, custom` = pr$c$n_high, `High, custom` = high_names(pr$c, pr$pwc),
                      check.names = FALSE)
    datatable(out, rownames = FALSE, caption = paste("Comparison group:", glabel()),
              options = list(dom = "tp", pageLength = 15, scrollX = TRUE)) %>%
      formatStyle("Overall Priority", color = COL_HIGH, fontWeight = "bold")
  })
  output$park_bars <- renderPlotly({
    pr <- park_rows(); pw <- pr$pw
    df <- do.call(rbind, lapply(seq_len(nrow(pw)), function(i) {
      data.frame(system = pr$d$water_system_name, pathway = pw$name[i],
                 pct = pr$d[[paste0("path_", pw$key[i], "_pct")]],
                 high = pr$d[[paste0("path_", pw$key[i], "_high")]])
    }))
    df$system <- make.unique(as.character(df$system))
    plot_ly(df, x = ~pathway, y = ~pct, color = ~system, type = "bar",
            text = ~ifelse(high, "High", ""), textposition = "outside") %>%
      add_segments(x = pw$name[1], xend = pw$name[nrow(pw)], y = input$high_pct, yend = input$high_pct,
                   inherit = FALSE, line = list(color = COL_HIGH, dash = "dash"), name = "High cut") %>%
      layout(barmode = "group", yaxis = list(title = "Pathway percentile", range = c(0, 110)),
             xaxis = list(title = ""), title = list(text = glabel(), font = list(size = 13)),
             legend = list(orientation = "h", y = -0.2))
  })
}

shinyApp(ui, server)
