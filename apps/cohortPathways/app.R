library(shiny)
library(dplyr)
library(bslib)
library(logger)
library(sunburstShinyWidget)
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
log_level(INFO)
log_info("reading input data")
data_dir <- "data"
if (file.exists("helpers.R")) {
  source("helpers.R")
} else {
  source("../cohortCounts/helpers.R")
}
not_found <- shiny::tags$span(style = "color: orange;", "Property not found.")
PROPERTIES <- properties::read.properties(file.path(data_dir, "app.properties"))
repo_link <- PROPERTIES$repo_link %||% not_found
atlas_link <- PROPERTIES$atlas_link %||% not_found
datasource <- PROPERTIES$datasource %||% not_found
datasource_name <- PROPERTIES$datasource_name %||% not_found
asset_name <- PROPERTIES$asset_name %||% not_found
chartData <- jsonlite::read_json(file.path(data_dir, "chartData.json"))
design <- jsonlite::read_json(file.path(data_dir, "design.json"))
eventCodes <- chartData$eventCodes %>%
  dplyr::bind_rows()
datasource_summary_file <- file.path(data_dir, "datasource_summary.json")
log_info("initializing Shiny UI")
ui <- fluidPage(
  shiny.info::version(
    as.character(packageVersion("cohortPathways")),
    position = "bottom right"
  ),
  shiny::tags$head(custom_styling()),
  bslib::page_navbar(
    theme = bslib::bs_theme(`navbar-bg` = "#005480", fg = "black", bg = "white"),
    underline = TRUE, title = "Cohort Pathways", id = "nav", nav_panel(
      "Introduction", card(
        make_intro_page(
          PROPERTIES,
          markdown_file = NULL, app_type = "cohort_pathway", datasource_summary_file,
          add_referenced_cohorts = TRUE, add_cohort_summary = FALSE
        )
      )
    ),
    nav_panel("Analysis", sunburstShinyWidget::sunburstUI("sunburst_plot")),
    nav_spacer(), nav_menu(
      title = "Links", align = "right", nav_item(a("Git Repository", href = repo_link, target = "_blank")),
      nav_item(a(asset_name, href = atlas_link, target = "_blank"))
    ),
  )
)
log_info("initializing Shiny Server")
server <- function(input, output) {
  sunburstServer("sunburst_plot", chartData, design)
}
log_info("running Shiny app")
shinyApp(ui = ui, server = server)
