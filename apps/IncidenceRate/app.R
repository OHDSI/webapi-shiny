library(shiny)
library(dplyr)
library(echarts4r)
library(reactable)
library(htmltools)
library(tippy)
library(bslib)
source("read_data.R")
if (file.exists("helpers.R")) {
  source("helpers.R")
} else {
  source("../cohortCounts/helpers.R")
}
data_dir <- "data"
app_data <- read_data(path = data_dir)
PROPERTIES <- properties::read.properties(file.path(data_dir, "app.properties"))
repo_link <- PROPERTIES$repo_link
atlas_link <- PROPERTIES$atlas_link
incidence_rate_name <- PROPERTIES$analysis_name
datasource <- PROPERTIES$datasource
api_url <- "http://api.ohdsi.org:8080/WebAPI/"
cohorts <- readr::read_csv(file.path(data_dir, "cohorts.csv"))
datasource_summary_file <- file.path(data_dir, "datasource_summary.json")
target_cohorts <- filter(cohorts, type == "target") %>%
  {
    setNames(
      pull(., "cohort_id"),
      pull(., "cohort_name")
    )
  }
outcome_cohorts <- filter(cohorts, type == "outcome") %>%
  {
    setNames(
      pull(., "cohort_id"),
      pull(., "cohort_name")
    )
  }
stopifnot(
  length(target_cohorts) >=
    1, length(outcome_cohorts) >=
    1
)
data_sources <- list.files("data", pattern = ".json$") %>%
  stringr::str_remove("_targetId.*$") %>%
  stringr::str_subset("^cohorts.$", negate = T) %>%
  stringr::str_unique()
ui <- fluidPage(
  shinyjs::useShinyjs(), shiny::tags$head(custom_styling()),
  bslib::page_navbar(
    theme = bslib::bs_theme(`navbar-bg` = "#005480", fg = "black", bg = "white"),
    title = "Incidence Rate Analysis", 
    id = "nav", 
    
    nav_panel(
      "Introduction", card(
        make_intro_page(
          PROPERTIES,
          markdown_file = NULL, app_type = "incidence_rate", datasource_summary_file,
          add_referenced_cohorts = TRUE, add_cohort_summary = FALSE
        )
      )
    ),
    nav_panel(
      "Analysis", card(
        tags$h5("Summary Statistics for the Cohort"),
        tags$span(
          bslib::tooltip(
            shiny::icon("circle-exclamation"),
            HTML("Dashboard, design and visualisation may differ from those in ATLAS")
          )
        ),
        reactableOutput("summary_table"),
        tags$br(), tags$h5("Summary Statistics for Strata within the Cohort"),
        reactableOutput("subgroup_table"),
        htmlOutput("selected_subset_text"),
        tags$span(
          bslib::tooltip(
            shiny::icon("circle-info"),
            HTML("Table legend:<br>1 criteria passed<br>0 criteria failed")
          ),
          echarts4rOutput("treemap")
        )
      ),
      
      
    ),
    nav_spacer(), nav_menu(
      title = "Links", align = "right", nav_item(a("Git Repository", href = repo_link, target = "_blank")),
      nav_item(a(incidence_rate_name, href = atlas_link, target = "_blank"))
    )
  )
)
server <- function(input, output) {
    x <- app_data$treemap_table %>%
      filter(
        data_source == datasource, target == input$target_id, outcome == input$outcome_id,
        subset_ids == input$box_click$name
      )
    if (nrow(x) !=
      1) {
      stop("Error with filtering. Only one subgroup should be selected!")    }
    glue::glue(
      "{x$cases} Cases, {x$time_at_risk} TAR, Rate: {round(x$rate_per_1k_years, 2)} <br> {x$total_persons} (%) people, {n_critera_passed} criteria passed, {n_critera_failed} criteria failed."
    )
  })
  output$summary_table <- renderReactable({
    app_data$summary_table %>%
      filter(data_source == datasource, target == input$target_id, outcome == input$outcome_id) %>%
      mutate(
        proportion_per_1k_persons = round(proportion_per_1k_persons, 2),
        rate_per_1k_years = round(rate_per_1k_years, 2)
      ) %>%
      select(
        Persons = total_persons, Cases = cases, `Proportion \n(per 1k person-years)` = proportion_per_1k_persons,
        `Time at risk \n(years)` = time_at_risk, `Rate \n(per 1k person-years)` = rate_per_1k_years
      ) %>%
      reactable(
        columns = list(
          Persons = colDef(header = tippy("Persons", "Total number of persons in the cohort.", placement = "right")),
          Cases = colDef(
            header = tippy("Cases", "Total number of persons that arrive to one the end point.", placement = "right")
          ),
          `Proportion \n(per 1k person-years)` = colDef(
            header = tippy(
              "Proportion \n(per 1k person-years)", "Thousands of cases in a year divided by persons.",
              placement = "right"
            )
          ),
          `Time at risk \n(years)` = colDef(
            header = tippy(
              "Time at risk \n(years)", "Estimated anount of time a person is at risk to arriving to the end point.",
              placement = "right"
            )
          ),
          `Rate \n(per 1k person-years)` = colDef(
            header = tippy(
              "Rate \n(per 1k person-years)", "Thousands of cases in a year divided by time at risk.",
              placement = "right"
            )
          )
        )
      )
  })
  selected_subgroup_ids <- reactive({
    if (!isTruthy(input$box_click$name) ||
      input$box_click$name == "None") {
      return(seq_len(length(unique(app_data$subgroup_table$subgroup_id))))
    }
    as.numeric(stringr::str_split(input$box_click$name, ",")[[1]])
  })
  output$subgroup_table <- renderReactable({
    style_function <- function(value, index) {
      if (index %in% selected_subgroup_ids()) {
        list(color = "red")
      } else {
        list(color = "black")
      }
    }
    app_data$subgroup_table %>%
      filter(data_source == datasource, target == input$target_id, outcome == input$outcome_id) %>%
      mutate(
        proportion_per_1k_persons = round(proportion_per_1k_persons, 2),
        rate_per_1k_years = round(rate_per_1k_years, 2)
      ) %>%
      select(
        `Stratify rule` = subgroup_name, Persons = total_persons, Cases = cases, `Proportion (per 1k person-years)` = proportion_per_1k_persons,
        `Time at risk (years)` = time_at_risk, `Rate (per 1k person-years)` = rate_per_1k_years
      ) %>%
      reactable(
        columns = list(
          `Stratify rule` = colDef(
            header = tippy("Stratify rule", "Groups defined inside the cohort.", placement = "right"),
            style = style_function
          ),
          Persons = colDef(
            header = tippy("Persons", "Total number of persons in the cohort.", placement = "right"),
            style = style_function
          ),
          Cases = colDef(
            header = tippy("Cases", "Total number of persons that arrive to one the end point.", placement = "right"),
            style = style_function
          ),
          `Proportion (per 1k person-years)` = colDef(
            header = tippy(
              "Proportion \n(per 1k person-years)", "Thousands of cases in a year divided by persons.",
              placement = "right"
            ),
            style = style_function
          ),
          `Time at risk (years)` = colDef(
            header = tippy(
              "Time at risk \n(years)", "Estimated anount of time a person is at risk to arriving to the end point.",
              placement = "right"
            ),
            style = style_function
          ),
          `Rate (per 1k person-years)` = colDef(
            header = tippy(
              "Rate \n(per 1k person-years)", "Thousands of cases in a year divided by time at risk.",
              placement = "right"
            ),
            style = style_function
          )
        ),
        sortable = FALSE
      )
  })
  treemap_table <- reactive({
    app_data$treemap_table %>%
      mutate(
        ids = stringr::str_replace(name, "None", "0") %>%
          stringr::str_split(",") %>%
          lapply(as.integer)
      ) %>%
      filter(data_source == datasource, target == input$target_id, outcome == input$outcome_id) %>%
      select(-data_source, -target_id, -outcome_id)
  })
  output$treemap <- renderEcharts4r({
    shinyjs::runjs("Shiny.setInputValue('box_click', {name: false})")
    app_data$treemap_table %>%
      filter(data_source == datasource, target == input$target_id, outcome == input$outcome_id) %>%
      select(name = subset_ids, value = cases) %>%
      e_charts() %>%
      e_treemap(roam = F) %>%
      e_toolbox_feature(feature = "saveAsImage") %>%
      e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}") %>%
      e_visual_map(value, inRange = list(color = c("#1f88a6", "#2fcefb", "#0f4251")))
  })
}
shinyApp(ui = ui, server = server)
