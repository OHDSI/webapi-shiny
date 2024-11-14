library(shiny)
library(dplyr)
library(echarts4r)
library(reactable)
library(bslib)
library(tippy)
library(ROhdsiWebApi)
library(logger)
library(glue)
library(markdown)
log_layout(layout_glue_colors)
log_level(INFO)
log_info("Reading the data")
source("read_data.R")
source("helpers.R")
log_info("Reading properties")
data_dir <- "data"
PROPERTIES <- properties::read.properties(file.path(data_dir, "app.properties"))
repo_link <- PROPERTIES$repo_link
cohort_name <- PROPERTIES$cohort_name
cohort_link <- PROPERTIES$cohort_link
markdown_file <- file.path(data_dir, "cohort_summary_markdown.txt")
datasource_summary_file <- file.path(data_dir, "datasource_summary.json")
if (!file.exists(markdown_file)) stop(glue("'{markdown_file}' does not exist"))
if (!file.exists(datasource_summary_file)) stop(glue("'{datasource_summary_file}' does not exist"))
app_data <- read_data(path = data_dir)
data_sources <- list.files("data", pattern = ".json$") %>%
  stringr::str_remove("_by_(person|event).json$") %>%
  stringr::str_unique()
ui <- fluidPage(
  shinyjs::useShinyjs(), shiny::tags$head(custom_styling()),
  bslib::page_navbar(
    theme = bslib::bs_theme(`navbar-bg` = "#005480", fg = "black", bg = "white"),
    underline = TRUE, title = "Cohort Inclusion Report", 
    id = "nav", 
    nav_panel(
      "Introduction", card(
        make_intro_page(
          PROPERTIES = PROPERTIES, markdown_file = markdown_file, app_type = "cohort_counts",
          datasource_summary_file = datasource_summary_file, add_cohort_summary = TRUE
        )
      )
    ),
    nav_panel(
      "Analysis", 
      layout_sidebar(
        sidebar = sidebar(
          conditionalPanel("input.nav === 'Introduction'", "Application's and cohort descriptions."),
          conditionalPanel(
            "input.nav === 'Analysis'", shinyWidgets::radioGroupButtons(
              inputId = "level", label = "Cohort unit", selected = "person", individual = TRUE, choiceNames = c("Person", "Event"),
              choiceValues = c("person", "event"),
              size = "sm", width = "100%"
            ),
            shinyWidgets::radioGroupButtons(
              inputId = "switch_view", label = "Cohort unit", selected = "Intersect", individual = TRUE,
              choiceNames = c("Intersect", "Attrition"),
              choiceValues = c("Intersect", "Attrition"),
              size = "sm", width = "100%"
          )
          ),
          tags$div(id = "filter_text_filler", HTML("<br><br><br>"))
        )
        ),
        card(
          fluidRow(column(width = 12, textOutput("upper_summary_text"))),
          fluidRow(
            column(
              width = 12, tags$div(
                id = "filter_text", "Having", tags$div(
                  style = "display:inline-block", selectInput(
                    "any_all", "", c("any", "all"),
                    selectize = F, width = "80px"
                  )
                ),
                "of selected criteria", tags$div(
                  style = "display:inline-block", selectInput(
                    "passed_failed", "", c("passed", "failed"),
                    selectize = F, width = "100px"
                  )
                )
              ),
              tags$div(id = "filter_text_filler", HTML("<br><br><br>"))
            )
          ),
          fluidRow(
            column(
              width = 8, span(
                bslib::tooltip(
                  shiny::icon("circle-info"),
                  HTML(
                    paste(
                      "Table legend:", "PASSED inclusion rules are highlited in red.", "FAILED inclusion rules are highlited in green.",
                      sep = "<br>"
                    )
                  )
                ),
                reactableOutput("inclusion_table")
              ),
              tags$br(), textOutput("lower_summary_text")
            )
          ),
          fluidRow(
            column(
              width = 12, tags$br(), tags$br(), htmlOutput("count_in_selected_subset_text"),
              echarts4rOutput("plot")
            )
          )        card(
          fluidRow(column(width = 12, textOutput("upper_summary_text"))),
          fluidRow(
            column(
              width = 12, tags$div(
                id = "filter_text", "Having", tags$div(
                  style = "display:inline-block", selectInput(
                    "any_all", "", c("any", "all"),
                    selectize = F, width = "80px"
                  )
                ),
                "of selected criteria", tags$div(
                  style = "display:inline-block", selectInput(
                    "passed_failed", "", c("passed", "failed"),
                    selectize = F, width = "100px"
                  )
                )
              ),
              tags$div(id = "filter_text_filler", HTML("<br><br><br>"))
            )
          ),
          fluidRow(
            column(
              width = 8, span(
                bslib::tooltip(
                  shiny::icon("circle-info"),
                  HTML(
                    paste(
                      "Table legend:", "PASSED inclusion rules are highlited in red.", "FAILED inclusion rules are highlited in green.",
                      sep = "<br>"
                    )
                  )
                ),
                reactableOutput("inclusion_table")
              ),
              tags$br(), textOutput("lower_summary_text")
            )
          ),
          fluidRow(
            column(
              width = 12, tags$br(), tags$br(), htmlOutput("count_in_selected_subset_text"),
              echarts4rOutput("plot")
            )
          )
        )
        )
      )
      
    ),
    nav_spacer(), nav_menu(
      title = "Links", align = "right", nav_item(a("Git Repository", href = repo_link, target = "_blank")),
      nav_item(a(cohort_name, href = cohort_link, target = "_blank"))
    ),
  )
)
server <- function(input, output) {
  shinyjs::hide("filter_text_filler")
  output$description <- renderText(ROhdsiWebApi::getCohortDefinition(cohortId = 101431, "http://api.ohdsi.org:8080/WebAPI/")[[3]])
  rows_to_highlight <- reactive(
    {
      if (!isTruthy(input$box_click$name) ||
        input$box_click$name == "None")
        return(FALSE)
      as.numeric(stringr::str_split(input$box_click$name, ",")[[1]])
    }
  )
  attritionView <- reactiveVal(1)
  observeEvent(
    input$switch_view, {
      if (attritionView() == 0) {
        updateActionLink(inputId = "switch_view", label = "View type")
        attritionView(1)
        shinyjs::hide("filter_text")
        shinyjs::show("filter_text_filler")
      }
      else {
        updateActionLink(inputId = "switch_view", label = "View type")
        attritionView(0)
        shinyjs::hide("filter_text_filler")
        shinyjs::show("filter_text")
      }
    }
  )
  observe(
    {
      if (!isTruthy(input$box_click$name) ||
        attritionView() == 1)
        return(HTML("<br>"))
      req(input$box_click$name)
      req(input$level)
      x <- app_data[[PROPERTIES$datasource]][[input$level]]$treemap_table %>%
        mutate(
          total = sum(value),
          percent = round(100 * value/total, 2)
        ) %>%
        filter(name == input$box_click$name) %>%
        mutate(
          value = format(value, big.mark = ",", scientific = FALSE),
          percent = paste0(percent, "%")
        )
      output$count_in_selected_subset_text <- renderPrint(
        {
          if (input$box_click$name == "None") {
          glue::glue("Number of {input$level}s not matching any inclusion rules: {x$value} ({x$percent})")
          }
          else {
          glue::glue(
            "Number of {input$level}s matching inclusion rules [{input$box_click$name}]: {x$value} ({x$percent})"
          )
          }
        }
      )
    }
  )
  output$inclusion_table <- renderReactable(
    {
      result <- tryCatch(
        {
          if (attritionView() == 0) {
          style_function <- function(value, index) {
            if (index %in% rows_to_highlight())
            list(color = "red")
            else list(color = "black")
          }
          log_info(paste("nrow"))
          log_info("---")
          log_info(paste("inc_table"))
          inc_table <- app_data[[1]][[input$level]]$inclusion_table
          log_info("inc_table: ", toString(inc_table))
          defaultSelected <- seq_len(nrow(inc_table))
          log_info(paste("defaultSelected: ", toString(defaultSelected)))
          log_info(paste("Value of defaultSelected: ", toString(defaultSelected)))
          inc_table %>%
            dplyr::mutate(
            percent_num = as.numeric(gsub("%", "", Percent))/100,
            total = Count/percent_num, to_gain = (total - Count)/total * 100
          ) %>%
            dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(., 0))) %>%
            dplyr::mutate(to_gain = sprintf("%.2f%%", to_gain)) %>%
            dplyr::mutate(Count = scales::comma(as.numeric(Count))) %>%
            dplyr::rename(N = Count, `% Satisfied` = Percent, `% To-Gain` = to_gain) %>%
            dplyr::select(-total, -percent_num) %>%
            reactable(
            selection = "multiple", onClick = "select", defaultSelected = defaultSelected,
            bordered = TRUE, columns = list(
              `Inclusion Rule` = colDef(
              style = style_function, header = tippy("Inclusion Rule", "Criteria to be included in the cohort.", placement = "right"),
              width = 500
            ),
              ID = colDef(style = style_function, align = "left", maxWidth = 40),
              N = colDef(
              style = style_function, header = tippy("N", "Number of persons that fullfill the criteria.", placement = "right")
            ),
              `% Satisfied` = colDef(
              style = style_function, header = tippy("% Satisfied", "Percent of the cohort that fullfill the criteria.", placement = "right")
            ),
              `% To-Gain` = colDef(
              style = style_function, header = tippy(
                "% To-Gain", "Percent of the cohort that doesn't fullfill the criteria.",
                placement = "right"
              )
            )
            )
          )
          }
          else if (attritionView() == 1) {
          app_data[[1]][[input$level]]$attrition_table %>%
            mutate(
              Count = scales::comma(as.numeric(Count)),
              pct_remain = round(pct_remain, 4),
              pct_diff = round(pct_diff, 4)
            ) %>%            reactable(
            sortable = FALSE, bordered = TRUE, columns = list(
              ID = colDef(name = "ID", maxWidth = 40),
              `Inclusion Rule` = colDef(
              name = "Inclusion Rule", header = tippy("Inclusion Rule", "Criteria to be included in the cohort.", placement = "right"),
              width = 500
            ),
              Count = colDef(
              name = "Count", header = tippy("Count", "Number of persons that fullfill the criteria.", placement = "right")
            ),
              pct_remain = colDef(
              name = "Percent remaining", format = colFormat(percent = TRUE),
              header = tippy(
                "Percent remaining", "Percentage of persons remaining in the cohort after fullfilling the criteria.",
                placement = "right"
              )
            ),
              pct_diff = colDef(
              name = "Percent difference", format = colFormat(percent = TRUE),
              header = tippy(
                "Percent difference", "Differebce in the percentage remaining after fullfilling the criteria.",
                placement = "right"
              )
            )
            )
          )
          }
          else {
          stop("There is a problem. attritionView should either be 1 or 0.")
          }
        }, error = function(e) {
          log_info("No data found")
          log_info(e$message)
        }
      )
    }
  )
  selected_rows <- reactive(getReactableState("inclusion_table", "selected"))
  log_info("selected_rows:")
  treemap_table <- reactive(
    {
      app_data[[1]][[input$level]]$treemap_table %>%
        mutate(
          ids = stringr::str_replace(name, "None", "0") %>%
          stringr::str_split(",") %>%
          lapply(as.integer)
        ) %>%
        mutate(
          include_in_summary = case_when(
          input$any_all == "any" && input$passed_failed == "passed" ~ purrr::map_lgl(.data$ids, ~any(. %in% selected_rows())),
          input$any_all == "all" && input$passed_failed == "passed" ~ purrr::map_lgl(.data$ids, ~all(. %in% selected_rows())),
          input$any_all == "any" && input$passed_failed == "failed" ~ purrr::map_lgl(.data$ids, ~any(!(selected_rows() %in% .))),
          input$any_all == "all" && input$passed_failed == "failed" ~ purrr::map_lgl(.data$ids, ~all(!(selected_rows() %in% .))),
          TRUE ~ TRUE
        )
        )
    }
  )
  log_info("treemap_table:")
  output$plot <- renderEcharts4r(
    {
      if (attritionView() == 0) {
        shinyjs::runjs("Shiny.setInputValue('box_click', {name: false})")
        app_data[[1]][[input$level]]$treemap_table %>%
          e_charts() %>%
          e_treemap() %>%
          e_toolbox_feature(feature = "saveAsImage") %>%
          e_on(query = ".", handler = "function(params) {Shiny.setInputValue('box_click', {name: params.name});}") %>%
          e_visual_map(value, inRange = list(color = c("#1f88a6", "#2fcefb", "#0f4251")))
      }
      else if (attritionView() == 1) {
        df <- tibble(app_data[[1]][[input$level]]$attrition_table)
        df <- select(df, ID, pct_diff) %>%
          mutate(pct_diff = round(pct_diff * 100, 2)) %>%
          rename(`Percent diffecerence` = pct_diff) %>%
          e_charts(ID) %>%
          e_bar(`Percent diffecerence`, itemStyle = list(color = "#0f4251")) %>%
          e_labels() %>%
          e_hide_grid_lines()
      }
      else {
        stop("There is a problem. attritionView should only be 0 or 1.")
      }
    }
  )
  output$upper_summary_text <- renderText(
    {
      s <- app_data[[1]][[input$level]]$summary_table
      glue::glue("Initial Count: {format(s$initial_index_events, big.mark=',', scientific=FALSE)}")
    }
  )
  output$lower_summary_text <- renderText(
    {
      denominator <- sum(treemap_table()$value)
      numerator <- treemap_table() %>%
        filter(include_in_summary) %>%
        pull(value) %>%
        sum()
      percent_included <- (scales::label_percent())(numerator/denominator)
      glue::glue("Final Count: {format(numerator, big.mark=',', scientific = FALSE)} ({percent_included})")
    }
  )
}
shinyApp(ui = ui, server = server)
