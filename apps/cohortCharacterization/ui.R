ui <- fluidPage(
  shinyjs::useShinyjs(), shiny::tags$head(custom_styling()),
  bslib::page_navbar(
    title = "Cohort Characterization Analysis", theme = bslib::bs_theme(`navbar-bg` = "#005480", fg = "black", bg = "white"),
    nav_panel(
      "Introduction", card(
        make_intro_page(
          PROPERTIES,
          markdown_file = NULL, app_type = "cohort_characterization", datasource_summary_file,
          add_referenced_cohorts = TRUE, add_cohort_summary = FALSE
        )
      )
    ),
    nav_panel(
      "Analysis", card(
        bslib::layout_sidebar(
          sidebar = sidebar(
            width = "400px", 
            shinyjs::disabled(
              pickerInput(
                "cohort",
                "Cohort name",
                c(
                  `Target cohort` = "targetCohort",
                  `Compare target vs comparator cohort` = "comparatorCohort"
                ),
                selected = "Target cohort",
                options = pickerOptions(actionsBox = TRUE)
              )
            ),
            uiOutput("mainSelector")
          ),
          card(
            min_height = "400px", p(
              shiny::tags$h2(analysis_name),
              style = "margin-bottom: 15px; font-size: 1.5em"
            ),
            uiOutput("tables"),
            tags$style(
              type = "text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }"
            )
          )
        )
      )
    ),
    nav_spacer(), 
    nav_menu(title = "Links",
             align = "right",
             nav_item(a(
               "Git Repository", href = repo_link, target = "_blank", 
               `data-toggle` = "tooltip", title = "Access the source code repository used for this dashboard."
             )),
             nav_item(a(
               analysis_name, href = atlas_link, target = "_blank", 
               `data-toggle` = "tooltip", title = "Access the Atlas asset designed for the creation of this dashboard."
             )))
  )
)
