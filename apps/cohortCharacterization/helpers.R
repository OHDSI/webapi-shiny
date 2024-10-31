`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

split_camel_case <- function(camelCaseString) {
  # Use gsub to insert a space before each uppercase letter, then trim whitespace
  splitString <- gsub("([A-Z])", " \\1", camelCaseString)
  return(trimws(splitString))
}

make_datasource_summary_html <- function(datasource_summary) {
  if (length(datasource_summary) == 0) {
    return(NULL)
  }
  datasource_summary %>%
    names() %>%
    lapply(function(nm) {
      tags$li(strong(tolower(split_camel_case(nm))), ": ", datasource_summary[[nm]])
    }) %>%
    tags$ol()
}

make_intro_page <- function(PROPERTIES,
                            markdown_file,
                            app_type,
                            datasource_summary_file,
                            add_referenced_cohorts = FALSE,
                            add_cohort_summary = FALSE) {
  stopifnot("PROPERTIES arg is missing" = !missing(PROPERTIES))
  app_type <- match.arg(app_type, c(
    "cohort_counts", "cohort_characterization", "incidence_rate",
    "cohort_pathway"
  ))
  if (add_cohort_summary) {
    stopifnot("markdown_file arg is missing" = !missing(markdown_file))
    if (!file.exists(markdown_file)) {
      stop(glue("'{markdown_file}' does not exist"))
    }
  }

  stopifnot("datasource_summary_file arg is missing" = !missing(datasource_summary_file))
  datasource_summary <- jsonlite::read_json(datasource_summary_file)
  datasource_summary_html <- make_datasource_summary_html(datasource_summary) %||%
    shiny::tags$span(style = "color: grey;", "Data source summary is empty.")

  not_found <- shiny::tags$span(style = "color: orange;", "Property not found.")

  shiny::tagList(
    h1("Study Asset Description:"),
    tags$ol(
      tags$li(strong("Name: "), PROPERTIES$cohort_name %||% PROPERTIES$analysis_name %||% PROPERTIES$asset_name %||% not_found),
      tags$li(strong("ID: "), PROPERTIES$asset_id %||% not_found),
      tags$li(strong("Author: "), PROPERTIES$author %||% not_found),
      tags$li(strong("Data Source: "), PROPERTIES$datasource_name %||% not_found),
      tags$li(strong("Generated Date: "), PROPERTIES$generated_date %||% not_found),
      tags$li(strong("Version ID: "), PROPERTIES$version_id %||% not_found),
      tags$li(strong("Generation ID: "), PROPERTIES$generation_id %||% not_found),
      if (isTRUE(add_referenced_cohorts)) {
        if (!is.null(PROPERTIES$referenced_cohorts)) {
          referenced_cohorts_html <- PROPERTIES$referenced_cohorts %>%
            strsplit(";") %>%
            unlist() %>%
            gsub("^\\s", "", .) %>%
            lapply(function(x) {
              print(x)
              shiny::tags$li(x)
            }) %>%
            shiny::tags$ol()
        } else {
          referenced_cohorts_html <- not_found
        }
        tags$li(strong("Referenced Cohorts: "), referenced_cohorts_html) # only in: Cohort Characterization, Incidence Rate, Cohort Pathway
      },
      if (app_type == "app_type") tags$li(strong("Records: "), PROPERTIES$record_count %||% not_found),
      if (app_type == "app_type") tags$li(strong("People: "), PROPERTIES$person_count %||% not_found)
    ),
    if (add_cohort_summary) {
      tagList(
        h1("Cohort Summary:"),
        shiny::includeMarkdown(path = markdown_file),
      )
    },
    h1("Dashboard Functionality:"),
    dashboard_func(app_type = app_type),
    h1("Author Description:"),
    div(PROPERTIES$author_notes %||% not_found),
    h1("Data Source Summary:"),
    datasource_summary_html,

    # p("The description of the cohort being analysed is: ", textOutput("description"))
  )
}


dashboard_func <- function(app_type) {
  app_type <- match.arg(app_type, c(
    "cohort_counts", "cohort_characterization", "incidence_rate",
    "cohort_pathway"
  ))
  type <- list(
    "cohort_counts" = tagList(
      div("The application provides important statistics and charts to describe cohort counts and percentages of persons or records in the cohort after applying a certain set of inclusion rules."),
      span(strong("Cohort unit: "), "Briefly describes how this function works within the app."),
      tags$ol(
        tags$li("Person: the person cohort unit is an individual fitting the criteria for inclusion in a cohort."),
        tags$li("Event: an event unit is an observable event from the data and is a building block of healthcare data that is tied to a person unit. ")
      ),
      span(strong("View type: "), "Toggle between person and event units by clicking the buttons on the top left."),
      tags$ol(
        tags$li("Intersect: describes a means of defining a cohort based on the intersection of multiple events (e.g. If you want to define a cohort of persons diagnosed with hypertension that took an ACE inhibitor within the last 30 days the intersect view would capture only those that fit both criteria in the applied time frame)."),
        tags$li("Attrition: is used to track and explain the loss of persons during the cohort definition process such that the researcher can see how inclusion and exclusion criteria impact their cohort.")
      )
    ),
    "cohort_characterization" = span("To toggle the results of your characterization analysis, use the drop-down options on the top left to select cohorts and the specific analysis of interest. The table shows values for the target cohort and the comparator cohort. â€œNullâ€ indicates no data for the cohort."),
    "incidence_rate" = span("Once you have a built analysis for incidence in the Atlas platform the RShiny app offers a visual presentation of your analysis."),
    "cohort_pathway" = span("The RShiny app will display cohort pathway results with an interactive sunburst. Users can click on the sunburst to see details for the segment on the top right. Cohort details are available on the top left. Scroll down past the image to see the tabular view if preferred.")
  )

  type[[app_type]]
}

custom_styling <- function() {
  shiny::tags$style(HTML("
    h1 {
    margin: 0;
    font-family: 'Calibri', sans-serif;
    font-size: 28px;
    font-style: italic;
    color: #0070C0;
    }

    h2 {
    margin: 0;
    font-family: 'Calibri', sans-serif;
    font-style: italic;
    font-size: 24px;
    color: #0070C0;
    }

    h3 {
    margin: 0;
    font-family: 'Calibri', sans-serif;
    font-style: italic;
    font-size: 20px;
    color: #0070C0;
    }

    h4 {
    margin: 0;
    font-family: 'Calibri', sans-serif;
    font-style: italic;
    font-size: 18px;
    color: #0070C0;
    }
  "))
}
