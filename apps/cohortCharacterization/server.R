server <- function(input, output, session) {
  T1 <- list()
  T2 <- list()
  observeEvent(
    input$cohort, {
      cohort <- req(input$cohort)
      if (cohort == "targetCohort") {
        picket_input <- pickerInput(
          inputId = "analysis", label = "Analysis name", choices = c(cohortNames$targetCohort),
          selected = c(cohortNames$targetCohort),
          multiple = TRUE, options = pickerOptions(actionsBox = TRUE)
        )
      }
      else if (cohort == "comparatorCohort") {
        picket_input <- pickerInput(
          inputId = "analysis", label = "Analysis name", choices = c(cohortNames$comparatorCohort),
          selected = c(cohortNames$comparatorCohort),
          multiple = TRUE, options = pickerOptions(actionsBox = TRUE)
        )
      }
      output$mainSelector <- renderUI(
        {
          picket_input
        }
      )
    }
  )
  a <- reactive(
    {
      req(input$analysis)
      req(cohortNames$targetCohort)
      logger::log_info("input$analysis: {input$analysis}")
      sapply(
        cohortNames$targetCohort, FUN = function(X) input$analysis %in%
          X
      )
    }
  )
  l <- reactive(
    {
      if (is.null(dim(a()))) {
        return(1)
      }
      else {
        length(a()[, 1])
      }
    }
  )
  r <- reactive(
    {
      lapply(
        seq_len(l()),
        function(x) {
          if (is.null(dim(a()))) {
          which(a())
          }
          else {
          which(a()[x, ])
          }
        }
      )
    }
  )
  b <- reactive(
    {
      sapply(
        cohortNames$comparatorCohort, FUN = function(X) input$analysis %in%
          X
      )
    }
  )
  m <- reactive(
    {
      if (is.null(dim(b()))) {
        return(1)
      }
      else {
        length(b()[, 1])
      }
    }
  )
  s <- reactive(
    {
      lapply(
        seq_len(m()),
        function(x) {
          if (is.null(dim(b()))) {
          which(b())
          }
          else {
          which(b()[x, ])
          }
        }
      )
    }
  )
  output$tables <- renderUI(
    {
      logger::log_info("Rendering table")
      if (isTruthy(input$cohort == "targetCohort")) {
        req(r())
        req(vapply(r(), isTruthy, logical(1)))
        lapply(
          seq_len(l()),
          function(x) {
          if (isTruthy(r()[[x]]) &&
            !is.null(targetCohort[[r()[[x]]]]$Avg)) {
            df <- targetCohort[[r()[[x]]]]
            count_index <- which(
            colnames(df) ==
              "Count"
          )
            df <- df %>%
            dplyr::select(1:(count_index - 1), Boxplot, Count, (count_index + 1):ncol(df))
            tags$div(
            class = "header", checked = NA, tags$h4(cohortNames$targetCohort[[r()[[x]]]], align = "right"),
            tags$hr(style = "border-top: 1px solid #000000;"),
            T1[[x]] <- reactable(
              df, sortable = TRUE, showSortable = FALSE, highlight = TRUE, searchable = TRUE,
              theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
              showPageSizeOptions = TRUE, pageSizeOptions = c(10, 15, 20),
              defaultPageSize = 15, style = list(maxWidth = 1600, maxHeight = 900),
              columns = list(
              Boxplot = colDef(
                cell = function(x) {
                div(class = "plot", img(src = sprintf("p%s.png", x)))
                }, width = 200, align = "center"
              )
            )
            )
          )
          }
          else {
            df <- targetCohort[[r()[[x]]]]
            tags$div(
            class = "header", checked = NA, tags$h4(cohortNames$targetCohort[[r()[[x]]]], align = "right"),
            tags$hr(style = "border-top: 1px solid #000000;"),
            T1[[x]] <- reactable(
              df, sortable = TRUE, showSortable = TRUE, highlight = TRUE, searchable = TRUE,
              theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
              showPageSizeOptions = TRUE, pageSizeOptions = c(10, 15, 20),
              defaultPageSize = 15, style = list(maxWidth = 1600, maxHeight = 900),
              columns = list(
              Percent = colDef(
                name = "N (%)", style = function(value) {
                value <- as.numeric(value)
                bar_style(width = value/100, color = "lightblue")
                }, cell = function(value, index, rowInfo) {
                count <- df[index, ]$Count
                glue::glue("{count} ({value}%)")
                }
              ),
              Count = colDef(show = FALSE)
            )
            )
          )
          }
          }
        )
      }
      else {
        req(l())
        req(sapply(s(), isTruthy))
        lapply(
          seq_len(l()),
          function(x) {
          df <- comparatorCohort[[s()[[x]]]]
          if ("Comparator percent" %in% names(df)) {
            df <- df %>%
            dplyr::mutate(`Comparator percent` = round(`Comparator percent`, 3))
          }
          tags$div(
            class = "header", checked = NA, tags$h4(cohortNames$comparatorCohort[[s()[[x]]]], align = "right"),
            tags$hr(style = "border-top: 1px solid #000000;"),
            T2[[x]] <- reactable(
            df, sortable = TRUE, showSortable = TRUE, highlight = TRUE, searchable = TRUE,
            theme = reactableTheme(color = "hsl(0, 0%, 0%)"),
            showPageSizeOptions = TRUE, pageSizeOptions = c(10, 15, 20),
            defaultPageSize = 15, style = list(maxWidth = 1600, maxHeight = 900)
          )
          )
          }
        )
      }
    }
  )
}
