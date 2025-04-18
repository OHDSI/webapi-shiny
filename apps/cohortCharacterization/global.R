library(shiny)
library(htmltools)
library(shinyWidgets)
library(reactable)
library(dplyr)
library(st)
library(ggplot2)
library(bslib)
data_dir <- "data"
if (file.exists("helpers.R")) {
  source("helpers.R")
} else {
  source("../cohortCounts/helpers.R")
}
csv_files <- list.files(data_dir, pattern = ".csv", full.names = T)
PROPERTIES <- properties::read.properties(file.path(data_dir, "app.properties"))
atlas_link <- PROPERTIES$atlas_link
repo_link <- PROPERTIES$repo_link
datasource <- PROPERTIES$datasource
atlas_url <- PROPERTIES$atlas_url
analysis_name <- PROPERTIES$analysis_name
datasource_summary_file <- file.path(data_dir, "datasource_summary.json")
app_data <- purrr::map(
  csv_files, ~ data.table::fread(., data.table = T) %>%
    mutate(`Covariate short name` = as.character(`Covariate short name`)) %>%
    mutate(`Covariate short name` = tolower(`Covariate short name`)) %>%
    mutate(`Covariate short name` = gsub("^ ", "", `Covariate short name`)) %>%
    mutate(`Covariate short name` = DescTools::StrCap(`Covariate short name`)) %>%
    mutate(`Analysis name` = gsub("([a-z])([A-Z])", "\\1 \\2", `Analysis name`, perl = TRUE)) %>%
    mutate(
      Percent = tryCatch(
        round(Percent, digits = 1),
        error = function(z) {
          return(NA)
        }
      )
    ) %>%
    mutate(
      Avg = tryCatch(
        round(Avg, digits = 1),
        error = function(z) {
          return(NA)
        }
      )
    ) %>%
    mutate(
      StdDev = tryCatch(
        round(StdDev, digits = 1),
        error = function(z) {
          return(NA)
        }
      )
    ) %>%
    mutate(
      `Target percent` = tryCatch(
        round(`Target percent`, digits = 1),
        error = function(z) {
          return(NA)
        }
      )
    ) %>%
    mutate(
      `Cohort percent` = tryCatch(
        round(`Cohort percent`, digits = 1),
        error = function(z) {
          return(NA)
        }
      )
    ) %>%
    mutate(
      `Std. Diff Of Mean` = tryCatch(
        round(`Std. Diff Of Mean`, digits = 1),
        error = function(z) {
          return(NA)
        }
      )
    ) %>%
    select(-contains("Value field")) %>%
    select(-contains("Missing Means Zero")) %>%
    select(-contains("Analysis ID")) %>%
    select(-contains("Strata ID")) %>%
    select(-contains("Cohort ID")) %>%
    select(-contains("Covariate ID")) %>%
    select(-contains("Covariate name"))
)
empty_columns <- lapply(
  seq_len(length(app_data)),
  function(x) {
    colSums(
      is.na(app_data[[x]]) |
        app_data[[x]] == ""
    ) ==
      nrow(is.na(app_data[[x]]))
  }
)
app_data <- lapply(
  seq_len(length(app_data)),
  function(x) {
    app_data[[x]] %>%
      purrr::discard(empty_columns[[x]])
  }
)
app_data <- lapply(
  seq_len(length(app_data)),
  function(x) {
    if (!is.null(app_data[[x]]$Avg)) {
      app_data[[x]] %>%
        mutate(Boxplot = x)
    } else {
      app_data[[x]]
    }
  }
)
lapply(
  seq_len(length(app_data)),
  function(a) {
    if (!is.null(app_data[[a]]$Avg)) {
      output <- ggplot(app_data[[a]], aes(x = `Analysis name`, fill = `Analysis name`)) +
        geom_boxplot(
          aes(ymin = Min, lower = P10, middle = Median, upper = P90, ymax = Max, ),
          alpha = 0, colour = "#0c439c", stat = "identity", show.legend = FALSE
        ) +
        coord_flip() +
        theme(
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)
        )
      png(
        sprintf("www/p%s.png", a),
        width = 170, height = 130, bg = "transparent"
      )
      plot(output)
      dev.off()
    } else {
      return()
    }
  }
)
comparatorCohort <- list()
targetCohort <- list()
for (z in 1:length(app_data)) {
  if (!is.null(app_data[[z]]$`Comparator cohort name`)) {
    comparatorCohort[[z]] <- app_data[[z]]
  } else {
    targetCohort[[z]] <- app_data[[z]]
  }
}
comparatorCohort <- Filter(
  Negate(is.null),
  comparatorCohort
)
inputFilesNames <- list(csv_files)
inputFilesNames <- lapply(
  inputFilesNames, function(x) {
    sub("^data/Export ", "", x)
  }
)
inputFilesNames <- lapply(
  inputFilesNames, function(x) {
    sub(".csv$", "", x)
  }
)
inputFilesNames <- lapply(
  inputFilesNames, function(x) {
    gsub("[()]", "", x, perl = TRUE)
  }
)
inputFilesNames <- lapply(
  inputFilesNames, function(x) {
    gsub("([a-z])([A-Z])", "\\1 \\2", x, perl = TRUE)
  }
)
inputFilesNames <- lapply(
  inputFilesNames, function(x) {
    tolower(x)
  }
)
inputFilesNames <- lapply(
  inputFilesNames, function(x) {
    stringr::str_to_sentence(x)
  }
)
targetListNames <- list()
comparatorListNames <- list()
inputListNames <- list()
for (x in 1:(length(inputFilesNames[[1]]))) {
  inputListNames[[x]] <- inputFilesNames[[1]][x]
}
for (x in 1:(length(inputFilesNames[[1]]) / 2)) {
  targetListNames[[x]] <- inputFilesNames[[1]][x]
}
for (y in ((length(inputFilesNames[[1]]) / 2) +
  1):(length(inputFilesNames[[1]]))) {
  comparatorListNames[[y]] <- inputFilesNames[[1]][y]
}
comparatorListNames <- Filter(
  Negate(is.null),
  comparatorListNames
)
cohortNames <- list(targetCohort = targetListNames, comparatorCohort = comparatorListNames)
bar_style <- function(width = 1, color = "lightblue") {
  position <- paste0(100 - width * 100, "%")
  list(
    background = sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, color),
    backgroundSize = "98% 88%", backgroundRepeat = "no-repeat", backgroundPosition = "center"
  )
}

download_file_name <- function(analysis_name, subname) {
  ts <- format(Sys.time(), format = "%Y-%m-%d-%H-%M-%S")
  glue::glue("{analysis_name}__{subname}__{ts}") %>%
    gsub("\\s", "_", .) %>%
    paste0(., ".csv")
}

FALSE
