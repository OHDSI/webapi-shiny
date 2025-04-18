path <- "data"
read_data <- function(path) {
  library(dplyr)
  library(stringr)
  library(glue)
  library(cli)
  library(purrr)
  library(jsonlite)
  library(tidyr)
  filenames <- list.files(path, pattern = ".json$") %>%
    setdiff("datasource_summary.json") %>%
    stringr::str_subset("^cohort_definitions.json$", negate = TRUE)
  data_sources <- filenames %>%
    stringr::str_remove("_targetId.+$") %>%
    stringr::str_unique()
  target_ids <- filenames %>%
    stringr::str_extract("targetId\\d+") %>%
    stringr::str_remove("targetId") %>%
    stringr::str_unique() %>%
    as.integer()
  outcome_ids <- filenames %>%
    stringr::str_extract("outcomeId\\d+") %>%
    stringr::str_remove("outcomeId") %>%
    stringr::str_unique() %>%
    as.integer()
  missing_files <- expand.grid(data_source = data_sources, target = target_ids, outcome = outcome_ids) %>%
    as_tibble() %>%
    mutate(filename = glue::glue("{data_source}_targetId{target}_outcomeId{outcome}.json")) %>%
    pull(filename) %>%
    setdiff(filenames)
  if (length(missing_files) >
    0) {
    cli::cli_abort("Missing json files with data! \n{paste(missing_files, collapse = ',\n')}")
  }
  df <- expand.grid(data_source = data_sources, target = target_ids, outcome = outcome_ids) %>%
    as_tibble() %>%
    mutate(filename = glue::glue("{data_source}_targetId{target}_outcomeId{outcome}.json")) %>%
    mutate(
      x = purrr::map(
        file.path("data", filename),
        ~ jsonlite::read_json(., simplifyVector = FALSE)
      )
    ) %>%
    mutate(subgroup_id = NA_real_, subgroup_name = NA_character_) %>%
    mutate(total_persons = purrr::map_int(x, ~ .[["summary"]]$totalPersons)) %>%
    mutate(time_at_risk = purrr::map_int(x, ~ .[["summary"]]$timeAtRisk)) %>%
    mutate(cases = purrr::map_int(x, ~ .[["summary"]]$cases)) %>%
    mutate(
      proportion_per_1k_persons = cases * 1000 / total_persons, rate_per_1k_years = cases * 1000 / time_at_risk
    ) %>%
    mutate(
      subgroup_df = map(
        x, ~ {
          stratify_stats <- .x$stratifyStats
          result <- if (!is.null(stratify_stats) &&
            length(stratify_stats) !=
              0) {
            tibble(x = stratify_stats) %>%
              unnest_wider(col = x) %>%
              transmute(
                subgroup_id = id, subgroup_name = name, total_persons = totalPersons, time_at_risk = timeAtRisk,
                cases = cases, proportion_per_1k_persons = cases * 1000 / totalPersons, rate_per_1k_years = cases *
                  1000 / timeAtRisk
              )
          } else {
            tibble(
              subgroup_id = NA_real_, subgroup_name = NA_character_, total_persons = NA_integer_,
              time_at_risk = NA_integer_, cases = NA_integer_, proportion_per_1k_persons = NA_real_,
              rate_per_1k_years = NA_real_
            )
          }
        }
      )
    )
  app_data <- list()
  app_data[["summary_table"]] <- df %>%
    select(
      data_source, target, outcome, total_persons, time_at_risk, cases, proportion_per_1k_persons,
      rate_per_1k_years
    )
  print(app_data[["summary_table"]])
  app_data[["subgroup_table"]] <- df %>%
    select(data_source, target, outcome, subgroup_df) %>%
    tidyr::unnest(col = subgroup_df)
  print(app_data[["subgroup_table"]])
  app_data[["treemap_table"]] <- df %>%
    select(data_source, target, outcome, x) %>%
    mutate(
      treemap_data = purrr::map(
        x, ~ .$treemapData %>%
          jsonlite::fromJSON(simplifyVector = FALSE) %>%
          tidy_treemap_data()
      )
    ) %>%
    select(-x) %>%
    tidyr::unnest(treemap_data) %>%
    rename(total_persons = totalPersons, subset_ids = name, time_at_risk = timeAtRisk) %>%
    mutate(
      proportion_per_1k_persons = cases * 1000 / total_persons, rate_per_1k_years = cases * 1000 / time_at_risk
    )
  print(app_data[["treemap_table"]])
  return(app_data)
}
tidy_treemap_data <- function(treemap_data) {
  name <- character()
  size <- numeric()
  cases <- numeric()
  timeAtRisk <- numeric()
  recurse <- function(lst) {
    for (item in lst) {
      if (is.list(item) &&
        "size" %in% names(item)) {
        stopifnot("name" %in% names(item))
        name <<- c(name, item$name)
        size <<- c(size, item$size)
        cases <<- c(cases, item$cases)
        timeAtRisk <<- c(timeAtRisk, item$timeAtRisk)
      } else if (is.list(item)) {
        recurse(item)
      } else {
        next
      }
    }
  }
  recurse(treemap_data)
  name2 <- purrr::map_chr(
    name, function(encoded_name) {
      stringr::str_split_1(encoded_name, "") %>%
        as.numeric() %>%
        {
          . * seq_along(.)
        } %>%
        as.character() %>%
        stringr::str_subset("0", negate = T) %>%
        stringr::str_c(collapse = ",") %>%
        {
          ifelse(. == "", "None", .)
        }
    }
  )
  return(dplyr::tibble(name = name2, totalPersons = size, cases = cases, timeAtRisk = timeAtRisk))
}
FALSE
