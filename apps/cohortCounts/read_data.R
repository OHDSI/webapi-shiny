read_data <- function(path) {
  require(dplyr)
  require(logger)
  data_sources <- list.files(path, pattern = ".json$") %>%
    setdiff("datasource_summary.json") %>%
    stringr::str_remove("_by_(person|event).json$") %>%
    stringr::str_unique()
  app_data <- list()
  for (d in data_sources) {
    for (e in c("event", "person")) {
      file_path <- file.path(path, glue::glue("{d}_by_{e}.json"))
      logger::log_info(paste("File path", file_path))
      if (!file.exists(file_path)) {
        stop(paste(file_path, "cannot be found!"))
      }
      x <- jsonlite::read_json(file_path, simplifyVector = FALSE)
      logger::log_info(paste("basecount", x$summary$baseCount))
      if (x$summary$baseCount == 0) {
        app_data[[d]][[e]][["summary_table"]] <- dplyr::tibble(initial_index_events = 0, final_index_events = 0, percent_included = 0)
        app_data[[d]][[e]][["inclusion_table"]] <- dplyr::tibble(ID = integer(), `Inclusion Rule` = character(), Count = integer(), Percent = numeric())
        app_data[[d]][[e]][["treemap_table"]] <- dplyr::tibble(name = character(), value = numeric())
        app_data[[d]][[e]][["attrition_table"]] <- dplyr::tibble(
          ID = integer(), `Inclusion Rule` = character(), Count = integer(), pct_remain = numeric(),
          pct_diff = numeric()
        )
        logger::log_info("No Data ---")
      } else {
        logger::log_info("Nonzero basecount")
        app_data[[d]][[e]][["summary_table"]] <- dplyr::tibble(
          initial_index_events = x$summary$baseCount, final_index_events = x$summary$finalCount,
          percent_included = x$summary$percentMatched
        )
        app_data[[d]][[e]][["inclusion_table"]] <- dplyr::tibble(value = x$inclusionRuleStats) %>%
          tidyr::unnest_wider(col = value) %>%
          mutate(id = id + 1) %>%
          select(ID = id, `Inclusion Rule` = name, Count = countSatisfying, Percent = percentSatisfying)
        app_data[[d]][[e]][["treemap_table"]] <- x$treemapData %>%
          jsonlite::fromJSON(simplifyVector = FALSE) %>%
          tidy_treemap_data()
        num_rules <- nrow(app_data[[d]][[e]][["inclusion_table"]])
        total <- sum(app_data[[d]][[e]][["treemap_table"]]$value)
        app_data[[d]][[e]][["attrition_table"]] <- purrr::map(
          seq(num_rules),
          function(x) {
            x <- paste(
              seq(x),
              collapse = ","
            )
            app_data[[d]][[e]][["treemap_table"]] %>%
              filter(
                grepl(
                  paste0("^", x),
                  name
                )
              ) %>%
              summarise(value = sum(value)) %>%
              transmute(name = x, n = value)
          }
        ) %>%
          bind_rows() %>%
          mutate(pct_remain = n / .env$total) %>%
          mutate(
            pct_diff = ifelse(
              name == "1", 1 - pct_remain, lag(pct_remain) -
                pct_remain
            )
          ) %>%
          mutate(
            ID = purrr::map_dbl(
              stringr::str_split(name, ","),
              ~ max(as.numeric(.))
            )
          ) %>%
          left_join(app_data[[d]][[e]][["inclusion_table"]][, 1:2], by = "ID") %>%
          select(ID, `Inclusion Rule`, Count = n, pct_remain, pct_diff)
      }
    }
    logger::log_info("---")
    return(app_data)
  }
}
tidy_treemap_data <- function(treemap_data) {
  name <- character()
  size <- numeric()
  recurse <- function(lst) {
    for (item in lst) {
      if (is.list(item) &&
        "size" %in% names(item)) {
        stopifnot("name" %in% names(item))
        name <<- c(name, item$name)
        size <<- c(size, item$size)
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
  return(dplyr::tibble(name = name2, value = size))
}
FALSE
