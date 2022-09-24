################################################################################
#
#'
#' Create results tables
#'
#
################################################################################

create_province_table <- function(results_province, vars,
                                  report = FALSE) {
  results_table <- rbind(
    data.frame(
      get_vars_info(x = names(results_province[[1]]), vars = vars),
      province = "Zambezia",
      estimate = coef(results_province[[1]]),
      confint(results_province[[1]])
    ) |>
      (\(x) { names(x)[5:6] <- c("lcl", "ucl"); x })(),
    data.frame(
      get_vars_info(x = names(results_province[[2]]), vars = vars),
      province = "Nampula",
      estimate = coef(results_province[[2]]),
      confint(results_province[[2]])
    ) |>
      (\(x) { names(x)[5:6] <- c("lcl", "ucl"); x })()
  )
  
  row.names(results_table) <- NULL
  
  if (report) {
    results_table <- results_table |>
      (\(x)
       {
         x[["estimate"]] <- ifelse(
           x[["estimate"]] < 1, 
           scales::percent(x[["estimate"]], accuracy = 0.01, suffix = "%"),
           scales::number(x[["estimate"]], accuracy = 0.01)
          )
         x[["lcl"]] <- ifelse(
           x[["lcl"]] < 1, 
           scales::percent(x[["lcl"]], accuracy = 0.01, suffix = "%"),
           scales::number(x[["lcl"]], accuracy = 0.01)
         )
         x[["ucl"]] <- ifelse(
           x[["ucl"]] < 1, 
           scales::percent(x[["ucl"]], accuracy = 0.01, suffix = "%"),
           scales::number(x[["ucl"]], accuracy = 0.01)
         )
         x
      }
      )()
  }
  
  results_table
}

create_strata_table <- function() {
  
}

create_study_group_table <- function() {
  
}



get_vars_info <- function(x, vars) {
  category <- stringr::str_remove_all(
    string = x, 
    pattern = paste(vars, collapse = "|")
  ) |>
    stringr::str_replace_all(
      pattern = "_", replacement = " "
    )
  
  variable <- stringr::str_extract_all(
    string = x, 
    pattern = paste(vars, collapse = "|"),
    simplify = TRUE
  ) |>
    (\(x) ifelse(x == "", category, variable))() |>
    stringr::str_replace_all(
      pattern = "_", replacement = " "
    ) |>
    stringr::str_to_title()
  
  data.frame(variable, category)
}