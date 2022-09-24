################################################################################
#
#'
#' Create results tables
#'
#
################################################################################

create_province_table <- function(results_province, 
                                  results_total,
                                  vars,
                                  report = FALSE,
                                  format = c("long", "wide")) {
  format <- match.arg(format)
  
  results_table <- rbind(
    data.frame(
      get_vars_info(x = names(results_province[[1]]), vars = vars),
      unit = "Zambezia",
      estimate = coef(results_province[[1]]),
      confint(results_province[[1]])
    ) |>
      (\(x) { names(x)[5:6] <- c("lcl", "ucl"); x })(),
    data.frame(
      get_vars_info(x = names(results_province[[2]]), vars = vars),
      unit = "Nampula",
      estimate = coef(results_province[[2]]),
      confint(results_province[[2]])
    ) |>
      (\(x) { names(x)[5:6] <- c("lcl", "ucl"); x })(),
    data.frame(
      get_vars_info(x = names(results_total), vars = vars),
      unit = "Overall",
      estimate = coef(results_total),
      confint(results_total)
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
  
  if (format == "wide") {
    results_table <- data.frame(
      results_table |>
        subset(unit == "Zambezia", select = -unit),
      results_table |>
        subset(unit == "Nampula", c(estimate, lcl, ucl)),
      results_table |>
        subset(unit == "Overall", c(estimate, lcl, ucl))
    ) |> 
      (\(x)
        {
          names(x) <- c(
            "variable", "category",
            lapply(
              X = c("zambezia", "nampula", "overall"),
              FUN = paste,
              c("estimate", "lcl", "ucl"),
              sep = "_"
            ) |>
              unlist()
          )
          x
        }
      )()
  }
}

create_strata_table <- function(results_strata,
                                results_total,
                                vars,
                                report = FALSE,
                                format = c("long", "wide")) {
  format <- match.arg(format)
  
  results_table <- rbind(
    create_results_tables(
      x = results_strata,
      vars = vars,
      strata = names(results_strata)
    ),
    create_results_table(
      x = results_total,
      vars = vars,
      strata = "Overall"
    )
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

create_study_group_table <- function() {
  
}



get_vars_info <- function(x, vars) {
  variable <- stringr::str_extract_all(
    string = x, 
    pattern = paste(vars, collapse = "|"),
    simplify = TRUE
  ) |>
    stringr::str_replace_all(
      pattern = "_", replacement = " "
    ) |>
    stringr::str_to_title()
  
  category <- stringr::str_remove_all(
    string = x, 
    pattern = paste(vars, collapse = "|")
  ) |>
    stringr::str_replace_all(
      pattern = "_", replacement = " "
    ) |>
    (\(x) ifelse(x == "", "Mean", x))()
  
  data.frame(variable, category)
}


create_results_table <- function(x, vars, strata) {
  data.frame(
    get_vars_info(x = names(x), vars = vars),
    unit = strata,
    estimate = coef(x),
    confint(x)
  ) |>
    (\(x) { names(x)[5:6] <- c("lcl", "ucl"); x })()
}

create_results_tables <- function(x, vars, strata) {
  vars <- rep(list(vars), length(x))
  strata <- as.list(strata)
  
  Map(
    f = create_results_table,
    x = x,
    vars = vars,
    strata = strata
  ) |>
    dplyr::bind_rows()
}