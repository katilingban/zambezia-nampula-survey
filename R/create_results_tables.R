################################################################################
#
#'
#' Create results tables
#'
#
################################################################################

## Create by province and overall results table --------------------------------

create_province_table <- function(results_province, 
                                  results_total,
                                  vars,
                                  indicator_list,
                                  study_round = c("Baseline", "Endline"),
                                  report = FALSE,
                                  format = c("long", "wide"),
                                  pivot = "strata") {
  study_round <- match.arg(study_round)
  format <- match.arg(format)
  
  results_table <- rbind(
    create_results_tables(
      x = results_province,
      vars = vars,
      indicator_list = indicator_list,
      study_round = study_round,
      strata = names(results_province),
      study_group = "Overall"
    ),
    create_results_table(
      x = results_total,
      vars = vars,
      indicator_list = indicator_list,
      study_round = study_round,
      strata = "Overall",
      study_group = "Overall"
    )
  )
  
  row.names(results_table) <- NULL
  
  if (report) {
    results_table <- format_results_table_report(results_table)
  }
  
  if (format == "wide") {
    results_table <- pivot_results_table(results_table, pivot = pivot)
  }
  
  results_table
}

## Create by strata and overall results table ----------------------------------

create_strata_table <- function(results_strata,
                                results_total,
                                vars,
                                indicator_list,
                                study_round = c("Baseline", "Endline"),
                                report = FALSE,
                                format = c("long", "wide")) {
  study_round <- match.arg(study_round)
  format <- match.arg(format)
  
  results_table <- rbind(
    create_results_tables(
      x = results_strata,
      vars = vars,
      indicator_list = indicator_list,
      study_round = rep(study_round, length(results_strata)),
      strata = names(results_strata),
      study_group = c(
        rep("Intervention", 4), "Control", rep("Intervention", 3), "Control"
      )
    ),
    create_results_table(
      x = results_total,
      vars = vars,
      indicator_list = indicator_list,
      study_round = study_round,
      strata = "Overall",
      study_group = "Overall"
    )
  )
  
  row.names(results_table) <- NULL
  
  if (report) {
    results_table <- format_results_table_report(results_table)
  }
  
  results_table
}

## Create by study group and overall results table -----------------------------

create_study_group_table <- function(results_study_group,
                                     results_total,
                                     vars,
                                     indicator_list,
                                     study_round = c("Baseline", "Endline"),
                                     report = FALSE,
                                     format = c("long", "wide"),
                                     pivot = "study_group") {
  study_round <- match.arg(study_round)
  format <- match.arg(format)
  
  results_table <- rbind(
    create_results_tables(
      x = results_study_group,
      vars = vars,
      indicator_list = indicator_list,
      study_round = rep(study_round, length(results_study_group)),
      strata = rep("Overall", length(results_study_group)),
      study_group = names(results_study_group)
    ),
    create_results_table(
      x = results_total,
      vars = vars,
      indicator_list = indicator_list,
      study_round = study_round,
      strata = "Overall",
      study_group = "Overall"
    )
  )
  
  row.names(results_table) <- NULL
  
  if (report) {
    results_table <- format_results_table_report(results_table)
  }
  
  if (format == "wide") {
    results_table <- pivot_results_table(results_table, pivot = pivot)
  }
  
  results_table
}

## Create by study group per province table ------------------------------------

create_study_group_province_table <- function(results_study_group_province,
                                              results_study_group,
                                              vars,
                                              indicator_list,
                                              study_round = c("Baseline", "Endline"),
                                              report = FALSE,
                                              format = c("long", "wide")) {
  study_round <- match.arg(study_round)
  format <- match.arg(format)
  
  results_table <- rbind(
    create_results_tables(
      x = results_study_group_province,
      vars = vars,
      indicator_list = indicator_list,
      study_round = rep(study_round, length(results_study_group_province)),
      strata = names(results_study_group_province) |> 
        stringr::str_split(pattern = " ", simplify = TRUE) |> 
        (\(x) x[ , 1])(),
      study_group = names(results_study_group_province) |> 
        stringr::str_split(pattern = " ", simplify = TRUE) |> 
        (\(x) x[ , 2])()
    ),
    create_results_tables(
      x = results_study_group,
      vars = vars,
      indicator_list = indicator_list,
      study_round = study_round,
      strata = "Overall",
      study_group = names(results_study_group)
    )
  )
  
  row.names(results_table) <- NULL
  
  if (report) {
    results_table <- format_results_table(results_table)
  }
  
  results_table
}

## Create full table

create_full_table <- function(variable_set,
                              indicator_list = survey_indicator_list,
                              baseline = get_tables_list("baseline", variable_set),
                              endline = get_tables_list("endline", variable_set)) {
  vars <- indicator_list |>
    subset(indicator_set_code == variable_set) |>
    (\(x) x[["indicator_variable"]])()
  
  vars_list <- rep(list(vars), 4)
  
  if (is.null(baseline) & is.null(endline)) {
    stop(
      "Baseline and endline cannot be both NULL."
    )
  } else {
    if (!is.null(baseline)) {
      if (!is.list(baseline)) {
        stop(
          "Baseline should be a list of results outputs."
        )
      } else {
        baseline_strata <- list(
          names(baseline[[1]]),
          names(baseline[[2]]),
          "Overall",
          names(baseline[[4]]) |> 
            stringr::str_split(pattern = " ", simplify = TRUE) |> 
            (\(x) x[ , 1])()
        )
        
        baseline_study_group <- list(
          "Overall",
          c(rep("Intervention", 4), "Control", rep("Intervention", 3), "Control"),
          names(baseline[[3]]),
          names(baseline[[4]]) |> 
            stringr::str_split(pattern = " ", simplify = TRUE) |> 
            (\(x) x[ , 2])()
        )
        
        baseline_table <- Map(
          f = create_results_tables,
          x = baseline[1:4],
          vars = vars_list,
          indicator_list = rep(list(indicator_list), 4),
          study_round = "Baseline",
          strata = baseline_strata,
          study_group = baseline_study_group
        ) |>
          dplyr::bind_rows() |>
          rbind(
            create_results_table(
              x = baseline[[5]],
              vars = vars,
              indicator_list = indicator_list,
              study_round = "Baseline",
              strata = "Overall",
              study_group = "Overall"
            )
          )
      }
    } else {
      baseline_table <- NULL
    }
    
    if (!is.null(endline)) {
      if (!is.list(endline)) {
        stop(
          "Endline should be a list of results outputs."
        )
      } else {
        endline_strata <- list(
          names(endline[[1]]),
          names(endline[[2]]),
          "Overall",
          names(endline[[4]]) |> 
            stringr::str_split(pattern = " ", simplify = TRUE) |> 
            (\(x) x[ , 1])()
        )
        
        endline_study_group <- list(
          "Overall",
          c(rep("Intervention", 4), "Control", rep("Intervention", 3), "Control"),
          names(endline[[3]]),
          names(endline[[4]]) |> 
            stringr::str_split(pattern = " ", simplify = TRUE) |> 
            (\(x) x[ , 2])()
        )
        
        endline_table <- Map(
          f = create_results_tables,
          x = endline[1:4],
          vars = vars_list,
          indicator_list = rep(list(indicator_list), 4),
          study_round = "Endline",
          strata = endline_strata,
          study_group = endline_study_group
        ) |>
          dplyr::bind_rows() |>
          rbind(
            create_results_table(
              x = endline[[5]],
              vars = vars,
              indicator_list = indicator_list,
              study_round = "Endline",
              strata = "Overall",
              study_group = "Overall"
            )
          )
      }
    } else {
      endline_table <- NULL
    }
  }

  results_table <- rbind(
    baseline_table, endline_table
  )
  
  row.names(results_table) <- NULL
  
  results_table
}

## Create baseline tables list

get_tables_list <- function(round, variable_set) {
  table_list <- c(
    paste(
      round, variable_set, 
      c("province", "strata", "study_group", "study_group_province"),
      sep = "_"
    ),
    paste(round, variable_set, sep = "_")
  ) 
  
  eval(
    parse(
      text = paste0(
        "list(",
        paste(table_list, collapse = ", "),
        ")"
      )
    )
  )
  # parse(
  #   text = paste0(
  #     "list(",
  #     paste(table_list, collapse = ", "),
  #     ")"
  #   )
  # )
}


## Get variable identifiers ----------------------------------,------------------

get_vars_info <- function(x, vars, indicator_list) {
  variable <- stringr::str_extract_all(
    string = x, 
    pattern = paste(vars, collapse = "|"),
    simplify = TRUE
  )
  
  category <- stringr::str_remove_all(
    string = x,
    pattern = paste(vars, collapse = "|")
  ) |>
    stringr::str_replace_all(
      pattern = "_", replacement = " "
    )
  
  data.frame(variable, category) |>
    dplyr::left_join(
      indicator_list,
      by = c("variable" = "indicator_variable")
    ) |>
    dplyr::mutate(
      indicator_category = ifelse(
        is.na(indicator_category), category, indicator_category
      ),
      indicator_variable = variable
    ) |>
    subset(
      select = c(
        indicator_set, indicator_set_code, indicator_variable, indicator_code,
        indicator, indicator_label, indicator_category
      )
    )
}

## Create results table --------------------------------------------------------

create_results_table <- function(x, vars,
                                 indicator_list,
                                 study_round, 
                                 strata, study_group) {  
  results_table <- data.frame(
    study_round = study_round,
    strata = strata,
    study_group = study_group,
    estimate = coef(x),
    confint(x)
  ) |>
    (\(x) { names(x)[5:6] <- c("lcl", "ucl"); x })()
  
  results_table <- data.frame(
    get_vars_info(
      x = row.names(results_table), vars = vars, 
      indicator_list = indicator_list
    ),
    results_table
  )
  
  results_table
}

create_results_tables <- function(x, vars, 
                                  indicator_list,
                                  study_round, strata, study_group) {
  vars <- rep(list(vars), length(x))
  indicator_list <- rep(list(indicator_list), length(x))
  study_round <- as.list(study_round)
  strata <- as.list(strata)
  study_group <- as.list(study_group)
  
  Map(
    f = create_results_table,
    x = x,
    vars = vars,
    indicator_list = indicator_list,
    study_round = study_round,
    strata = strata,
    study_group = study_group
  ) |>
    dplyr::bind_rows()
}

## Create survey characteristics tables ----------------------------------------

create_design_table <- function(x, vars,
                                indicator_list,
                                study_round, 
                                strata, study_group) {  
  design_table <- data.frame(
    study_round = study_round,
    strata = strata,
    study_group = study_group,
    standard_error = SE(x),
    design_effect = deff(x)
  )
  
  design_table <- data.frame(
    get_vars_info(
      x = row.names(design_table), vars = vars, 
      indicator_list = indicator_list
    ),
    design_table
  )
  
  design_table
}

create_design_tables <- function(x, vars, 
                                 indicator_list,
                                 study_round, strata, study_group) {
  vars <- rep(list(vars), length(x))
  indicator_list <- rep(list(indicator_list), length(x))
  study_round <- as.list(study_round)
  strata <- as.list(strata)
  study_group <- as.list(study_group)
  
  Map(
    f = create_results_table,
    x = x,
    vars = vars,
    indicator_list = indicator_list,
    study_round = study_round,
    strata = strata,
    study_group = study_group
  ) |>
    dplyr::bind_rows()
}


## Format results table for reports --------------------------------------------

format_results_table_report <- function(results_table, 
                                        accuracy = 0.1, 
                                        suffix = "") {
  results_table |>
    (\(x)
     {
       x[["estimate"]] <- ifelse(
         #x[["estimate"]] < 1, 
         x[["indicator_category"]] != "Mean",
         scales::percent(x[["estimate"]], accuracy = accuracy, suffix = suffix),
         scales::number(x[["estimate"]], accuracy = accuracy)
       )
       x[["lcl"]] <- ifelse(
         #x[["lcl"]] < 1,
         x[["indicator_category"]] != "Mean",
         scales::percent(x[["lcl"]], accuracy = accuracy, suffix = suffix),
         scales::number(x[["lcl"]], accuracy = accuracy)
       )
       x[["ucl"]] <- ifelse(
         #x[["ucl"]] < 1, 
         x[["indicator_category"]] != "Mean",
         scales::percent(x[["ucl"]], accuracy = accuracy, suffix = suffix),
         scales::number(x[["ucl"]], accuracy = accuracy)
       )
       x
    }
    )()
}

## Pivot results table for reports ---------------------------------------------

pivot_results_table <- function(results_table, pivot) {
  type <- unique(results_table[[pivot]])
  var_names <- c(
    "indicator_set", "indicator", "indicator_label", "indicator_category", 
    "study_round", "strata", "study_group"
  ) |>
    (\(x) x[x != pivot])()
  
  ## Trim results_table
  results_table <- results_table |>
    subset(
      select = c(-indicator_set_code, -indicator_variable, -indicator_code)
    )
  
  data.frame(
    results_table |>
      subset(
        eval(parse(text = paste0(pivot, " == type[1]"))), 
        select = eval(parse(text = paste0("-", pivot)))
      ),
    results_table |>
      subset(
        eval(parse(text = paste0(pivot, " == type[2]"))), 
        select = c(estimate, lcl, ucl)
      ),
    results_table |>
      subset(
        eval(parse(text = paste0(pivot, " == type[3]"))), 
        select = c(estimate, lcl, ucl)
      )
  ) |> 
    (\(x)
     {
       names(x) <- c(
         var_names,
         lapply(
           X = tolower(type),
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

