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
      study_round = study_round,
      strata = names(results_province),
      study_group = "Overall"
    ),
    create_results_table(
      x = results_total,
      vars = vars,
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
                                study_round = c("Baseline", "Endline"),
                                report = FALSE,
                                format = c("long", "wide")) {
  study_round <- match.arg(study_round)
  format <- match.arg(format)
  
  results_table <- rbind(
    create_results_tables(
      x = results_strata,
      vars = vars,
      study_round = rep(study_round, length(results_strata)),
      strata = names(results_strata),
      study_group = c(
        rep("Intervention", 4), "Control", rep("Intervention", 3), "Control"
      )
    ),
    create_results_table(
      x = results_total,
      vars = vars,
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
      study_round = rep(study_round, length(results_study_group)),
      strata = rep("Overall", length(results_study_group)),
      study_group = names(results_study_group)
    ),
    create_results_table(
      x = results_total,
      vars = vars,
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
                                              study_round = c("Baseline", "Endline"),
                                              report = FALSE,
                                              format = c("long", "wide")) {
  study_round <- match.arg(study_round)
  format <- match.arg(format)
  
  results_table <- rbind(
    create_results_tables(
      x = results_study_group_province,
      vars = vars,
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

create_full_table <- function(baseline = NULL,
                              endline = NULL,
                              vars) {
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
          study_round = "Baseline",
          strata = baseline_strata,
          study_group = baseline_study_group
        ) |>
          dplyr::bind_rows() |>
          rbind(
            create_results_table(
              x = baseline[[5]],
              vars = vars,
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
        
        baseline_study_group <- list(
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
          study_round = "Endline",
          strata = endline_strata,
          study_group = endline_study_group
        ) |>
          dplyr::bind_rows() |>
          rbind(
            create_results_table(
              x = endline[[5]],
              vars = vars,
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

## Get variable identifiers ----------------------------------,------------------

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


# create_results_table <- function(x, vars, strata, 
#                                  sep = NULL, group_name = NULL) {
  # if (!is.null(sep)) {
  #   strata <- stringr::str_split(
  #     string = strata, pattern = sep, simplify = TRUE
  #   ) |>
  #     data.frame()
  #   
  #   if (!is.null(group_name)) {
  #     names(strata) <- group_name
  #   } else {
  #     names(strata) <- paste0("unit", seq_len(ncol(strata)))
  #   }
  # }

## Create results table --------------------------------------------------------

create_results_table <- function(x, vars, study_round, strata, study_group) {  
  results_table <- data.frame(
    study_round = study_round,
    strata = strata,
    study_group = study_group,
    estimate = coef(x),
    confint(x)
  ) |>
    (\(x) { names(x)[5:6] <- c("lcl", "ucl"); x })()
  
  results_table <- data.frame(
    get_vars_info(x = row.names(results_table), vars = vars),
    results_table
  )
  
  results_table
}

create_results_tables <- function(x, vars, study_round, strata, study_group) {
  vars <- rep(list(vars), length(x))
  study_round <- as.list(study_round)
  strata <- as.list(strata)
  study_group <- as.list(study_group)
  
  Map(
    f = create_results_table,
    x = x,
    vars = vars,
    study_round = study_round,
    strata = strata,
    study_group = study_group
  ) |>
    dplyr::bind_rows()
}

## Format results table for reports --------------------------------------------

format_results_table_report <- function(results_table) {
  results_table |>
    (\(x)
     {
       x[["estimate"]] <- ifelse(
         #x[["estimate"]] < 1, 
         x[["category"]] != "Mean",
         scales::percent(x[["estimate"]], accuracy = 0.01, suffix = "%"),
         scales::number(x[["estimate"]], accuracy = 0.01)
       )
       x[["lcl"]] <- ifelse(
         #x[["lcl"]] < 1,
         x[["category"]] != "Mean",
         scales::percent(x[["lcl"]], accuracy = 0.01, suffix = "%"),
         scales::number(x[["lcl"]], accuracy = 0.01)
       )
       x[["ucl"]] <- ifelse(
         #x[["ucl"]] < 1, 
         x[["category"]] != "Mean",
         scales::percent(x[["ucl"]], accuracy = 0.01, suffix = "%"),
         scales::number(x[["ucl"]], accuracy = 0.01)
       )
       x
    }
    )()
}

## Pivot results table for reports ---------------------------------------------

pivot_results_table <- function(results_table, pivot) {
  type <- unique(results_table[[pivot]])
  var_names <- c(
    "variable", "category", "study_round", "strata", "study_group"
  ) |>
    (\(x) x[x != pivot])()
  
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

