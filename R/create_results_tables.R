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
                                  report = FALSE,
                                  format = c("long", "wide")) {
  format <- match.arg(format)
  
  results_table <- rbind(
    create_results_tables(
      x = results_province,
      vars = vars,
      strata = names(results_province)
    ),
    create_results_table(
      x = results_total,
      vars = vars,
      strata = "Overall"
    )
  )
  
  row.names(results_table) <- NULL
  
  if (report) {
    results_table <- format_results_table_report(results_table)
  }
  
  if (format == "wide") {
    results_table <- pivot_results_table(results_table)
  }
  
  results_table
}

## Create by strata and overall results table ----------------------------------

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
    results_table <- format_results_table_report(results_table)
  }
  
  results_table
}

## Create by study group and overall results table -----------------------------

create_study_group_table <- function(results_study_group,
                                     results_total,
                                     vars,
                                     report = FALSE,
                                     format = c("long", "wide")) {
  format <- match.arg(format)
  
  results_table <- rbind(
    create_results_tables(
      x = results_study_group,
      vars = vars,
      strata = names(results_study_group)
    ),
    create_results_table(
      x = results_total,
      vars = vars,
      strata = "Overall"
    )
  )
  
  row.names(results_table) <- NULL
  
  if (report) {
    results_table <- format_results_table_report(results_table)
  }
  
  if (format == "wide") {
    results_table <- pivot_results_table(results_table)
  }
  
  results_table
}

## Create by study group per province table ------------------------------------

create_study_group_province_table <- function(results_study_group_province,
                                              results_study_group,
                                              vars,
                                              report = FALSE,
                                              format = c("long", "wide")) {
  format <- match.arg(format)
  
  results_table <- rbind(
    create_results_tables(
      x = results_study_group_province,
      vars = vars,
      strata = names(results_study_group_province)
    ),
    create_results_tables(
      x = results_study_group,
      vars = vars,
      strata = names(results_study_group)
    )
  )
  
  row.names(results_table) <- NULL
  
  if (report) {
    results_table <- format_results_table(results_table)
  }
  
  results_table
}

## Get variable identifiers ----------------------------------------------------

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

create_results_table <- function(x, vars, strata) {  
  results_table <- data.frame(
    strata = strata,
    estimate = coef(x),
    confint(x)
  ) |>
    (\(x) { names(x)[3:4] <- c("lcl", "ucl"); x })()
  
  results_table <- data.frame(
    get_vars_info(x = row.names(results_table), vars = vars),
    results_table
  )
  
  results_table
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

pivot_results_table <- function(results_table) {
  type <- unique(results_table[["strata"]]) 
  
  data.frame(
    results_table |>
      subset(strata == type[1], select = -strata),
    results_table |>
      subset(strata == type[2], c(estimate, lcl, ucl)),
    results_table |>
      subset(strata == type[3], c(estimate, lcl, ucl))
  ) |> 
    (\(x)
     {
       names(x) <- c(
         "variable", "category",
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
