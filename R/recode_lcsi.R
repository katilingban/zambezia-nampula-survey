################################################################################
#
#'
#' Recode LCSI indicators/data
#'
#
################################################################################

lcsi_recode_strategy <- function(x, na_values) {
  if (any(!is.na(na_values))) {
    ifelse(
      x %in% na_values, NA,
      ifelse(
        x %in% c(1, 4), 1, 0
      )
    )
  } else {
    ifelse(x %in% c(1, 4), 1, 0)
  }
}


lcsi_recode_strategies <- function(vars, .data, na_values = NA) {
  .data <- .data[vars]
  
  apply(
    X = .data,
    MARGIN = 2,
    FUN = lcsi_recode_strategy,
    na_values = na_values
  ) |>
    data.frame()
}


lcsi_classify_strategy <- function(lcsi, 
                                   phase = c(2, 3, 4, 2, 2, 3, 3, 4, 
                                              2, 4, 3, 2, 3, 3)) {
  if (length(lcsi) != length(phase)) {
    stop(
      "Set of livelihood coping strategies not the same length as the
      phase classifying values. Please check strategies set and/or
      phase classifying values."
    )
  }
  
  (lcsi * phase) |>
    (\(x) ifelse(x == 0, 1, x))() |>
    unlist()
}

################################################################################
#
#'
#' Calculate LCSI
#'
#
################################################################################

lcsi_calculate_index <- function(lcsi_df, add = TRUE,
                                 phase = c(2, 3, 4, 2, 2, 3, 3, 4, 
                                           2, 4, 3, 2, 3, 3)) {
  lcsi <- apply(
    X = lcsi_df,
    MARGIN = 1,
    FUN = lcsi_classify_strategy,
    phase = phase,
    simplify = FALSE
  ) |>
    dplyr::bind_rows()
  
  if (add) {
    data.frame(
      lcsi_df, 
      lcsi = lcsi)
  } else {
    lcsi
  }
}


lcsi_classify <- function(lcsi, add = FALSE, spread = FALSE,
                          phase = c(2, 3, 4, 2, 2, 3, 3, 4, 
                                    2, 4, 3, 2, 3, 3)) {  
  lcsi <- apply(
    X = lcsi,
    MARGIN = 1,
    FUN = max,
    na.rm = TRUE,
    simplify = FALSE
  ) |>
    unlist() |>
    (\(x) ifelse(x == -Inf, NA, x))()
  
  lcsi_class <- lcsi |>
    (\(x) c("secure", "stress", "crisis", "emergency")[x])() |>
    (\(x) factor(x, levels = c("secure", "stress", "crisis", "emergency")))()
    
  if (spread) {
    lcsi_class <- spread_vector_to_columns(
      x = lcsi_class,
      prefix = "lcsi"
    )
  }
  
  if (add) {
    data.frame(lcsi, lcsi_class)
  } else {
    lcsi_class
  }
}


################################################################################
#
#'
#' Overall recode function
#'
#
################################################################################

lcsi_recode <- function(vars,
                        .data,
                        na_values = NA,
                        phase = c(2, 3, 4, 2, 2, 3, 3, 4, 
                                  2, 4, 3, 2, 3, 3)) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- lcsi_recode_strategies(
    vars = vars, .data = .data, na_values = na_values
  )
  
  recoded_vars |>
    lcsi_calculate_index(add = FALSE, phase = phase) |>
    lcsi_classify(add = TRUE, spread = TRUE, phase = phase) |>
    (\(x) data.frame(core_vars, recoded_vars, x))()
}

