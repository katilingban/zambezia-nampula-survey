################################################################################
#
#'
#' Recode food stocks indicators/data
#'
#'
#
################################################################################

## Recode responses to a food stock indicator question -------------------------

stock_recode_response <- function(x, na_values, binary = TRUE) {
  na_type <- get_na_type(x)
  
  if (binary) {
    ifelse(
      x %in% na_values, na_type,
      ifelse(x == 2, 0, 1)
    )
  } else {
    ifelse(x %in% na_values, na_type, x)
  }
}

## Recode responses to specific food stock indicator questions -----------------

stock_recode_responses <- function(vars, .data, 
                                   na_values = c(5, 6, 8, 9), 
                                   binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = stock_recode_response,
    x = as.list(x),
    na_values = rep(list(na_values), length(vars)),
    binary = as.list(rep(c(binary, FALSE), length(vars) / 2)) 
  ) |>
    dplyr::bind_cols()
}

## Recode responses to amounts of stock for specific food item -----------------

stock_recode_amount <- function(vars, .data, prefix) {
  x <- .data[[vars[2]]]
  
  x <- ifelse(.data[[vars[1]]] == 0, 0, x)
  
  reserve_df <- spread_vector_to_columns(
    x = x,
    fill = 0:4,
    na_rm = FALSE,
    prefix = prefix
  )
  
  data.frame(x, reserve_df) |>
    (\(x) { names(x)[1] <- paste0(prefix, "_reserve"); x })()
}

## Recode responses to amounts of stock for multiple food items ----------------

stock_recode_amounts <- function(vars, .data, 
                                 prefix = c("corn", "rice", "millet", 
                                            "sorghum", "cassava", "sweet_potato", 
                                            "legumes")) {
  Map(
    f = stock_recode_amount,
    vars = c(list(vars[1:2]), list(vars[3:4]), list(vars[5:6]), 
             list(vars[7:8]), list(vars[9:10]), list(vars[11:12]), 
             list(vars[13:14])),
    .data = rep(list(.data), length(prefix)),
    prefix = as.list(prefix)
  ) |>
    dplyr::bind_cols()
}

## Overall recode function -----------------------------------------------------

stock_recode <- function(vars, .data, 
                         na_values = c(5, 6, 8, 9), 
                         binary = TRUE,
                         prefix = c("corn", "rice", "millet", 
                                    "sorghum", "cassava", "sweet_potato", 
                                    "legumes")) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  stock_df <- stock_recode_responses(
    vars = vars,
    .data = .data
  )
  
  recoded_vars <- stock_recode_amounts(
    vars = vars,
    .data = stock_df
  )
  
  data.frame(core_vars, recoded_vars)
}


