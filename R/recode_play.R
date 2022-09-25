################################################################################
#
#'
#' Recode child play indicators
#'
#
################################################################################

play_recode_response <- function(x, na_values) {
  ifelse(
    x %in% na_values, NA,
    ifelse(x == 2, 0, 1)
  )
}


play_recode_responses <- function(vars, .data, na_values) {
  x <- .data[vars]
  
  apply(
    X = x,
    MARGIN = 2,
    FUN = play_recode_response,
    na_values = c(8, 9),
    simplify = TRUE
  ) |>
    data.frame() |>
    (\(x) { names(x) <- vars; x } )()
}


play_recode <- function(vars, .data, na_values) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- play_recode_responses(
    vars = vars,
    .data = .data,
    na_values = na_values
  )
  
  data.frame(core_vars, recoded_vars)
}

