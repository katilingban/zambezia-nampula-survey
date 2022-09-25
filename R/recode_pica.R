################################################################################
#
#'
#' Recode pica
#'
#
################################################################################

## Recode responses to specific pica questions ---------------------------------

pica_recode_var <- function(x, na_values) {
  ifelse(x %in% na_values, NA, x)
}

## Recode responses to all pica questions --------------------------------------

pica_recode_vars <- function(vars, .data, na_values) {
  vars <- .data[vars]
  
  apply(
    X = vars,
    MARGIN = 2,
    FUN = pica_recode_var,
    na_values = na_values,
    simplify = TRUE
  ) |>
    data.frame()
}

## Recode pica diagnosis -------------------------------------------------------

pica_recode_diagnosis <- function(x) {
  ifelse(x >= 3, 1, 0)
}

## Recode pica frequency -------------------------------------------------------

pica_recode_frequency <- function(x) {
  pica_frequencies <- spread_vector_to_columns(
    x = x,
    fill = 1:5,
    na_rm = FALSE,
    prefix = "pica_frequency"
  )
}

## Recode the response to pica knowledge ---------------------------------------

pica_recode_response <- function(x) {
  pica_response <- spread_vector_to_columns(
    x = x,
    fill = 1:5,
    na_rm = FALSE,
    prefix = "pica_response"
  )
}

## Overall recode function -----------------------------------------------------

pica_recode <- function(vars, .data, na_values) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  pica_df <- pica_recode_vars(
    vars = vars,
    .data = .data,
    na_values = na_values
  )
  
  recoded_vars <- data.frame(
    pica_probable = pica_recode_diagnosis(x = pica_df[[vars[1]]]),
    pica_recode_frequency(x = pica_df[[vars[2]]]),
    pica_recode_response(x = pica_df[[vars[2]]]),
    pica_perception = pica_df[[vars[3]]]
  )
  
  data.frame(core_vars, recoded_vars)
}
