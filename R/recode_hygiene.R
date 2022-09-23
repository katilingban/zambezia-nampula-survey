################################################################################
#
#'
#' Recode hygiene indicators/data
#' 
#' lusd9, lusd10, lusd11
#' caha1, caha2, caha2_other, caha3
#' 
#'
#
################################################################################

## Recode responses to specific questions --------------------------------------

hygiene_recode_response <- function(x, na_values) {
  ifelse(x %in% na_values, NA, x)
}

## Recode responses to multiple questions --------------------------------------

hygiene_recode_responses <- function(vars, .data, na_values) {
  x <- .data[vars]
  
  apply(
    X = x,
    MARGIN = 2,
    FUN = hygiene_recode_response,
    na_values = na_values,
    simplify = TRUE
  ) |>
    data.frame() |>
    (\(x) { names(x) <- vars; x } )()
}

## Recode response to select multiple events of handwashing --------------------

hygiene_recode_events <- function(vars, .data, fill, na_rm = FALSE, prefix) {
  x <- .data[[vars]]
  
  split_select_multiples(
    x = x, 
    fill = fill, 
    na_rm = na_rm, 
    prefix = prefix
  )
}

## Overall recode function -----------------------------------------------------

hygiene_recode <- function(vars, .data, na_values) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  hygiene_df <- hygiene_recode_responses(
    vars = vars,
    .data = .data,
    na_values = na_values
  )
  
  recoded_vars <- data.frame(
    hygiene_wash_recent = ifelse(hygiene_df[[vars[1]]] == 2, 0, 1),
    hygiene_recode_events(
      vars = vars[2],
      .data = hygiene_df,
      fill = 1:6,
      na_rm = FALSE,
      prefix = "handwash_event"
    ),
    hygiene_wash_appropriate = ifelse(hygiene_df[[vars[3]]] == 2, 0, 1),
    hygiene_child_defecation = ifelse(hygiene_df[[vars[4]]] %in% 1:2, 1, 0),
    hygiene_child_disposal = ifelse(hygiene_df[[vars[5]]] %in% c(1, 7), 1, 0),
    hygiene_child_diaper = ifelse(hygiene_df[[vars[6]]] == 1, 1, 0)
  )
  
  data.frame(core_vars, recoded_vars)
} 

