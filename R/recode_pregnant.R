################################################################################
#
#'
#' Recode pregnant women indicators
#' 
#' WH1	Are you currently pregnant?
#' WH2	Do you have a prenatal card?
#' WH3	Please, can you hand me the card?
#' WH4	During this pregnancy did you have malaria?
#' WH5	During this pregnancy did you have anaemia?
#' WH6	During this pregnancy did you exclude some type of food from your 
#'   routine diet?
#' WH6a	Specify types of food excluded from routine diet
#' WH7	During this pregnancy did you include some type of food from your 
#'   routine diet?
#' WH7a	Specify types of food included from routine diet
#' WH8	After this pregnancy do you want to have more children?
#' PREG1	Can you please tell me what symptoms are warning signs during 
#'   pregnancy?
#' PREG2	What will you do when labor pain begins?
#' PREG3	Can you tell me what symptoms indicate danger to the health of a 
#' newborn child?
#'
#
################################################################################

## Recode responses to a specific pregnancy indicator question -----------------

preg_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses to multiple pregnancy indicator question -------------------

preg_recode_responses <- function(vars, .data, 
                                  na_values = c(rep(list(c(8, 9)), 2), 
                                                list(c(88, 99)), 
                                                rep(list(c(8, 9)), 5), 
                                                rep(list(c(88, 99)), 3)), 
                                  binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = preg_recode_response,
    x = as.list(x),
    na_values = na_values,
    binary = as.list(c(rep(binary, 2), FALSE, rep(binary, 5), rep(FALSE, 3)))
  ) |>
    dplyr::bind_cols()
}

## Recode responses to card retention indicator --------------------------------

preg_recode_card <- function(vars, .data) {
  x <- .data[[vars]]
  
  spread_vector_to_columns(
    x = x,
    fill = 1:3,
    na_rm= FALSE, 
    prefix = "pnc_card"
  )
}

## Recode responses to pregnancy danger signs indicators -----------------------

preg_recode_danger <- function(vars, .data, na_rm = TRUE, prefix = "danger") {
  x <- .data[[vars]]
  
  danger_df <- split_select_multiples(
    x = x,
    fill = 1:10,
    na_rm = na_rm,
    prefix = prefix
  )
  
  danger_all <- rowSums(danger_df, na.rm = FALSE)
  
  danger_prop <- ifelse(danger_all == 10, 1, 0)
  
  data.frame(danger_df, danger_all, danger_prop)
}

## Recode responses to what respondent would do if she goes into labour --------

preg_recode_labor <- function(vars, .data, na_rm = FALSE, prefix = "labor") {
  x <- .data[[vars]]
  
  spread_vector_to_columns(
    x = x,
    fill = 1:6,
    na_rm = na_rm,
    prefix = prefix
  )
}

## Recode responses to danger signs for newborn --------------------------------

preg_recode_newborn <- function(vars, .data, na_rm = TRUE, prefix = "newborn") {
  x <- .data[[vars]]
  
  split_select_multiples(
    x = x,
    fill = 1:8,
    na_rm = na_rm,
    prefix = prefix
  )
}

## Overall recode function -----------------------------------------------------

preg_recode <- function(vars, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  preg_df <- preg_recode_responses(
    vars = vars, .data = .data
  )
  
  recoded_vars <- data.frame(
    preg_card = preg_df[[vars[2]]],
    preg_recode_card(vars = vars[3], .data = preg_df),
    preg_malaria = preg_df[[vars[4]]],
    preg_anaemia = preg_df[[vars[5]]],
    preg_exclude = preg_df[[vars[6]]],
    preg_include = preg_df[[vars[7]]],
    preg_more = preg_df[[vars[8]]],
    preg_recode_danger(vars = vars[9], .data = preg_df),
    preg_recode_labor(vars = vars[10], .data = preg_df),
    preg_recode_newborn(vars = vars[11], .data = preg_df)
  )
  
  data.frame(core_vars, recoded_vars)
}

