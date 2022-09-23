################################################################################
#
#'
#' Recode family planning indicators 
#' 
#' PF1	Have you ever used or tried a method to delay or avoid pregnancy?
#' BS1	Is there an ideal moment to wait after birth before trying to get 
#'   pregnant again?
#' BS1a	What is the ideal period to wait?
#' BS2	What are the benefits of waiting?
#' BS2a	Specify other benefits
#' BS3	Are there benefits to waiting until after age 18 years?
#' BS3a	Specify other benefits
#' BS4	Are there problems that can occur for a woman to be pregnant when she 
#'   has more than four children?
#' BS4a	Specify other problems when a woman has more than four children
#' ABOR1	Is there an ideal moment to wait after a spontaneous abortion before 
#'   trying to get pregnant again?
#' ABOR1a	What is the ideal period to wait?
#'
#
################################################################################

## Recode other responses to benefits of waiting before next pregnancy ---------

fp_recode_bs2a <- function(x, y) {
  ## Recode other planning benefits
  x <- ifelse(
    stringr::str_detect(x, "5| 5") & stringr::str_detect(y, "growth|grow|development"),  
    paste0(stringr::str_remove(x, "5| 5"), " 7"), 
    x
  )
  
  x
}

## Recode other responses to benefits of waiting before first pregnancy --------

fp_recode_bs3a <- function(x, y) {
  ## Recode other planning benefits
  x <- ifelse(
    stringr::str_detect(x, "5| 5") & stringr::str_detect(y, "education|studies|study|professional|Studying"),  
    paste0(stringr::str_remove(x, "5| 5"), " 7"), 
    x
  )
  
  x <- ifelse(
    stringr::str_detect(x, "5| 5") & stringr::str_detect(y, "danger|death|sezarian"),  
    paste0(stringr::str_remove(x, "5| 5"), " 8"), 
    x
  )
  
  x <- ifelse(
    stringr::str_detect(x, "5| 5") & stringr::str_detect(y, "growth|mature|maturity|grow"),  
    paste0(stringr::str_remove(x, "5| 5"), " 9"), 
    x
  )

  x
}

## Recode benefits of waiting before pregnancy after abortion ------------------

fp_recode_bs4a <- function(x, y) {
  ## Recode other planning benefits
  x[stringr::str_detect(y, "mother|health")] <- 7
  x[stringr::str_detect(y, "cesarean")]      <- 8  
  x[stringr::str_detect(y, "depend|Depend")] <- 9
  x
}

## Recode responses for a specific question ------------------------------------

fp_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses for multiple questions -------------------------------------

fp_recode_responses <- function(vars, .data,
                                na_values = c(rep(list(c(8, 9)), 2), 
                                              list(c(8, 9, 78, 88, 99)),
                                              rep(list(c(88, 99)), 3),
                                              list(c(8, 9)),
                                              list(c(88, 99))),
                                binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = fp_recode_response,
    x = as.list(x),
    na_values = na_values,
    binary = as.list(c(rep(binary, 2), rep(FALSE, 4), 
                       binary, FALSE))
  ) |>
    dplyr::bind_cols()
}

## Recode ideal waiting time to get pregnant again (minimum 18 months) ---------

fp_recode_wait_time <- function(vars, .data) {
  x <- .data[[vars]]
  
  data.frame(
    fp_wait_time = x,
    fp_wait_time_appropriate = ifelse(x >= 18, 1, 0)
  )
}

## Recode benefits of waiting for next pregnancy -------------------------------

fp_recode_benefit_next <- function(vars, .data, fill = 1:7, 
                                   na_rm = FALSE, prefix = "benefit_next") {
  x <- .data[[vars]]
  
  split_select_multiples(
    x = x,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  )
}

## Recode benefits for waiting before first pregnancy --------------------------

fp_recode_benefit_first <- function(vars, .data, fill = 1:9, 
                                    na_rm = FALSE, prefix = "benefit_first") {
  x <- .data[[vars]]
  
  split_select_multiples(
    x = x,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  )
}

## Recode dangers of multiparity -----------------------------------------------

fp_recode_multiparity <- function(vars, .data, fill = 1:9, 
                                  na_rm = FALSE, 
                                  prefix = "multiparity_danger") {
  x <- .data[[vars]]
  
  spread_vector_to_columns(
    x = x,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  )
}

## Recode reasons for waiting after spontaneous abortion -----------------------

fp_recode_wait_abort <- function(vars, .data) {
  x <- .data[[vars]]
  
  ifelse(x >= 6, 1, 0)
}

## Overall recode --------------------------------------------------------------

fp_recode <- function(vars, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  .data[[vars[4]]] <- fp_recode_bs2a(
    x = .data[[vars[4]]], y = .data[["bs2a_en"]]
  )
  
  .data[[vars[5]]] <- fp_recode_bs3a(
    x = .data[[vars[5]]], y = .data[["bs3a_en"]]
  )
  
  .data[[vars[6]]] <- fp_recode_bs4a(
    x = .data[[vars[6]]], y = .data[["bs4a_en"]]
  )
  
  x <- .data[vars]
  
  fp_df <- fp_recode_responses(vars = vars, .data = .data)
  
  recoded_vars <- data.frame(
    fp_use = fp_df[[vars[1]]],
    fp_wait = fp_df[[vars[2]]],
    fp_recode_wait_time(vars = vars[3], .data = fp_df),
    fp_recode_benefit_next(vars = vars[4], .data = fp_df),
    fp_recode_benefit_first(vars = vars[5], .data = fp_df),
    fp_recode_multiparity(vars = vars[6], .data = fp_df),
    fp_wait_abort = fp_df[[vars[7]]],
    fp_wait_abort_appropriate = fp_recode_wait_abort(
      vars = vars[8], .data = fp_df
    )
  )
  
  data.frame(core_vars, recoded_vars)
}
