################################################################################
#
#'
#' CHM1	Have you ever given birth to a boy or girl who was born alive but later 
#'   died?
#' CHM2	Have any of your children died before their 5th birthday?
#' FANSIDAR1	During your last pregnancy were you given SP/Fansidar or 
#'   cotrimoxazole liek this one (show tablets) to treat your malaria during 
#'   your prenatal care visits?
#' FANSIDAR2	How many times did you take SP/Fansidar or cotrimoxazole during 
#'   your last pregnancy?
#' FOL1	During your last pregnancy, did you receive iron supplements to take?
#' TT1	During your last pregnancy, were you given an injection in the arm to 
#'   prevent the baby from getting tetanus, that is, convulsions after birth?
#' TT2	During your last pregnancy, how many times did you get this tetanus 
#'   injection?
#'
#'
#
################################################################################

## Recode responses to individual reproductive health questions ----------------

rh_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses to multiple reproductive health questions ------------------

rh_recode_responses <- function(vars, .data,
                                na_values = rep(list(c(8, 9, 88, 99)), 7),
                                binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = rh_recode_response,
    x = as.list(x),
    na_values = na_values,
    binary = as.list(c(rep(binary, 3), FALSE, 
                       rep(binary, 2), FALSE))
  ) |>
    dplyr::bind_cols()
}

## Recode responses to malaria during pregnancy indicators ---------------------

rh_recode_malaria <- function(vars, .data) {
  x <- .data[vars]
  
  mal_prevalence <- ifelse(x[[vars[1]]] == 1, 1, 0)
  mal_no_treatment <- ifelse(x[[vars[2]]] == 1, 1, 0)
  mal_appropriate_treatment <- ifelse(x[[vars[2]]] == 4, 1, 0)
  
  data.frame(
    mal_prevalence, mal_no_treatment, mal_appropriate_treatment
  )
}

## Recode responses to tetanus toxoid coverage during pregnancy indicators -----

rh_recode_tetanus <- function(vars, .data) {
  x <- .data[vars]
  
  tt_any <- x[[vars[1]]]
  tt_two_more <- ifelse(x[[vars[2]]] != 1, 1, 0)
  
  data.frame(tt_any, tt_two_more)
}

## Overall recode function -----------------------------------------------------

rh_recode <- function(vars, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  rh_df <- rh_recode_responses(
    vars = vars, .data = .data
  )
  
  recoded_vars <- data.frame(
    mort1 = rh_df[[vars[1]]],
    mort2 = rh_df[[vars[2]]],
    rh_recode_malaria(vars = vars[3:4], .data = rh_df),
    folate = rh_df[[vars[5]]],
    rh_recode_tetanus(vars = vars[6:7], .data = rh_df)
  )
  
  data.frame(core_vars, recoded_vars)
}

