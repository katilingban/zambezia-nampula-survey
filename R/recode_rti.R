################################################################################
#
#'
#' Recode childhood illness - respiratory tract infection
#' 
#' RI1	Has **${child_random_name}** had a cough or difficulty in breathing in 
#'   the last two weeks?
#' RI2	Did you seek advice or treatment for the breathing problem? 
#' RI3	Where did you seek advice or treatment?
#' CH1	Has **${child_random_name}** had a cough in the last two weeks?
#' CH1a	When **${child_random_name}** had a cough was it accompanied by fever?
#' CH2	When **${child_random_name}** had a cough, did he/she breathe more 
#'   rapidly than usual, with short and rapid breaths?
#' CH3	Did you seek advice or treatment for the cause of the cough?
#' CH4	Where did you seek advice or treatment?
#' CH5	Was **${child_random_name}** given any medicine to treat his/her 
#'   illness?
#' CH5a	What medicine was given to **${child_random_name}**
#' CH5a_other	Specify other medicine given
#'
#
################################################################################

## Recode responses to individual questions ------------------------------------

rti_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses to multiple questions --------------------------------------

rti_recode_responses <- function(vars, .data, 
                                 na_values = c(rep(list(c(8, 9)), 4),
                                               list(c(88, 99)),
                                               list(c(8, 9)),
                                               list(c("88", "99"))),
                                 binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = rti_recode_response,
    x = as.list(x),
    na_values = na_values,
    binary = c(
      rep(list(binary), 3), 
      rep(list(FALSE), 2), binary, FALSE
    ) 
  ) |>
    dplyr::bind_cols()
}

################################################################################
#
#'
#' Diagnose respiratory tract infection
#'
#
################################################################################

rti_recode_diagnosis <- function(vars = c("ch1", "ch1a", "ch2"), 
                                 .data) {
  x <- .data[vars]
  
  ifelse(
    x[[vars[1]]] == 1 & (x[[vars[2]]] == 1 | x[[vars[3]]] == 1), 1, 0
  )
}

## Recode point-of-care --------------------------------------------------------

rti_recode_poc <- function(vars = "ch4", .data) {
  x <- .data[[vars]]
  
  spread_vector_to_columns(
    x = x,
    fill = 1:6,
    na_rm = FALSE,
    prefix = "rti_poc"
  )
}

## Recode treatment ------------------------------------------------------------

rti_recode_treatment <- function(vars = "ch5a", .data) {
  x <- .data[[vars]]
  
  split_select_multiples(
    x = x,
    fill = 1:5,
    na_rm = FALSE,
    prefix = "rti_treatment"
  )
}

## Overall recode function -----------------------------------------------------

rti_recode <- function(vars, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  rti_df <- rti_recode_responses(
    vars = vars, .data = .data
  )
  
  recoded_vars <- data.frame(
    rti_episode = rti_recode_diagnosis(.data = rti_df),
    rti_treatment = rti_df[[vars[4]]],
    rti_recode_poc(.data = rti_df),
    rti_recode_treatment(.data = rti_df)
  )
  
  data.frame(core_vars, recoded_vars)
}

