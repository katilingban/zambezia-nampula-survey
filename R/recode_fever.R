################################################################################
#
#'
#' Recode childhood illness - fever
#' 
#' FEVER1	Has **${child_random_name}** been ill with a fever at any time in the 
#' last  two weeks?
#' FEVER2	Did you seek advice or treatment for the fever? 
#' FEVER3	Where did you seek advice or treatment?
#' FEVER4	Was a malaria rapid diagnostic test (RDT) done to 
#'   **${child_random_name}**?
#' FEVER5	Was a laboratory test (blood smear) for diagnosing malaria done for 
#'   **${child_random_name}**?
#' FEVER6	Was the result (RDT/Blood smear) positive for malaria?
#' FEVER6a	What treatment was given to **${child_random_name}**?
#' FEVER7	Did **${child_random_name}** take the antimalarial treatment the same 
#'   day or the day after the onset of the fever?
#' 
#'
#
################################################################################

## Recode responses to specific fever indicator question -----------------------

fever_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses to multiple fever indicator questions ----------------------

fever_recode_responses <- function(vars, .data, 
                                   na_values = c(8, 9, 88, 99, "88", "99"), 
                                   binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = fever_recode_response,
    x = as.list(x),
    na_values = rep(list(na_values), length(vars)),
    binary = c(
      rep(list(binary), 2), FALSE, rep(list(binary), 3), FALSE, binary
    ) 
  ) |>
    dplyr::bind_cols()
}

## Recode reported point-of-care for fever -------------------------------------

fever_recode_poc <- function(vars, .data) {
  x <- .data[[vars]]
  
  spread_vector_to_columns(
    x = x,
    fill = 1:6,
    na_rm = FALSE,
    prefix = "fever_poc"
  )
}

## Recode responses to malaria treatment ---------------------------------------

fever_recode_malaria <- function(vars, .data) {
  x <- .data[[vars]]
  
  split_select_multiples(
    x = x,
    fill = 1:7,
    na_rm = FALSE,
    prefix = "fever_malaria"
  )
}

## Overall recode funtion ------------------------------------------------------

fever_recode <- function(vars, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  fever_df <- fever_recode_responses(
    vars = vars, .data = .data
  )
  
  recoded_vars <- data.frame(
    fever_episode = fever_df[[vars[1]]],
    fever_treatment = fever_df[[vars[2]]],
    fever_poc = fever_df[[vars[3]]],
    fever_recode_poc(
      vars = vars[3], .data = fever_df
    ),
    fever_rdt = fever_df[[vars[4]]],
    fever_smear = fever_df[[vars[5]]],
    fever_test = ifelse(
      fever_df[[vars[4]]] == 1 | fever_df[[vars[5]]] == 1, 1, 0
    ),
    fever_test_result = fever_df[[vars[6]]],
    fever_recode_malaria(
      vars = vars[7], .data = fever_df
    ),
    fever_malaria_intake = fever_df[[vars[8]]]
  )
  
  data.frame(core_vars, recoded_vars)
}

