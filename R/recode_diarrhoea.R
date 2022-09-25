################################################################################
#
#'
#' Recode childhood illness - diarrhoea
#' 
#' ORT1	Has **${child_random_name}** had diarrhoea in the last two weeks?
#' ORT1a	On the worst of the days, how many times did **${child_random_name}** 
#'   defecate?
#' ORT1b	How many days did the diarrhoea of **${child_random_name}** last?
#' ORT1c	Does **${child_random_name}** still have diarrhoea?
#' ORT2	Was there blood in the stool?
#' ORT3	Did you seek treatment for the diarrhoea?
#' ORT4	Where did you seek advice or treatment?
#' ORT5a	Did you give **${child_random_name}** a drink made from a packet 
#'   (oral rehydration salts) or oral mixture?
#' ORT5b	Did you give **${child_random_name}** a home-made mixture of water, 
#'   salt, and sugar?
#' ORT5c	Did you give **${child_random_name}** an appropriate drink for 
#'   treating diarrhoea (acquired in a pharmacy)?
#' ORT5d	Was **${child_random_name}** given anything else to treat diarrhoea?
#' ORT5e	What was given to treat diarrhoea?
#' ORT5e_specify	Specify other treatment given
#' ORT6	Did you give **${child_random_name}** the same amount of liquid, more 
#'   or less than usual?
#' ORT7	Did you give **${child_random_name}** the same amount of food, more or 
#'   less than usual?
#'
#
################################################################################

## Recode responses to specific diarrhoea indicator question

dia_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses to multiple diarrhoea indicator questions

dia_recode_responses <- function(vars, .data, 
                                 na_values = c(list(c(8, 9, 88, 99)),
                                               rep(list(c(88, 99)), 2),
                                               rep(list(c(8, 9, 88, 99)), 8),
                                               list(c("88", "99")),
                                               rep(list(c(8, 9, 88, 99)), 2)),
                                 binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = dia_recode_response,
    x = as.list(x),
    na_values = na_values,
    binary = c(
      binary, rep(list(FALSE), 2), 
      rep(list(binary), 3),
      FALSE,
      rep(list(binary), 4),
      rep(list(FALSE), 3)
    ) 
  ) |>
    dplyr::bind_cols()
}


################################################################################
#
#'
#' Diagnose diarrhoea
#'
#
################################################################################

dia_recode_diagnosis <- function(vars = c(paste0("ort1", c("", letters[1:3])), 
                                          "ort2"), 
                                 .data) {
  x <- .data[vars]
  
  ifelse(
    x[[vars[1]]] == 1 & x[[vars[2]]] > 2 & 
      (x[[vars[3]]] > 2 | x[[vars[4]]] == 1 | x[[vars[5]]] == 1), 1, 0
  )
}


################################################################################
#
#'
#' Diarrhoea treatment point of care
#'
#
################################################################################

dia_recode_poc <- function(vars = "ort4", .data) {
  x <- .data[[vars]]
  
  spread_vector_to_columns(
    x = x,
    fill = 1:6,
    na_rm = FALSE,
    prefix = "diarrhoea_poc"
  )
}


dia_recode_ors <- function(vars = paste0("ort5", letters[1:3]), .data) {
  x <- .data[vars]
  
  ifelse(x[[vars[1]]] == 1 | x[[vars[2]]] == 1 | x[[vars[3]]] == 1, 1, 0)
}



################################################################################
#
#'
#' Diarrhoea treatment
#'
#
################################################################################


dia_recode_treatment <- function(vars = "ort5e", .data) {
  x <- .data[[vars]]
  
  split_select_multiples(
    x = x,
    fill = 1:10,
    na_rm = FALSE,
    prefix = "diarrhoea_treatment"
  )
}


################################################################################
#
#'
#' Diarrhoea liquids intake
#'
#
################################################################################

dia_recode_liquids <- function(vars = "ort6", .data,
                               label = c("nothing", "much_less", 
                                         "less", "same", "more")) {
  x <- .data[[vars]]
  
  data.frame(
    diarrhoea_liquids = x,
    diarrhoea_liquids_adequate = ifelse(x == 5, 1, 0),
    spread_vector_to_columns(
      x = x,
      fill = 1:5,
      na_rm = FALSE,
      prefix = "diarrhoea_liquids"
    ) |>
      (\(x) { names(x) <- paste0("diarrhoea_liquids_", label); x } )()
  )
}


################################################################################
#
#'
#' Diarrhoea foods intake
#'
#
################################################################################

dia_recode_foods <- function(vars = "ort7", .data,
                               label = c("nothing", "much_less", 
                                         "less", "same", "more")) {
  x <- .data[[vars]]
  
  data.frame(
    diarrhoea_foods = x,
    diarrhoea_foods_adequate = ifelse(x %in% 4:5, 1, 0),
    spread_vector_to_columns(
      x = x,
      fill = 1:5,
      na_rm = FALSE,
      prefix = "diarrhoea_foods"
    ) |>
      (\(x) { names(x) <- paste0("diarrhoea_foods_", label); x } )()
  )
}


################################################################################
#
#'
#' Recode diarrhoea indicators
#'
#
################################################################################

dia_recode <- function(vars, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  dia_df <- dia_recode_responses(
    vars = vars, .data = .data
  )
  
  recoded_vars <- data.frame(
    diarrhoea_episode = dia_recode_diagnosis(.data = dia_df),
    diarrhoea_treatment = dia_df[[vars[6]]],
    dia_recode_poc(.data = dia_df),
    diarroea_treatment_ors = dia_recode_ors(.data = dia_df),
    dia_recode_treatment(.data = dia_df),
    dia_recode_liquids(.data = dia_df),
    dia_recode_foods(.data = dia_df)
  )
  
  data.frame(core_vars, recoded_vars)
}

