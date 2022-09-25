################################################################################
#
#'
#' Recode pre- and post-natal care
#' 
#' SPC1	Where was the delivery of your last child completed?
#' SPC2	How many pre-natal care visits did you complete during your last 
#'   pregnancy or your current pregnancy?
#' SPC2a	Were you treated well by the staff at the health facility during 
#'   your pre-natal visits?
#' SPC2b	Were you treated well by the staff at the health facility when you 
#'   delivered your last child?
#' SPC3	Who assisted the delivery of your last child?
#' SPC4	Would you return to the health facility to deliver your next child?
#' SPC5	Did you have any difficulty or problems reaching the facility?
#' SPC5a	Which difficulties?
#' SPC6	How long after the birth of your last child did you go to the facility 
#'   for a check-up of the baby?
#' SPC6a	How many hours after birth of your last child did you go to the 
#'   facility for a check-up of the baby?
#' SPC6b	How many days after birth of your last child did you go to the 
#'   facility for a check-up of the baby?
#' SPC7	How long after the birth of your last child did you go to the facility 
#'   for a check-up of yourself?
#' SPC7a	How many hours after birth of your last child did you go to the 
#'   facility for a check-up yourself?
#' SPC7b	How many days after birth of your last child did you go to the 
#'   facility for a check-up of yourself?
#' THER1	After the birth of your last child, did he/she receive protection 
#'   against the cold (cleaning/drying, direct skin to skin contact with the 
#'   mother, covering the body and head of the child)?
#'
#
################################################################################

## Recode responses to an individual natal care question -----------------------

nc_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses to multiple natal care questions ---------------------------

nc_recode_responses <- function(vars, .data,
                                na_values = c(rep(list(c(88, 99)), 2),
                                              rep(list(c(8, 9)), 2),
                                              list(c(88, 99)),
                                              rep(list(c(8, 9)), 2),
                                              rep(list(c(88, 99)), 7),
                                              list(c(8, 9))),
                                binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = nc_recode_response,
    x = as.list(x),
    na_values = na_values,
    binary = as.list(c(rep(FALSE, 2), rep(binary, 2), FALSE, 
                     rep(binary, 2), rep(FALSE, 7), binary))
  ) |>
    dplyr::bind_cols()
}

## Recode responses for where most recent birth happened -----------------------

nc_recode_location <- function(vars, .data, 
                               fill = 1:5, na_rm = FALSE, 
                               prefix = "delivery_location") {
  x <- .data[[vars]]
  
  data.frame(
    delivery_location = x,
    spread_vector_to_columns(
      x = x,
      fill = fill,
      na_rm = na_rm,
      prefix = prefix
    )
  )
}

## Recode antenatal care coverage ----------------------------------------------

nc_recode_anc <- function(vars, .data) {
  x <- .data[[vars]]
  
  #anc_one <- ifelse(x[[vars[1]]] != 1 & x[[vars[2]]] %in% c(1:3, 5:6), 1, 0)
  #anc_four <- ifelse(x[[vars[1]]] %in% 4:5, 1, 0)
  anc_four <- ifelse(x %in% 4:5, 1, 0)
  
  #data.frame(anc_one, anc_four)
  anc_four
}

## Recode responses to who assisted in delivery of child -----------------------

nc_recode_assist <- function(vars, .data, 
                             fill = 1:9, na_rm = FALSE, 
                             prefix = "delivery_assist") {
  x <- .data[[vars]]
  
  x <- stringr::str_remove_all(x, pattern = " 88|88| 99|99")
  
  data.frame(
    delivery_assist = x,
    split_select_multiples(
      x = x,
      fill = fill,
      na_rm = na_rm,
      prefix = prefix
    )
  )
}

## Recode responses to difficulties during delivery ----------------------------

nc_recode_difficulties <- function(vars, .data,
                                   fill = 1:6, na_rm = FALSE,
                                   prefix = "delivery_difficulty") {
  x <- .data[[vars]]
  
  data.frame(
    delivery_difficulty = x,
    spread_vector_to_columns(
      x = x,
      fill = fill,
      na_rm = na_rm,
      prefix = prefix
    )
  )
}

## Recode post-natal care indicators -------------------------------------------

nc_recode_pnc <- function(vars, .data, prefix) {
  x <- .data[vars]
  
  days_to_pnc <- ifelse(
    x[[vars[1]]] == 1, 0,
    ifelse(
      x[[vars[1]]] == 2, x[[vars[2]]] / 24, x[[vars[3]]]
    )
  )
  
  pnc_check <- ifelse(
    days_to_pnc == 0, 1,
    ifelse(
      days_to_pnc > 0 & days_to_pnc <= 2, 2,
      ifelse(
        days_to_pnc > 2 & days_to_pnc < 42, 3, 0
      )
    )
  )
  
  data.frame(
    days_to_pnc,
    pnc_check,
    spread_vector_to_columns(
      x = pnc_check,
      fill = 0:3,
      na_rm = FALSE,
      prefix = prefix
    )
  )
}

## Overall recode function -----------------------------------------------------

nc_recode <- function(vars, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  nc_df <- nc_recode_responses(
    vars = vars, .data = .data
  )
  
  recoded_vars <- data.frame(
    nc_recode_location(vars = vars[1], .data = nc_df),
    anc_four = nc_recode_anc(vars = vars[2], .data = nc_df),
    anc_well = nc_df[[vars[3]]],
    delivery_well = nc_df[[vars[4]]],
    nc_recode_assist(vars = vars[5], .data = nc_df),
    delivery_return = nc_df[[vars[6]]],
    nc_recode_difficulties(vars = vars[8], .data = nc_df),
    nc_recode_pnc(vars = vars[9:11], .data = nc_df, prefix = "pnc_mother") |>
      (\(x) { names(x)[1:2] <- paste0("mother_", names(x)[1:2]); x } )(),
    nc_recode_pnc(vars = vars[12:14], .data = nc_df, prefix = "pnc_child") |>
      (\(x) { names(x)[1:2] <- paste0("child_", names(x)[1:2]); x } )(),
    nc_protect = nc_df[[vars[15]]]
  )
  
  data.frame(core_vars, recoded_vars)
}
