################################################################################
#
#'
#' Recode ICFI - breastfeeding indicators
#' 
#' ebf_ever	Was **${child_random_name}** ever breastfed?
#' ebf_eibf	How long after birth was **${child_random_name}** first put to the 
#'   breast?
#' ebf_eibf_hr	Please record "Hours"
#' ebf_eibf_day	Please record "Days"
#' ebf_oth_liquid	In the first two days after delivery, was 
#'   **${child_random_name}** given anything other than breast milk to eat or 
#'   drink – anything at all like water, infant formula?
#' bf_yest	Was **${child_random_name}** breastfed yesterday during the day or 
#'   at night?
#' bf_bottle	"Did **${child_random_name}** drink anything from a bottle with 
#'   a nipple yesterday during the day or at night?"
#' liquid_water	Plain water?
#' liquid_bms	Infant formula, such as ...?
#' liquid_bms_num	If “yes”: How many times did **${child_random_name}** drink 
#'   formula?
#' liquid_yogurt	Yogurt drinks such as ...?
#' liquid_yogur_num	If “yes”: How many times did **${child_random_name}** 
#'   drink yogurt?
#' liquid_yogurt_sweet	If “yes”: Was the yogurt or were any of the yogurt 
#'   drinks a sweet or flavoured type of yogurt drink?
#' liquid_chocolate	Chocolate-flavoured drinks including those made from syrups 
#'   or powders?
#' liquid_juice	Fruit juice or fruit-flavoured drinks including those made from 
#'   syrups or powders?
#' liquid_drink	Sodas, malt drinks, sports drinks or energy drinks?
#' liquid_tea	Tea, coffee, or herbal drinks?
#' liquid_tea_sweet	If “yes”: Was the drink/ Were any of these drinks sweetened?
#' liquid_broth	Clear broth or clear soup?
#' liquid_other	Any other liquids?
#' liquid_other_specify	If “yes”: what was the liquid or what were the liquids?
#' liquid_other_sweet	If “yes”: Was the drink or were any of these drinks 
#'   sweetened?
#'   
#'
#
################################################################################

## Recode responses for specific breastfeeding indicator question --------------

bf_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses for multiple breastfeeding indicator questions -------------

bf_recode_responses <- function(vars, .data, 
                                na_values = c(rep(list(c(8, 9)), 2),
                                              rep(list(c("88", "99")), 2),
                                              rep(list(c(8, 9)), 5),
                                              list(c("88", "99")),
                                              list(c(8, 9)),
                                              list(c("88", "99")),
                                              rep(list(c(8, 9)), 9)),
                                binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = bf_recode_response,
    x = as.list(x),
    na_values = na_values,
    binary = c(
      list(binary), rep(list(FALSE), 3), rep(list(binary), 5), list(FALSE),
      list(binary), list(FALSE), rep(list(binary), 9)
    ) 
  ) |>
    dplyr::bind_cols() |>
    (\(x) 
      {
        data.frame(
          age_months = .data[["age_months"]],
          x
        ) 
      } 
    )()
}

## Map breastfeeding indicator variables ---------------------------------------

bf_map_vars <- function(survey_codebook) {
  survey_codebook |>
    subset(
      subset = stringr::str_detect(vars, "ebf_|bf_|liquid_"),
      select = vars
    ) |>
    (\(x) x[["vars"]] )() |>
    (\(x) x[x != "liquid_other_specify"])()
}

bf_recode_liquids <- function(vars, .data) {
  x <- .data[vars]
  
  rowSums(x, na.rm = TRUE)
}

################################################################################
#
#'
#' Recode breastfeeding - Exlusive breastfeeding
#'
#
################################################################################

bf_recode_exclusive <- function(vars = bf_vars_map[c(1, 6:21)], .data) {
  x <- .data[vars]
  
  n_liquids <- x |>
    subset(select = c(-age_months, -bf_yest)) |>
    rowSums(na.rm = TRUE)
  
  ifelse(
    x[[vars[1]]] >= 6, NA,
    ifelse(
      x[[vars[3]]] == 1 & n_liquids == 0, 1, 0
    )
  )
}


################################################################################
#
#'
#' Recode breastfeeding - continuing breastfeeding
#'
#
################################################################################

bf_recode_continuing <- function(vars = c("age_months", "bf_yest"), .data) {
  x <- .data[vars]
  
  ifelse(
    x[[vars[1]]] < 6, NA, x[[vars[2]]]
  )
}


################################################################################
#
#'
#' Recode breastfeeding - early initiation of breastfeeding
#'
#
################################################################################

bf_recode_early <- function(vars = c("age_months", "ebf_eibf", 
                                     "ebf_eibf_hr", "ebf_eibf_day"), 
                            .data) {
  x <- .data[vars]
  
  ifelse(
    x[[vars[1]]] > 24, NA,
    ifelse(
      x[[vars[2]]] == 0 | x[[vars[3]]] <= 1, 1, 0
    )
  )
}

## Recode breastfeeding indicator for ICFI -------------------------------------

bf_calculate_icfi <- function(vars = c("age_months", "bf_yest"), .data) {
  x <- .data[vars]
  
  bf_continuing <- ifelse(
    x[[vars[1]]] < 6, NA, x[[vars[2]]]
  )
  
  icfi_group <- cut(
    x[[vars[1]]], breaks = c(0, 5, 8, 11, 24), include.lowest = TRUE
  ) |>
    as.numeric()
  
  bf_icfi <- ifelse(icfi_group %in% 2:3, bf_continuing * 2, bf_continuing)
  
  bf_icfi
}

################################################################################
#
#'
#' Recode breastfeeding
#'
#
################################################################################

bf_recode <- function(vars = bf_vars_map, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  bf_df <- bf_recode_responses(vars = vars, .data = .data)
  
  vars <- c("age_months", vars)
  
  recoded_vars <- data.frame(
    bf_ever = bf_df[[vars[2]]],
    bf_early = bf_recode_early(.data = bf_df),
    bf_exclusive = bf_recode_exclusive(
      vars = vars[c(1, 6:21)], .data = bf_df
    ),
    bf_continuing = bf_recode_continuing(.data = bf_df),
    bf_icfi = bf_calculate_icfi(.data = bf_df)
  )
  
  data.frame(core_vars, recoded_vars)
}

