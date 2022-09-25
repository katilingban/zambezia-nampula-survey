################################################################################
#
#'
#' Recode ICFI indicators
#'
#
################################################################################

icfi_calculate_score <- function(bf_recoded_data, 
                                 fg_recoded_data, 
                                 meal_recoded_data) {
  icfi_score <- bf_recoded_data[["bf_icfi"]] + 
    fg_recoded_data[["fg_icfi"]] +
    meal_recoded_data[["meal_icfi"]]
  
  icfi_good <- ifelse(icfi_score == 6, 1, 0)
  
  data.frame(icfi_score, icfi_good)
}

iycf_classify <- function(.data,
                          bf_recoded_data, 
                          fg_recoded_data, 
                          meal_recoded_data) {
  icfi_df <- icfi_calculate_score(
    bf_recoded_data, fg_recoded_data, meal_recoded_data
  )
  
  iycf_good <- ifelse(
    .data[["age_months"]] < 6 & bf_recoded_data[["bf_exclusive"]], 1,
    ifelse(
      .data[["age_months"]] %in% 6:24 & icfi_df[["icfi_good"]] == 1, 1, 0
    )
  )
  
  data.frame(
    bf_exclusive = bf_recoded_data[["bf_exclusive"]],
    icfi_df, 
    iycf_good)
}


iycf_recode <- function(.data,
                        bf_recoded_data, 
                        fg_recoded_data, 
                        meal_recoded_data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  age_months <- .data[["age_months"]]
  
  iycf_df <- iycf_classify(
    .data, bf_recoded_data, fg_recoded_data, meal_recoded_data
  )
  
  data.frame(core_vars, age_months, iycf_df)
}