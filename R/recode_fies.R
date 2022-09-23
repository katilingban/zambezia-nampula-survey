################################################################################
#
#' 
#' Process and recode Food Insecurity Experience Scale (FIES) data/indicators
#'
#' Relevant variables:
#'
#'   fies01 You or others in your household worried about not having enough 
#'     food to eat because of a lack of money or other resources? 1=Yes; 2=No
#'
#'   fies02 Still thinking about the last 30 DAYS, was there a time when you 
#'     or others in your household were unable to eat healthy and nutritious 
#'     food because of a lack of money or other resources? 1=Yes; 2=No
#'
#'   fies03 Was there a time when you or others in your household ate only a 
#'     few kinds of foods because of a lack of money or other resources?
#'    1=Yes; 2=No
#'
#'   fies04 Was there a time when you or others in your household had to skip 
#'     a meal because there was not enough money or other resources to get food?
#'     1=Yes; 2=No
#'
#'   fies05 Still thinking about the last 30 DAYS, was there a time when you or 
#'     others in your household ate less than you thought you should because of 
#'     a lack of money or other resources? 1=Yes; 2=No
#'
#'   fies06 Was there a time when your household ran out of food because of a 
#'     lack of money or other resources? 1=Yes; 2=No
#'
#'   fies07 Was there a time when you or others in your household were hungry 
#'    but did not eat because there was not enough money or other resources for 
#'    food? 1=Yes; 2=No
#'
#'   fies08 Was there a time when you or others in your hou sehold went without 
#'     eating for a whole day because of a lack of money or other resources?
#'     1=Yes; 2=No
#'     
#
################################################################################

## Recode responses to individual FIES question --------------------------------

fies_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses to multiple FIES questions ---------------------------------

fies_recode_responses <- function(vars, .data, 
                                  na_values = c(8, 9), 
                                  binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = fies_recode_response,
    x = as.list(x),
    na_values = rep(list(na_values), length(vars)),
    binary = rep(list(binary), length(vars)) 
  ) |>
    dplyr::bind_cols()
}

## Calculate FIES score --------------------------------------------------------

fies_calculate_score <- function(fies_df, na_rm = FALSE, add = TRUE) {
  fies_score <- rowSums(fies_df, na.rm = na_rm)
  
  if (add) {
    fies_score <- data.frame(fies_df, fies_score)
  } else {
    fies_score
  }
  
  fies_score
}

## Overall recode function -----------------------------------------------------

fies_recode <- function(vars = paste0("fies0", 1:8), 
                        .data, 
                        na_values = c(8, 9), 
                        na_rm = TRUE) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- fies_recode_responses(
    vars = vars,
    .data = .data,
    na_values = na_values,
    binary = TRUE
  ) |>
    fies_calculate_score(na_rm = na_rm, add = TRUE)
  
  data.frame(core_vars, recoded_vars)
}

