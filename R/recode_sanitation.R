################################################################################
#
#' 
#' Process and recode sanitation data/indicators
#'
#' Relevant variables:
#'
#'   lusd1 - Does the family in this household use a latrine? integer variables
#'     for the following categorical values:
#'     
#'     1=Yes; 
#'     2=No
#'
#'   lusd2 - Do you share this latrine with other families? integer variables
#'     for the following categorical values:
#'
#'     1=Yes; 
#'     2=No
#'
#'   lusd3 - How many families use this latrine? text entry
#'
#'   lusd4- What type of latrine does this household own? integer variables
#'     for the following categorical values:
#'
#'     1=Toilet with flush; 
#'     2=Toilet without flush; 
#'     3=Improved latrine; 
#'     4=Traditional improved latrine; 
#'     5=Unimproved latrine; 
#'     6=Do not have a latrine/open defecation; 
#'     88=Don't know; 
#'     99=No response
#'
#'   lusd5 - Does this latrine have a lid? integer variables for the following
#'     categorical values:
#'
#'     1=Yes; 
#'     2=No
#'
#'  lusd6 - Where is the latrine located? integer variables for the following
#'    categorical values:
#'
#'    1=In the house; 
#'    2=In the yard; 
#'    3=In another place; 
#'    88=Don't know; 
#'    99=No response
#'
#'   lusd7 - Are there members of this family who defecate in the garden/farm 
#'     where you cultivate your agricultural products to eat? integer variables
#'     for the following categorical values:
#'
#'     1=Yes; 
#'     2=No
#'
#'   lusd8 - Have you seen anyone in your family defecate in the open in the 
#'     last 7 days? integer veriable for the following categorical values:
#'
#'     1=Yes; 
#'     2=No
#'     
#
################################################################################

## Recode responses to specific question ---------------------------------------

san_recode_response <- function(x, na_values) {
  ifelse(x %in% na_values, NA, x)
}

## Recode responses to multiple questions --------------------------------------

san_recode_responses <- function(vars, .data, na_values) {
  x <- .data[vars]
  
  san_df <- apply(
    X = x,
    MARGIN = 2,
    FUN = san_recode_response,
    na_values = na_values,
    simplify = TRUE
  ) |>
    data.frame()
  
  san_df[ , paste0("lusd", c(1:2, 5, 7, 8))] <- ifelse(
    san_df[ , paste0("lusd", c(1:2, 5, 7, 8))] == 2, 0, 1
  )
  
  san_df
}

################################################################################
#
#'
#' Sanitation ladder
#' 
#'   Open defecation
#'   Unimproved sanitation facilities
#'   Improved but limited sanitation facilities
#'   Basic sanitation facilities
#' 
#
################################################################################

san_recode_open <- function(vars, .data) {
  x <- .data[vars]
  
  ifelse(x[[vars[1]]] == 0 | is.na(x[[vars[2]]]), 1, 0)
}

san_recode_unimproved <- function(vars, .data) {
  x <- .data[[vars]]
  
  ifelse(x == 5, 1, 0)
}

san_recode_limited <- function(vars, .data) {
  x <- .data[vars]
  
  ifelse(x[[vars[1]]] != 5 & x[[vars[2]]] == 1, 1, 0)
}

san_recode_basic <- function(vars, .data) {
  x <- .data[vars]
  
  ifelse(x[[vars[1]]] == 5 & x[[vars[2]]] != 1, 1, 0)
}


################################################################################
#
#'
#' Overall recode function
#'
#
################################################################################

san_recode <- function(vars, .data, na_values) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  san_df <- san_recode_responses(
    vars = vars, 
    .data = .data, 
    na_values = na_values
  )
  
  recoded_vars <- data.frame(
    san_open = san_recode_open(vars = c("lusd1", "lusd4"), .data = san_df),
    san_unimproved = san_recode_unimproved(vars = "lusd4", .data = san_df),
    san_limited = san_recode_limited(vars = c("lusd2", "lusd4"), .data = san_df),
    san_basic = san_recode_basic(vars = c("lusd2", "lusd4"), .data = san_df),
    san_open_garden = san_df[["lusd7"]],
    san_open_past = san_df[["lusd8"]]
  )
  
  data.frame(core_vars, recoded_vars)
}

