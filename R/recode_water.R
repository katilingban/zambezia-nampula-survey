################################################################################
#
#' 
#' Process and recode water data/indicators
#'
#' Relevant variables:
#'
#'   wt2 - Usually, where do you get your drinking water for the family members 
#'     of the household? integer variables for the following categorial values:
#'     
#'     1=House faucet; 
#'     2=Yard faucet; 
#'     3=Neighbours faucet; 
#'     4=Public faucet; 
#'     5=Water pump; 
#'     6=Well with lid; 
#'     7=Open well; 
#'     8=Protected stream; 
#'     9=Non-protected stream; 
#'     10=Rain; 
#'     11=River/lake/lagoon; 
#'     12=Bottled; 
#'     13=Other; 
#'     88=Don't know; 
#'     99=No response
#'
#'   wt2_other - Specify other source of drinking water; text entry; will need
#'     translation
#'
#'   wt3 - Who usually goes to this source to collect water for the family?
#'     integer variables for the following categorial values:
#'   
#'     1=Adult males; 
#'     2=Adult females; 
#'     3=Young girls; 
#'     4=Young boys; 
#'     5=Other; 
#'     88=Don't know; 
#'     99=No response
#'
#'   wt3a - How long does it take for this person to go collect water, counting 
#'     total time of travel to go, come back, and collect water? integer
#'
#'   wt3b	How many times has this person collected water in the past 7 days?
#'     integer
#'
#'   wt4 - In the last month, has there been a moment in which your household 
#'     did not have sufficient water to drink?
#'     
#'     1=Yes, at least one time; 
#'     2=No, it was always sufficient; 
#'     88=Don't know; 
#'     99=No response
#'
#'   wt4a - What was the principal reason for not being able to access water in 
#'     a sufficient quantity?
#'
#'     1=Water not available at the source; 
#'     2=Water is too expensive; 
#'     3=Source not accessible; 
#'     88=Don't know; 
#'     99=No response
#'
#'   wt5 - Have you ever done anything to the water to make it cleaner to drink?
#'
#'     1=Yes; 
#'     2=No
#'
#'   wt6 - What do you normally do to make your water cleaner to drink?
#'
#'     1=Boil; 
#'     2=Lixivia/chlorine; 
#'     3=Certeza; 
#'     4=Filter with cloth; 
#'     5=Use water filter (ceramic, sand, compost, etc); 
#'     6=Solar disinfection; 
#'     7=Let stand and sit; 
#'     8=Other; 
#'     88=Don't know; 
#'     99=No response   
#'     
#
################################################################################

## Recode responses from a specific question -----------------------------------

water_recode_response <- function(x, na_values) {
  ifelse(x %in% na_values, NA, x)
}

## Recode responses from multiple questions ------------------------------------

water_recode_responses <- function(vars, .data, na_values) {
  x <- .data[vars]
  
  water_df <- apply(
    X = x,
    MARGIN = 2,
    FUN = water_recode_response,
    na_values = na_values,
    simplify = TRUE
  ) |>
    data.frame()
  
  water_df[ , "wt4"] <- ifelse(water_df[ , "wt4"] == 2, 1, 0)
  
  water_df[ , "wt5"] <- ifelse(
    water_df[ , "wt5"] %in% 8:9, NA,
    ifelse(water_df[ , "wt5"] == 2, 0, 1)
  )
  
  water_df
}

################################################################################
#
#'
#' Recode JMP ladder indicators
#' 
#'   surface water
#'   unimproved water source
#'   improved but limited water source
#'   basic water source
#'   sufficient water source
#'
#
################################################################################

water_recode_surface <- function(vars, .data) {
  x <- .data[[vars]]
  
  ifelse(x == 11, 1, 0)
}

water_recode_unimproved <- function(vars, .data) {
  x <- .data[[vars]]
  
  ifelse(x %in% c(7, 9), 1, 0)
}


water_recode_limited <- function(vars, .data) {
  x <- .data[vars]
  
  ifelse(
    x[[vars[1]]] %in% c(1:6, 8, 10, 12) & x[[vars[2]]] > 30, 1, 0
  )
}

water_recode_basic <- function(vars, .data) {
  x <- .data[vars]
  
  ifelse(
    x[[vars[1]]] %in% c(1:6, 8, 10, 12) & x[[vars[2]]] <= 30, 1, 0
  )
}

water_recode_sufficient <- function(vars, .data) {
  x <- .data[vars]
  
  ifelse(
    x[[vars[1]]] %in% 1:2 & x[[vars[2]]] == 1, 1, 0
  )
}

################################################################################
#
#'
#' Recode water collector
#'
#
################################################################################

water_recode_collector <- function(vars, .data,
                                   prefix = "water_collector",
                                   label = c("men", "women", "boys", 
                                             "girls", "others")) {
  x <- .data[[vars]]
  
  data.frame(
    water_collector = x,
    spread_vector_to_columns(
      x = x,
      fill = 1:5,
      na_rm = FALSE,
      prefix = prefix
    ) |>
      (\(x) { names(x) <- paste0(prefix, "_", label); x } )()
  )
}

################################################################################
#
#'
#' Recode water sufficiency
#'
#
################################################################################

water_recode_sufficiency <- function(vars, .data,
                                    prefix = "water_sufficient_reasons") {
  x <- .data[vars]
  
  data.frame(
    water_sufficiency = x[[1]],
    water_sufficient_reasons = x[[2]],
    spread_vector_to_columns(
      x = x[[2]],
      fill = 1:3,
      na_rm = FALSE,
      prefix = prefix
    )
  )
}

################################################################################
#
#'
#' Recode water filter
#'
#
################################################################################

water_recode_filter <- function(vars, .data) {
  x <- .data[[vars]]
  
  ifelse(x %in% c(1:3, 5:6), 1, 0)
}

## Overall recode function -----------------------------------------------------

water_recode <- function(vars, .data, na_values) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  water_df <- water_recode_responses(
    vars = vars, 
    .data = .data, 
    na_values = na_values
  )
  
  recoded_vars <- data.frame(
    water_surface = water_recode_surface(
      vars = "wt2", .data = water_df
    ),
    water_unimproved = water_recode_unimproved(
      vars = "wt2", .data = water_df
    ),
    water_limited = water_recode_limited(
      vars = c("wt2", "wt3a"), .data = water_df
    ),
    water_basic = water_recode_basic(
      vars = c("wt2", "wt3a"), .data = water_df
    ),
    water_sufficient = water_recode_sufficient(
      vars = c("wt2", "wt4"), .data = water_df
    ),
    water_recode_sufficiency(
      vars = c("wt4", "wt4a"), .data = water_df
    ),
    water_recode_collector(
      vars = "wt3", .data = water_df
    ),
    water_collection_time = water_df[["wt3a"]],
    water_filter_use = water_df[["wt5"]],
    water_filter_adequate = water_recode_filter(
      vars = "wt6", .data = water_df
    )
  )
  
  data.frame(core_vars, recoded_vars)
}
