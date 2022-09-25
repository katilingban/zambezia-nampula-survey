################################################################################
#
#'
#' Recode ICFI - meals
#' 
#' food_num	How many times did **${child_random_name}** eat any solid, 
#'   semi-solid or soft foods yesterday during the day or night?
#'
#
################################################################################

## Recode responses to specific child meals question ---------------------------

meal_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses to multiple child meals questions --------------------------

meal_recode_responses <- function(vars, .data, 
                                  na_values = 9,
                                  binary = FALSE) {
  x <- .data[vars]
  
  Map(
    f = meal_recode_response,
    x = x,
    na_values = na_values,
    binary = binary
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

## Recode meal frequency component of ICFI -------------------------------------

meal_calculate_icfi <- function(age_months, meal_frequency) {
  icfi_group <- cut(
    age_months, breaks = c(0, 5, 8, 11, 24), include.lowest = TRUE
  ) |>
    as.numeric()
  
  ifelse(
    icfi_group == 2, (meal_frequency == 1) + (meal_frequency > 1) * 2,
    ifelse(
      icfi_group == 3, (meal_frequency %in% 1:2) + (meal_frequency > 2) * 2,
      ifelse(
        icfi_group == 4, (meal_frequency == 2) + 
          (meal_frequency == 3) * 2 + (meal_frequency > 3) * 3, NA
      )
    )
  )
}

## Overall recode function -----------------------------------------------------

meal_recode <- function(vars, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  meal_frequency <- meal_recode_response(
    x = .data[[vars]], na_values = 9, binary = FALSE
  )
  
  meal_icfi <- meal_calculate_icfi(
    .data[["age_months"]], meal_frequency
  )
  
  data.frame(core_vars, meal_frequency, meal_icfi)
}

