################################################################################
#
#'
#' Miscellaneous recode functions for endline data
#'
#
################################################################################

refactor_var_categorical <- function(x, y, choices) {
  factor(
    x = x,
    levels = choices |>
      subset(list_name == y) |>
      (\(x) x[["name"]])(),
    labels = choices |>
      subset(list_name == y) |>
      (\(x) x[["label::English (en)"]])()
  )
}


refactor_age_group <- function(x, breaks, age_group_labels) {
  cut(
    x = x,
    breaks = breaks,
    labels = age_group_labels,
    include.lowest = TRUE, right = FALSE
  ) |>
    (\(x) ifelse(is.na(x), "No response", as.character(x)))() |>
    factor(levels = c(age_group_labels, "No response"))
}


calculate_age_child <- function(date_of_birth, 
                                survey_date, 
                                reported_age_months,
                                age_units = c("days", "months", "years")) {
  age_units <- match.arg(age_units)
  
  age_days <- (as.Date(survey_date) - as.Date(date_of_birth)) |>
    as.numeric()
  age_months <- (age_days / (365.25 / 12)) |>
    (\(x) ifelse(is.na(x), reported_age_months, x))()
  age_years <- (age_days / 365.25) |>
    (\(x) ifelse(is.na(x), reported_age_months / 12, x))()
  age_days <- ifelse(
    is.na(age_days), reported_age_months * (365.25 / 12), age_days
  )

  if (age_units == "days") {
    age <- age_days
  }
  
  if (age_units == "months") {
    age <- age_months
  }
  
  if (age_units == "years") {
    age <- age_years
  }
  
  age
}