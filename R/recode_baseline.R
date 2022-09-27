################################################################################
#
#'
#' Get household/respondent data only
#'
#
################################################################################

get_respondent_data <- function(.data) {
  .data |>
    dplyr::group_by(sbjnum) |>
    subset(cid == 1) |>
    dplyr::ungroup()
}



################################################################################
#
#'
#' Recode select one categorial responses with NAs
#'
#
################################################################################

recode_var_categorical <- function(var, 
                                   na_values = c("[NÃO LER] Não sabe", 
                                                 "[NÃO LER] Não responde",
                                                 "NR", "NS","NSA"),
                                   replacement = "No response") {
  haven::as_factor(var) |>
    (\(x)
     {
       levels(x) <- ifelse(
         levels(x) %in% na_values,
         replacement,
         levels(x)
       )
       x
    }
    )()
}


recode_vars_categorical <- function(vars, .data, 
                                    na_values = c("[NÃO LER] Não sabe", 
                                                  "[NÃO LER] Não responde",
                                                  "NR", "NS","NSA"),
                                    replacement = "No response") {
  x <- .data[vars]
  
  apply(
    X = x,
    MARGIN = 2,
    FUN = recode_var_categorical,
    na_values = na_values,
    replacement = replacement,
    simplify = TRUE
  )
}


recode_age_respondent <- function(age, birth_date, survey_date) {
  reported_age <- as.integer(age)
  calculated_age <- ((as.Date(survey_date) - as.Date(birth_date)) / 365.25) |>
    floor() |>
    as.integer()
  
  ifelse(
    is.na(reported_age), calculated_age, reported_age
  )
}

recode_age_group_respondent <- function(age, reported_age) {
  x <- cut(
    x = age,
    breaks = c(seq(from = 15, to = 50, by = 5), Inf),
    labels = c("15 to 19 years", "20 to 24 years", "25 to 29 years", 
               "30 to 34 years", "35 to 39 years", "40 to 44 years", 
               "45 to 49 years", "50 years or more"),
    include.lowest = TRUE, right = FALSE
  )
  
  ifelse(
    is.na(x) & haven::as_factor(reported_age) %in% c("NS", "NR"),
    "No response", as.character(x)
  ) |>
    factor(levels = c(levels(x), "No response"))
}


recode_education_years <- function(education_years) {
  as.integer(education_years)
}

recode_education_group <- function(education_years, reported_years) {
  x <- cut(
    x = education_years,
    breaks = c(0, 6, 12, Inf),
    labels = c("0 to 5 years", "6 to 11 years", "12 or more years"),
    include.lowest = TRUE, right = FALSE
  )
  
  ifelse(
    is.na(x) & haven::as_factor(reported_years) %in% c("NS", "NR"),
    "No response", as.character(x)
  ) |>
    factor(levels = c(levels(x), "No response"))
}


recode_age_child <- function(age, birth_date, survey_date) {
  reported_age <- as.integer(age)
  calculated_age <- ((as.Date(survey_date) - as.Date(birth_date)) / 365.25) |>
    floor() |>
    as.integer()
  
  ifelse(
    is.na(reported_age), calculated_age, reported_age
  )
}

recode_age_group_child <- function(age, reported_age) {
  x <- cut(
    x = age,
    breaks = c(0, 6, 12, 24, 36, 48, 60),
    labels = c("0 to 5 months", "6-11 months", "12 to 23 months",
               "24 to 35 months", "36 to 47 months", "48 to 59 months"),
    include.lowest = TRUE, right = FALSE
  )
  
  ifelse(
    is.na(x) & haven::as_factor(reported_age) %in% c("NS", "NR"),
    "No response", as.character(x)
  ) |>
    factor(levels = c(levels(x), "No response"))
}

recode_breastfeeding_child <- function(bf, 
                                       na_values = c("[NÃO LER] Não sabe", 
                                                     "[NÃO LER] Não responde",
                                                     "NR", "NS","NSA")) {
  ifelse(
    bf %in% na_values, "NA_integer_",
    ifelse(
      bf == 2, 1, 0
    )
  )
}


recode_age_parent_at_birth <- function(age) {
  x <- cut(
    x = age,
    breaks = c(0, 15, 20, 25, 30, 35, 40, 45, 50, Inf),
    labels = c("less than 15 years", "15 to 19 years", "20 to 24 years",
               "25 to 29 years", "30 to 34 years", "35 to 39 years",
               "40 to 44 years", "45 to 49 years", "50 years or more"),
    include.lowest = TRUE, right = FALSE
  )
  
  ifelse(
    is.na(x) & haven::as_factor(age) %in% c("NS", "NR"),
    "No response", as.character(x)
  ) |>
    factor(levels = c(levels(x), "No response"))
}

recode_age_group_spouse <- function(age, reported_age) {
  x <- cut(
    x = age,
    breaks = c(seq(from = 15, to = 70, by = 5), Inf),
    labels = c("15 to 19 years", "20 to 24 years", "25 to 29 years", 
               "30 to 34 years", "35 to 39 years", "40 to 44 years", 
               "45 to 49 years", "50 to 54 years", "55 to 59 years", 
               "60 to 64 years", "65 to 69 years", "70 years or more"),
    include.lowest = TRUE, right = FALSE
  )
  
  ifelse(
    is.na(x) & haven::as_factor(reported_age) %in% c("NS", "NR"),
    "No response", as.character(x)
  ) |>
    factor(levels = c(levels(x), "No response"))
}


recode_time_in_location <- function(time) {
  as.integer(time) |>
    (\(x) ifelse(x %in% c(888886:888887), NA, x))()
}


recode_time_in_location_group <- function(time, reported_time) {
  x <- cut(
    x = time,
    breaks = c(0, 1, 6, 12, 24, 36, 48, 60, 72, 84, 96, 108, 121, Inf),
    labels = c("less than 1 month", "1 to 5 months", "6 to 11 months", 
               "12 to 23 months", "24 to 35 months", "36 to 47 months", 
               "48 to 59 months", "60 to 71 months", "72 to 83 monhs", 
               "84 to 95 months", "96 to 107 months", "108 to 120 months",
               "more than 10 years"),
    include.lowest = TRUE, right = FALSE
  )
  
  ifelse(
    is.na(x) & haven::as_factor(reported_time) %in% c("888886", "888887"),
    "No response", as.character(x)
  ) |>
    factor(levels = c(levels(x), "No response"))
}


recode_group_type <- function(group, group_name) {
  ifelse(
    group == 2, "Not a member of any group",
    ifelse(
      group %in% c("NS", "NR", "NSA"), "No response", group_name
    )
  )
}


recode_presentation_type <- function(presentation, presentation_name) {
  ifelse(
    presentation == 2, "Not a member of any group",
    ifelse(
      presentation %in% c("NS", "NR", "NSA"), "No response", presentation_name
    )
  )
}