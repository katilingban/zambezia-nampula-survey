################################################################################
#
#'
#' Recode mother characteristics indicators
#' 
#' mother_age	What is your age?
#' resp_marital_status	What is your marital status?
#' resp_edu	How many years of education have you completed?
#' q02	Does your husband/partner actually live in this household?
#' q02a	What is the age of your husband/partner?
#' q02b	What is the highest educational attainment of your husband/partner?
#' q03	For how many years has your family (household) lived in this place?
#'
#
################################################################################

carer_recode_age <- function(x, na_values) {
  ifelse(x %in% na_values, NA, x)
}

carer_recode_marital_status <- function(x, na_values,
                                        status = c("single", "married", 
                                                   "civil_union", 
                                                   "divorced_separated",
                                                   "widowed"),
                                        prefix) {
  x <- ifelse(x %in% na_values, NA, x)
  
  marital_status <- data.frame(
    marital_status = x, 
    spread_vector_to_columns(
      x = x,
      fill = 1:5,
      na_rm = FALSE,
      prefix = prefix
    ) |>
      (\(x) { names(x) <- paste0(prefix, "_", status); x })()
  )
  
  marital_status
}


carer_recode_education <- function(x, na_values,
                                   edu_level = c(paste0("grade", 1:12),
                                                 "professional", "non_college",
                                                 "college", "literacy"),
                                   prefix) {
  x <- ifelse(x %in% na_values, NA, x)
  
  carer_education <- data.frame(
    x, 
    spread_vector_to_columns(
      x = x,
      fill = 1:16,
      na_rm = FALSE,
      prefix = prefix
    ) |>
      (\(x) { names(x) <- paste0(prefix, "_", edu_level); x })()
  )
  
  names(carer_education)[1] <- paste0(prefix, "_education")
  
  carer_education
}


carer_recode <- function(.data, 
                         age_na_values, marital_na_values,
                         education_na_values) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  carer_age <- carer_recode_age(
    x = .data[["mother_age"]], na_values = age_na_values
  )
  
  carer_marital_status <- carer_recode_marital_status(
    x = .data[["resp_marital_status"]], 
    na_values = marital_na_values,
    prefix = "carer"
  )
  
  carer_education <- carer_recode_education(
    x = .data[["resp_edu"]], 
    na_values = education_na_values,
    prefix = "carer"
  )
  
  carer_with_partner <- ifelse(
    .data[["q02"]] %in% c(8, 9), NA,
    ifelse(
      .data[["q02"]] == 2, 0, 1
    )
  )
  
  partner_age <- carer_recode_age(
    x = .data[["q02a"]], na_values = age_na_values
  )
  
  partner_education <- carer_recode_education(
    x = .data[["q02b"]], 
    na_values = education_na_values,
    prefix = "partner"
  )
  
  data.frame(
    core_vars, carer_age, carer_marital_status, carer_education,
    carer_with_partner, partner_age, partner_education
  )
}


