################################################################################
#
#'
#' Recode PMTCT
#' 
#' PMTCT1	During your pregnancy with your last child or during your current 
#'   pregnancy, were you offered voluntary counseling and testing (VCT)?
#' PMTCT2	Did you receive the results?
#' PMTCT3	Were you offered medication to lower the chance of your child 
#'   getting HIV?
#'
#
################################################################################

## Recode resposes to a specific PMTCT indicator question ----------------------

pmtct_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses to multiple PMTCT indicator questions ----------------------

pmtct_recode_responses <- function(vars, .data,
                                   na_values = c(8, 9),
                                   binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = pmtct_recode_response,
    x = as.list(x),
    na_values = rep(list(na_values), length(vars)),
    binary = rep(list(binary), length(vars))
  ) |>
    dplyr::bind_cols()
}

## Overall recode function -----------------------------------------------------

pmtct_recode <- function(vars, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- pmtct_recode_responses(vars = vars, .data = .data)
  
  data.frame(core_vars, recoded_vars)
}

