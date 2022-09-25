################################################################################
#
#'
#' Recode mosquito net during pregnancy
#' 
#' IDK1	During your pregnancy with your last child or during your current 
#'   pregnancy, did you receive a mosquite net?
#' IDK2	During your pregnancy with your last child or during your current 
#'   pregnancy, did you sleep/do you sleep under a mosquito net?
#'
#
################################################################################

## Recode responses to pregnancy net indicator ---------------------------------

pnet_recode_response <- function(x, na_values, binary = TRUE) {
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

## Recode responses to pregnancy net indicators --------------------------------

pnet_recode_responses <- function(vars, .data,
                                  na_values = c(8, 9),
                                  binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = pnet_recode_response,
    x = as.list(x),
    na_values = rep(list(na_values), length(vars)),
    binary = rep(list(binary), length(vars))
  ) |>
    dplyr::bind_cols()
}

## Overall recode function -----------------------------------------------------

pnet_recode <- function(vars, .data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- pnet_recode_responses(vars = vars, .data = .data)
  
  data.frame(core_vars, recoded_vars)
}

