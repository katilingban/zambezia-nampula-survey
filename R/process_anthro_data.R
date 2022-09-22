################################################################################
#
#'
#' Get anthropometric data from survey_fin.dta
#' 
#' @param .data Raw Stata data from survey_fin.dta. Default to 
#'   `raw_data_stata_with_anthro`.
#'
#' @return A data.frame with anthropometric data for each child
#'
#
################################################################################

get_anthro_data <- function(.data = raw_data_stata_with_anthro) {
  ## Unique identifier
  id <- paste0(.data$sbjnum, .data$cid)
  
  ## Province
  province <- .data$prov
  
  ## Districts
  district <- .data$distrito
  
  ## Enumeration area
  ea <- .data$enum1
  
  ## Get age in months
  age <- .data$pb
  
  ## Sex
  sex <- .data$pg
  
  ## Get weight
  weight <- .data$cpeso
  
  ## Get height or length
  height <- ifelse(is.na(.data$ccomprimento), .data$caltura, .data$ccomprimento)
  
  ## Get MUAC
  muac <- .data$cbraco
  
  ## Oedema
  oedema <- .data$cmalnut
  
  ## Standing variable
  standing <- ifelse(is.na(.data$ccomprimento), 1, 2)
  
  ## Concatenate
  anthro_data <- tibble::tibble(
    id, province, district, ea, age, sex, weight, height, muac, oedema, standing
  )
  
  ## Return df
  anthro_data
}