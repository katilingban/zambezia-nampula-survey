################################################################################
#
#'
#' Get associations indicator set variables
#'
#
################################################################################

get_association_variables <- function(raw_data_clean) {
  raw_data_clean |>
    subset(
      select = c(
        id, spid, district, ea_code, geolocation, 
        mother_id, mother_age, mother_carer_sex,
        q05, q05_specify, q06, q06_content, q06a, q06b, q07, q07_specify
      )
    )
}


translate_association_text <- function(raw_association_df) {
  
}