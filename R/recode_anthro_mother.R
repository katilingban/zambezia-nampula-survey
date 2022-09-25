################################################################################
#
#'
#' Recode mother anthro indicators
#'
#
################################################################################

recode_anthro_mother <- function(raw_data_clean, 
                                 bmi_labs = FALSE, 
                                 muac_labs = FALSE) {
  x <- raw_data_clean |>
    subset(
      select = c(
        id, spid, district, ea_code, geolocation, 
        mother_age, mweight, mheight, mmuac, wh1
      )
    ) |>
    dplyr::mutate(
      bmi = mweight / ((mheight / 100) ^ 2),
      bmi_class = classify_bmi(bmi, labs = bmi_labs),
      bmi_underweight = ifelse(bmi_class != 1, 0, 1),
      bmi_overweight = ifelse(bmi_class != 3, 0, 1),
      bmi_obese = ifelse(bmi_class != 4, 0, 1),
      muac_class = classify_muac(mmuac, labs = muac_labs),
      muac_undernutrition = ifelse(muac_class == 1, 1, 0)
    )
  
  ## Return
  x
}



################################################################################
#
#'
#' Classify BMI
#'
#
################################################################################

classify_bmi <- function(bmi, labs = c("underweight", "normal", 
                                       "overweight", "obese")) {
  x <- cut(
    x = bmi,
    breaks = c(0, 18.5, 25, 30, Inf),
    labels = labs,
    include.lowest = TRUE
  )
  
  x
}


################################################################################
#
#'
#' Classify MUAC
#'
#
################################################################################

classify_muac <- function(muac, labs = c("acute malnutrition", "normal")) {
  x <- cut(
    x = muac,
    breaks = c(0, 18.5, Inf),
    labels = labs,
    include.lowest = TRUE
  )
  
  x
}