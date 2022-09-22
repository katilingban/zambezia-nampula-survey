################################################################################
#
#'
#' Boxplot for weight, height and MUAC
#'
#
################################################################################

plot_anthro_outliers <- function(.data = anthro_data_review) {
  withr::with_par(
    new = list(par(mfrow = c(1, 3))),
    {
      with(.data, 
        {
          #boxplot(age, width = 5, ylab = "Age (months)")
          boxplot(weight, width = 3, ylab = "Weight (kgs)")
          boxplot(height, width = 3, ylab = "Height (cms)")
          boxplot(muac, width = 3, ylab = "MUAC (cms)")
        }
      )
    }
  )
}


################################################################################
#
#'
#' Plot bivariate outliers
#'
#
################################################################################

plot_anthro_bivariate <- function(.data = anthro_data_review,
                                  outlier_weight_height,
                                  outlier_weight_muac, 
                                  outlier_height_muac, 
                                  outlier_weight_age,
                                  outlier_height_age, 
                                  outlier_muac_age) {
  withr::with_par(
    new = list(par(mfrow = c(1, 3))),
    {
      with(
        .data,
        {
          plot(
            weight, height, 
            pch = ifelse(id %in% outlier_weight_height$id, 20, 1), 
            col = ifelse(id %in% outlier_weight_height$id, "red", "black")
          )
          plot(
            weight, muac,
            pch = ifelse(id %in% outlier_weight_muac$id, 20, 1), 
            col = ifelse(id %in% outlier_weight_muac$id, "red", "black")
          )
          plot(
            height, muac,
            pch = ifelse(id %in% outlier_height_muac$id, 20, 1), 
            col = ifelse(id %in% outlier_height_muac$id, "red", "black")
          )
          plot(
            weight, age,
            pch = ifelse(id %in% outlier_weight_age$id, 20, 1), 
            col = ifelse(id %in% outlier_weight_age$id, "red", "black")
          )
          plot(
            height, age,
            pch = ifelse(id %in% outlier_height_age$id, 20, 1), 
            col = ifelse(id %in% outlier_height_age$id, "red", "black")
          )
          plot(
            muac, age,
            pch = ifelse(id %in% outlier_muac_age$id, 20, 1), 
            col = ifelse(id %in% outlier_muac_age$id, "red", "black")
          )
        }
      )
    }
  )
}


################################################################################
#
#'
#' Calculate z-scores and flag problematic values
#'
#
################################################################################

calculate_zscore <- function(.data = anthro_data_review) {
  ## Calculate z-score and apply WHO flagging criteria
  anthro_data_zscore <- .data |>
    dplyr::mutate(age_in_days = age * (365.25 / 12)) |>
    zscorer::addWGSR(
      sex = "sex",
      firstPart = "weight",
      secondPart = "age_in_days",
      index = "wfa"
    ) |>
    zscorer::addWGSR(
      sex = "sex",
      firstPart = "height",
      secondPart = "age_in_days",
      index = "hfa"
    ) |>
    zscorer::addWGSR(
      sex = "sex",
      firstPart = "weight",
      secondPart = "height",
      index = "wfh"
    ) |>
    flag_who(hlaz = "hfaz", waz = "wfaz", whlz = "wfhz")
  
  ## Return
  tibble::tibble(anthro_data_zscore)
}


################################################################################
#
# Skew and kurtosis
#
################################################################################



################################################################################
#
#'
#' Calculate u5mr
#'
#
################################################################################

calculate_u5mr_census <- function(pop = c(1072751, 1027673, 991164, 967276, 953438),
                                  .data = anthro_data_review_zscore) {
  ep <- pop / sum(pop)
  expected <- ep * nrow(.data)
  t <- 0:4 
  x <- lm(log(pop) ~ t)
  u5mr <- (abs(x$coefficients[2]) / 365.25) * 10000
  u5mr
}


################################################################################
#
#'
#' Age ratio
#'
#'
#
################################################################################

calculate_age_ratio <- function(.data = anthro_data_review_zscore) {
  ageGroup <- with(.data,
    {
      ifelse(age %in% 6:29, 1, 2)                 
    }
  )
  
  age_ratio <- sum(ageGroup == 1) / sum(ageGroup == 2)
  
  age_ratio
}


################################################################################
#
#' 
#' Plot age by sex structure
#'
#
################################################################################

plot_age_structure <- function(.data = anthro_data_review_zscore) {
  .data <- .data |>
    dplyr::mutate(
      age_group = cut(age, 
                      breaks = c(0, 17, 29, 41, 53, 59),
                      labels = c("6 to 17", "18 to 29", "30 to 41", "42 to 53", "54 to 59"))
    )
  
  pyramid.plot(
    .data$age_group, .data$sex, 
    main = "Age group (months)", 
    xlab = "Frequency (Males | Females)", 
    ylab = "Year-centred age-group", 
    cex.names = 0.9
  )
}


