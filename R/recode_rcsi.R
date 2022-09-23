################################################################################
#
#' 
#' Process and recode reduced coping strategy index (rCSI) data/indicators
#'
#' Relevant variables:
#'
#' How many times in the past week have you had to:
#'
#'   rcsi1	Rely on less preferred and less expensive foods? 
#'     88=Don't know; 99=No response
#'
#'   rcsi2	Borrow food, or rely on help from a friend or relative?
#'     88=Don't know; 99=No response
#'
#'   rcsi3 Limit portion size at mealtimes?
#'     88=Don't know; 99=No response
#'
#'   rcsi4	Restrict consumption by adults in order for small children to eat?
#'     88=Don't know; 99=No response
#'
#'   rcsi5	Reduce number of meals eaten in a day?
#'     88=Don't know; 99=No response
#'     
#
################################################################################

rcsi_recode_strategy <- function(x, na_values = NA) {
  if (any(!is.na(na_values))) {
    ifelse(
      x %in% na_values, NA,
      ifelse(
        x > 7, 7, x
      )
    )
  } else {
    ifelse(x > 7, 7, x)
  }
}

rcsi_recode_strategies <- function(vars, .data, na_values = NA) {
  .data <- .data[vars]
  
  apply(
    X = .data,
    MARGIN = 2,
    FUN = rcsi_recode_strategy,
    na_values = na_values
  ) |>
    data.frame()
}


################################################################################
#
#'
#' Calculate rCSI
#'
#
################################################################################

rcsi_calculate_index <- function(rcsi_df, add = TRUE, 
                                 weights = c(1, 2, 1, 3, 1)) {
  weights <- weights |>
    lapply(
      FUN = rep,
      times = nrow(rcsi_df)
    ) |>
    (\(x) { names(x) <- paste0("w", 1:length(weights)); x })() |>
    dplyr::bind_cols()
  
  rcsi_df_weighted <- rcsi_df * weights
  
  rcsi <- rowSums(rcsi_df_weighted, na.rm = TRUE)
  
  if (add) {
    data.frame(
      rcsi_df,
      rcsi = rcsi
    )
  } else {
    rcsi
  }
}


################################################################################
#
#'
#' Classify rCSI into phases
#'
#
################################################################################

rcsi_classify <- function(rcsi, add = FALSE, spread = FALSE, 
                          phase = 3, cutoff = c(3, 18, 42)) {
  breaks <- c(0, cutoff[1:(phase - 1)], Inf)
  
  if (phase == 3) {
    #labels <- paste0("phase", 1:phase)
    labels <- c("minimal", "stressed", "crisis")
  }
  
  if (phase == 4) {
    #labels <- paste0("phase", 1:phase)
    labels <- c("minimal", "stressed", "crisis", "emergency")
  }
  
    
  rcsi_class <- cut(
    x = rcsi,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE, right = TRUE
  )
  
  if (spread) {
    rcsi_class <- data.frame(
      rcsi_class = rcsi_class,
      spread_vector_to_columns(x = rcsi_class, prefix = "rcsi")
    )
  }
  
  if (add) {
    rcsi_class <- data.frame(
      rcsi, rcsi_class
    )
  }
  
  rcsi_class
}


################################################################################
#
#'
#' Overall recode function
#'
#
################################################################################

rcsi_recode <- function(vars,
                       .data,
                       na_values = NA,
                       phase = 3,
                       cutoff = c(3, 18, 42)) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- rcsi_recode_strategies(
    vars = vars, .data = .data, na_values = na_values
  )
  
  recoded_vars |>
    rcsi_calculate_index(add = FALSE) |>
    rcsi_classify(add = TRUE, spread = TRUE, phase = phase, cutoff = cutoff) |>
    (\(x) data.frame(core_vars, recoded_vars, x))()
}
