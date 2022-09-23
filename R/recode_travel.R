################################################################################
#
#'
#' Recode travel indicators/data
#'
#
################################################################################

travel_recode_mode_other <- function(x, y) {
  ## Recode other modes of travel
  x[y %in% c("Barco", "Canoa")]                              <- 7
  x[y %in% c("Carro chapa")]                                 <- 8
  x[y %in% c("Chate")]                                       <- 9
  x[y %in% c("Comboio")]                                     <- 10
  x[stringr::str_detect(y, pattern = "Mota|Moto|mota|moto")] <- 11
  
  x
}

travel_recode_mode <- function(x, na_values, fill, na_rm = FALSE, 
                               prefix, label = NULL) {
  x <- ifelse(x %in% na_values, NA, x)
  
  y <- spread_vector_to_columns(
    x = x,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  )
  
  if (!is.null(label)) {
    names(y) <- paste0(prefix, "_", label)
  }
  
  travel_df <- data.frame(x, y) |>
    (\(x) { names(x)[1] <- prefix; x } )()
  
  travel_df
}


travel_recode_modes <- function(vars, .data, na_values, fill, na_rm = FALSE, 
                                prefix, label = NULL) {
  x <- .data[vars]
  x <- apply(X = x, MARGIN = 2, list) |>
    unlist(recursive = FALSE)
  
  Map(
    f = travel_recode_mode,
    x = x,
    na_values = na_values,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix,
    label = label
  ) |>
    dplyr::bind_cols()
}


travel_recode_time <- function(x, na_values) {
  ifelse(x %in% na_values, NA, x)
}

travel_recode_times <- function(vars, .data, na_values) {
  x <- .data[vars]
  
  apply(
    X = x,
    MARGIN = 2,
    FUN = travel_recode_time,
    na_values = na_values,
    simplify = TRUE
  ) |>
    data.frame() |>
    (\(x) 
      {
        names(x) <- paste0(
          "travel_times_", 
          c("health_facility", "local_markets", "water_sources")
        )
        x
      }
     )()
}


travel_recode <- function(.data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  travel_modes <- travel_recode_modes(
    vars = c("gi1", "gi2m", "gi3m", "wt1m"),
    .data = .data,
    na_values = rep(list(c(88, 99)), length(vars)),
    fill = list(1:11, 1:5, 1:5, 1:5),
    na_rm = rep(list(FALSE), length(vars)),
    prefix = paste0(
      "travel_modes_", 
      c("town", "health_facility", "local_markets", "water_sources")
    ),
    label = rep(list(NULL), length(vars))
  )
  
  travel_times <- travel_recode_times(
    vars = c("gi2t", "gi3t", "wt1t"),
    .data = .data,
    na_values = c(88, 99, 888, 999)
  )
  
  data.frame(
    core_vars, travel_modes, travel_times
  )
}
