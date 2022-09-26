################################################################################
#
#'
#' Get core variables required for analysis
#'
#' @param .data A roughly cleaned/processed raw dataset 
#'
#
################################################################################

get_core_variables <- function(.data, type = c("baseline", "endline")) {
  type <- match.arg(type)
  
  if (type == "baseline") {
    .data |>
      dplyr::mutate(
        hh_id = sbjnum,
        ch_id = paste0(sbjnum, cid),
        province = haven::as_factor(prov),
        district = haven::as_factor(distrito),
        ea_code = paste0(
          prov, 
          stringr::str_pad(distrito, width = 2, side = "left", pad = 0), 
          stringr::str_pad(post, width = 3, side = "left", pad = 0), 
          stringr::str_pad(enum1, width = 3, side = "left", pad = 0)
        ),
        ea_id = enum1
      ) |>
      subset(
        select = c(
          hh_id, ch_id, province, district, 
          ea_code, ea_id, longitude, latitude, study_group,
          cluster_sample_prob_obs, ind_sample_prob_obs,
          sample_prob_obs, sample_weight_obs
        )
      )
  } else {
    .data |>
      subset(
        select = c(
          id, spid, district, ea_code, geolocation
        )
      )  
  }
}


################################################################################
#
#'
#' Recode yes no responses
#' 
#' @param x A vector to recode 
#' @param na_values A vector of values in x that correspond to an NA. Default
#'   to NULL
#'   
#'
#
################################################################################

recode_yes_no <- function(x, na_values = NULL, detect = c("yes", "no")) {
  ## Which value to detect
  detect <- match.arg(detect)
  
  ## Recode NAs
  if (!is.null(na_values)) {
    x <- ifelse(
      x %in% na_values, NA, x
    )
  }
  
  ## Convert x to uppercase to make recoding rules easier
  if (inherits(x, "character")) {
    x <- toupper(x)
  }

  if (detect == "yes") {
    ## Recode x to 1 and 0  
    x <- ifelse(
      isTRUE(x) | x == "T" | x == 1 | x == "Y" | x == "YES", 1, 0
    )
  } else {
    ## Recode x to 1 and 0
    x <- ifelse(
      isFALSE(x) | x == "F" | x == 0 | x == "N" | x == "NO", 0, 1
    )
  }
  
  ## Return
  x
}


################################################################################
#
#'
#' Split a vector of values from an ODK select multiple type of response
#'
#
################################################################################

split_select_multiple <- function(x, fill, na_rm = FALSE, prefix) {
  if (na_rm) {
    if (is.na(x)) {
      rep(NA_integer_, times = length(fill)) |>
        (\(x) { names(x) <- paste0(prefix, "_", fill); x })()
    } else {
      stringr::str_split(x, pattern = " ") |> 
        unlist() |> 
        as.integer() |> 
        spread_vector_to_columns(fill = fill, prefix = prefix) |>
        colSums(na.rm = TRUE)
    }
  } else {
    stringr::str_split(x, pattern = " ") |> 
      unlist() |> 
      as.integer() |> 
      spread_vector_to_columns(fill = fill, prefix = prefix) |>
      colSums(na.rm = TRUE)
  }
}

split_select_multiples <- function(x, fill, na_rm = FALSE, prefix) {
  lapply(
    X = x,
    FUN = split_select_multiple,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  ) |>
    dplyr::bind_rows()
}


################################################################################
#
#'
#' Get NA types
#'
#
################################################################################

get_na_type <- function(x) {
  if (inherits(x, "character")) {
    na_type <- NA_character_
  }
  
  if (inherits(x, "integer")) {
    na_type <- NA_integer_
  }
  
  if (inherits(x, "numeric")) {
    na_type <- NA_real_
  }
  
  na_type
}


################################################################################
#
#'
#' Convert character vector of categorical responses into unique variables
#' 
#' Function transforms a vector of categorical responses into `n` number of
#' new columns/variables equal to the number of unique categorical values.
#' 
#' @param x Vector of categorical values 
#' @param prefix A character string to prepend to the names of the new columns
#'   to be created
#' 
#'  
#'
#
################################################################################

spread_vector_to_columns <- function(x, fill = NULL, na_rm = FALSE, prefix) {
  values <- sort(unique(x), na.last = NA)
  
  if (!is.null(fill)) {
    values <- c(values, fill[!fill %in% values]) |>
      sort(na.last = NA)
  }
  
  if (na_rm) {
    values <- c(values, NA_integer_)
  }
  
  values <- values |>
    stringr::str_replace_all(
      pattern = " ", replacement = "_"
    )
  
  col_names <- paste(prefix, values, sep = "_")
  
  lapply(
    X = x,
    FUN = function(x, y) ifelse(x == y, 1, 0),
    y = values
  ) |>
    (\(x) do.call(rbind, x))() |>
    data.frame() |>
    (\(x) { names(x) <- col_names; x })()
}


