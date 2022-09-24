################################################################################
#
#'
#' Get data for specific child
#' 
#' @param child_num Specific child number to get data for. Can be between
#'   1 to 10
#' @param .data A data.frame with child data in different columns. Default is
#'   `raw_data_spss`
#'   
#' @return A data.frame with child specific data
#'
#
################################################################################

get_per_child_df <- function(child_num, .data = raw_data_spss) {
  ## Get df for specific child
  child_df <- names(.data) |> 
    (\(x) x[stringr::str_detect(x, pattern = paste0("I_", child_num, "_"))])() |>
    (\(x) data.frame(cid = child_num, .data[ , x]))()
    
  ## Get other df
  other_df <- names(.data) |>
    (\(x) x[stringr::str_detect(x, pattern = "I_", negate = TRUE)])() |>
    (\(x) data.frame(.data[ , x], pid = seq_len(nrow(.data))))()
  
  per_child_df <- tibble::tibble(other_df, child_df) |>
    dplyr::rename_with(
      .fn = ~ stringr::str_remove_all(.x, pattern = "I_[0-9]{1,2}_")
    )
  
  ## Return
  per_child_df
}


################################################################################
#
#'
#' Convert SPSS data to one child per row
#'
#' @param child_num Vector of child numbers to get data for. Default is from
#'   1 to 10
#' @param .data A data.frame with child data in different columns. Default is
#'   `raw_data_spss`
#'   
#' @return A data.frame with all child specific data
#'
#
################################################################################

get_all_child_df <- function(child_num = 1:10, .data = raw_data_spss) {
  all_child_df <- lapply(
    X = child_num, FUN = get_per_child_df, .data = .data
  ) |>
    dplyr::bind_rows()
  
  ## Return
  all_child_df
}


################################################################################
#
#'
#' Hash identifiable information (e.g. names) from specific vectors in data
#' 
#' @param x Vector to hash
#' @param algo Hashing algorithin to use. Either "md5", "sha1", "crc32", 
#'   "sha256", "sha512", "xxhash32", "xxhash64", "murmur32", "spookyhash", or
#'   "blake3". Default to "md5"
#' 
#' @return A character vector composed of strings of fixed length containing
#'   the requested hash value
#'
#
################################################################################

hash_data <- function(x, 
                      algo = c("md5", "sha1", "crc32", "sha256", "sha512",
                               "xxhash32", "xxhash64", "murmur32", "spookyhash",
                               "blake3")) {
  lapply(
    X = x,
    FUN = digest::digest, algo = match.arg(algo)
  ) |>
    unlist()
}


################################################################################
#
#'
#' Process baseline data
#'
#
#
################################################################################

process_baseline_data <- function(.data) {
  .data |>
    dplyr::mutate(
      ## demographics - head of household (respondent)
      respondent_sex = recode_var_categorical(rsex),
      respondent_age = recode_age_respondent(
        q01a, q01b_date, vend_date
      ),
      respondent_age_group = recode_age_group_respondent(
        respondent_age, q01a
      ),
      respondent_language = recode_var_categorical(idiomaq),
      respondent_civil_status = recode_var_categorical(q01e),
      respondent_education_years = recode_education_years(q01d),
      respondent_education_group = recode_education_group(
        respondent_education_years, q01d
      ),
      respondent_occupation = recode_var_categorical(igs1),
      ## demographics - children
      respondent_child_relationship = recode_var_categorical(pa),
      child_sex = recode_var_categorical(pg),
      child_age = as.integer(pb),
      child_age_group = recode_age_group_child(child_age, pb),
      child_currently_breastfeeding = recode_breastfeeding_child(eb1),
      child_parent_age_at_birth = recode_age_parent_at_birth(pf),
      child_location_of_birth = recode_var_categorical(ph),
      child_caesarean_birth = ifelse(pl == 2, 0, 1),
      child_complications_at_birth = ifelse(pk == 2, 0, 1),
      child_low_birth_weight = ifelse(pi == 2, 0, 1),
      source_of_income = recode_var_categorical(ig1),
      monthly_income = recode_var_categorical(q08),
      partner_occupation = recode_var_categorical(igs2),
      .keep = "unused"
    )
}