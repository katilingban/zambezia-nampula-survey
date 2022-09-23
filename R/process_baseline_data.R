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
      ig1 = recode_var_categorical(ig1),
      q08 = recode_var_categorical(q08),
      igs1 = recode_var_categorical(igs1),
      igs2 = recode_var_categorical(igs2)
    )
}