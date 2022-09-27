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
  
dplyr::mutate(
  .data = .data,
  ## demographics - head of household (respondent)
  respondent_sex = recode_var_categorical(rsex),
  respondent_age_years = recode_age_respondent(
    q01a, q01b_date, vend_date
  ),
  respondent_age_group = recode_age_group_respondent(
    respondent_age_years, q01a
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
  child_age_months = as.integer(pb),
  child_age_group = recode_age_group_child(child_age_months, pb),
  child_currently_breastfeeding = recode_breastfeeding_child(eb1),
  child_parent_age_at_birth = recode_age_parent_at_birth(pf),
  child_location_of_birth = recode_var_categorical(ph),
  child_caesarean_birth = ifelse(pl == 2, 0, 1),
  child_complications_at_birth = ifelse(pk == 2, 0, 1),
  child_low_birth_weight = ifelse(pi == 2, 0, 1),
  ## demographics - spouse
  spouse_age_years = as.integer(q02b),
  spouse_age_group = recode_age_group_spouse(spouse_age_years, q02b),
  spouse_education_years = recode_education_years(q02e),
  spouse_education_group = recode_education_group(
    spouse_education_years, q02e
  ),
  spouse_occupation = recode_var_categorical(igs2),
  #spouse_biologic_parent = ,
  spouse_lives_in_home = recode_var_categorical(q02a),
  ## household income
  persons_living_in_household = as.integer(famsize),
  children_under_five_living_in_household = as.integer(famsize1),
  pregnant_women_living_in_household = as.integer(famsize2),
  monthly_household_income = recode_var_categorical(q08),
  source_of_household_income = recode_var_categorical(ig1),
  sufficiency_of_household_income = recode_var_categorical(ig2),
  sufficiency_of_family_resource = recode_var_categorical(ig3),
  household_income_against_expenses = recode_var_categorical(ig4),
  ## household structure
  home_ownership_own = recode_yes_no(as.integer(sdh5)),
  home_ownership_rent = recode_yes_no(as.integer(sdh6)),
  home_ownership_loan = recode_yes_no(as.integer(sdh7)),
  number_of_rooms_in_home = haven::as_factor(sdh3) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  number_of_bedrooms_in_home = haven::as_factor(sdh3) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  roofing_material = recode_var_categorical(sdh1) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  floor_material = recode_var_categorical(sdh2) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  time_living_in_location_in_months = recode_time_in_location(q03),
  time_living_in_location_group = recode_time_in_location_group(
    time_living_in_location_in_months, q03
  ),
  ## household amenities
  communication_and_information_access_electricity = recode_yes_no(
    as.integer(cdcg1)
  ),
  communication_and_information_access_cellphone = recode_yes_no(
    as.integer(cdcg4)
  ),
  communication_and_information_access_computer = recode_yes_no(
    as.integer(cdcg7)
  ),
  communication_and_information_access_landline = recode_yes_no(
    as.integer(cdcg6)
  ),
  communication_and_information_access_radio = recode_yes_no(
    as.integer(cdcg2)
  ),
  communication_and_information_access_television = recode_yes_no(
    as.integer(cdcg3)
  ),
  amenities_housekeeper_childcare_employee = recode_yes_no(
    as.integer(cdcg14)
  ),
  amenities_refrigerator = recode_yes_no(
    as.integer(cdcg11)
  ),
  amenities_refrigerator_alternative = recode_yes_no(
    as.integer(cdcg11a)
  ),
  number_of_mosquito_nets = recode_var_categorical(cdcg13) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  fuel_used_for_cooking = recode_var_categorical(cfegs1) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  location_of_food_preparation = recode_var_categorical(cfegs3) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  fuel_used_for_lighting = recode_var_categorical(cfegs5) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  ## mode of daily travel
  usual_mode_of_travel = recode_var_categorical(gi1) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  time_to_travel_to_health_centre = as.integer(gi2t),
  mode_of_travel_to_health_centre = recode_var_categorical(gi2m) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  time_to_travel_to_local_markets = as.integer(gi3t),
  mode_of_travel_to_local_markets = recode_var_categorical(gi3m) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  time_to_travel_to_primary_school = as.integer(gi4t),
  mode_of_travel_to_primary_school = recode_var_categorical(gi4m) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  time_to_travel_to_secondary_school = as.integer(gi5t),
  mode_of_travel_to_secondary_school = recode_var_categorical(gi5m) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  ## household decision making
  marrying_age = recode_var_categorical(ge1) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  using_condoms = recode_var_categorical(ge2) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  household_responsibilities = recode_var_categorical(ge3) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  family_planning = recode_var_categorical(ge4) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  agricultural_tasks = recode_var_categorical(ge5) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  household_finances = recode_var_categorical(ge6) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  child_rearing = recode_var_categorical(ge7) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  child_discipline = recode_var_categorical(ge8) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  healthcare_in_pregnancy = recode_var_categorical(ge9) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  healthcare_for_child = recode_var_categorical(ge10) |>
    (\(x) ifelse(x %in% c("NS", "NR", "NSA"), "No response", x))(),
  ## community groups participation
  
  
  
  .keep = "unused"
)
}