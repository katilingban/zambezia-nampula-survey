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
    child_sex_integer = as.integer(child_sex),
    child_age_months = as.integer(pb),
    child_age_days = child_age_months * (365.25 / 12),
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
    #number_of_rooms_in_home = haven::as_factor(sdh3),
    number_of_rooms_in_home = recode_var_categorical(sdh3),
    number_of_bedrooms_in_home = recode_var_categorical(sdh4),
    roofing_material = recode_var_categorical(sdh1),
    floor_material = recode_var_categorical(sdh2),
    time_living_in_location_in_months = recode_time_in_location(q03),
    time_living_in_location_group = recode_time_in_location_group(
      time_living_in_location_in_months, q03
    ),
    ## household amenities
    electricity = recode_yes_no(as.integer(cdcg1)),
    cellphone = recode_yes_no(as.integer(cdcg4)),
    computer = recode_yes_no(as.integer(cdcg7)),
    landline = recode_yes_no(as.integer(cdcg6)),
    radio = recode_yes_no(as.integer(cdcg2)),
    television = recode_yes_no(as.integer(cdcg3)),
    housekeeper_childcare_employee = recode_yes_no(as.integer(cdcg14)),
    refrigerator = recode_yes_no(as.integer(cdcg11)),
    refrigerator_alternative = recode_yes_no(as.integer(cdcg11a)),
    number_of_mosquito_nets = recode_var_categorical(cdcg13),
    fuel_used_for_cooking = recode_var_categorical(cfegs1),
    location_of_food_preparation = recode_var_categorical(cfegs3),
    fuel_used_for_lighting = recode_var_categorical(cfegs5),
    ## mode of daily travel
    usual_mode_of_travel = recode_var_categorical(gi1),
    time_to_travel_to_health_centre = as.integer(gi2t),
    mode_of_travel_to_health_centre = recode_var_categorical(gi2m),
    time_to_travel_to_local_markets = as.integer(gi3t),
    mode_of_travel_to_local_markets = recode_var_categorical(gi3m),
    time_to_travel_to_primary_school = as.integer(gi4t),
    mode_of_travel_to_primary_school = recode_var_categorical(gi4m),
    time_to_travel_to_secondary_school = as.integer(gi5t),
    mode_of_travel_to_secondary_school = recode_var_categorical(gi5m),
    ## household decision making
    marrying_age = recode_var_categorical(ge1),
    using_condoms = recode_var_categorical(ge2),
    household_responsibilities = recode_var_categorical(ge3),
    family_planning = recode_var_categorical(ge4),
    agricultural_tasks = recode_var_categorical(ge5),
    household_finances = recode_var_categorical(ge6),
    child_rearing = recode_var_categorical(ge7),
    child_discipline = recode_var_categorical(ge8),
    healthcare_in_pregnancy = recode_var_categorical(ge9),
    healthcare_for_child = recode_var_categorical(ge10),
    ## community groups participation
    group_membership = ifelse(q05 == 2, 0, 1),
    group_membership_type = recode_group_type(q05, q05_spec),
    presentation_participation = ifelse(q06 == 2, 0, 1),
    presentation_topic = recode_presentation_type(q06, q06_spec),
    presentation_facilitator = recode_var_categorical(q06a) |>
      (\(x) ifelse(x == "ONG (especifique)", q06a_spec, x))(),
    information_application = ifelse(q06b == 2, 0, 1),
    health_tasks_participation = ifelse(q07 == 2, 0, 1),
    health_tasks_participation_type = recode_var_categorical(q07) |>
      (\(x) ifelse(x == "Sim. Especifique", q07_spec, x))(),
    ## Child anthropometry
    child_height = ifelse(caltura_fi == 2, caltura_check, caltura),
    child_length = ifelse(ccomprimento_fi == 2, ccomprimento_check, ccomprimento),
    child_height_length = ifelse(is.na(child_height), child_length, child_height),
    child_height_length = ifelse(is.na(child_length), child_height, child_length),
    child_standing = ifelse(is.na(child_height), 2, 1),
    child_weight = ifelse(cpeso_fi == 2, cpeso_check, cpeso),
    child_muac = as.numeric(cbraco),
    global_wasting_by_muac = ifelse(child_muac < 12.5, 1, 0),
    moderate_wasting_by_muac = ifelse(child_muac < 12.5 & child_muac >= 11.5, 1, 0),
    severe_wasting_by_muac = ifelse(child_muac < 11.5, 1, 0),
    severe_wasting_by_oedema = ifelse(cmalnut == 1, 1, 0),
    ## Water
    surface_water_source = ifelse(wt2 == 11, 1, 0),
    unimproved_water_source = ifelse(wt2 %in% c(7, 9), 1, 0),
    limited_water_source = ifelse(
      wt2 %in% c(1:6, 8, 10, 12) & wt3a > 30, 1, 0
    ),
    basic_water_source = ifelse(
      wt2 %in% c(1:6, 8, 10, 12) & wt3a <= 30, 1, 0
    ),
    sufficient_water_source = ifelse(
      wt2 %in% 1:2 & wt4 == 1, 1, 0
    ),
    ## Sanitation
    open_defecation = ifelse(lusd1 == 2 | lusd4 == 6, 1, 0),
    unimproved_toilet_facility = ifelse(lusd4 == 5, 1, 0),
    limited_toilet_facility = ifelse(lusd2 == 1 & lusd4 != 5, 1, 0),
    basic_toilet_facility = ifelse(lusd2 == 2 & lusd4 != 5, 1, 0),
    ## Hygiene
    no_handwashing_facility = ifelse(
      mao1 == 2, 1, 
      ifelse(
        mao1 %in% 3:4, NA, 0
      )
    ),
    limited_handwashing_facility = ifelse(
      mao1 == 1 & (mao1a == 2 | mao1b == 3), 1, 0
    ),
    basic_handwashing_facility = ifelse(
      mao1 == 1 & mao1a == 1 & mao1b != 3, 1, 0
    ),
    ## Diarrhoea
    diarrhoea_episode = ifelse(ort1 == 2, 0, 1),
    diarrhoea_seek_treatment = ifelse(ort3 == 2, 0, 1),
    diarrhoea_point_of_care = recode_var_categorical(ort4),
    diarrhoea_treatment_with_ors = ifelse(
      ort5a == 1 | ort5b == 1 | ort5c == 1, 1, 0
    ),
    ## Fever
    fever_episode = ifelse(fiber1 == 2, 0, 1),
    fever_seek_treatment = ifelse(fiber2 == 2, 0, 1),
    fever_point_of_care = recode_var_categorical(fiber3),
    fever_malaria_test = ifelse(fiber4 == 1 | fiber5 == 1, 1, 0),
    fever_malaria_episode = ifelse(fiber6 == 1, 1, 0),
    fever_malaria_coartem = ifelse(fiber6a_1 == 1, 1, 0),
    fever_malaria_amodiaquina_artesunato = ifelse(fiber6a_2 == 1, 1, 0),
    fever_malaria_fansidar = ifelse(fiber6a_3 == 1, 1, 0),
    fever_malaria_quinino = ifelse(fiber6a_4 == 1, 1, 0),
    fever_malaria_quinino_injection = ifelse(is.na(fiber6a_4), NA, 0),
    fever_malaria_artesunato = ifelse(fiber6a_6 == 1, 1, 0),
    fever_malaria_paracetamol_comprimido_xarope = ifelse(fiber6a_7 == 1, 1, 0),
    fever_malaria_treatment_intake = ifelse(fiber7 == 1, 1, 0),
    ## RTI
    rti_episode = ifelse(ch1 == 1 & (ch1a == 1 | ch2 == 1), 1, 0),
    rti_seek_treatment = ifelse(ch3 == 1, 1, 0),
    rti_point_of_care = recode_var_categorical(ch4),
    rti_treatment_antiobioticos = ifelse(ch5a_1 == 1, 1, 0),
    rti_treatment_paracetamol = ifelse(ch5a_2 == 1, 1, 0),
    rti_treatment_aspirina = ifelse(ch5a_3 == 1, 1, 0),
    rti_treatment_ibuprofeno = ifelse(ch5a_4 == 1, 1, 0),
    rti_treatment_other = ifelse(ch5a_5 == 1, 1, 0),
    ## Mental health
    phq8_score = ment1 + ment2 + ment3 + ment4 + ment5 + ment6 + ment7 + ment8,
    major_depression = ifelse(phq8_score > 10 & phq8_score <= 20, 1, 0),
    severe_depression = ifelse(phq8_score > 20, 1, 0),
    at_least_major_depression = ifelse(phq8_score > 10, 1, 0),
    alcohol_consumption = recode_var_categorical(ment9),
    ## Pregnant
    currently_pregnant = ifelse(wh1 == 2, 0, 1),
    weeks_of_gestation_self_report = as.integer(wh4),
    prenatal_card_self_report = ifelse(wh2 == 2, 0, 1),
    prenatal_card_available = ifelse(wh3 == 2, 0, 1),
    malaria_during_pregnancy = ifelse(wh5 == 2, 0, 1),
    anemia_during_pregnancy = ifelse(wh6 == 2, 0, 1),
    excluded_foods_from_diet = ifelse(wh7 == 2, 0, 1),
    included_foods_from_diet = ifelse(wh8 == 2, 0, 1),
    wants_more_children = ifelse(wh9 == 2, 0, 1),
    vaginal_bleeding = preg1_1,
    severe_headache = preg1_2,
    blurry_vision = preg1_3,
    swollen_extremities = preg1_4,
    convulsions = preg1_5,
    fever = preg1_6,
    intense_abdominal_pain = preg1_7,
    loss_of_consciousness = preg1_8,
    fatigue = preg1_9,
    plans_when_labor_begins = recode_var_categorical(preg2),
    ### PMTCT and malaria prevention
    offered_voluntary_counselling_and_testing = ifelse(pmtct1 == 2, 0, 1),
    received_vct_results = ifelse(pmtct2 == 2, 0, 1),
    offered_medication_to_reduce_child_risk = ifelse(pmtct3 == 2, 0, 1),
    received_mosquito_net = ifelse(ins1 == 2, 0, 1),
    slept_under_mosquito_net = ifelse(ins2 == 2, 0, 1),
    ### Natal care
    location_of_last_delivery = recode_var_categorical(spc1),
    number_of_prenatal_visits = recode_var_categorical(spc2),
    at_least_four_anc_visits = ifelse(spc2 %in% 4:5, 1, 0),
    treated_well_during_anc = ifelse(spc2a == 2, 0, 1),
    treated_well_at_delivery = ifelse(spc2b == 2, 0, 1),
    delivery_assisted_by_doctor = spc3_1,
    delivery_assisted_by_nurse = spc3_2,
    delivery_assisted_by_midwife = spc3_3,
    delivery_assisted_by_other_person = spc3_4,
    delivery_assisted_by_traditional_midwife = spc3_5,
    delivery_assisted_by_community_health_worker = spc3_6,
    delivery_assisted_by_relative_or_friend = spc3_7,
    delivery_assisted_by_other = spc3_8,
    delivery_assisted_by_nobody = spc3_9,
    difficulty_reaching_facility_due_to_cost = spc5a_1,
    difficulty_reaching_facility_due_to_distance = spc5a_2,
    difficulty_reaching_facility_due_to_stigma = spc5a_3,
    difficulty_reaching_facility_due_to_poor_roads = spc5a_4,
    difficulty_reaching_facility_due_to_other_reasons = spc5a_5,
    difficulty_reaching_facility_no_difficulty = spc5a_6,
    time_to_postnatal_check_for_child = recode_var_categorical(spc6),
    time_to_postnatal_check_for_mother = recode_var_categorical(spc7),
    given_malaria_treatment_during_pregnancy = ifelse(fansidar1 == 2, 0, 1),
    took_malaria_treatment_during_pregnancy = ifelse(fansidar2 == 1, 0, 1),
    completed_malaria_treatment_during_pregnancy = ifelse(fansidar2 == 4, 1, 0),
    at_least_one_tetanus_toxoid_vaccination = ifelse(tt1 == 2, 0, 1),
    two_or_more_tetanus_toxoid_vaccination = ifelse(tt2 != 1, 1, 0),
    ferrous_sulfate_supplementation = ifelse(fol1 == 2, 0, 1),
    vitamin_a_supplementation_during_pregnancy = ifelse(va1 == 2, 0, 1),
    ## Family planning
    attempted_to_delay_or_prevent_pregnancy = ifelse(pf1 == 2, 0, 1),
    benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother = bs2_1,
    benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby = bs2_2,
    benefit_of_waiting_for_next_pregnancy_avoid_poverty = bs2_3,
    benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated = bs2_4,
    benefit_of_waiting_for_next_pregnancy_other_reasons = bs2_5,
    benefit_of_waiting_for_next_pregnancy_none = bs2_6,
    benefit_of_waiting_until_18_years_of_age = ifelse(bs3 == 2, 0, 1),
    benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother = bs3a_1,
    benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby = bs3a_2,
    benefit_of_waiting_until_18_years_of_age_avoid_poverty = bs3a_3,
    benefit_of_waiting_until_18_years_of_age_more_likley_that_children_are_educated = bs3a_4,
    benefit_of_waiting_until_18_years_of_age_other_reasons = bs3a_5,
    benefit_of_waiting_until_18_years_of_age_none = bs3a_6,
    problem_with_having_more_than_4_children = ifelse(bs4 == 2, 0, 1),
    problem_with_having_more_than_4_children_maternal_mortality = bs4a_1,
    problem_with_having_more_than_4_children_child_mortality = bs4a_2,
    problem_with_having_more_than_4_children_poverty = bs4a_3,
    problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated = bs4a_4,
    problem_with_having_more_than_4_children_other_reasons = bs4a_5,
    problem_with_having_more_than_4_children_none = bs4a_6,
    ## EPI
    immunisation_card_retention_self_report = ifelse(imm1 == 2, 0, 1),
    immunisation_card_retention = ifelse(imm2 == 1, 1, 0),
    immunisation_bcg = ifelse(imm2a1 == 1, 1, 0),
    immunisation_polio_first_dose = ifelse(imm2a2 %in% 1:4, 1, 0),
    immunisation_polio_second_dose = ifelse(imm2a2 %in% 2:4, 1, 0),
    immunisation_polio_third_dose = ifelse(imm2a2 %in% 3:4, 1, 0),
    immunisation_polio_fourth_dose = ifelse(imm2a2 == 4, 1, 0),
    immunisation_pentavalent_first_dose = ifelse(imm2a3 %in% 1:3, 1, 0),
    immunisation_pentavalent_second_dose = ifelse(imm2a3 %in% 2:3, 1, 0),
    immunisation_pentavalent_third_dose = ifelse(imm2a3 == 3, 1, 0),
    immunisation_measles_first_dose = ifelse(imm2a4 %in% 1:2, 1, 0),
    immunisation_measles_second_dose = ifelse(imm2a4 == 2, 1, 0),
    immunisation_pneumococcal_first_dose = ifelse(imm2a5 %in% 1:3, 1, 0),
    immunisation_pneumococcal_second_dose = ifelse(imm2a5 %in% 2:3, 1, 0),
    immunisation_pneumococcal_third_dose = ifelse(imm2a5 == 3, 1, 0),
    immunisation_rotavirus_first_dose = ifelse(imm2a6 %in% 1:2, 1, 0),
    immunisation_rotavirus_second_dose = ifelse(imm2a6 == 2, 1, 0),
    immunisation_fully_immunised = ifelse(
      child_age_months >= 12 & child_age_months < 24 &
        immunisation_bcg + 
        immunisation_polio_first_dose +
        immunisation_polio_second_dose + 
        immunisation_polio_third_dose +
        immunisation_polio_fourth_dose + 
        immunisation_pentavalent_first_dose +
        immunisation_pentavalent_second_dose + 
        immunisation_pentavalent_third_dose +
        immunisation_measles_first_dose + 
        immunisation_measles_second_dose +
        immunisation_pneumococcal_first_dose + 
        immunisation_pneumococcal_second_dose +
        immunisation_pneumococcal_third_dose + 
        immunisation_rotavirus_first_dose +
        immunisation_rotavirus_second_dose == 15, 1, 0
    ),
    immunisation_age_appropriate_immunisation = ifelse(
      child_age_months >= 0 & child_age_months < 1.5 & 
        immunisation_bcg == 1 &
        immunisation_polio_first_dose == 1, 1,
      ifelse(
        child_age_months >= 1.5 & child_age_months < 2.5 &
          immunisation_bcg == 1 & 
          immunisation_polio_first_dose == 1 &
          immunisation_polio_second_dose == 1 & 
          immunisation_pentavalent_first_dose == 1 &
          immunisation_pneumococcal_first_dose == 1 &
          immunisation_rotavirus_first_dose == 1, 1,
        ifelse(
          child_age_months >= 2.5 & child_age_months < 3.5 &
            immunisation_bcg == 1 & 
            immunisation_polio_first_dose == 1 &
            immunisation_polio_second_dose == 1 & 
            immunisation_polio_third_dose == 1 &
            immunisation_pentavalent_first_dose == 1 &
            immunisation_pentavalent_second_dose == 1 &
            immunisation_pneumococcal_first_dose == 1 &
            immunisation_pneumococcal_second_dose == 1 &
            immunisation_rotavirus_first_dose == 1 &
            immunisation_rotavirus_second_dose == 1, 1,
          ifelse(
            child_age_months >= 3.5 & child_age_months < 9 &
              immunisation_bcg == 1 & 
              immunisation_polio_first_dose == 1 &
              immunisation_polio_second_dose == 1 & 
              immunisation_polio_third_dose == 1 &
              immunisation_polio_fourth_dose == 1 &
              immunisation_pentavalent_first_dose == 1 &
              immunisation_pentavalent_second_dose == 1 &
              immunisation_pentavalent_third_dose == 1 &
              immunisation_pneumococcal_first_dose == 1 &
              immunisation_pneumococcal_second_dose == 1 &
              immunisation_pentavalent_third_dose == 1 &
              immunisation_rotavirus_first_dose == 1 &
              immunisation_rotavirus_second_dose == 1, 1,
            ifelse(
              child_age_months >= 9 & child_age_months < 18 &
                immunisation_bcg == 1 & 
                immunisation_polio_first_dose == 1 &
                immunisation_polio_second_dose == 1 & 
                immunisation_polio_third_dose == 1 &
                immunisation_polio_fourth_dose == 1 &
                immunisation_pentavalent_first_dose == 1 &
                immunisation_pentavalent_second_dose == 1 &
                immunisation_pentavalent_third_dose == 1 &
                immunisation_pneumococcal_first_dose == 1 &
                immunisation_pneumococcal_second_dose == 1 &
                immunisation_pentavalent_third_dose == 1 &
                immunisation_rotavirus_first_dose == 1 &
                immunisation_rotavirus_second_dose == 1 &
                immunisation_measles_first_dose == 1, 1,
              ifelse(
                child_age_months >= 18 &
                  immunisation_bcg == 1 & 
                  immunisation_polio_first_dose == 1 &
                  immunisation_polio_second_dose == 1 & 
                  immunisation_polio_third_dose == 1 &
                  immunisation_polio_fourth_dose == 1 &
                  immunisation_pentavalent_first_dose == 1 &
                  immunisation_pentavalent_second_dose == 1 &
                  immunisation_pentavalent_third_dose == 1 &
                  immunisation_pneumococcal_first_dose == 1 &
                  immunisation_pneumococcal_second_dose == 1 &
                  immunisation_pentavalent_third_dose == 1 &
                  immunisation_rotavirus_first_dose == 1 &
                  immunisation_rotavirus_second_dose == 1 &
                  immunisation_measles_first_dose == 1 &
                  immunisation_measles_second_dose == 1, 1, 0
              )
            )
          )
        )
      )
    ),
    ## Vitamin A and deworming
    vitamin_a_supplementation_coverage = ifelse(
      child_age_months >= 6 & child_age_months < 12 & vas1 != 1, 1,
      ifelse(
        child_age_months >= 12 & child_age_months < 60 & vas2 >= 3, 1, 0
      )
    ),
    deworming_coverage = ifelse(vas3 == 2, 0, 1),
    ## IYCF/ICFI - 6-23 months
    food_group_breastmilk = ifelse(eb1 == 2, 1, 0),
    food_group_dairy = ifelse(nut1l != 1, 1, 0),
    food_group_starch = ifelse(nut1a !=1 | nut1c != 1, 1, 0),
    food_group_vitamin_a_rich = ifelse(
      nut1b != 1 | nut1d != 1 | nut1e != 1 | nut1n != 1, 1, 0
    ),
    food_group_other_fruits_vegetables = ifelse(nut1f != 1, 1, 0),
    food_group_legumes = ifelse(nut1k != 1, 1, 0),
    food_group_meat = ifelse(nut1g != 1 | nut1h != 1 | nut1j != 1, 1, 0),
    food_group_eggs = ifelse(nut1i != 1, 1, 0),
    food_groups_score = food_group_breastmilk + food_group_dairy + 
      food_group_starch + food_group_vitamin_a_rich + 
      food_group_other_fruits_vegetables +
      food_group_legumes + food_group_meat + food_group_eggs,
    minimum_dietary_diversity = ifelse(
      child_age_months < 6 | child_age_months >= 24, NA,
      ifelse(
        food_groups_score >= 5, 1, 0
      )
    ),
    ## Breastfeeding (less than 24 months)
    ever_breastfed = ifelse(eb1 == 1 | eb1 == 2, 1, 0),
    early_initiation_of_breastfeeding = ifelse(eb2 == 1 | eb2_hrs <= 1, 1, 0),
    ## Exclusive breastfeeding (less than 6 months)
    exclusive_breastfeeding = ifelse(
      eb2 == 2 & nut2 == 1, 1, 0
    ),
    ## Women's decision making
    freedom_and_control = recode_var_categorical(von1),
    control_over_destiny = recode_var_categorical(von2),
    make_decision_without_husband = recode_var_categorical(von3),
    willingly_participate_in_survey = recode_var_categorical(von4),
    .keep = "unused"
  ) |>
    ## Child anthropometry
    zscorer::addWGSR(
      sex = "child_sex_integer",
      firstPart = "child_weight",
      secondPart = "child_age_days",
      index = "wfa"
    ) |>
    zscorer::addWGSR(
      sex = "child_sex_integer",
      firstPart = "child_height_length",
      secondPart = "child_age_days",
      standing = "child_standing",
      index = "hfa"
    ) |>
    zscorer::addWGSR(
      sex = "child_sex_integer",
      firstPart = "child_weight",
      secondPart = "child_height_length",
      standing = "child_standing",
      index = "wfh"
    ) |>
    (\(x) { x$hfaz <- ifelse(x$hfaz > 6 | x$hfaz < -6, NA, x$hfaz); x })() |>
    (\(x) { x$wfaz <- ifelse(x$wfaz > 5 | x$wfaz < -6, NA, x$wfaz); x })() |>
    (\(x) { x$wfhz <- ifelse(x$wfhz > 5 | x$wfhz < -5, NA, x$wfhz); x })() |>
    (\(x) 
      {
        ## Stunting
        x$global_stunting = ifelse(x$hfaz < -2, 1, 0)
        x$moderate_stunting = ifelse(x$hfaz >= -3 & x$hfaz < -2, 1, 0)
        x$severe_stunting = ifelse(x$hfaz < -3, 1, 0)
        ## Underweight
        x$global_underweight = ifelse(x$wfaz < -2, 1, 0)
        x$moderate_underweight = ifelse(x$wfaz >= -3 & x$wfaz < -2, 1, 0)
        x$severe_underweight = ifelse(x$wfaz < -3, 1, 0)
        ## Wasting
        x$global_wasting_by_weight_for_height = ifelse(x$wfhz < -2, 1, 0)
        x$moderate_wasting_by_weight_for_height = ifelse(x$wfhz >= -3 & x$wfhz < -2, 1, 0)
        x$severe_wasting_by_weight_for_height = ifelse(x$wfhz < -3, 1, 0)
        x
      }
    )() |>
    (\(x)
     {
       data.frame(
         x,
         wdds_recode_groups(
           vars = wdds_map_fg_vars(
             staples = c("nutmul1", "nutmul2"),
             green_leafy = "nutmul10",
             other_vita = c("nutmul11", "nutmul12"),
             fruits_vegetables = c("nutmul13", "nutmul14"),
             organ_meat = "nutmul6",
             meat_fish = c("nutmul7", "nutmul8"),
             eggs = "nutmul9",
             legumes = c("nutmul3", "nutmul4"),
             milk = "nutmul5"
           ),
           .data = x
         ) |>
           wdds_calculate_score(add = TRUE)
       )
     }
    )() |>
    (\(x)
      {
        data.frame(
          x,
          mddw_recode_groups(
            vars = mddw_map_fg_vars(
              staples = c("nutmul1", "nutmul2"),
              pulses = "nutmul3",
              nuts_seeds = "nutmul4",
              milk = "nutmul5",
              meat_fish = c("nutmul6", "nutmul7", "nutmul8"), 
              eggs = "nutmul9", 
              green_leafy = "nutmul10",
              other_vita = c("nutmul11", "nutmul12"),
              vegetables = "nutmul13",
              fruits = "nutmul14"
            ),
            .data = x
          ) |>
            mddw_calculate_score(add = TRUE)
        )
      }
    )() |>
    (\(x)
      {
        play_df <- data.frame(
          x,
          play_recode_responses(
            vars = paste0(
              "play", 
              c(paste0(1, letters[1:7]), 2, paste0(3, letters[1:6]))
            ),
            .data = x,
            na_values = c(8, 9)
          ) |>
            (\(x)
              {
                names(x) <- c(
                  "sing_to_or_with_child", "take_child_for_a_walk", 
                  "play_a_game_with_child", "read_books_or_look_at_photos",
                  "tell_stories_to_child", "identify_objects_with_child",
                  "draw_things_to_or_with_child", "child_has_place_for_toys",
                  "play_with_child_during_bath", "play_with_child_while_feeding_child",
                  "play_with_child_while_changing_clothes", 
                  "play_with_child_while_working_at_home",
                  "play_with_child_while_working_in_the_field",
                  "play_with_child_during_free_time"
                )
                x
              }
            )()
        )
      }
    )()
}