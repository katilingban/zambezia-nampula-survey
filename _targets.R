################################################################################
#
# Project build script
#
################################################################################

# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)


# Set build options ------------------------------------------------------------

options(
  survey.lonely.psu = "adjust"  ## Adjust variance for stratum with single PSU
)


# Groups of targets ------------------------------------------------------------

## Data downloads --------------------------------------------------------------
data_downloads <- tar_plan(
  ### Download baseline data files
  tar_target(
    name = baseline_data_downloads,
    command = download_all_baseline_data(overwrite = TRUE),
    cue = tar_cue("thorough")
  ),
  ### Download baseline and endline sampling list
  tar_target(
    name = survey_sampling_list_download,
    command = download_googledrive(
      filename = "VIGH-UNICEF_EA_Sample_Final_01252019.xlsx",
      overwrite = TRUE
    ),
    cue = tar_cue("thorough")
  )
)

## Read raw data ---------------------------------------------------------------
raw_data_baseline <- tar_plan(
  ### Sampling list for baseline and endline
  survey_sampling_list = openxlsx::read.xlsx(
    xlsxFile = survey_sampling_list_download$local_path,
    sheet = 1
  ),
  ### Read raw baseline data
  baseline_raw_data_spss = read_spss_data(
    file_list = baseline_data_downloads
  ),
  baseline_raw_data_stata = read_stata_data(
    file_list = baseline_data_downloads,
    filename = "survey_fin.dta"
  ),
  baseline_anthro_zscore_data = read_csv_data(
    file_list = baseline_data_downloads, 
    filename = "who_anthrofin_zscore.csv"
  ),
  baseline_raw_data_stata_with_anthro = read_stata_data(
    file_list = baseline_data_downloads,
    filename = "survey_plus_who_fin.dta"
  ),
  baseline_clean_data = read_csv_data(
    file_list = baseline_data_downloads, 
    filename = "Zambezia and Nampula baseline survey dataset.csv",
    fileEncoding = "latin1"
  ),
  baseline_readme_text = read_text_data(
    file_list = baseline_data_downloads, filename = ".txt"
  ),
  baseline_cleanstr1 = read_text_data(
    file_list = baseline_data_downloads, 
    filename = "cleanstr1", widths = 150
  ),
  baseline_quality_control_corrections = read_text_data(
    file_list = baseline_data_downloads, 
    filename = "qual", widths = 150
  ),
  baseline_final_dataset_processing = read_text_data(
    file_list = baseline_data_downloads, 
    filename = "final_dataset", widths = 150
  ),
  ### Read raw endline data
  endline_raw_data = get_endline_data()
)


## Process data ----------------------------------------------------------------
processed_data_baseline <- tar_plan(
  ### Get baseline sampling weights
  baseline_sample_weight = calculate_weights(
    .data = baseline_raw_data_stata,
    survey_sampling_list
  ),
  baseline_data_processed = process_baseline_data(
    .data = baseline_raw_data_stata
  ),
  ### Process baseline data with weights
  baseline_data_weighted = dplyr::left_join(
    #x = baseline_raw_data_stata,
    x = baseline_data_processed,
    y = baseline_sample_weight |>
      subset(
        select = c(
          ea_id, study_group, 
          cluster_sample_prob_obs, ind_sample_prob_obs,
          sample_prob_obs, sample_weight_obs
        )
      ),
    by = c("enum1" = "ea_id")
  ),
  ### Set baseline data survey design for children data
  # baseline_child_survey_design = survey::svydesign(
  #   ids = ~enum1 + sbjnum,
  #   fpc = ~cluster_sample_prob_obs + ind_sample_prob_obs,
  #   strata = ~prov + strata,
  #   data = baseline_data_weighted,
  #   pps = "brewer"
  # ),
  baseline_child_survey_design = survey::svydesign(
    ids = ~enum1,
    fpc = ~sample_prob_obs,
    strata = ~prov + strata,
    data = baseline_data_weighted,
    pps = "brewer"
  ),
  ### Set baseline data survey design for hh/respondent data
  # baseline_hh_survey_design = survey::svydesign(
  #   ids = ~enum1 + sbjnum,
  #   fpc = ~cluster_sample_prob_obs + ind_sample_prob_obs,
  #   strata = ~prov + strata,
  #   data = baseline_data_weighted |>
  #     get_respondent_data(),
  #   pps = "brewer"
  # ),
  baseline_hh_survey_design = survey::svydesign(
    ids = ~enum1,
    fpc = ~sample_prob_obs,
    strata = ~prov + strata,
    data = baseline_data_weighted |>
      get_respondent_data(),
    pps = "brewer"
  )
)


## Analysis --------------------------------------------------------------------
analysis_baseline <- tar_plan(
  ### Baseline results - demographics respondent
  baseline_demo_respondent = estimate_total(
    vars = c("respondent_sex", "respondent_age_years",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = baseline_hh_survey_design
  ),
  baseline_demo_respondent_province = estimate_province(
    vars = c("respondent_sex", "respondent_age_years",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = baseline_hh_survey_design
  ),
  baseline_demo_respondent_strata = estimate_strata(
    vars = c("respondent_sex", "respondent_age_years",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = baseline_hh_survey_design
  ),
  baseline_demo_respondent_study_group = estimate_study_group(
    vars = c("respondent_sex", "respondent_age_years",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = baseline_hh_survey_design
  ),
  baseline_demo_respondent_study_group_province = estimate_study_group_province(
    vars = c("respondent_sex", "respondent_age_years",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - demographics children
  baseline_demo_child = estimate_total(
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    design = baseline_child_survey_design
  ),
  baseline_demo_child_province = estimate_province(
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    design = baseline_child_survey_design
  ),
  baseline_demo_child_strata = estimate_strata(
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    design = baseline_child_survey_design
  ),
  baseline_demo_child_study_group = estimate_study_group(
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    design = baseline_child_survey_design
  ),
  baseline_demo_child_study_group_province = estimate_study_group_province(
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    design = baseline_child_survey_design
  ),
  ### Baseline results - demographics spouse
  baseline_demo_spouse = estimate_total(
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    design = baseline_hh_survey_design
  ),
  baseline_demo_spouse_province = estimate_province(
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    design = baseline_hh_survey_design
  ),
  baseline_demo_spouse_strata = estimate_strata(
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    design = baseline_hh_survey_design
  ),
  baseline_demo_spouse_study_group = estimate_study_group(
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    design = baseline_hh_survey_design
  ),
  baseline_demo_spouse_study_group_province = estimate_study_group_province(
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - household income
  baseline_hh_income = estimate_total(
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_income_province = estimate_province(
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_income_strata = estimate_strata(
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_income_study_group = estimate_study_group(
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_income_study_group_province = estimate_study_group_province(
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - household structure
  baseline_hh_structure = estimate_total(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_structure_province = estimate_province(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_structure_strata = estimate_strata(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_structure_study_group = estimate_study_group(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_structure_study_group_province = estimate_study_group_province(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - household amenities
  baseline_hh_amenities = estimate_total(
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_amenities_province = estimate_province(
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_amenities_strata = estimate_strata(
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_amenities_study_group = estimate_study_group(
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_amenities_study_group_province = estimate_study_group_province(
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - mode of daily travel
  baseline_hh_travel = estimate_total(
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_travel_province = estimate_province(
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_travel_strata = estimate_strata(
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_travel_study_group = estimate_study_group(
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_travel_study_group_province = estimate_study_group_province(
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - household decision making
  baseline_hh_decision = estimate_total(
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_decision_province = estimate_province(
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_decision_strata = estimate_strata(
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_decision_study_group = estimate_study_group(
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_decision_study_group_province = estimate_study_group_province(
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    design = baseline_hh_survey_design
  )
)


## Outputs ---------------------------------------------------------------------
outputs_baseline <- tar_plan(
  ### Baseline demographics table - respondent
  baseline_demo_respondent_table = create_province_table(
    baseline_demo_respondent_province,
    baseline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    report = FALSE
  ),
  baseline_demo_respondent_table_report = create_province_table(
    baseline_demo_respondent_province,
    baseline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    report = TRUE, format = "wide"
  ),
  baseline_demo_respondent_strata_table = create_strata_table(
    baseline_demo_respondent_strata,
    baseline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    report = FALSE
  ),
  baseline_demo_respondent_study_group_table = create_study_group_table(
    baseline_demo_respondent_study_group,
    baseline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    report = FALSE
  ),
  baseline_demo_respondent_study_group_table_report = create_study_group_table(
    baseline_demo_respondent_study_group,
    baseline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    report = TRUE, format = "wide"
  ),
  baseline_demo_respondent_study_group_province_table = create_study_group_province_table(
    baseline_demo_respondent_study_group_province,
    baseline_demo_respondent_study_group,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    report = FALSE
  ),
  ### Baseline demographics table - child
  baseline_demo_child_table = create_province_table(
    baseline_demo_child_province,
    baseline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    report = FALSE
  ),
  baseline_demo_child_table_report = create_province_table(
    baseline_demo_child_province,
    baseline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    report = TRUE, format = "wide"
  ),
  baseline_demo_child_strata_table = create_strata_table(
    baseline_demo_child_strata,
    baseline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    report = FALSE
  ),
  baseline_demo_child_study_group_table = create_study_group_table(
    baseline_demo_child_study_group,
    baseline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    report = FALSE
  ),
  baseline_demo_child_study_group_table_report = create_study_group_table(
    baseline_demo_child_study_group,
    baseline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    report = TRUE, format = "wide"
  ),
  baseline_demo_child_study_group_province_table = create_study_group_province_table(
    baseline_demo_child_study_group_province,
    baseline_demo_child_study_group,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    report = FALSE
  ),
  ### Baseline demographics table - spouse
  baseline_demo_spouse_table = create_province_table(
    baseline_demo_spouse_province,
    baseline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    report = FALSE
  ),
  baseline_demo_spouse_table_report = create_province_table(
    baseline_demo_spouse_province,
    baseline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    report = TRUE, format = "wide"
  ),
  baseline_demo_spouse_strata_table = create_strata_table(
    baseline_demo_spouse_strata,
    baseline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    report = FALSE
  ),
  baseline_demo_spouse_study_group_table = create_study_group_table(
    baseline_demo_spouse_study_group,
    baseline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    report = FALSE
  ),
  baseline_demo_spouse_study_group_table_report = create_study_group_table(
    baseline_demo_spouse_study_group,
    baseline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    report = TRUE, format = "wide"
  ),
  baseline_demo_spouse_study_group_province_table = create_study_group_province_table(
    baseline_demo_spouse_study_group_province,
    baseline_demo_spouse_study_group,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    report = FALSE
  ),
  ### Baseline household income table
  baseline_hh_income_table = create_province_table(
    baseline_hh_income_province,
    baseline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    report = FALSE
  ),
  baseline_hh_income_table_report = create_province_table(
    baseline_hh_income_province,
    baseline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    report = TRUE, format = "wide"
  ),
  baseline_hh_income_strata_table = create_strata_table(
    baseline_hh_income_strata,
    baseline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    report = FALSE
  ),
  baseline_hh_income_study_group_table = create_study_group_table(
    baseline_hh_income_study_group,
    baseline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    report = FALSE
  ),
  baseline_hh_income_study_group_table_report = create_study_group_table(
    baseline_hh_income_study_group,
    baseline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    report = TRUE, format = "wide"
  ),
  baseline_hh_income_study_group_province_table = create_study_group_province_table(
    baseline_hh_income_study_group_province,
    baseline_hh_income_study_group,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    report = FALSE
  ),
  ### Baseline household structure table
  baseline_hh_structure_table = create_province_table(
    baseline_hh_structure_province,
    baseline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    report = FALSE
  ),
  baseline_hh_structure_table_report = create_province_table(
    baseline_hh_structure_province,
    baseline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    report = TRUE, format = "wide"
  ),
  baseline_hh_structure_strata_table = create_strata_table(
    baseline_hh_structure_strata,
    baseline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    report = FALSE
  ),
  baseline_hh_structure_study_group_table = create_study_group_table(
    baseline_hh_structure_study_group,
    baseline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    report = FALSE
  ),
  baseline_hh_structure_study_group_table_report = create_study_group_table(
    baseline_hh_structure_study_group,
    baseline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    report = TRUE, format = "wide"
  ),
  baseline_hh_structure_study_group_province_table = create_study_group_province_table(
    baseline_hh_structure_study_group_province,
    baseline_hh_structure_study_group,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_bedrooms_in_home", "roofing_material", "floor_material",
      "time_living_in_location_in_months", "time_living_in_location_group"
    ),
    report = FALSE
  ),
  ### Baseline household amenities table
  baseline_hh_amenities_table = create_province_table(
    baseline_hh_amenities_province,
    baseline_hh_amenities,
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    report = FALSE
  ),
  baseline_hh_amenities_table_report = create_province_table(
    baseline_hh_amenities_province,
    baseline_hh_amenities,
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    report = TRUE, format = "wide"
  ),
  baseline_hh_amenities_strata_table = create_strata_table(
    baseline_hh_amenities_strata,
    baseline_hh_amenities,
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    report = FALSE
  ),
  baseline_hh_amenities_study_group_table = create_study_group_table(
    baseline_hh_amenities_study_group,
    baseline_hh_amenities,
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    report = FALSE
  ),
  baseline_hh_amenities_study_group_table_report = create_study_group_table(
    baseline_hh_amenities_study_group,
    baseline_hh_amenities,
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    report = TRUE, format = "wide"
  ),
  baseline_hh_amenities_study_group_province_table = create_study_group_province_table(
    baseline_hh_amenities_study_group_province,
    baseline_hh_amenities_study_group,
    vars = c(
      "communication_and_information_access_electricity",
      "communication_and_information_access_cellphone",
      "communication_and_information_access_computer",
      "communication_and_information_access_landline",
      "communication_and_information_access_radio",
      "communication_and_information_access_television",
      "amenities_housekeeper_childcare_employee",
      "amenities_refrigerator",
      "amenities_refrigerator_alternative",
      "number_of_mosquito_nets",
      "fuel_used_for_cooking",
      "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    report = FALSE
  ),
  ### Baseline household travel table
  baseline_hh_travel_province_table = create_province_table(
    baseline_hh_travel_province,
    baseline_hh_travel,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    report = FALSE
  ),
  baseline_hh_travel_province_table_report = create_province_table(
    baseline_hh_travel_province,
    baseline_hh_travel,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    report = TRUE, format = "wide"
  ),
  baseline_hh_travel_strata_table = create_strata_table(
    baseline_hh_travel_strata,
    baseline_hh_travel,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    report = FALSE
  ),
  baseline_hh_travel_study_group_table = create_study_group_table(
    baseline_hh_travel_study_group,
    baseline_hh_travel,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    report = FALSE
  ),
  baseline_hh_travel_study_group_table_report = create_study_group_table(
    baseline_hh_travel_study_group,
    baseline_hh_travel,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    report = TRUE, format = "wide"
  ),
  baseline_hh_travel_study_group_province_table = create_study_group_province_table(
    baseline_hh_travel_study_group_province,
    baseline_hh_travel_study_group,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    report = FALSE
  ),
  ### Baseline household decisions table
  baseline_hh_decision_province_table = create_province_table(
    baseline_hh_decision_province,
    baseline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    report = FALSE
  ),
  baseline_hh_decision_province_table_report = create_province_table(
    baseline_hh_decision_province,
    baseline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    report = TRUE, format = "wide"
  ),
  baseline_hh_decision_strata_table = create_strata_table(
    baseline_hh_decision_strata,
    baseline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    report = FALSE
  ),
  baseline_hh_decision_study_group_table = create_study_group_table(
    baseline_hh_decision_study_group,
    baseline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    report = FALSE
  ),
  baseline_hh_decision_study_group_table_report = create_study_group_table(
    baseline_hh_decision_study_group,
    baseline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    report = TRUE, format = "wide"
  ),
  baseline_hh_decision_study_group_province_table = create_study_group_province_table(
    baseline_hh_decision_study_group_province,
    baseline_hh_decision_study_group,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    report = FALSE
  )
)


## Reports ---------------------------------------------------------------------
reports_baseline <- tar_plan(
  ##
)

## Deploy targets --------------------------------------------------------------
deploy <- tar_plan(
  ##
)

# Set seed --------------------------------------------------------------------
set.seed(1977)

# Concatenate targets ----------------------------------------------------------

list(
  data_downloads,
  raw_data_baseline,
  processed_data_baseline,
  analysis_baseline,
  outputs_baseline,
  reports_baseline,
  deploy
)