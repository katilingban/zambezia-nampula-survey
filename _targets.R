################################################################################
#
# Project build script
#
################################################################################

# Load packages and load project-specific functions in R folder ----------------
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)


# Set build options ------------------------------------------------------------

## Set options for survey package
options(
  survey.lonely.psu = "adjust"  ## Adjust variance for stratum with single PSU
)

## Authenticate with Google Drive
googledrive::drive_auth(
  email = Sys.getenv("GOOGLE_AUTH_EMAIL"),
  path = Sys.getenv("GOOGLE_AUTH_FILE")
)

## Authenticate with Google Sheets
googlesheets4::gs4_auth(
  email = Sys.getenv("GOOGLE_AUTH_EMAIL"),
  path = Sys.getenv("GOOGLE_AUTH_FILE")
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

## Spatial datasets ------------------------------------------------------------
data_spatial <- tar_plan(
  moz_country = get_country(),
  moz_provinces = get_provinces(),
  moz_districts = get_districts(),
  moz_posts = get_posts(),
  moz_settlements = mozambique::settlements,
  survey_provinces = subset(
    moz_provinces, ADM1_PT %in% c("Zambezia", "Nampula")
  ),
  survey_districts = subset(
    moz_districts, ADM1_PT %in% c("Zambezia", "Nampula")
  ),
  survey_posts = subset(
    moz_posts, ADM1_PT %in% c("Zambezia", "Nampula")
  )
)

## Supporting/reference datasets -----------------------------------------------
data_reference <- tar_plan(
  ### Read indicator list from Google Sheets -----------------------------------
  survey_indicator_list_id = googlesheets4::gs4_find() |>
    subset(name == "zambezia_nampula_survey_indicators") |>
    (\(x) x$id)(),
  tar_target(
    name = survey_indicator_list,
    command = googlesheets4::read_sheet(ss = survey_indicator_list_id),
    cue = tar_cue("always")
  ),
  ### Read choices list for endline survey -------------------------------------
  survey_endline_form_id = googlesheets4::gs4_find() |>
    subset(name == "improving_nutrition_status_u5") |>
    (\(x) x$id)(),
  tar_target(
    name = survey_endline_choices,
    command = googlesheets4::read_sheet(
      ss = survey_endline_form_id, sheet = "choices"
    ) |>
      subset(!is.na(list_name))
  )
)


## Read raw baseline data ------------------------------------------------------
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
  )
)

## Process baseline data -------------------------------------------------------
processed_data_baseline <- tar_plan(
  ### Create baseline raw data sf object ---------------------------------------
  baseline_raw_data_sf = sf::st_as_sf(
    x = baseline_raw_data_stata |>
      subset(!is.na(longitude) | !is.na(latitude)),
    coords = c("longitude", "latitude"),
    crs = sf::st_crs(4326)
  ),
  ### Get baseline sampling weights --------------------------------------------
  baseline_sample_weight = calculate_weights(
    .data = baseline_raw_data_stata,
    survey_sampling_list
  ),
  baseline_data_processed = process_baseline_data(
    .data = baseline_raw_data_stata
  ),
  ### Process baseline data with weights ---------------------------------------
  baseline_data_weighted = dplyr::left_join(
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
  ### Set baseline survey design for children ----------------------------------
  baseline_child_survey_design = survey::svydesign(
    ids = ~enum1,
    fpc = ~sample_prob_obs,
    strata = ~province + strata,
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
  ### Set baseline survey design for households/respondent ---------------------
  baseline_hh_survey_design = survey::svydesign(
    ids = ~enum1,
    fpc = ~sample_prob_obs,
    strata = ~province + strata,
    data = baseline_data_weighted |>
      get_respondent_data(),
    pps = "brewer"
  )
)

## Baseline analysis -----------------------------------------------------------
analysis_baseline <- tar_plan(
  ### Baseline results - demographics respondent -------------------------------
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
  ### Baseline results - demographics children ---------------------------------
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
  ### Baseline results - demographics spouse -----------------------------------
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
  ### Baseline results - household income --------------------------------------
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
  ### Baseline results - household structure -----------------------------------
  baseline_hh_structure = estimate_total(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_structure_province = estimate_province(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_structure_strata = estimate_strata(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_structure_study_group = estimate_study_group(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_structure_study_group_province = estimate_study_group_province(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - household amenities -----------------------------------
  baseline_hh_amenities = estimate_total(
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_amenities_province = estimate_province(
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_amenities_strata = estimate_strata(
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_amenities_study_group = estimate_study_group(
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_amenities_study_group_province = estimate_study_group_province(
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - mode of daily travel ----------------------------------
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
  ### Baseline results - household decision making -----------------------------
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
  ),
  ### Baseline results - household membership in community groups --------------
  baseline_hh_groups = estimate_total(
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_groups_province = estimate_province(
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_groups_strata = estimate_strata(
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_groups_study_group = estimate_study_group(
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_hh_groups_study_group_province = estimate_study_group_province(
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - child anthropometry -----------------------------------
  baseline_child_anthro = estimate_total(
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 60)
  ),
  baseline_child_anthro_province = estimate_province(
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 60)
  ),
  baseline_child_anthro_strata = estimate_strata(
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 60)
  ),
  baseline_child_anthro_study_group = estimate_study_group(
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 60)
  ),
  baseline_child_anthro_study_group_province = estimate_study_group_province(
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 60)
  ),
  ### Baseline results - WDDS --------------------------------------------------
  baseline_wdds = estimate_total(
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_wdds_province = estimate_province(
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_wdds_strata = estimate_strata(
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_wdds_study_group = estimate_study_group(
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_wdds_study_group_province = estimate_study_group_province(
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  ### Baseline results - MDD-W -------------------------------------------------
  baseline_mddw = estimate_total(
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_mddw_province = estimate_province(
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_mddw_strata = estimate_strata(
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_mddw_study_group = estimate_study_group(
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_mddw_study_group_province = estimate_study_group_province(
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  ### Baseline results - child development -------------------------------------
  baseline_child_dev = estimate_total(
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_child_dev_province = estimate_province(
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_child_dev_strata = estimate_strata(
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_child_dev_study_group = estimate_study_group(
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_child_dev_study_group_province = estimate_study_group_province(
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - WASH --------------------------------------------------
  baseline_wash = estimate_total(
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_wash_province = estimate_province(
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_wash_strata = estimate_strata(
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_wash_study_group = estimate_study_group(
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    design = baseline_hh_survey_design
  ),
  baseline_wash_study_group_province = estimate_study_group_province(
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    design = baseline_hh_survey_design
  ),
  ### Baseline results - period prevalence of childhood illnesses --------------
  baseline_child_ill = estimate_total(
    vars = c(
      "diarrhoea_episode", "fever_episode", "rti_episode"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_ill_province = estimate_province(
    vars = c(
      "diarrhoea_episode", "fever_episode", "rti_episode"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_ill_strata = estimate_strata(
    vars = c(
      "diarrhoea_episode", "fever_episode", "rti_episode"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_ill_study_group = estimate_study_group(
    vars = c(
      "diarrhoea_episode", "fever_episode", "rti_episode"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_ill_study_group_province = estimate_study_group_province(
    vars = c(
      "diarrhoea_episode", "fever_episode", "rti_episode"
    ),
    design = baseline_child_survey_design
  ),
  ### Baseline results - treatment seeking for childhood illnesses -------------
  baseline_child_ill_treatment = estimate_total(
    vars = c(
      "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_ill_treatment_province = estimate_province(
    vars = c(
      "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_ill_treatment_strata = estimate_strata(
    vars = c(
      "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_ill_treatment_study_group = estimate_study_group(
    vars = c(
      "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_ill_treatment_study_group_province = estimate_study_group_province(
    vars = c(
      "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    design = baseline_child_survey_design
  ),
  ### Baseline results - women's mental health and alcohol consumption ---------
  baseline_women_phq8 = estimate_total(
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    design = baseline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  baseline_women_phq8_province = estimate_province(
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    design = baseline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  baseline_women_phq8_strata = estimate_strata(
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    design = baseline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  baseline_women_phq8_study_group = estimate_study_group(
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    design = baseline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  baseline_women_phq8_study_group_province = estimate_study_group_province(
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    design = baseline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  ### Baseline results - pregnancy characteristics -----------------------------
  baseline_pregnant = estimate_total(
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    design = baseline_hh_survey_design |>
      subset(currently_pregnant == 1)
  ),
  baseline_pregnant_province = estimate_province(
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    design = baseline_hh_survey_design |>
      subset(currently_pregnant == 1)
  ),
  baseline_pregnant_strata = estimate_strata(
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    design = baseline_hh_survey_design |>
      subset(currently_pregnant == 1)
  ),
  baseline_pregnant_study_group = estimate_study_group(
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    design = baseline_hh_survey_design |>
      subset(currently_pregnant == 1)
  ),
  baseline_pregnant_study_group_province = estimate_study_group_province(
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    design = baseline_hh_survey_design |>
      subset(currently_pregnant == 1)
  ),
  ### Baseline results - PMTCT and mosquito net --------------------------------
  baseline_pregnant_prevention = estimate_total(
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_pregnant_prevention_province = estimate_province(
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_pregnant_prevention_strata = estimate_strata(
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_pregnant_prevention_study_group = estimate_study_group(
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_pregnant_prevention_study_group_province = estimate_study_group_province(
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  ### Baseline results - natal care --------------------------------------------
  baseline_natal_care = estimate_total(
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_natal_care_province = estimate_province(
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_natal_care_strata = estimate_strata(
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_natal_care_study_group = estimate_study_group(
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_natal_care_study_group_province = estimate_study_group_province(
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  ### Baseline results - Family planning ----------------------------------------------------------
  baseline_family_planning = estimate_total(
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_family_planning_province = estimate_province(
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_family_planning_strata = estimate_strata(
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_family_planning_study_group = estimate_study_group(
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_family_planning_study_group_province = estimate_study_group_province(
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  ### Baseline results - EPI ---------------------------------------------------
  baseline_child_immunisation = estimate_total(
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_immunisation_province = estimate_province(
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_immunisation_strata = estimate_strata(
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_immunisation_study_group = estimate_study_group(
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    design = baseline_child_survey_design
  ),
  baseline_child_immunisation_study_group_province = estimate_study_group_province(
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    design = baseline_child_survey_design
  ),
  ### Baseline results - full immunisation -------------------------------------
  baseline_child_immunisation_full = estimate_total(
    vars = "immunisation_fully_immunised",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 12 & child_age_months < 24)
  ),
  baseline_child_immunisation_full_province = estimate_province(
    vars = "immunisation_fully_immunised",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 12 & child_age_months < 24)
  ),
  baseline_child_immunisation_full_strata = estimate_strata(
    vars = "immunisation_fully_immunised",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 12 & child_age_months < 24)
  ),
  baseline_child_immunisation_full_study_group = estimate_study_group(
    vars = "immunisation_fully_immunised",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 12 & child_age_months < 24)
  ),
  baseline_child_immunisation_full_study_group_province = estimate_study_group_province(
    vars = "immunisation_fully_immunised",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 12 & child_age_months < 24)
  ),
  ### Baseline results - IYCF --------------------------------------------------
  baseline_iycf = estimate_total(
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 24)
  ),
  baseline_iycf_province = estimate_province(
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 24)
  ),
  baseline_iycf_strata = estimate_strata(
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 24)
  ),
  baseline_iycf_study_group = estimate_study_group(
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 24)
  ),
  baseline_iycf_study_group_province = estimate_study_group_province(
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 24)
  ),
  ### Baseline results - breastfeeding -----------------------------------------
  baseline_breastfeeding = estimate_total(
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    design = baseline_child_survey_design |>
      subset(child_age_months < 24)
  ),
  baseline_breastfeeding_province = estimate_province(
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    design = baseline_child_survey_design |>
      subset(child_age_months < 24)
  ),
  baseline_breastfeeding_strata = estimate_strata(
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    design = baseline_child_survey_design |>
      subset(child_age_months < 24)
  ),
  baseline_breastfeeding_study_group = estimate_study_group(
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    design = baseline_child_survey_design |>
      subset(child_age_months < 24)
  ),
  baseline_breastfeeding_study_group_province = estimate_study_group_province(
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    design = baseline_child_survey_design |>
      subset(child_age_months < 24)
  ),
  ### Baseline results - exclusive breastfeeding -------------------------------
  baseline_ebf = estimate_total(
    vars = "exclusive_breastfeeding",
    design = baseline_child_survey_design |>
      subset(child_age_months < 6)
  ),
  baseline_ebf_province = estimate_province(
    vars = "exclusive_breastfeeding",
    design = baseline_child_survey_design |>
      subset(child_age_months < 6)
  ),
  baseline_ebf_strata = estimate_strata(
    vars = "exclusive_breastfeeding",
    design = baseline_child_survey_design |>
      subset(child_age_months < 6)
  ),
  baseline_ebf_study_group = estimate_study_group(
    vars = "exclusive_breastfeeding",
    design = baseline_child_survey_design |>
      subset(child_age_months < 6)
  ),
  baseline_ebf_study_group_province = estimate_study_group_province(
    vars = "exclusive_breastfeeding",
    design = baseline_child_survey_design |>
      subset(child_age_months < 6)
  ),
  ### Baseline results - vitamin A supplementation for children 6-59 -----------
  baseline_child_vita = estimate_total(
    vars = "vitamin_a_supplementation_coverage",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6)
  ),
  baseline_child_vita_province = estimate_province(
    vars = "vitamin_a_supplementation_coverage",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6)
  ),
  baseline_child_vita_strata = estimate_strata(
    vars = "vitamin_a_supplementation_coverage",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6)
  ),
  baseline_child_vita_study_group = estimate_study_group(
    vars = "vitamin_a_supplementation_coverage",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6)
  ),
  baseline_child_vita_study_group_province = estimate_study_group_province(
    vars = "vitamin_a_supplementation_coverage",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 6)
  ),
  ### Baseline results - deworming coverage for children 12-59 -----------------
  baseline_deworming = estimate_total(
    vars = "deworming_coverage",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 12)
  ),
  baseline_deworming_province = estimate_province(
    vars = "deworming_coverage",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 12)
  ),
  baseline_deworming_strata = estimate_strata(
    vars = "deworming_coverage",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 12)
  ),
  baseline_deworming_study_group = estimate_study_group(
    vars = "deworming_coverage",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 12)
  ),
  baseline_deworming_study_group_province = estimate_study_group_province(
    vars = "deworming_coverage",
    design = baseline_child_survey_design |>
      subset(child_age_months >= 12)
  ),
  ### Baseline results - women's decision making -------------------------------
  baseline_wem = estimate_total(
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    design = baseline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  baseline_wem_province = estimate_province(
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    design = baseline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  baseline_wem_strata = estimate_strata(
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    design = baseline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  baseline_wem_study_group = estimate_study_group(
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    design = baseline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  baseline_wem_study_group_province = estimate_study_group_province(
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    design = baseline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  ### Baseline results - women's anthropometry ---------------------------------
  baseline_women_anthro = estimate_total(
    vars = c("body_mass_index", "bmi_class"),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_women_anthro_province = estimate_province(
    vars = c("body_mass_index", "bmi_class"),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_women_anthro_strata = estimate_strata(
    vars = c("body_mass_index", "bmi_class"),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_women_anthro_study_group = estimate_study_group(
    vars = c("body_mass_index", "bmi_class"),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  baseline_women_anthro_study_group_province = estimate_study_group_province(
    vars = c("body_mass_index", "bmi_class"),
    design = baseline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  )
)


## Outputs - Tables ------------------------------------------------------------
outputs_tables_baseline <- tar_plan(
  ### Baseline demographics table - respondent ---------------------------------
  baseline_demo_respondent_table = create_province_table(
    baseline_demo_respondent_province,
    baseline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_demo_respondent_table_report = create_province_table(
    baseline_demo_respondent_province,
    baseline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_demo_respondent_strata_table = create_strata_table(
    baseline_demo_respondent_strata,
    baseline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_demo_respondent_study_group_table = create_study_group_table(
    baseline_demo_respondent_study_group,
    baseline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_demo_respondent_study_group_table_report = create_study_group_table(
    baseline_demo_respondent_study_group,
    baseline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_demo_respondent_study_group_province_table = create_study_group_province_table(
    baseline_demo_respondent_study_group_province,
    baseline_demo_respondent_study_group,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline demographics table - child --------------------------------------
  baseline_demo_child_table = create_province_table(
    baseline_demo_child_province,
    baseline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline demographics table - spouse -------------------------------------
  baseline_demo_spouse_table = create_province_table(
    baseline_demo_spouse_province,
    baseline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_demo_spouse_table_report = create_province_table(
    baseline_demo_spouse_province,
    baseline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_demo_spouse_strata_table = create_strata_table(
    baseline_demo_spouse_strata,
    baseline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_demo_spouse_study_group_table = create_study_group_table(
    baseline_demo_spouse_study_group,
    baseline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_demo_spouse_study_group_table_report = create_study_group_table(
    baseline_demo_spouse_study_group,
    baseline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_demo_spouse_study_group_province_table = create_study_group_province_table(
    baseline_demo_spouse_study_group_province,
    baseline_demo_spouse_study_group,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline household income table ------------------------------------------
  baseline_hh_income_table = create_province_table(
    baseline_hh_income_province,
    baseline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline household structure table ---------------------------------------
  baseline_hh_structure_table = create_province_table(
    baseline_hh_structure_province,
    baseline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_hh_structure_table_report = create_province_table(
    baseline_hh_structure_province,
    baseline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_hh_structure_strata_table = create_strata_table(
    baseline_hh_structure_strata,
    baseline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_hh_structure_study_group_table = create_study_group_table(
    baseline_hh_structure_study_group,
    baseline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_hh_structure_study_group_table_report = create_study_group_table(
    baseline_hh_structure_study_group,
    baseline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_hh_structure_study_group_province_table = create_study_group_province_table(
    baseline_hh_structure_study_group_province,
    baseline_hh_structure_study_group,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline household amenities table ---------------------------------------
  baseline_hh_amenities_table = create_province_table(
    baseline_hh_amenities_province,
    baseline_hh_amenities,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_hh_amenities_table_report = create_province_table(
    baseline_hh_amenities_province,
    baseline_hh_amenities,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_hh_amenities_strata_table = create_strata_table(
    baseline_hh_amenities_strata,
    baseline_hh_amenities,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_hh_amenities_study_group_table = create_study_group_table(
    baseline_hh_amenities_study_group,
    baseline_hh_amenities,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_hh_amenities_study_group_table_report = create_study_group_table(
    baseline_hh_amenities_study_group,
    baseline_hh_amenities,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_hh_amenities_study_group_province_table = create_study_group_province_table(
    baseline_hh_amenities_study_group_province,
    baseline_hh_amenities_study_group,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline household travel table ------------------------------------------
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline household decisions table ---------------------------------------
  baseline_hh_decision_province_table = create_province_table(
    baseline_hh_decision_province,
    baseline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
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
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline household community group membership table ----------------------
  baseline_hh_groups_province_table = create_province_table(
    baseline_hh_groups_province,
    baseline_hh_groups,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_hh_groups_province_table_report = create_province_table(
    baseline_hh_groups_province,
    baseline_hh_groups,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_hh_groups_strata_table = create_strata_table(
    baseline_hh_groups_strata,
    baseline_hh_groups,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_hh_groups_study_group_table = create_study_group_table(
    baseline_hh_groups_study_group,
    baseline_hh_groups,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report =  FALSE
  ),
  baseline_hh_groups_study_group_table_report = create_study_group_table(
    baseline_hh_groups_study_group,
    baseline_hh_groups,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_hh_groups_study_group_province_table = create_study_group_province_table(
    baseline_hh_groups_study_group_province,
    baseline_hh_groups_study_group,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline child anthropometry table ---------------------------------------
  baseline_child_anthro_province_table = create_province_table(
    baseline_child_anthro_province,
    baseline_child_anthro,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_anthro_province_table_report = create_province_table(
    baseline_child_anthro_province,
    baseline_child_anthro,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_anthro_strata_table = create_strata_table(
    baseline_child_anthro_strata,
    baseline_child_anthro,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_anthro_study_group_table = create_study_group_table(
    baseline_child_anthro_study_group,
    baseline_child_anthro,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_anthro_study_group_table_report = create_study_group_table(
    baseline_child_anthro_study_group,
    baseline_child_anthro,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_anthro_study_group_province_table = create_study_group_province_table(
    baseline_child_anthro_study_group_province,
    baseline_child_anthro_study_group,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline WDDS table ------------------------------------------------------
  baseline_wdds_province_table = create_province_table(
    baseline_wdds_province,
    baseline_wdds,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_wdds_province_table_report = create_province_table(
    baseline_wdds_province,
    baseline_wdds,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_wdds_strata_table = create_strata_table(
    baseline_wdds_strata,
    baseline_wdds,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_wdds_study_group_table = create_study_group_table(
    baseline_wdds_study_group,
    baseline_wdds,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_wdds_study_group_table_report = create_study_group_table(
    baseline_wdds_study_group,
    baseline_wdds,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_wdds_study_group_province_table =  create_study_group_province_table(
    baseline_wdds_study_group_province,
    baseline_wdds_study_group,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline MDDW table ------------------------------------------------------
  baseline_mddw_province_table = create_province_table(
    baseline_mddw_province,
    baseline_mddw,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_mddw_province_table_report = create_province_table(
    baseline_mddw_province,
    baseline_mddw,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_mddw_strata_table = create_strata_table(
    baseline_mddw_strata,
    baseline_mddw,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_mddw_study_group_table = create_study_group_table(
    baseline_mddw_study_group,
    baseline_mddw,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_mddw_study_group_table_report = create_study_group_table(
    baseline_mddw_study_group,
    baseline_mddw,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_mddw_study_group_province_table = create_study_group_province_table(
    baseline_mddw_study_group_province,
    baseline_mddw_study_group,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline child development table -----------------------------------------
  baseline_child_dev_province_table = create_province_table(
    baseline_child_dev_province,
    baseline_child_dev,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_dev_province_table_report = create_province_table(
    baseline_child_dev_province,
    baseline_child_dev,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_dev_strata_table = create_strata_table(
    baseline_child_dev_strata,
    baseline_child_dev,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_dev_study_group_table = create_study_group_table(
    baseline_child_dev_study_group,
    baseline_child_dev,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_dev_study_group_table_report = create_study_group_table(
    baseline_child_dev_study_group,
    baseline_child_dev,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_dev_study_group_province_table = create_study_group_province_table(
    baseline_child_dev_study_group_province,
    baseline_child_dev_study_group,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline WASH table ------------------------------------------------------
  baseline_wash_province_table = create_province_table(
    baseline_wash_province,
    baseline_wash,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_wash_province_table_report = create_province_table(
    baseline_wash_province,
    baseline_wash,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_wash_strata_table = create_strata_table(
    baseline_wash_strata,
    baseline_wash,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_wash_study_group_table = create_study_group_table(
    baseline_wash_study_group,
    baseline_wash,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_wash_study_group_table_report = create_study_group_table(
    baseline_wash_study_group,
    baseline_wash,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_wash_study_group_province_table = create_study_group_province_table(
    baseline_wash_study_group_province,
    baseline_wash_study_group,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline childhood illnesses table ---------------------------------------
  baseline_child_ill_province_table = create_province_table(
    baseline_child_ill_province,
    baseline_child_ill,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_ill_province_table_report = create_province_table(
    baseline_child_ill_province,
    baseline_child_ill,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_ill_strata_table = create_strata_table(
    baseline_child_ill_strata,
    baseline_child_ill,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_ill_study_group_table = create_study_group_table(
    baseline_child_ill_study_group,
    baseline_child_ill,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_ill_study_group_table_report = create_study_group_table(
    baseline_child_ill_study_group,
    baseline_child_ill,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_ill_study_group_province_table = create_study_group_province_table(
    baseline_child_ill_study_group_province,
    baseline_child_ill_study_group,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline PHQ8 and alcohol consupmtion table ------------------------------
  baseline_women_phq8_province_table = create_province_table(
    baseline_women_phq8_province,
    baseline_women_phq8,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_women_phq8_province_table_report = create_province_table(
    baseline_women_phq8_province,
    baseline_women_phq8,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_women_phq8_strata_table = create_strata_table(
    baseline_women_phq8_strata,
    baseline_women_phq8,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_women_phq8_study_group_table = create_study_group_table(
    baseline_women_phq8_study_group,
    baseline_women_phq8,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_women_phq8_study_group_table_report = create_study_group_table(
    baseline_women_phq8_study_group,
    baseline_women_phq8,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_women_phq8_study_group_province_table = create_study_group_province_table(
    baseline_women_phq8_study_group_province,
    baseline_women_phq8_province,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline pregnant table --------------------------------------------------
  baseline_pregnant_province_table = create_province_table(
    baseline_pregnant_province,
    baseline_pregnant,
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_pregnant_province_table_report = create_province_table(
    baseline_pregnant_province,
    baseline_pregnant,
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_pregnant_strata_table = create_strata_table(
    baseline_pregnant_strata,
    baseline_pregnant,
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_pregnant_study_group_table_report = create_study_group_table(
    baseline_pregnant_study_group,
    baseline_pregnant,
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_pregnant_study_group_province_table = create_study_group_province_table(
    baseline_pregnant_study_group_province,
    baseline_pregnant_study_group,
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline pregnant prevention of disease table ----------------------------
  baseline_pregnant_prevention_province_table = create_province_table(
    baseline_pregnant_prevention_province,
    baseline_pregnant_prevention,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_pregnant_prevention_province_table_report = create_province_table(
    baseline_pregnant_prevention_province,
    baseline_pregnant_prevention,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_pregnant_prevention_strata_table = create_strata_table(
    baseline_pregnant_prevention_strata,
    baseline_pregnant_prevention,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_pregnant_prevention_study_group_table = create_study_group_table(
    baseline_pregnant_prevention_study_group,
    baseline_pregnant_prevention,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_pregnant_prevention_study_group_table_report = create_study_group_table(
    baseline_pregnant_prevention_study_group,
    baseline_pregnant_prevention,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_pregnant_prevention_study_group_province_table = create_study_group_province_table(
    baseline_pregnant_prevention_study_group_province,
    baseline_pregnant_prevention_study_group,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline natal care table ------------------------------------------------
  baseline_natal_care_province_table = create_province_table(
    baseline_natal_care_province,
    baseline_natal_care,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_natal_care_province_table_report = create_province_table(
    baseline_natal_care_province,
    baseline_natal_care,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_natal_care_strata_table = create_strata_table(
    baseline_natal_care_strata,
    baseline_natal_care,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_natal_care_study_group_table = create_study_group_table(
    baseline_natal_care_study_group,
    baseline_natal_care,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_natal_care_study_group_table_report = create_study_group_table(
    baseline_natal_care_study_group,
    baseline_natal_care,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_natal_care_study_group_province_table = create_study_group_province_table(
    baseline_natal_care_study_group_province,
    baseline_natal_care_study_group,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline family planning table -------------------------------------------
  baseline_family_planning_province_table = create_province_table(
    baseline_family_planning_province,
    baseline_family_planning,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_family_planning_province_table_report = create_province_table(
    baseline_family_planning_province,
    baseline_family_planning,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_family_planning_strata_table = create_strata_table(
    baseline_family_planning_strata,
    baseline_family_planning,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_family_planning_study_group_table = create_study_group_table(
    baseline_family_planning_study_group,
    baseline_family_planning,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_family_planning_study_group_table_report = create_study_group_table(
    baseline_family_planning_study_group,
    baseline_family_planning,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_family_planning_study_group_province_table = create_study_group_province_table(
    baseline_family_planning_study_group_province,
    baseline_family_planning_study_group,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline child immunisation table ----------------------------------------
  baseline_child_immunisation_province_table = create_province_table(
    baseline_child_immunisation_province,
    baseline_child_immunisation,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_immunisation_province_table_report = create_province_table(
    baseline_child_immunisation_province,
    baseline_child_immunisation,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_immunisation_strata_table = create_strata_table(
    baseline_child_immunisation_strata,
    baseline_child_immunisation,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_immunisation_study_group_table = create_study_group_table(
    baseline_child_immunisation_study_group,
    baseline_child_immunisation,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_immunisation_study_group_table_report = create_study_group_table(
    baseline_child_immunisation_study_group,
    baseline_child_immunisation,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_immunisation_study_group_province_table = create_study_group_province_table(
    baseline_child_immunisation_study_group_province,
    baseline_child_immunisation_study_group,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline child immunisation full table -----------------------------------
  baseline_child_immunisation_full_province_table = create_province_table(
    baseline_child_immunisation_full_province,
    baseline_child_immunisation_full,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_immunisation_full_province_table_report = create_province_table(
    baseline_child_immunisation_full_province,
    baseline_child_immunisation_full,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_immunisation_full_strata_table = create_strata_table(
    baseline_child_immunisation_full_strata,
    baseline_child_immunisation_full,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_immunisation_full_study_group_table = create_study_group_table(
    baseline_child_immunisation_full_study_group,
    baseline_child_immunisation_full,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_immunisation_full_study_group_table_report = create_study_group_table(
    baseline_child_immunisation_full_study_group,
    baseline_child_immunisation_full,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_immunisation_full_study_group_province_table = create_study_group_province_table(
    baseline_child_immunisation_full_study_group_province,
    baseline_child_immunisation_full_study_group,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline IYCF table ------------------------------------------------------
  baseline_iycf_province_table = create_province_table(
    baseline_iycf_province,
    baseline_iycf,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_iycf_province_table_report = create_province_table(
    baseline_iycf_province,
    baseline_iycf,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_iycf_strata_table = create_strata_table(
    baseline_iycf_strata,
    baseline_iycf,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_iycf_study_group_table = create_study_group_table(
    baseline_iycf_study_group,
    baseline_iycf,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_iycf_study_group_table_report = create_study_group_table(
    baseline_iycf_study_group,
    baseline_iycf,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_iycf_study_group_province_table =  create_study_group_province_table(
    baseline_iycf_study_group_province,
    baseline_iycf_province,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline breastfeeding table ---------------------------------------------
  baseline_breastfeeding_province_table = create_province_table(
    baseline_breastfeeding_province,
    baseline_breastfeeding,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_breastfeeding_province_table_report = create_province_table(
    baseline_breastfeeding_province,
    baseline_breastfeeding,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_breastfeeding_strata_table = create_strata_table(
    baseline_breastfeeding_strata,
    baseline_breastfeeding,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_breastfeeding_stugy_group_table = create_study_group_table(
    baseline_breastfeeding_study_group,
    baseline_breastfeeding,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_breastfeeding_study_group_table_report = create_study_group_table(
    baseline_breastfeeding_study_group,
    baseline_breastfeeding,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_breastfeeding_study_group_province_table = create_study_group_province_table(
    baseline_breastfeeding_study_group_province,
    baseline_breastfeeding_study_group,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline exclusive breastfeeding table -----------------------------------
  baseline_ebf_province_table = create_province_table(
    baseline_ebf_province,
    baseline_ebf,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_ebf_province_table_report = create_province_table(
    baseline_ebf_province,
    baseline_ebf,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_ebf_strata_table = create_strata_table(
    baseline_ebf_strata,
    baseline_ebf,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_ebf_study_group_table = create_study_group_table(
    baseline_ebf_study_group,
    baseline_ebf,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_ebf_study_group_table_report = create_study_group_table(
    baseline_ebf_study_group,
    baseline_ebf,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_ebf_study_group_province_table = create_study_group_province_table(
    baseline_ebf_study_group_province,
    baseline_ebf_study_group,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline child vitamin A coverage table ----------------------------------
  baseline_child_vita_province_table = create_province_table(
    baseline_child_vita_province,
    baseline_child_vita,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_vita_province_table_report = create_province_table(
    baseline_child_vita_province,
    baseline_child_vita,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_vita_strata_table = create_strata_table(
    baseline_child_vita_strata,
    baseline_child_vita,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_vita_study_group_table = create_study_group_table(
    baseline_child_vita_study_group,
    baseline_child_vita,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_child_vita_study_group_table_report = create_study_group_table(
    baseline_child_vita_study_group,
    baseline_child_vita,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_child_vita_study_group_province_table = create_study_group_province_table(
    baseline_child_vita_study_group_province,
    baseline_child_vita_study_group,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline deworming coverage table ----------------------------------------
  baseline_deworming_province_table = create_province_table(
    baseline_deworming_province,
    baseline_deworming,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_deworming_province_table_report = create_province_table(
    baseline_deworming_province,
    baseline_deworming,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_deworming_strata_table = create_strata_table(
    baseline_deworming_strata,
    baseline_deworming,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_deworming_study_group_table = create_study_group_table(
    baseline_deworming_study_group,
    baseline_deworming,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_deworming_study_group_table_report = create_study_group_table(
    baseline_deworming_study_group,
    baseline_deworming,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_deworming_study_group_province_table = create_study_group_province_table(
    baseline_deworming_study_group_province,
    baseline_deworming_study_group,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline women's decision making table -----------------------------------
  baseline_wem_province_table = create_province_table(
    baseline_wem_province,
    baseline_wem,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_wem_province_table_report = create_province_table(
    baseline_wem_province,
    baseline_wem,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_wem_strata_table = create_strata_table(
    baseline_wem_strata,
    baseline_wem,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_wem_study_group_table = create_study_group_table(
    baseline_wem_study_group,
    baseline_wem,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_wem_study_group_table_report = create_study_group_table(
    baseline_wem_study_group,
    baseline_wem,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_wem_study_group_province_table = create_study_group_province_table(
    baseline_wem_study_group_province,
    baseline_wem_study_group,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Baseline women's anthropometry table -------------------------------------
  baseline_women_anthro_province_table = create_province_table(
    baseline_women_anthro_province,
    baseline_women_anthro,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_women_anthro_province_table_report = create_province_table(
    baseline_women_anthro_province,
    baseline_women_anthro,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = TRUE, pivot = "wide"
  ),
  baseline_women_anthro_strata_table = create_strata_table(
    baseline_women_anthro_strata,
    baseline_women_anthro,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_women_anthro_study_group_table = create_study_group_table(
    baseline_women_anthro_study_group,
    baseline_women_anthro,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  baseline_women_anthro_study_group_table_report = create_study_group_table(
    baseline_women_anthro_study_group,
    baseline_women_anthro,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  baseline_women_anthro_study_group_province_table = create_study_group_province_table(
    baseline_women_anthro_study_group_province,
    baseline_women_anthro_study_group,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = FALSE
  )
)

## Read raw endline data -------------------------------------------------------
raw_data_endline <- tar_plan(
  ### Read raw endline data 
  endline_raw_data = get_endline_data() |>
    process_respondent_data() |>
    process_child_data() |>
    ### Filter out training data
    subset(
      (as.Date(today) >= as.Date("2022-04-04") & 
        as.Date(today) <= as.Date("2022-05-11")) |
        as.Date(today) >= "2022-05-21"
    ) |>
    ### Clean up enumeration area identifiers
    clean_endline_ea_ids(survey_sampling_list) |>
    clean_endline_identifiers(survey_sampling_list_endline) |>
    clean_anthro_data(),
  ### Update sampling list for endline
  survey_sampling_list_endline = add_ea_info(survey_sampling_list),
  ### Create SF object of raw endline data for cleaning
  endline_raw_data_sf = create_data_sf(endline_raw_data),
  ### Get subset of data that need EAs to be cleaned/checked
  endline_data_eas_for_checking = table(endline_raw_data$fgh_id) |> 
    data.frame() |> 
    (\(x) x[x$Freq < 8, "Var1"])() |>
    as.character() |>
    as.integer(),
  endline_raw_data_check_sf = subset(
    endline_raw_data_sf, 
    as.integer(fgh_id) %in% endline_data_eas_for_checking |
      as.integer(fgh_id) < 30000
  ),
  ### Get subset of data for anthropometry cleaning/checks
  flagged_anthro_dataset = create_flagged_child_anthro_data(
    .data = endline_raw_data
  ),
  flagged_endline_records = flagged_anthro_dataset |>
    subset(flag != 0 & child_age_months >= 6 & child_age_months < 60),
  flagged_woman_anthro_dataset = create_flagged_woman_anthro_data(
    .data = endline_raw_data
  ),
  flagged_endline_women_records = flagged_woman_anthro_dataset |>
    subset(flag != 0 & resp_sex == 2 & q01 >= 15 & q01 < 60)
)

## Process endline data --------------------------------------------------------
processed_data_endline <- tar_plan(
  ### Get endline sampling weights --------------------------------------------
  endline_sample_weight = calculate_weights(
    .data = endline_raw_data,
    survey_sampling_list_endline,
    type = "endline"
  ),
  ### Process endline data
  endline_data_processed = process_endline_data(
    endline_raw_data, survey_endline_choices
  ), #|>
    #clean_anthro_data(),
  ### Process endline data with weights ---------------------------------------
  endline_data_weighted = dplyr::left_join(
    x = endline_data_processed,
    y = endline_sample_weight |>
      subset(
        select = c(
          ea_id, fgh_id, study_group, 
          cluster_sample_prob_obs, ind_sample_prob_obs,
          sample_prob_obs, sample_weight_obs
        )
      ),
    by = c("ea_id", "fgh_id")
  ),
  ### Set endline survey design for children ----------------------------------
  endline_child_survey_design = survey::svydesign(
    ids = ~ea_id,
    fpc = ~sample_prob_obs,
    strata = ~province + strata,
    data = endline_data_weighted,
    pps = "brewer"
  ),
  ### Set endline survey design for households/respondent ---------------------
  endline_hh_survey_design = survey::svydesign(
    ids = ~ea_id,
    fpc = ~sample_prob_obs,
    strata = ~province + strata,
    data = endline_data_weighted |>
      dplyr::group_by(id) |>
      subset(child_id == 1 | is.na(child_id)) |>
      dplyr::ungroup(),
    pps = "brewer"
  )
)

## Endline analysis -----------------------------------------------------------
analysis_endline <- tar_plan(
  ### endline results - demographics respondent -------------------------------
  endline_demo_respondent = estimate_total(
    vars = c("respondent_sex", "respondent_age_years",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = endline_hh_survey_design
  ),
  endline_demo_respondent_province = estimate_province(
    vars = c("respondent_sex", "respondent_age_years",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = endline_hh_survey_design
  ),
  endline_demo_respondent_strata = estimate_strata(
    vars = c("respondent_sex", "respondent_age_years",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = endline_hh_survey_design
  ),
  endline_demo_respondent_study_group = estimate_study_group(
    vars = c("respondent_sex", "respondent_age_years",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = endline_hh_survey_design
  ),
  endline_demo_respondent_study_group_province = estimate_study_group_province(
    vars = c("respondent_sex", "respondent_age_years",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = endline_hh_survey_design
  ),
  ### endline results - demographics children ---------------------------------
  endline_demo_child = estimate_total(
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    design = endline_child_survey_design
  ),
  endline_demo_child_province = estimate_province(
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    design = endline_child_survey_design
  ),
  endline_demo_child_strata = estimate_strata(
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    design = endline_child_survey_design
  ),
  endline_demo_child_study_group = estimate_study_group(
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    design = endline_child_survey_design
  ),
  endline_demo_child_study_group_province = estimate_study_group_province(
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    design = endline_child_survey_design
  ),
  ### endline results - demographics spouse -----------------------------------
  endline_demo_spouse = estimate_total(
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    design = endline_hh_survey_design
  ),
  endline_demo_spouse_province = estimate_province(
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    design = endline_hh_survey_design
  ),
  endline_demo_spouse_strata = estimate_strata(
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    design = endline_hh_survey_design
  ),
  endline_demo_spouse_study_group = estimate_study_group(
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    design = endline_hh_survey_design
  ),
  endline_demo_spouse_study_group_province = estimate_study_group_province(
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    design = endline_hh_survey_design
  ),
  ### endline results - household income --------------------------------------
  endline_hh_income = estimate_total(
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_income_province = estimate_province(
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_income_strata = estimate_strata(
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_income_study_group = estimate_study_group(
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_income_study_group_province = estimate_study_group_province(
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    design = endline_hh_survey_design
  ),
  ### endline results - household structure -----------------------------------
  endline_hh_structure = estimate_total(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_structure_province = estimate_province(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_structure_strata = estimate_strata(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_structure_study_group = estimate_study_group(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_structure_study_group_province = estimate_study_group_province(
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    design = endline_hh_survey_design
  ),
  ### endline results - household amenities -----------------------------------
  endline_hh_amenities = estimate_total(
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_amenities_province = estimate_province(
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_amenities_strata = estimate_strata(
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_amenities_study_group = estimate_study_group(
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_amenities_study_group_province = estimate_study_group_province(
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    design = endline_hh_survey_design
  ),
  ### endline results - mode of daily travel ----------------------------------
  endline_hh_travel = estimate_total(
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_travel_province = estimate_province(
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_travel_strata = estimate_strata(
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_travel_study_group = estimate_study_group(
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_travel_study_group_province = estimate_study_group_province(
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    design = endline_hh_survey_design
  ),
  ### endline results - household decision making -----------------------------
  endline_hh_decision = estimate_total(
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_decision_province = estimate_province(
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_decision_strata = estimate_strata(
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_decision_study_group = estimate_study_group(
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_decision_study_group_province = estimate_study_group_province(
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    design = endline_hh_survey_design
  ),
  ### endline results - household membership in community groups --------------
  endline_hh_groups = estimate_total(
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_groups_province = estimate_province(
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_groups_strata = estimate_strata(
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_groups_study_group = estimate_study_group(
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    design = endline_hh_survey_design
  ),
  endline_hh_groups_study_group_province = estimate_study_group_province(
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    design = endline_hh_survey_design
  ),
  ### endline results - child anthropometry -----------------------------------
  endline_child_anthro = estimate_total(
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    design = endline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 60)
  ),
  endline_child_anthro_province = estimate_province(
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    design = endline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 60)
  ),
  endline_child_anthro_strata = estimate_strata(
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    design = endline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 60)
  ),
  endline_child_anthro_study_group = estimate_study_group(
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    design = endline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 60)
  ),
  endline_child_anthro_study_group_province = estimate_study_group_province(
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    design = endline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 60)
  ),
  ### endline results - WDDS --------------------------------------------------
  endline_wdds = estimate_total(
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_wdds_province = estimate_province(
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_wdds_strata = estimate_strata(
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_wdds_study_group = estimate_study_group(
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_wdds_study_group_province = estimate_study_group_province(
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  ### endline results - MDD-W -------------------------------------------------
  endline_mddw = estimate_total(
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_mddw_province = estimate_province(
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_mddw_strata = estimate_strata(
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_mddw_study_group = estimate_study_group(
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_mddw_study_group_province = estimate_study_group_province(
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  ### endline results - child development -------------------------------------
  endline_child_dev = estimate_total(
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    design = endline_hh_survey_design
  ),
  endline_child_dev_province = estimate_province(
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    design = endline_hh_survey_design
  ),
  endline_child_dev_strata = estimate_strata(
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    design = endline_hh_survey_design
  ),
  endline_child_dev_study_group = estimate_study_group(
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    design = endline_hh_survey_design
  ),
  endline_child_dev_study_group_province = estimate_study_group_province(
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    design = endline_hh_survey_design
  ),
  ### endline results - WASH --------------------------------------------------
  endline_wash = estimate_total(
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    design = endline_hh_survey_design
  ),
  endline_wash_province = estimate_province(
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    design = endline_hh_survey_design
  ),
  endline_wash_strata = estimate_strata(
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    design = endline_hh_survey_design
  ),
  endline_wash_study_group = estimate_study_group(
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    design = endline_hh_survey_design
  ),
  endline_wash_study_group_province = estimate_study_group_province(
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    design = endline_hh_survey_design
  ),
  ### endline results - period prevalence of childhood illnesses --------------
  endline_child_ill = estimate_total(
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    design = endline_child_survey_design
  ),
  endline_child_ill_province = estimate_province(
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    design = endline_child_survey_design
  ),
  endline_child_ill_strata = estimate_strata(
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    design = endline_child_survey_design
  ),
  endline_child_ill_study_group = estimate_study_group(
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    design = endline_child_survey_design
  ),
  endline_child_ill_study_group_province = estimate_study_group_province(
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    design = endline_child_survey_design
  ),
  ### endline results - women's mental health and alcohol consumption ---------
  endline_women_phq8 = estimate_total(
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    design = endline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  endline_women_phq8_province = estimate_province(
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    design = endline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  endline_women_phq8_strata = estimate_strata(
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    design = endline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  endline_women_phq8_study_group = estimate_study_group(
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    design = endline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  endline_women_phq8_study_group_province = estimate_study_group_province(
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    design = endline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  ### endline results - pregnancy characteristics -----------------------------
  endline_pregnant = estimate_total(
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    design = endline_hh_survey_design |>
      subset(currently_pregnant == 1)
  ),
  endline_pregnant_province = estimate_province(
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    design = endline_hh_survey_design |>
      subset(currently_pregnant == 1)
  ),
  endline_pregnant_strata = estimate_strata(
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    design = endline_hh_survey_design |>
      subset(currently_pregnant == 1)
  ),
  endline_pregnant_study_group = estimate_study_group(
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    design = endline_hh_survey_design |>
      subset(currently_pregnant == 1)
  ),
  endline_pregnant_study_group_province = estimate_study_group_province(
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    design = endline_hh_survey_design |>
      subset(currently_pregnant == 1)
  ),
  ### endline results - PMTCT and mosquito net --------------------------------
  endline_pregnant_prevention = estimate_total(
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_pregnant_prevention_province = estimate_province(
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_pregnant_prevention_strata = estimate_strata(
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_pregnant_prevention_study_group = estimate_study_group(
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_pregnant_prevention_study_group_province = estimate_study_group_province(
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  ### endline results - natal care --------------------------------------------
  endline_natal_care = estimate_total(
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_natal_care_province = estimate_province(
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_natal_care_strata = estimate_strata(
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_natal_care_study_group = estimate_study_group(
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_natal_care_study_group_province = estimate_study_group_province(
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  ### endline results - Family planning ----------------------------------------------------------
  endline_family_planning = estimate_total(
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_family_planning_province = estimate_province(
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_family_planning_strata = estimate_strata(
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_family_planning_study_group = estimate_study_group(
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_family_planning_study_group_province = estimate_study_group_province(
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  ### endline results - EPI ---------------------------------------------------
  endline_child_immunisation = estimate_total(
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    design = endline_child_survey_design
  ),
  endline_child_immunisation_province = estimate_province(
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    design = endline_child_survey_design
  ),
  endline_child_immunisation_strata = estimate_strata(
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    design = endline_child_survey_design
  ),
  endline_child_immunisation_study_group = estimate_study_group(
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    design = endline_child_survey_design
  ),
  endline_child_immunisation_study_group_province = estimate_study_group_province(
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    design = endline_child_survey_design
  ),
  ### endline results - full immunisation -------------------------------------
  endline_child_immunisation_full = estimate_total(
    vars = "immunisation_fully_immunised",
    design = endline_child_survey_design |>
      subset(child_age_months >= 12 & child_age_months < 24)
  ),
  endline_child_immunisation_full_province = estimate_province(
    vars = "immunisation_fully_immunised",
    design = endline_child_survey_design |>
      subset(child_age_months >= 12 & child_age_months < 24)
  ),
  endline_child_immunisation_full_strata = estimate_strata(
    vars = "immunisation_fully_immunised",
    design = endline_child_survey_design |>
      subset(child_age_months >= 12 & child_age_months < 24)
  ),
  endline_child_immunisation_full_study_group = estimate_study_group(
    vars = "immunisation_fully_immunised",
    design = endline_child_survey_design |>
      subset(child_age_months >= 12 & child_age_months < 24)
  ),
  endline_child_immunisation_full_study_group_province = estimate_study_group_province(
    vars = "immunisation_fully_immunised",
    design = endline_child_survey_design |>
      subset(child_age_months >= 12 & child_age_months < 24)
  ),
  ### endline results - IYCF --------------------------------------------------
  endline_iycf = estimate_total(
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    design = endline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 24)
  ),
  endline_iycf_province = estimate_province(
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    design = endline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 24)
  ),
  endline_iycf_strata = estimate_strata(
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    design = endline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 24)
  ),
  endline_iycf_study_group = estimate_study_group(
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    design = endline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 24)
  ),
  endline_iycf_study_group_province = estimate_study_group_province(
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    design = endline_child_survey_design |>
      subset(child_age_months >= 6 & child_age_months < 24)
  ),
  ### endline results - breastfeeding -----------------------------------------
  endline_breastfeeding = estimate_total(
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    design = endline_child_survey_design |>
      subset(child_age_months < 24)
  ),
  endline_breastfeeding_province = estimate_province(
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    design = endline_child_survey_design |>
      subset(child_age_months < 24)
  ),
  endline_breastfeeding_strata = estimate_strata(
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    design = endline_child_survey_design |>
      subset(child_age_months < 24)
  ),
  endline_breastfeeding_study_group = estimate_study_group(
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    design = endline_child_survey_design |>
      subset(child_age_months < 24)
  ),
  endline_breastfeeding_study_group_province = estimate_study_group_province(
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    design = endline_child_survey_design |>
      subset(child_age_months < 24)
  ),
  ### endline results - exclusive breastfeeding -------------------------------
  endline_ebf = estimate_total(
    vars = "exclusive_breastfeeding",
    design = endline_child_survey_design |>
      subset(child_age_months < 6)
  ),
  endline_ebf_province = estimate_province(
    vars = "exclusive_breastfeeding",
    design = endline_child_survey_design |>
      subset(child_age_months < 6)
  ),
  endline_ebf_strata = estimate_strata(
    vars = "exclusive_breastfeeding",
    design = endline_child_survey_design |>
      subset(child_age_months < 6)
  ),
  endline_ebf_study_group = estimate_study_group(
    vars = "exclusive_breastfeeding",
    design = endline_child_survey_design |>
      subset(child_age_months < 6)
  ),
  endline_ebf_study_group_province = estimate_study_group_province(
    vars = "exclusive_breastfeeding",
    design = endline_child_survey_design |>
      subset(child_age_months < 6)
  ),
  ### endline results - vitamin A supplementation for children 6-59 -----------
  endline_child_vita = estimate_total(
    vars = "vitamin_a_supplementation_coverage",
    design = endline_child_survey_design |>
      subset(child_age_months >= 6)
  ),
  endline_child_vita_province = estimate_province(
    vars = "vitamin_a_supplementation_coverage",
    design = endline_child_survey_design |>
      subset(child_age_months >= 6)
  ),
  endline_child_vita_strata = estimate_strata(
    vars = "vitamin_a_supplementation_coverage",
    design = endline_child_survey_design |>
      subset(child_age_months >= 6)
  ),
  endline_child_vita_study_group = estimate_study_group(
    vars = "vitamin_a_supplementation_coverage",
    design = endline_child_survey_design |>
      subset(child_age_months >= 6)
  ),
  endline_child_vita_study_group_province = estimate_study_group_province(
    vars = "vitamin_a_supplementation_coverage",
    design = endline_child_survey_design |>
      subset(child_age_months >= 6)
  ),
  ### endline results - deworming coverage for children 12-59 -----------------
  endline_deworming = estimate_total(
    vars = "deworming_coverage",
    design = endline_child_survey_design |>
      subset(child_age_months >= 12)
  ),
  endline_deworming_province = estimate_province(
    vars = "deworming_coverage",
    design = endline_child_survey_design |>
      subset(child_age_months >= 12)
  ),
  endline_deworming_strata = estimate_strata(
    vars = "deworming_coverage",
    design = endline_child_survey_design |>
      subset(child_age_months >= 12)
  ),
  endline_deworming_study_group = estimate_study_group(
    vars = "deworming_coverage",
    design = endline_child_survey_design |>
      subset(child_age_months >= 12)
  ),
  endline_deworming_study_group_province = estimate_study_group_province(
    vars = "deworming_coverage",
    design = endline_child_survey_design |>
      subset(child_age_months >= 12)
  ),
  ### endline results - women's decision making -------------------------------
  endline_wem = estimate_total(
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    design = endline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  endline_wem_province = estimate_province(
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    design = endline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  endline_wem_strata = estimate_strata(
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    design = endline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  endline_wem_study_group = estimate_study_group(
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    design = endline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  endline_wem_study_group_province = estimate_study_group_province(
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    design = endline_hh_survey_design |>
      subset(respondent_sex == "Mulher")
  ),
  ### endline results - women's anthropometry ---------------------------------
  endline_women_anthro = estimate_total(
    vars = c("body_mass_index", "bmi_class"),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_women_anthro_province = estimate_province(
    vars = c("body_mass_index", "bmi_class"),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_women_anthro_strata = estimate_strata(
    vars = c("body_mass_index", "bmi_class"),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_women_anthro_study_group = estimate_study_group(
    vars = c("body_mass_index", "bmi_class"),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  ),
  endline_women_anthro_study_group_province = estimate_study_group_province(
    vars = c("body_mass_index", "bmi_class"),
    design = endline_hh_survey_design |>
      subset(
        respondent_sex == "Mulher" & 
          respondent_age_years >= 15 & 
          respondent_age_years < 50
      )
  )
)

## Outputs - endline -----------------------------------------------------------
outputs_tables_endline <- tar_plan(
  ### Endline demographics table - respondent ---------------------------------
  endline_demo_respondent_table = create_province_table(
    endline_demo_respondent_province,
    endline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_demo_respondent_table_report = create_province_table(
    endline_demo_respondent_province,
    endline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_demo_respondent_strata_table = create_strata_table(
    endline_demo_respondent_strata,
    endline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_demo_respondent_study_group_table = create_study_group_table(
    endline_demo_respondent_study_group,
    endline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_demo_respondent_study_group_table_report = create_study_group_table(
    endline_demo_respondent_study_group,
    endline_demo_respondent,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_demo_respondent_study_group_province_table = create_study_group_province_table(
    endline_demo_respondent_study_group_province,
    endline_demo_respondent_study_group,
    vars = c("respondent_sex", "respondent_age_years", "respondent_age_group", 
             "respondent_language", "respondent_civil_status", 
             "respondent_education_years", "respondent_education_group", 
             "respondent_occupation"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline demographics table - child --------------------------------------
  endline_demo_child_table = create_province_table(
    endline_demo_child_province,
    endline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_demo_child_table_report = create_province_table(
    endline_demo_child_province,
    endline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_demo_child_strata_table = create_strata_table(
    endline_demo_child_strata,
    endline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_demo_child_study_group_table = create_study_group_table(
    endline_demo_child_study_group,
    endline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_demo_child_study_group_table_report = create_study_group_table(
    endline_demo_child_study_group,
    endline_demo_child,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_demo_child_study_group_province_table = create_study_group_province_table(
    endline_demo_child_study_group_province,
    endline_demo_child_study_group,
    vars = c("child_sex", "child_age_months",
             "child_age_group", "child_currently_breastfeeding",
             "child_parent_age_at_birth", "child_location_of_birth",
             "child_caesarean_birth", "child_complications_at_birth",
             "child_low_birth_weight"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline demographics table - spouse -------------------------------------
  endline_demo_spouse_table = create_province_table(
    endline_demo_spouse_province,
    endline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_demo_spouse_table_report = create_province_table(
    endline_demo_spouse_province,
    endline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_demo_spouse_strata_table = create_strata_table(
    endline_demo_spouse_strata,
    endline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_demo_spouse_study_group_table = create_study_group_table(
    endline_demo_spouse_study_group,
    endline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_demo_spouse_study_group_table_report = create_study_group_table(
    endline_demo_spouse_study_group,
    endline_demo_spouse,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_demo_spouse_study_group_province_table = create_study_group_province_table(
    endline_demo_spouse_study_group_province,
    endline_demo_spouse_study_group,
    vars = c("spouse_age_years", "spouse_age_group",
             "spouse_education_years", "spouse_education_group",
             "spouse_occupation", "spouse_lives_in_home"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline household income table ------------------------------------------
  endline_hh_income_table = create_province_table(
    endline_hh_income_province,
    endline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_income_table_report = create_province_table(
    endline_hh_income_province,
    endline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_income_strata_table = create_strata_table(
    endline_hh_income_strata,
    endline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_income_study_group_table = create_study_group_table(
    endline_hh_income_study_group,
    endline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_income_study_group_table_report = create_study_group_table(
    endline_hh_income_study_group,
    endline_hh_income,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_income_study_group_province_table = create_study_group_province_table(
    endline_hh_income_study_group_province,
    endline_hh_income_study_group,
    vars = c(
      "persons_living_in_household", "children_under_five_living_in_household",
      "pregnant_women_living_in_household", "monthly_household_income",
      "source_of_household_income", "sufficiency_of_household_income",
      "sufficiency_of_family_resource", "household_income_against_expenses"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline household structure table ---------------------------------------
  endline_hh_structure_table = create_province_table(
    endline_hh_structure_province,
    endline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_structure_table_report = create_province_table(
    endline_hh_structure_province,
    endline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_structure_strata_table = create_strata_table(
    endline_hh_structure_strata,
    endline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_structure_study_group_table = create_study_group_table(
    endline_hh_structure_study_group,
    endline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_structure_study_group_table_report = create_study_group_table(
    endline_hh_structure_study_group,
    endline_hh_structure,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_structure_study_group_province_table = create_study_group_province_table(
    endline_hh_structure_study_group_province,
    endline_hh_structure_study_group,
    vars = c(
      "home_ownership_own", "home_ownership_rent", "home_ownership_loan",
      "number_of_rooms_in_home", "number_of_bedrooms_in_home", 
      "roofing_material", "floor_material", "time_living_in_location_in_months", 
      "time_living_in_location_group"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline household amenities table ---------------------------------------
  endline_hh_amenities_table = create_province_table(
    endline_hh_amenities_province,
    endline_hh_amenities,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_amenities_table_report = create_province_table(
    endline_hh_amenities_province,
    endline_hh_amenities,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_amenities_strata_table = create_strata_table(
    endline_hh_amenities_strata,
    endline_hh_amenities,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_amenities_study_group_table = create_study_group_table(
    endline_hh_amenities_study_group,
    endline_hh_amenities,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_amenities_study_group_table_report = create_study_group_table(
    endline_hh_amenities_study_group,
    endline_hh_amenities,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_amenities_study_group_province_table = create_study_group_province_table(
    endline_hh_amenities_study_group_province,
    endline_hh_amenities_study_group,
    vars = c(
      "electricity", "cellphone", "computer", "landline", "radio", "television",
      "housekeeper_childcare_employee", "refrigerator", 
      "refrigerator_alternative", "number_of_mosquito_nets",
      "fuel_used_for_cooking", "location_of_food_preparation",
      "fuel_used_for_lighting"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline household travel table ------------------------------------------
  endline_hh_travel_province_table = create_province_table(
    endline_hh_travel_province,
    endline_hh_travel,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_travel_province_table_report = create_province_table(
    endline_hh_travel_province,
    endline_hh_travel,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_travel_strata_table = create_strata_table(
    endline_hh_travel_strata,
    endline_hh_travel,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_travel_study_group_table = create_study_group_table(
    endline_hh_travel_study_group,
    endline_hh_travel,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_travel_study_group_table_report = create_study_group_table(
    endline_hh_travel_study_group,
    endline_hh_travel,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_travel_study_group_province_table = create_study_group_province_table(
    endline_hh_travel_study_group_province,
    endline_hh_travel_study_group,
    vars = c(
      "usual_mode_of_travel",
      "time_to_travel_to_health_centre", "mode_of_travel_to_health_centre",
      "time_to_travel_to_local_markets", "mode_of_travel_to_local_markets",
      "time_to_travel_to_primary_school", "mode_of_travel_to_primary_school",
      "time_to_travel_to_secondary_school", "mode_of_travel_to_secondary_school"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline household decisions table ---------------------------------------
  endline_hh_decision_province_table = create_province_table(
    endline_hh_decision_province,
    endline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_decision_province_table_report = create_province_table(
    endline_hh_decision_province,
    endline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_decision_strata_table = create_strata_table(
    endline_hh_decision_strata,
    endline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_decision_study_group_table = create_study_group_table(
    endline_hh_decision_study_group,
    endline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_decision_study_group_table_report = create_study_group_table(
    endline_hh_decision_study_group,
    endline_hh_decision,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_decision_study_group_province_table = create_study_group_province_table(
    endline_hh_decision_study_group_province,
    endline_hh_decision_study_group,
    vars = c(
      "marrying_age", "using_condoms", "household_responsibilities",
      "family_planning", "agricultural_tasks", "household_finances",
      "child_rearing", "child_discipline", "healthcare_in_pregnancy",
      "healthcare_for_child"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline household community group membership table ----------------------
  endline_hh_groups_province_table = create_province_table(
    endline_hh_groups_province,
    endline_hh_groups,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_groups_province_table_report = create_province_table(
    endline_hh_groups_province,
    endline_hh_groups,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_groups_strata_table = create_strata_table(
    endline_hh_groups_strata,
    endline_hh_groups,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_hh_groups_study_group_table = create_study_group_table(
    endline_hh_groups_study_group,
    endline_hh_groups,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report =  FALSE
  ),
  endline_hh_groups_study_group_table_report = create_study_group_table(
    endline_hh_groups_study_group,
    endline_hh_groups,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_hh_groups_study_group_province_table = create_study_group_province_table(
    endline_hh_groups_study_group_province,
    endline_hh_groups_study_group,
    vars = c(
      "group_membership", "presentation_participation", 
      "information_application", "health_tasks_participation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline child anthropometry table ---------------------------------------
  endline_child_anthro_province_table = create_province_table(
    endline_child_anthro_province,
    endline_child_anthro,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_anthro_province_table_report = create_province_table(
    endline_child_anthro_province,
    endline_child_anthro,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_anthro_strata_table = create_strata_table(
    endline_child_anthro_strata,
    endline_child_anthro,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_anthro_study_group_table = create_study_group_table(
    endline_child_anthro_study_group,
    endline_child_anthro,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_anthro_study_group_table_report = create_study_group_table(
    endline_child_anthro_study_group,
    endline_child_anthro,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_anthro_study_group_province_table = create_study_group_province_table(
    endline_child_anthro_study_group_province,
    endline_child_anthro_study_group,
    vars = c(
      "hfaz", "global_stunting", "moderate_stunting", "severe_stunting",
      "wfaz", "global_underweight", "moderate_underweight", "severe_underweight",
      "wfhz", "global_wasting_by_weight_for_height",
      "moderate_wasting_by_weight_for_height",
      "severe_wasting_by_weight_for_height",
      "child_muac", "global_wasting_by_muac", "moderate_wasting_by_muac",
      "severe_wasting_by_muac", "severe_wasting_by_oedema"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline WDDS table ------------------------------------------------------
  endline_wdds_province_table = create_province_table(
    endline_wdds_province,
    endline_wdds,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_wdds_province_table_report = create_province_table(
    endline_wdds_province,
    endline_wdds,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_wdds_strata_table = create_strata_table(
    endline_wdds_strata,
    endline_wdds,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_wdds_study_group_table = create_study_group_table(
    endline_wdds_study_group,
    endline_wdds,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_wdds_study_group_table_report = create_study_group_table(
    endline_wdds_study_group,
    endline_wdds,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_wdds_study_group_province_table =  create_study_group_province_table(
    endline_wdds_study_group_province,
    endline_wdds_study_group,
    vars = c(
      "wdds_staples", "wdds_green_leafy", "wdds_other_vita",
      "wdds_fruits_vegetables", "wdds_organ_meat", "wdds_meat_fish",
      "wdds_eggs", "wdds_legumes", "wdds_milk", "wdds"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline MDDW table ------------------------------------------------------
  endline_mddw_province_table = create_province_table(
    endline_mddw_province,
    endline_mddw,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_mddw_province_table_report = create_province_table(
    endline_mddw_province,
    endline_mddw,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_mddw_strata_table = create_strata_table(
    endline_mddw_strata,
    endline_mddw,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_mddw_study_group_table = create_study_group_table(
    endline_mddw_study_group,
    endline_mddw,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_mddw_study_group_table_report = create_study_group_table(
    endline_mddw_study_group,
    endline_mddw,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_mddw_study_group_province_table = create_study_group_province_table(
    endline_mddw_study_group_province,
    endline_mddw_study_group,
    vars = c(
      "mddw_staples", "mddw_pulses", "mddw_nuts_seeds", "mddw_milk",
      "mddw_meat_fish", "mddw_eggs", "mddw_green_leafy", "mddw_other_vita",
      "mddw_vegetables", "mddw_fruits", "mddw_score", "mddw"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline child development table -----------------------------------------
  endline_child_dev_province_table = create_province_table(
    endline_child_dev_province,
    endline_child_dev,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_dev_province_table_report = create_province_table(
    endline_child_dev_province,
    endline_child_dev,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_dev_strata_table = create_strata_table(
    endline_child_dev_strata,
    endline_child_dev,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_dev_study_group_table = create_study_group_table(
    endline_child_dev_study_group,
    endline_child_dev,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_dev_study_group_table_report = create_study_group_table(
    endline_child_dev_study_group,
    endline_child_dev,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_dev_study_group_province_table = create_study_group_province_table(
    endline_child_dev_study_group_province,
    endline_child_dev_study_group,
    vars = c(
      "sing_to_or_with_child", "take_child_for_a_walk", 
      "play_a_game_with_child", "read_books_or_look_at_photos",
      "tell_stories_to_child", "identify_objects_with_child",
      "draw_things_to_or_with_child", "child_has_place_for_toys",
      "play_with_child_during_bath", "play_with_child_while_feeding_child",
      "play_with_child_while_changing_clothes", 
      "play_with_child_while_working_at_home",
      "play_with_child_while_working_in_the_field",
      "play_with_child_during_free_time"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline WASH table ------------------------------------------------------
  endline_wash_province_table = create_province_table(
    endline_wash_province,
    endline_wash,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_wash_province_table_report = create_province_table(
    endline_wash_province,
    endline_wash,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_wash_strata_table = create_strata_table(
    endline_wash_strata,
    endline_wash,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_wash_study_group_table = create_study_group_table(
    endline_wash_study_group,
    endline_wash,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_wash_study_group_table_report = create_study_group_table(
    endline_wash_study_group,
    endline_wash,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_wash_study_group_province_table = create_study_group_province_table(
    endline_wash_study_group_province,
    endline_wash_study_group,
    vars = c(
      "surface_water_source", "unimproved_water_source", "limited_water_source",
      "basic_water_source", "sufficient_water_source",
      "open_defecation", "unimproved_toilet_facility", "limited_toilet_facility",
      "basic_toilet_facility",
      "no_handwashing_facility", "limited_handwashing_facility",
      "basic_handwashing_facility"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline childhood illnesses table ---------------------------------------
  endline_child_ill_province_table = create_province_table(
    endline_child_ill_province,
    endline_child_ill,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_ill_province_table_report = create_province_table(
    endline_child_ill_province,
    endline_child_ill,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_ill_strata_table = create_strata_table(
    endline_child_ill_strata,
    endline_child_ill,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_ill_study_group_table = create_study_group_table(
    endline_child_ill_study_group,
    endline_child_ill,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_ill_study_group_table_report = create_study_group_table(
    endline_child_ill_study_group,
    endline_child_ill,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_ill_study_group_province_table = create_study_group_province_table(
    endline_child_ill_study_group_province,
    endline_child_ill_study_group,
    vars = c(
      "diarrhoea_episode", "diarrhoea_seek_treatment", 
      "diarrhoea_point_of_care", "diarrhoea_treatment_with_ors", 
      "fever_episode", "fever_seek_treatment", "fever_point_of_care",
      "fever_malaria_test", "fever_malaria_episode", "fever_malaria_coartem",
      "fever_malaria_amodiaquina_artesunato", "fever_malaria_fansidar", 
      "fever_malaria_quinino", "fever_malaria_quinino_injection",
      "fever_malaria_artesunato", "fever_malaria_paracetamol_comprimido_xarope",
      "fever_malaria_treatment_intake", "rti_episode", "rti_seek_treatment", 
      "rti_point_of_care", "rti_treatment_antibioticos",
      "rti_treatment_paracetamol", "rti_treatment_aspirina",
      "rti_treatment_ibuprofeno", "rti_treatment_other"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline PHQ8 and alcohol consupmtion table ------------------------------
  endline_women_phq8_province_table = create_province_table(
    endline_women_phq8_province,
    endline_women_phq8,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_women_phq8_province_table_report = create_province_table(
    endline_women_phq8_province,
    endline_women_phq8,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_women_phq8_strata_table = create_strata_table(
    endline_women_phq8_strata,
    endline_women_phq8,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_women_phq8_study_group_table = create_study_group_table(
    endline_women_phq8_study_group,
    endline_women_phq8,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_women_phq8_study_group_table_report = create_study_group_table(
    endline_women_phq8_study_group,
    endline_women_phq8,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_women_phq8_study_group_province_table = create_study_group_province_table(
    endline_women_phq8_study_group_province,
    endline_women_phq8_province,
    vars = c(
      "phq8_score", "major_depression", "severe_depression",
      "at_least_major_depression", "alcohol_consumption"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline pregnant table --------------------------------------------------
  endline_pregnant_province_table = create_province_table(
    endline_pregnant_province,
    endline_pregnant,
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_pregnant_province_table_report = create_province_table(
    endline_pregnant_province,
    endline_pregnant,
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_pregnant_strata_table = create_strata_table(
    endline_pregnant_strata,
    endline_pregnant,
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_pregnant_study_group_table_report = create_study_group_table(
    endline_pregnant_study_group,
    endline_pregnant,
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_pregnant_study_group_province_table = create_study_group_province_table(
    endline_pregnant_study_group_province,
    endline_pregnant_study_group,
    vars = c(
      "weeks_of_gestation_self_report",
      "prenatal_card_self_report", "prenatal_card_available",
      "malaria_during_pregnancy", "anemia_during_pregnancy",
      "excluded_foods_from_diet", "included_foods_from_diet",
      "wants_more_children", "vaginal_bleeding", "severe_headache",
      "blurry_vision", "swollen_extremities", "convulsions",
      "fever", "intense_abdominal_pain", "loss_of_consciousness",
      "fatigue", "plans_when_labor_begins"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline pregnant prevention of disease table ----------------------------
  endline_pregnant_prevention_province_table = create_province_table(
    endline_pregnant_prevention_province,
    endline_pregnant_prevention,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_pregnant_prevention_province_table_report = create_province_table(
    endline_pregnant_prevention_province,
    endline_pregnant_prevention,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_pregnant_prevention_strata_table = create_strata_table(
    endline_pregnant_prevention_strata,
    endline_pregnant_prevention,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_pregnant_prevention_study_group_table = create_study_group_table(
    endline_pregnant_prevention_study_group,
    endline_pregnant_prevention,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_pregnant_prevention_study_group_table_report = create_study_group_table(
    endline_pregnant_prevention_study_group,
    endline_pregnant_prevention,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_pregnant_prevention_study_group_province_table = create_study_group_province_table(
    endline_pregnant_prevention_study_group_province,
    endline_pregnant_prevention_study_group,
    vars = c(
      "offered_voluntary_counselling_and_testing",
      "received_vct_results", "offered_medication_to_reduce_child_risk",
      "received_mosquito_net", "slept_under_mosquito_net"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline natal care table ------------------------------------------------
  endline_natal_care_province_table = create_province_table(
    endline_natal_care_province,
    endline_natal_care,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_natal_care_province_table_report = create_province_table(
    endline_natal_care_province,
    endline_natal_care,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_natal_care_strata_table = create_strata_table(
    endline_natal_care_strata,
    endline_natal_care,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_natal_care_study_group_table = create_study_group_table(
    endline_natal_care_study_group,
    endline_natal_care,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_natal_care_study_group_table_report = create_study_group_table(
    endline_natal_care_study_group,
    endline_natal_care,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_natal_care_study_group_province_table = create_study_group_province_table(
    endline_natal_care_study_group_province,
    endline_natal_care_study_group,
    vars = c(
      "location_of_last_delivery", "number_of_prenatal_visits",
      "at_least_four_anc_visits", "treated_well_during_anc",
      "treated_well_at_delivery", "delivery_assisted_by_doctor",
      "delivery_assisted_by_nurse", "delivery_assisted_by_midwife",
      "delivery_assisted_by_other_person", 
      "delivery_assisted_by_traditional_midwife",
      "delivery_assisted_by_community_health_worker",
      "delivery_assisted_by_relative_or_friend", "delivery_assisted_by_other", 
      "delivery_assisted_by_nobody", "difficulty_reaching_facility_due_to_cost",
      "difficulty_reaching_facility_due_to_distance",
      "difficulty_reaching_facility_due_to_stigma",
      "difficulty_reaching_facility_due_to_poor_roads",
      "difficulty_reaching_facility_due_to_other_reasons",
      "difficulty_reaching_facility_no_difficulty",
      "time_to_postnatal_check_for_child", "time_to_postnatal_check_for_mother",
      "given_malaria_treatment_during_pregnancy", 
      "took_malaria_treatment_during_pregnancy",
      "completed_malaria_treatment_during_pregnancy",
      "at_least_one_tetanus_toxoid_vaccination",
      "two_or_more_tetanus_toxoid_vaccination",
      "ferrous_sulfate_supplementation",
      "vitamin_a_supplementation_during_pregnancy"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline family planning table -------------------------------------------
  endline_family_planning_province_table = create_province_table(
    endline_family_planning_province,
    endline_family_planning,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_family_planning_province_table_report = create_province_table(
    endline_family_planning_province,
    endline_family_planning,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_family_planning_strata_table = create_strata_table(
    endline_family_planning_strata,
    endline_family_planning,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_family_planning_study_group_table = create_study_group_table(
    endline_family_planning_study_group,
    endline_family_planning,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_family_planning_study_group_table_report = create_study_group_table(
    endline_family_planning_study_group,
    endline_family_planning,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_family_planning_study_group_province_table = create_study_group_province_table(
    endline_family_planning_study_group_province,
    endline_family_planning_study_group,
    vars = c(
      "attempted_to_delay_or_prevent_pregnancy",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_mother",
      "benefit_of_waiting_for_next_pregnancy_less_danger_to_health_of_baby",
      "benefit_of_waiting_for_next_pregnancy_avoid_poverty",
      "benefit_of_waiting_for_next_pregnancy_more_likely_that_children_are_educated",
      "benefit_of_waiting_for_next_pregnancy_other_reasons",
      "benefit_of_waiting_for_next_pregnancy_none",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_mother",
      "benefit_of_waiting_until_18_years_of_age_less_danger_to_health_of_baby",
      "benefit_of_waiting_until_18_years_of_age_avoid_poverty",
      "benefit_of_waiting_until_18_years_of_age_more_likely_that_children_are_educated",
      "benefit_of_waiting_until_18_years_of_age_other_reasons",
      "benefit_of_waiting_until_18_years_of_age_none",
      "problem_with_having_more_than_4_children_maternal_mortality",
      "problem_with_having_more_than_4_children_child_mortality",
      "problem_with_having_more_than_4_children_poverty",
      "problem_with_having_more_than_4_children_more_likely_that_children_are_not_educated",
      "problem_with_having_more_than_4_children_other_reasons",
      "problem_with_having_more_than_4_children_none"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline child immunisation table ----------------------------------------
  endline_child_immunisation_province_table = create_province_table(
    endline_child_immunisation_province,
    endline_child_immunisation,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_immunisation_province_table_report = create_province_table(
    endline_child_immunisation_province,
    endline_child_immunisation,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_immunisation_strata_table = create_strata_table(
    endline_child_immunisation_strata,
    endline_child_immunisation,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_immunisation_study_group_table = create_study_group_table(
    endline_child_immunisation_study_group,
    endline_child_immunisation,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_immunisation_study_group_table_report = create_study_group_table(
    endline_child_immunisation_study_group,
    endline_child_immunisation,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_immunisation_study_group_province_table = create_study_group_province_table(
    endline_child_immunisation_study_group_province,
    endline_child_immunisation_study_group,
    vars = c(
      "immunisation_card_retention_self_report", "immunisation_card_retention",
      "immunisation_bcg", "immunisation_polio_first_dose",
      "immunisation_polio_second_dose", "immunisation_polio_third_dose",
      "immunisation_polio_fourth_dose", "immunisation_pentavalent_first_dose",
      "immunisation_pentavalent_second_dose", "immunisation_pentavalent_third_dose",
      "immunisation_measles_first_dose", "immunisation_measles_second_dose",
      "immunisation_pneumococcal_first_dose", "immunisation_pneumococcal_second_dose",
      "immunisation_pneumococcal_third_dose", "immunisation_rotavirus_first_dose",
      "immunisation_rotavirus_second_dose",
      "immunisation_age_appropriate_immunisation"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline child immunisation full table -----------------------------------
  endline_child_immunisation_full_province_table = create_province_table(
    endline_child_immunisation_full_province,
    endline_child_immunisation_full,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_immunisation_full_province_table_report = create_province_table(
    endline_child_immunisation_full_province,
    endline_child_immunisation_full,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_immunisation_full_strata_table = create_strata_table(
    endline_child_immunisation_full_strata,
    endline_child_immunisation_full,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_immunisation_full_study_group_table = create_study_group_table(
    endline_child_immunisation_full_study_group,
    endline_child_immunisation_full,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_immunisation_full_study_group_table_report = create_study_group_table(
    endline_child_immunisation_full_study_group,
    endline_child_immunisation_full,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_immunisation_full_study_group_province_table = create_study_group_province_table(
    endline_child_immunisation_full_study_group_province,
    endline_child_immunisation_full_study_group,
    vars = "immunisation_fully_immunised",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline IYCF table ------------------------------------------------------
  endline_iycf_province_table = create_province_table(
    endline_iycf_province,
    endline_iycf,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_iycf_province_table_report = create_province_table(
    endline_iycf_province,
    endline_iycf,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_iycf_strata_table = create_strata_table(
    endline_iycf_strata,
    endline_iycf,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_iycf_study_group_table = create_study_group_table(
    endline_iycf_study_group,
    endline_iycf,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_iycf_study_group_table_report = create_study_group_table(
    endline_iycf_study_group,
    endline_iycf,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_iycf_study_group_province_table =  create_study_group_province_table(
    endline_iycf_study_group_province,
    endline_iycf_province,
    vars = c(
      "food_group_breastmilk", "food_group_dairy", "food_group_starch",
      "food_group_vitamin_a_rich", "food_group_other_fruits_vegetables",
      "food_group_legumes", "food_group_meat", "food_group_eggs",
      "food_groups_score", "minimum_dietary_diversity"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline breastfeeding table ---------------------------------------------
  endline_breastfeeding_province_table = create_province_table(
    endline_breastfeeding_province,
    endline_breastfeeding,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_breastfeeding_province_table_report = create_province_table(
    endline_breastfeeding_province,
    endline_breastfeeding,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_breastfeeding_strata_table = create_strata_table(
    endline_breastfeeding_strata,
    endline_breastfeeding,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_breastfeeding_stugy_group_table = create_study_group_table(
    endline_breastfeeding_study_group,
    endline_breastfeeding,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_breastfeeding_study_group_table_report = create_study_group_table(
    endline_breastfeeding_study_group,
    endline_breastfeeding,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_breastfeeding_study_group_province_table = create_study_group_province_table(
    endline_breastfeeding_study_group_province,
    endline_breastfeeding_study_group,
    vars = c("ever_breastfed", "early_initiation_of_breastfeeding"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline exclusive breastfeeding table -----------------------------------
  endline_ebf_province_table = create_province_table(
    endline_ebf_province,
    endline_ebf,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_ebf_province_table_report = create_province_table(
    endline_ebf_province,
    endline_ebf,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_ebf_strata_table = create_strata_table(
    endline_ebf_strata,
    endline_ebf,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_ebf_study_group_table = create_study_group_table(
    endline_ebf_study_group,
    endline_ebf,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_ebf_study_group_table_report = create_study_group_table(
    endline_ebf_study_group,
    endline_ebf,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_ebf_study_group_province_table = create_study_group_province_table(
    endline_ebf_study_group_province,
    endline_ebf_study_group,
    vars = "exclusive_breastfeeding",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline child vitamin A coverage table ----------------------------------
  endline_child_vita_province_table = create_province_table(
    endline_child_vita_province,
    endline_child_vita,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_vita_province_table_report = create_province_table(
    endline_child_vita_province,
    endline_child_vita,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_vita_strata_table = create_strata_table(
    endline_child_vita_strata,
    endline_child_vita,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_vita_study_group_table = create_study_group_table(
    endline_child_vita_study_group,
    endline_child_vita,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_child_vita_study_group_table_report = create_study_group_table(
    endline_child_vita_study_group,
    endline_child_vita,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_child_vita_study_group_province_table = create_study_group_province_table(
    endline_child_vita_study_group_province,
    endline_child_vita_study_group,
    vars = "vitamin_a_supplementation_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline deworming coverage table ----------------------------------------
  endline_deworming_province_table = create_province_table(
    endline_deworming_province,
    endline_deworming,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_deworming_province_table_report = create_province_table(
    endline_deworming_province,
    endline_deworming,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_deworming_strata_table = create_strata_table(
    endline_deworming_strata,
    endline_deworming,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_deworming_study_group_table = create_study_group_table(
    endline_deworming_study_group,
    endline_deworming,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_deworming_study_group_table_report = create_study_group_table(
    endline_deworming_study_group,
    endline_deworming,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_deworming_study_group_province_table = create_study_group_province_table(
    endline_deworming_study_group_province,
    endline_deworming_study_group,
    vars = "deworming_coverage",
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline women's decision making table -----------------------------------
  endline_wem_province_table = create_province_table(
    endline_wem_province,
    endline_wem,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_wem_province_table_report = create_province_table(
    endline_wem_province,
    endline_wem,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_wem_strata_table = create_strata_table(
    endline_wem_strata,
    endline_wem,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_wem_study_group_table = create_study_group_table(
    endline_wem_study_group,
    endline_wem,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_wem_study_group_table_report = create_study_group_table(
    endline_wem_study_group,
    endline_wem,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_wem_study_group_province_table = create_study_group_province_table(
    endline_wem_study_group_province,
    endline_wem_study_group,
    vars = c(
      "freedom_and_control", "control_over_destiny",
      "make_decision_without_husband", "willingly_participate_in_survey"
    ),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  ### Endline women's anthropometry table -------------------------------------
  endline_women_anthro_province_table = create_province_table(
    endline_women_anthro_province,
    endline_women_anthro,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_women_anthro_province_table_report = create_province_table(
    endline_women_anthro_province,
    endline_women_anthro,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = TRUE, pivot = "wide"
  ),
  endline_women_anthro_strata_table = create_strata_table(
    endline_women_anthro_strata,
    endline_women_anthro,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_women_anthro_study_group_table = create_study_group_table(
    endline_women_anthro_study_group,
    endline_women_anthro,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = FALSE
  ),
  endline_women_anthro_study_group_table_report = create_study_group_table(
    endline_women_anthro_study_group,
    endline_women_anthro,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = TRUE, format = "wide"
  ),
  endline_women_anthro_study_group_province_table = create_study_group_province_table(
    endline_women_anthro_study_group_province,
    endline_women_anthro_study_group,
    vars = c("body_mass_index", "bmi_class"),
    indicator_list = survey_indicator_list,
    report = FALSE
  )
)


## Outputs - overall -----------------------------------------------------------
outputs_overall <- tar_plan(
  ### Overall table output - respondent demographics ---------------------------
  overall_demo_respondent = create_full_table(
    variable_set = "demo_respondent", 
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_demo_respondent_province, 
      baseline_demo_respondent_strata,
      baseline_demo_respondent_study_group,
      baseline_demo_respondent_study_group_province,
      baseline_demo_respondent
    ),
    endline = list(
      endline_demo_respondent_province, 
      endline_demo_respondent_strata,
      endline_demo_respondent_study_group,
      endline_demo_respondent_study_group_province,
      endline_demo_respondent
    )
  ),
  ### Overall table output - child demographics --------------------------------
  overall_demo_child = create_full_table(
    variable_set = "demo_child",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_demo_child_province, 
      baseline_demo_child_strata,
      baseline_demo_child_study_group,
      baseline_demo_child_study_group_province,
      baseline_demo_child
    ),
    endline = list(
      endline_demo_child_province, 
      endline_demo_child_strata,
      endline_demo_child_study_group,
      endline_demo_child_study_group_province,
      endline_demo_child
    )
  ),
  ### Overall table output - spouse demographics -------------------------------
  overall_demo_spouse = create_full_table(
    variable_set = "demo_spouse",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_demo_spouse_province, 
      baseline_demo_spouse_strata,
      baseline_demo_spouse_study_group,
      baseline_demo_spouse_study_group_province,
      baseline_demo_spouse
    ),
    endline = list(
      endline_demo_spouse_province, 
      endline_demo_spouse_strata,
      endline_demo_spouse_study_group,
      endline_demo_spouse_study_group_province,
      endline_demo_spouse
    )
  ),
  ### Overall table output - household income ----------------------------------
  overall_hh_income = create_full_table(
    variable_set = "hh_income",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_hh_income_province, 
      baseline_hh_income_strata,
      baseline_hh_income_study_group,
      baseline_hh_income_study_group_province,
      baseline_hh_income
    ),
    endline = list(
      endline_hh_income_province, 
      endline_hh_income_strata,
      endline_hh_income_study_group,
      endline_hh_income_study_group_province,
      endline_hh_income
    )
  ),
  ### Overall table output - household structure -------------------------------
  overall_hh_structure = create_full_table(
    variable_set = "hh_structure",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_hh_structure_province, 
      baseline_hh_structure_strata,
      baseline_hh_structure_study_group,
      baseline_hh_structure_study_group_province,
      baseline_hh_structure
    ),
    endline = list(
      endline_hh_structure_province, 
      endline_hh_structure_strata,
      endline_hh_structure_study_group,
      endline_hh_structure_study_group_province,
      endline_hh_structure
    )
  ),
  ### Overall table output - household amenities -------------------------------
  overall_hh_amenities = create_full_table(
    variable_set = "hh_amenities",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_hh_amenities_province, 
      baseline_hh_amenities_strata,
      baseline_hh_amenities_study_group,
      baseline_hh_amenities_study_group_province,
      baseline_hh_amenities
    ),
    endline = list(
      endline_hh_amenities_province, 
      endline_hh_amenities_strata,
      endline_hh_amenities_study_group,
      endline_hh_amenities_study_group_province,
      endline_hh_amenities
    )
  ),
  ### Overall table output - household decision --------------------------------
  overall_hh_decision = create_full_table(
    variable_set = "hh_decision",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_hh_decision_province, 
      baseline_hh_decision_strata,
      baseline_hh_decision_study_group,
      baseline_hh_decision_study_group_province,
      baseline_hh_decision
    ),
    endline = list(
      endline_hh_decision_province, 
      endline_hh_decision_strata,
      endline_hh_decision_study_group,
      endline_hh_decision_study_group_province,
      endline_hh_decision
    )
  ),
  ### Overall table output - household groups ----------------------------------
  overall_hh_groups = create_full_table(
    variable_set = "hh_groups",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_hh_groups_province, 
      baseline_hh_groups_strata,
      baseline_hh_groups_study_group,
      baseline_hh_groups_study_group_province,
      baseline_hh_groups
    ),
    endline = list(
      endline_hh_groups_province, 
      endline_hh_groups_strata,
      endline_hh_groups_study_group,
      endline_hh_groups_study_group_province,
      endline_hh_groups
    )
  ),
  ### Overall table travel - household travel ----------------------------------
  overall_hh_travel = create_full_table(
    variable_set = "hh_travel",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_hh_travel_province, 
      baseline_hh_travel_strata,
      baseline_hh_travel_study_group,
      baseline_hh_travel_study_group_province,
      baseline_hh_travel
    ),
    endline = list(
      endline_hh_travel_province, 
      endline_hh_travel_strata,
      endline_hh_travel_study_group,
      endline_hh_travel_study_group_province,
      endline_hh_travel
    )
  ),
  ### Overall table output - WASH ----------------------------------------------
  overall_wash = create_full_table(
    variable_set = "wash",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_wash_province, 
      baseline_wash_strata,
      baseline_wash_study_group,
      baseline_wash_study_group_province,
      baseline_wash
    ),
    endline = list(
      endline_wash_province, 
      endline_wash_strata,
      endline_wash_study_group,
      endline_wash_study_group_province,
      endline_wash
    )
  ),
  ### Overall table output - women's empowerment -------------------------------
  overall_wem = create_full_table(
    variable_set = "wem",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_wem_province, 
      baseline_wem_strata,
      baseline_wem_study_group,
      baseline_wem_study_group_province,
      baseline_wem
    ),
    endline = list(
      endline_wem_province, 
      endline_wem_strata,
      endline_wem_study_group,
      endline_wem_study_group_province,
      endline_wem
    )
  ),
  ### Overall table output - PHQ8 ----------------------------------------------
  overall_women_phq8 = create_full_table(
    variable_set = "women_phq8",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_women_phq8_province, 
      baseline_women_phq8_strata,
      baseline_women_phq8_study_group,
      baseline_women_phq8_study_group_province,
      baseline_women_phq8
    ),
    endline = list(
      endline_women_phq8_province, 
      endline_women_phq8_strata,
      endline_women_phq8_study_group,
      endline_women_phq8_study_group_province,
      endline_women_phq8
    )
  ),
  ### Overall table output - pregnant ------------------------------------------
  overall_pregnant = create_full_table(
    variable_set = "pregnant",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_pregnant_province, 
      baseline_pregnant_strata,
      baseline_pregnant_study_group,
      baseline_pregnant_study_group_province,
      baseline_pregnant
    ),
    endline = list(
      endline_pregnant_province, 
      endline_pregnant_strata,
      endline_pregnant_study_group,
      endline_pregnant_study_group_province,
      endline_pregnant
    )
  ),
  ### Overall table output - pregnant_prevention -------------------------------
  overall_pregnant_prevention = create_full_table(
    variable_set = "pregnant_prevention",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_pregnant_prevention_province, 
      baseline_pregnant_prevention_strata,
      baseline_pregnant_prevention_study_group,
      baseline_pregnant_prevention_study_group_province,
      baseline_pregnant_prevention
    ),
    endline = list(
      endline_pregnant_prevention_province, 
      endline_pregnant_prevention_strata,
      endline_pregnant_prevention_study_group,
      endline_pregnant_prevention_study_group_province,
      endline_pregnant_prevention
    )
  ),
  ### Overall table output - natal care ----------------------------------------
  overall_natal_care = create_full_table(
    variable_set = "natal_care",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_natal_care_province, 
      baseline_natal_care_strata,
      baseline_natal_care_study_group,
      baseline_natal_care_study_group_province,
      baseline_natal_care
    ),
    endline = list(
      endline_natal_care_province, 
      endline_natal_care_strata,
      endline_natal_care_study_group,
      endline_natal_care_study_group_province,
      endline_natal_care
    )
  ),
  ### Overall table output - family planning -----------------------------------
  overall_family_planning = create_full_table(
    variable_set = "family_planning",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_family_planning_province, 
      baseline_family_planning_strata,
      baseline_family_planning_study_group,
      baseline_family_planning_study_group_province,
      baseline_family_planning
    ),
    endline = list(
      endline_family_planning_province, 
      endline_family_planning_strata,
      endline_family_planning_study_group,
      endline_family_planning_study_group_province,
      endline_family_planning
    )
  ),
  ### Overall table output - WDDS ----------------------------------------------
  overall_wdds = create_full_table(
    variable_set = "wdds",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_wdds_province, 
      baseline_wdds_strata,
      baseline_wdds_study_group,
      baseline_wdds_study_group_province,
      baseline_wdds
    ),
    endline = list(
      endline_wdds_province, 
      endline_wdds_strata,
      endline_wdds_study_group,
      endline_wdds_study_group_province,
      endline_wdds
    )
  ),
  ### Overall table output - MDDW ----------------------------------------------
  overall_mddw = create_full_table(
    variable_set = "mddw",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_mddw_province, 
      baseline_mddw_strata,
      baseline_mddw_study_group,
      baseline_mddw_study_group_province,
      baseline_mddw
    ),
    endline = list(
      endline_mddw_province, 
      endline_mddw_strata,
      endline_mddw_study_group,
      endline_mddw_study_group_province,
      endline_mddw
    )
  ),
  ### Overall table output - women anthropometry  ------------------------------
  overall_women_anthro = create_full_table(
    variable_set = "women_anthro",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_women_anthro_province, 
      baseline_women_anthro_strata,
      baseline_women_anthro_study_group,
      baseline_women_anthro_study_group_province,
      baseline_women_anthro
    ),
    endline = list(
      endline_women_anthro_province, 
      endline_women_anthro_strata,
      endline_women_anthro_study_group,
      endline_women_anthro_study_group_province,
      endline_women_anthro
    )
  ),
  ### Overall table output - Child development  --------------------------------
  overall_child_dev = create_full_table(
    variable_set = "child_dev",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_child_dev_province, 
      baseline_child_dev_strata,
      baseline_child_dev_study_group,
      baseline_child_dev_study_group_province,
      baseline_child_dev
    ),
    endline = list(
      endline_child_dev_province, 
      endline_child_dev_strata,
      endline_child_dev_study_group,
      endline_child_dev_study_group_province,
      endline_child_dev
    )
  ),
  ### Overall table output - childhood illnesses -------------------------------
  overall_child_ill = create_full_table(
    variable_set = "child_ill",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_child_ill_province, 
      baseline_child_ill_strata,
      baseline_child_ill_study_group,
      baseline_child_ill_study_group_province,
      baseline_child_ill
    ),
    endline = list(
      endline_child_ill_province, 
      endline_child_ill_strata,
      endline_child_ill_study_group,
      endline_child_ill_study_group_province,
      endline_child_ill
    )
  ),
  ### Overall table output - child immunisation --------------------------------
  overall_child_immunisation = create_full_table(
    variable_set = "child_immunisation",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_child_immunisation_province, 
      baseline_child_immunisation_strata,
      baseline_child_immunisation_study_group,
      baseline_child_immunisation_study_group_province,
      baseline_child_immunisation
    ),
    endline = list(
      endline_child_immunisation_province, 
      endline_child_immunisation_strata,
      endline_child_immunisation_study_group,
      endline_child_immunisation_study_group_province,
      endline_child_immunisation
    )
  ),
  ### Overall table output - child immunisation full ---------------------------
  overall_child_immunisation_full = create_full_table(
    variable_set = "child_immunisation_full",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_child_immunisation_full_province, 
      baseline_child_immunisation_full_strata,
      baseline_child_immunisation_full_study_group,
      baseline_child_immunisation_full_study_group_province,
      baseline_child_immunisation_full
    ),
    endline = list(
      endline_child_immunisation_full_province, 
      endline_child_immunisation_full_strata,
      endline_child_immunisation_full_study_group,
      endline_child_immunisation_full_study_group_province,
      endline_child_immunisation_full
    )
  ),
  ### Overall table output - vitamin A supplementation -------------------------
  overall_child_vita = create_full_table(
    variable_set = "child_vita",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_child_vita_province, 
      baseline_child_vita_strata,
      baseline_child_vita_study_group,
      baseline_child_vita_study_group_province,
      baseline_child_vita
    ),
    endline = list(
      endline_child_vita_province, 
      endline_child_vita_strata,
      endline_child_vita_study_group,
      endline_child_vita_study_group_province,
      endline_child_vita
    )
  ),
  ### Overall table output - deworming -----------------------------------------
  overall_deworming = create_full_table(
    variable_set = "deworming",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_deworming_province, 
      baseline_deworming_strata,
      baseline_deworming_study_group,
      baseline_deworming_study_group_province,
      baseline_deworming
    ),
    endline = list(
      endline_deworming_province, 
      endline_deworming_strata,
      endline_deworming_study_group,
      endline_deworming_study_group_province,
      endline_deworming
    )
  ),
  ### Overall table output - breastfeeding -------------------------------------
  overall_breastfeeding = create_full_table(
    variable_set = "breastfeeding",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_breastfeeding_province, 
      baseline_breastfeeding_strata,
      baseline_breastfeeding_study_group,
      baseline_breastfeeding_study_group_province,
      baseline_breastfeeding
    ),
    endline = list(
      endline_breastfeeding_province, 
      endline_breastfeeding_strata,
      endline_breastfeeding_study_group,
      endline_breastfeeding_study_group_province,
      endline_breastfeeding
    )
  ),
  ### Overall table output - exclusive breastfeeding ---------------------------
  overall_ebf = create_full_table(
    variable_set = "ebf",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_ebf_province, 
      baseline_ebf_strata,
      baseline_ebf_study_group,
      baseline_ebf_study_group_province,
      baseline_ebf
    ),
    endline = list(
      endline_ebf_province, 
      endline_ebf_strata,
      endline_ebf_study_group,
      endline_ebf_study_group_province,
      endline_ebf
    )
  ),
  ### Overall table output - IYCF ----------------------------------------------
  overall_iycf = create_full_table(
    variable_set = "iycf",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_iycf_province, 
      baseline_iycf_strata,
      baseline_iycf_study_group,
      baseline_iycf_study_group_province,
      baseline_iycf
    ),
    endline = list(
      endline_iycf_province, 
      endline_iycf_strata,
      endline_iycf_study_group,
      endline_iycf_study_group_province,
      endline_iycf
    )
  ),
  ### Overall table output - child anthro --------------------------------------
  overall_child_anthro = create_full_table(
    variable_set = "child_anthro",
    indicator_list = survey_indicator_list,
    baseline = list(
      baseline_child_anthro_province, 
      baseline_child_anthro_strata,
      baseline_child_anthro_study_group,
      baseline_child_anthro_study_group_province,
      baseline_child_anthro
    ),
    endline = list(
      endline_child_anthro_province, 
      endline_child_anthro_strata,
      endline_child_anthro_study_group,
      endline_child_anthro_study_group_province,
      endline_child_anthro
    )
  ),
  ### Overall results table ----------------------------------------------------
  overall_results_all = rbind(
    overall_demo_respondent,
    overall_demo_child,
    overall_demo_spouse,
    overall_hh_income,
    overall_hh_structure,
    overall_hh_amenities,
    overall_hh_decision,
    overall_hh_groups,
    overall_hh_travel,
    overall_wash,
    overall_wem,
    overall_women_phq8,
    overall_pregnant,
    overall_pregnant_prevention,
    overall_natal_care,
    overall_family_planning,
    overall_wdds,
    overall_mddw,
    overall_women_anthro,
    overall_child_dev,
    overall_child_ill,
    overall_child_immunisation,
    overall_child_immunisation_full,
    overall_child_vita,
    overall_deworming,
    overall_breastfeeding,
    overall_ebf,
    overall_iycf,
    overall_child_anthro
  ),
  ### Baseline results all -----------------------------------------------------
  baseline_results_all = subset(
    overall_results_all, study_round == "Baseline"
  ),
  ### Endline results all ------------------------------------------------------
  endline_results_all = subset(
    overall_results_all, study_round == "Endline"
  ),
  ### Create XLSX output for all tables ----------------------------------------
  overall_results_xlsx = create_results_spreadsheet(
    sheet = as.list(
      c(unique(survey_indicator_list$indicator_set_code), "overall")
    ),
    x = list(
      overall_demo_respondent,
      overall_demo_child,
      overall_demo_spouse,
      overall_hh_income,
      overall_hh_structure,
      overall_hh_amenities,
      overall_hh_decision,
      overall_hh_groups,
      overall_hh_travel,
      overall_wash,
      overall_wem,
      overall_women_phq8,
      overall_pregnant,
      overall_pregnant_prevention,
      overall_natal_care,
      overall_family_planning,
      overall_wdds,
      overall_mddw,
      overall_women_anthro,
      overall_child_dev,
      overall_child_ill,
      overall_child_immunisation,
      overall_child_immunisation_full,
      overall_child_vita,
      overall_deworming,
      overall_breastfeeding,
      overall_ebf,
      overall_iycf,
      overall_child_anthro,
      overall_results_all
    ),
    filename = "outputs/survey_results.xlsx"
  ),
  ### Create XLSX output for all baseline tables
  tar_target(
    name = baseline_results_xlsx,
    command = {
      baseline_wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(
        wb = baseline_wb, sheetName = "baseline"
      )
      openxlsx::writeData(
        wb = baseline_wb,
        sheet = "baseline",
        x = baseline_results_all
      )
      openxlsx::setColWidths(
        wb = baseline_wb,
        sheet = "baseline",
        cols = seq_len(ncol(baseline_results_all)),
        widths = "auto"
      )
      openxlsx::saveWorkbook(
        wb = baseline_wb,
        file = "outputs/baseline_results.xlsx",
        overwrite = TRUE
      )
    }
  ),
  baseline_results_csv = write.csv(
    x = baseline_results_all,
    file = "outputs/baseline_results.csv",
    row.names = FALSE
  ),
  endline_results_csv = write.csv(
    x = endline_results_all,
    file = "outputs/endline_results.csv",
    row.names = FALSE
  )
)

## Analysis - difference-in-difference -----------------------------------------
analysis_comparison <- tar_plan(
  
)

## Outputs - difference-in-difference ------------------------------------------
outputs_comparison <- tar_plan(
  
)

## Reports ---------------------------------------------------------------------
reports <- tar_plan(
  ### Survey locations map -----------------------------------------------------
  tar_render(
    name = survey_locations_map,
    path = "reports/endline_survey_locations.Rmd",
    output_dir = "outputs",
    knit_root_dir = here::here()
  ),
  ### Baseline report ----------------------------------------------------------
  tar_render(
    name = baseline_survey_results_report,
    path = "reports/baseline_survey_results_report.Rmd",
    output_dir = "outputs",
    knit_root_dir = here::here()
  )
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
  data_spatial,
  data_reference,
  raw_data_baseline,
  processed_data_baseline,
  analysis_baseline,
  outputs_tables_baseline,
  raw_data_endline,
  processed_data_endline,
  analysis_endline,
  outputs_tables_endline,
  outputs_overall,
  analysis_comparison,
  outputs_comparison,
  reports,
  deploy
)