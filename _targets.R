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

## Data downloads
data_downloads <- tar_plan(
  ## Download baseline data files
  tar_target(
    name = baseline_data_downloads,
    command = download_all_baseline_data(overwrite = TRUE),
    cue = tar_cue("thorough")
  ),
  ## Download baseline and endline sampling list
  tar_target(
    name = survey_sampling_list_download,
    command = download_googledrive(
      filename = "VIGH-UNICEF_EA_Sample_Final_01252019.xlsx",
      overwrite = TRUE
    ),
    cue = tar_cue("thorough")
  )
)

## Read raw data
raw_data <- tar_plan(
  ## Sampling list for baseline and endline
  survey_sampling_list = openxlsx::read.xlsx(
    xlsxFile = survey_sampling_list_download$local_path,
    sheet = 1
  ),
  ## Read raw baseline data
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
  ## Read raw endline data
  endline_raw_data = get_endline_data()
)


## Process data
processed_data <- tar_plan(
  ## Get baseline sampling weights
  baseline_sample_weight = calculate_weights(
    .data = baseline_raw_data_stata,
    survey_sampling_list
  ),
  baseline_data_processed = process_baseline_data(
    .data = baseline_raw_data_stata
  ),
  ## Process baseline data with weights
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
  ## Set baseline data survey design for children data
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
  ## Set baseline data survey design for hh/respondent data
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
  ),
  ## Process baseline data Income and occupation
  baseline_work_data = process_work_baseline(
    .data = baseline_data_weighted
  )
)


## Analysis
analysis <- tar_plan(
  ## Baseline results - demographics respondent
  baseline_demographics_respondent = estimate_total(
    vars = c("respondent_sex", "respondent_age",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = baseline_hh_survey_design
  ),
  baseline_demographics_respondent_province = estimate_province(
    vars = c("respondent_sex", "respondent_age",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = baseline_hh_survey_design
  ),
  baseline_demographics_respondent_strata = estimate_strata(
    vars = c("respondent_sex", "respondent_age",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = baseline_hh_survey_design
  ),
  baseline_demographics_respondent_study_group = estimate_study_group(
    vars = c("respondent_sex", "respondent_age",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = baseline_hh_survey_design
  ),
  baseline_demographics_respondent_study_group_province = estimate_study_group_province(
    vars = c("respondent_sex", "respondent_age",
             "respondent_age_group", "respondent_language", 
             "respondent_civil_status", "respondent_education_years",
             "respondent_education_group", "respondent_occupation"),
    design = baseline_hh_survey_design
  )#,
  ## Baseline results - demographics children
  # baseline_demographics_respondent = estimate_tota(
  #   vars = c("respondent_sex", "responden_age",
  #            "respondent_age_group", "respondent_language", 
  #            "respondent_civil_status", "respondent_education_years",
  #            "respondent_education_group", "respondent_occupation"),
  #   design = baseline_hh_survey_design
  # ),
  # baseline_demographics_respondent_province = estimate_province(
  #   vars = c("respondent_sex", "responden_age",
  #            "respondent_age_group", "respondent_language", 
  #            "respondent_civil_status", "respondent_education_years",
  #            "respondent_education_group", "respondent_occupation"),
  #   design = baseline_hh_survey_design
  # ),
  # baseline_demographics_respondent_strata = estimate_strata(
  #   vars = c("respondent_sex", "responden_age",
  #            "respondent_age_group", "respondent_language", 
  #            "respondent_civil_status", "respondent_education_years",
  #            "respondent_education_group", "respondent_occupation"),
  #   design = baseline_hh_survey_design
  # ),
  # baseline_demographics_study_group = estimate_study_group(
  #   vars = c("respondent_sex", "responden_age",
  #            "respondent_age_group", "respondent_language", 
  #            "respondent_civil_status", "respondent_education_years",
  #            "respondent_education_group", "respondent_occupation"),
  #   design = baseline_hh_survey_design
  # ),
  # baseline_demographics_study_group_province = estimate_study_group_province(
  #   vars = c("respondent_sex", "responden_age",
  #            "respondent_age_group", "respondent_language", 
  #            "respondent_civil_status", "respondent_education_years",
  #            "respondent_education_group", "respondent_occupation"),
  #   design = baseline_hh_survey_design
  # )
)


## Outputs
outputs <- tar_plan(
  ##
)


## Reports
reports <- tar_plan(
  ##
)

## Deploy targets
deploy <- tar_plan(
  ##
)

## Set seed
set.seed(1977)

# Concatenate targets ----------------------------------------------------------
list(
  data_downloads,
  raw_data,
  processed_data,
  analysis,
  outputs,
  reports,
  deploy
)