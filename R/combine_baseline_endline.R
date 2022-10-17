################################################################################
#
#'
#' Create baseline-endline dataset for priority indicators
#'
#
################################################################################

combine_baseline_endline <- function(vars,
                                     baseline_data, 
                                     endline_data,
                                     type = c("child", "hh")) {
  type <- match.arg(type)
  
  if (type == "hh") {
    baseline_data <- baseline_data |>
      get_respondent_data()
    
    endline_data <- endline_data |>
      dplyr::group_by(id) |>
      subset(child_id == 1 | is.na(child_id)) |>
      dplyr::ungroup()
  }
  
  ## Process baseline
  baseline <- data.frame(
    cluster_id = as.integer(baseline_data$enum1),
    study_round = "Baseline",
    baseline_data |>
      subset(
        select = eval(
          parse(text = paste0("c(", paste(vars, collapse = ", "), ")"))
        )
      )
  )
  
  ## Process endline
  endline <- data.frame(
    cluster_id = as.integer(endline_data$fgh_id),
    study_round = "Endline",
    endline_data |>
      subset(
        select = eval(
          parse(text = paste0("c(", paste(vars, collapse = ", "), ")"))
        )
      )
  )
  
  combined_weighted <- rbind(baseline, endline) |>
    dplyr::mutate(
      respondent_sex = ifelse(
        respondent_sex == "Mulher", "Female",
        ifelse(
          respondent_sex == "Homem", "Male", respondent_sex
        )
      ) |>
        factor(levels = c("Male", "Female")),
      child_sex = ifelse(
        child_sex == "Femenino", "Female",
        ifelse(
          child_sex == "Masculino", "Male", child_sex
        )
      ) |>
        factor(levels = c("Male", "Female")),
      study_group = ifelse(
        study_group == "COM", "Control", "Intervention"
      ),
      study_round = factor(study_round, levels = c("Baseline", "Endline"))
    )
  
  combined_weighted
}




# vars <- c(
#   "diarrhoea_episode", "fever_episode", "rti_episode",
#   "immunisation_fully_immunised", "immunisation_age_appropriate_immunisation", 
#   "vitamin_a_supplementation_coverage", "deworming_coverage", "wfaz", "hfaz",
#   "wfhz", "global_stunting", "moderate_stunting", "severe_stunting",
#   "global_underweight", "moderate_underweight", "severe_underweight", 
#   "global_wasting_by_weight_for_height", "moderate_wasting_by_weight_for_height",
#   "severe_wasting_by_weight_for_height", "global_wasting_by_muac",
#   "moderate_wasting_by_muac", "severe_wasting_by_muac", 
#   "severe_wasting_by_oedema", "minimum_dietary_diversity",
#   "exclusive_breastfeeding", "body_mass_index", "bmi_class", "wdds", "mddw"
# )



