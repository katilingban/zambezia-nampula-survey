################################################################################
#
#'
#' Create codebook for endline survey
#'
#
################################################################################

create_codebook <- function(survey_questions, survey_choices, 
                            raw_data, meta = FALSE) {
  vars <- get_variables(survey_questions, meta = meta) |>
    tolower()
  
  questions <- get_questions(survey_questions, meta = meta)
  
  types <- get_types(survey_questions, raw_data, meta = meta)
  
  choices <- get_choices(survey_questions, survey_choices, meta = meta)
  
  codebook <- tibble::tibble(
    vars, questions, types, choices
  )
  
  codebook
}


################################################################################
#
#'
#' Get variables from XLSForm
#'
#'
#
################################################################################

get_variables <- function(survey_questions, meta = FALSE) {
  meta_names <- c("id", "tags", "uuid", "notes", "edited", "status", "version",
                  "duration", "xform_id", "attachments", "geolocation", 
                  "media_count", "total_media", "submitted_by", "date_modified", 
                  "instanceID", "submission_time", "xform_id_string", 
                  "instanceName", "bamboo_dataset_id")
  
  questions_df <- survey_questions |> 
    subset(!is.na(name) & !type %in% c("note", "begin group", "end group"))
  
  vars <- questions_df |> (\(x) x$name)()
  
  if (meta) {
    vars <- c(meta_names, vars)
  }
  
  vars
}


################################################################################
#
#'
#' Get questions from XLSForm
#'
#'
#
################################################################################

get_questions <- function(survey_questions, meta = FALSE) {
  meta_names <- c("id", "tags", "uuid", "notes", "edited", "status", "version",
                  "duration", "xform_id", "attachments", "geolocation", 
                  "media_count", "total_media", "submitted_by", "date_modified", 
                  "instanceID", "submission_time", "xform_id_string", 
                  "instanceName", "bamboo_dataset_id")
  
  meta_questions <- meta_names |>
    (\(x) gsub(pattern = "_", replacement = " ", x = x))() |>
    (\(x) gsub(pattern = "id|ID", replacement = "Identifier", x = x))() |>
    stringr::str_replace(
      pattern = "instanceIdentifier", replacement = "instance identifier"
    ) |>
    stringr::str_replace(
      pattern = "instanceName", replacement = "instance name"
    ) |>
    stringr::str_to_title() |>
    (\(x) gsub(
      pattern = "Uuidentifier", replacement = "Unique User Identifier", x = x
    ))() |>
    (\(x) gsub(
      pattern = "Xform", replacement = "XForm", x = x
    ))()    
  
  questions_df <- survey_questions |> 
    subset(!is.na(name) & !type %in% c("note", "begin group", "end group"))
  
  questions <- questions_df |> (\(x) x$`label::English (en)`)()
  
  if (meta) {
    questions <- c(meta_questions, questions)
  }
  
  questions
}


################################################################################
#
#'
#' Get variable types from XLSForm
#'
#'
#
################################################################################

get_types <- function(survey_questions, raw_data, meta = FALSE) {
  vars <- get_variables(survey_questions, meta = meta)
  
  types <- lapply(X = vars, FUN = function(x) class(raw_data[[x]])) |>
    (\(x) do.call(rbind, x))() |>
    (\(x) x[ , 1])()
  
  types
}


################################################################################
#
#'
#' Get variable choices from XLSForm
#'
#'
#
################################################################################

get_choices <- function(survey_questions, survey_choices, meta = FALSE) {
  choice_names <- survey_choices |>
    subset(!is.na(list_name)) |>
    (\(x) x$list_name)() |>
    unique() |>
    c("")
  
  choice_pattern <- paste(
    choice_names[1:length(choice_names) - 1], 
    collapse = "|"
  )
  
  choices <- lapply(
    X = choice_names,
    FUN = function(x) {
      y <- subset(survey_choices, list_name == x)
      z <- paste(y$name, y$`label::English (en)`, sep = "=") |>
        paste(collapse = "; ")
    }
  ) |>
    (\(x) do.call(rbind, x))()
  
  questions_df <- survey_questions |>
    subset(!is.na(name) & !type %in% c("note", "begin group", "end group")) |>
    subset(select = type) |>
    (\(x)
     {
       x$choice_names <- stringr::str_extract(
         string = x$type, 
         pattern = choice_pattern
       ) |>
         (\(x) ifelse(is.na(x), "", x))()
       x
    })()
  
  choices_list <- questions_df |>
    dplyr::left_join(
      data.frame(choice_names, choices)
    ) |>
    subset(select = -type)
  
  if (meta) {
    vars <- get_variables(survey_questions, meta = meta)
    vars <- vars[!vars %in% survey_questions$name]
    
    meta_df <- data.frame(
      choice_names = rep("", length(vars)),
      choices = rep("", length(vars))
    )
    
    choices_list <- rbind(meta_df, choices_list)
  }
  
  choices_list
}

