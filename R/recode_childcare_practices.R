################################################################################
#
#' 
#' Process and recode childcare practices data/indicators
#'
#' Relevant variables:
#'
#'   ccare1 - Can you tell me what symptoms indicate that a child needs 
#'     immediate medical attention?  Please tell me all the symptoms that you 
#'     can. integer variables for the following categorical values:
#' 
#'     1=Fever; 
#'     2=Blood in stool; 
#'     3=Diarrhoea with dehydration (lack of tears/sunken eyes); 
#'     4=Cough, rapid respiration and/or difficulty breathing; 
#'     5=Unable to drink water, breastfeed, or eat; 
#'     6=Vomiting; 
#'     7=Convulsions; 
#'     8=Loss of consciousness; 
#'     9=Fatigue/no response/not wanting to play; 
#'     10=Neck rigidity; 
#'     88=Don't know; 
#'     99=No response
#'
#'   ccare2 - In relation to the child's health issues (e.g., vaccines, care, 
#'     etc.), do you have the support or involvement of the child's father?
#'     integer variables for the following categorical values:
#'     
#'     1=Yes; 
#'     2=No
#'
#'   ccare3 - In your opinion, what are the barriers or difficulties in taking 
#'     a child to receive health care treatment? integer variables for the
#'     following categorical values:
#'   
#'     1=Distance; 
#'     2=Transport; 
#'     3=Money; 
#'     4=Poor treatment at the health facility/hospital; 
#'     5=Other; 
#'     88=Don't know; 
#'     99=No response
#'     
#
################################################################################

ccare_recode_responses <- function(x, na_values) {
  ifelse(x %in% na_values, NA, x)
}

ccare_danger_recode <- function(vars, .data, 
                                na_values = c("88", "99"),
                                fill = 1:10, 
                                na_rm = TRUE, 
                                prefix = "ccare_danger",
                                threshold = 8) {
  x <- .data[[vars]]
  
  x <- ccare_recode_responses(x, na_values = na_values)
  
  danger_df <- split_select_multiples(
    x = x, fill = fill, na_rm = na_rm, prefix = prefix
  )
  
  ccare_danger_score <- rowSums(danger_df, na.rm = TRUE) |>
    (\(x) ifelse(x == 0, NA_integer_, x))()
  
  ccare_danger_prop <- ifelse(ccare_danger_score >= threshold, 1, 0)
  
  data.frame(
    danger_df, ccare_danger_score, ccare_danger_prop
  )
}


ccare_participation_recode <- function(x, na_values) {
  ifelse(
    x %in% na_values, NA,
    ifelse(
      x == 2, 0, 1
    )
  )
}

ccare_barriers_recode <- function(vars, .data, 
                                  na_values = c("88", "99"),
                                  fill = 1:5, 
                                  na_rm = TRUE, 
                                  prefix = "ccare_barriers") {
  x <- .data[[vars]]
  
  x <- ccare_recode_responses(x, na_values = na_values)
  
  split_select_multiples(
    x = x, fill = fill, na_rm = na_rm, prefix = prefix
  )
}



ccare_recode <- function(.data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- data.frame(
    ccare_danger_recode(vars = "ccare1", .data = .data),
    ccare_participation = ccare_participation_recode(
      x = .data[["ccare2"]], na_values = c(8, 9)
    ),
    ccare_barriers_recode(vars = "ccare3", .data = .data)
  )
  
  data.frame(core_vars, recoded_vars)
}
