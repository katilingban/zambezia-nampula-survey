################################################################################
#
# Process and recode decision making data/indicators
#
# Relevant variables:
#
#   ge1 - Appropriate age to marry? integer variables for the following
#     categorical values:
#     
#     1=The men; 
#     2=The women; 
#     3=Both men and women; 
#     88=Don't know; 
#     99=No response
#
#   ge2 - Use of condoms? integer variables for the following categorical
#     values:
# 
#     1=The men; 
#     2=The women; 
#     3=Both men and women; 
#     88=Don't know; 
#     99=No response
#
#   ge3 - Household responsibilities? integer variables for the following
#     categorical values:
#
#     1=The men; 
#     2=The women; 
#     3=Both men and women; 
#     88=Don't know; 
#     99=No response
#
#   ge4 - Family planning - number of childre to have? integer variables for
#     the following categorical values:
# 
#     1=The men; 
#     2=The women; 
#     3=Both men and women; 
#     88=Don't know; 
#     99=No response
#
#   ge5 - Family/land chores? integer variables for the following categorical
#     values:
#   
#     1=The men; 
#     2=The women; 
#     3=Both men and women; 
#     88=Don't know; 
#     99=No response
#
#   ge6 - Administration of finances (money) in the home? integer variables
#     for the following categorical values:
#
#     1=The men; 
#     2=The women; 
#     3=Both men and women; 
#     88=Don't know; 
#     99=No response
#
#   ge7 - How to raise children? integer variables for the following
#     categorical values:
#
#     1=The men; 
#     2=The women; 
#     3=Both men and women; 
#     88=Don't know; 
#     99=No response
#
#   ge8 - Hitting/spanking children? integer variables for the following
#     categorical values:
#     
#     1=The men; 
#     2=The women; 
#     3=Both men and women; 
#     88=Don't know; 
#     99=No response
#   
#   ge9 - Seeking health care for pregnancy?	integer variables for the
#     following categorical values:
#   
#     1=The men; 
#     2=The women; 
#     3=Both men and women; 
#     88=Don't know; 
#     99=No response 
#
#   ge10 - Seeking health care for a child? integer variables for the
#     following categorical values:
# 
#     1=The men; 
#     2=The women; 
#     3=Both men and women; 
#     88=Don't know; 
#     99=No response
#
#   von1 - Please, how much freedong of choice do you feel you have on what 
#     happens to your life? integer variables for the following categorical
#     values:
#
#     1=No choice; 
#     2=Little choice; 
#     3=Some choice; 
#     4=A lot of choice; 
#     88=Don't know; 
#     99=No response
#
#   von2 - Until what point do you feel you can decide your own destiny?
#     integer variables for the following categorical values:
# 
#     1=Not at all; 
#     2=A little; 
#     3=Enough; 
#     4=A lot; 
#     88=Don't know; 
#     99=No response
#
#   von3	In general, do you think you can make decisions by yourself, freely, 
#     without consulting your husband? Please, to what extent can you do this?
#     integer variables for the following categorical values:
#
#       1=Never; 
#       2=Sometimes; 
#       3=Almost always; 
#       4=Always; 
#       88=Don't know; 
#       99=No response
#
#   von4	Did you accept to participate in this survey of your own volition?
#     integer variables for the following categorical values:
#   
#     1=I accepted to participate voluntarily and freely; 
#     2=Yes, but I need the consent of the male head of household; 
#     3=No, I needed the consent of the male head of household; 
#     4=I need someone else's consent; 
#     88=Don't know; 
#     99=No response
#
################################################################################

wem_recode_response <- function(x, na_values) {
  ifelse(x %in% na_values, NA_integer_, x)
}

wem_recode_responses <- function(vars, .data, na_values) {
  vars <- .data[vars]
  
  apply(
    X = vars,
    MARGIN = 2,
    FUN = wem_recode_response,
    na_values = na_values,
    simplify = TRUE
  ) |>
    data.frame() |>
    (\(x) { names(x) <- names(vars); x } )()
}


wem_recode_decision <- function(x, 
                                fill = NULL,
                                na_rm = FALSE,
                                prefix,
                                label = NULL) {
  wem_decision <- spread_vector_to_columns(
    x = x,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  )
  
  if (!is.null(label)) {
    names(wem_decision) <- paste0(prefix, "_", label)
    wem_decision
  }
  
  data.frame(x, wem_decision) |>
    (\(x) { names(x)[1] <- prefix; x } )()
}

wem_recode_decisions <- function(vars, .data, na_rm = FALSE, label = NULL) {
  x <- .data[vars]
  
  Map(
    f = wem_recode_decision,
    x = apply(X = x, MARGIN = 2, list) |>
      unlist(recursive = FALSE),
    prefix = vars,
    na_rm = na_rm,
    label = rep(list(label), length(vars))
  ) |>
    dplyr::bind_cols()
}

wem_recode <- function(.data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  wem_recoded_vars <- wem_recode_responses(
    vars = c(paste0("ge", 1:10), paste0("von", 1:4)) ,
    .data = .data,
    na_values = c(88, 99)
  ) 
  
  ge_recoded_vars <- wem_recode_decisions(
    vars = paste0("ge", 1:10),
    .data = wem_recoded_vars,
    na_rm = FALSE,
    label = c("men", "women", "both")
  )
  
  von_recoded_vars <- data.frame(
    wem_recode_decisions(
      vars = "von1",
      .data = wem_recoded_vars,
      na_rm = FALSE,
      label = c("no_choice", "little_choice", "some_choice", "lots_choices")
    ),
    von1_some_lots = ifelse(wem_recoded_vars[["von1"]] %in% 3:4, 1, 0),
    wem_recode_decisions(
      vars = "von2",
      .data = wem_recoded_vars,
      na_rm = FALSE,
      label = c("no", "little", "enough", "a_lot")
    ),
    von2_enough_lots = ifelse(wem_recoded_vars[["von2"]] %in% 3:4, 1, 0),
    wem_recode_decisions(
      vars = "von3",
      .data = wem_recoded_vars,
      na_rm = FALSE,
      label = c("never", "sometimes", "almost", "always")
    ),
    von3_almost_always = ifelse(wem_recoded_vars[["von3"]] %in% 3:4, 1, 0),
    wem_recode_decisions(
      vars = "von4",
      .data = wem_recoded_vars,
      na_rm = FALSE,
      label = c("freely", "freely_consent", "husband_consent", "someone_consent")
    ),
    von4_some_freely = ifelse(wem_recoded_vars[["von2"]] %in% 3:4, 0, 1)
  ) 
  
  data.frame(core_vars, ge_recoded_vars, von_recoded_vars)
}

