################################################################################
#
#'
#' Recode MDD-W
#'
#
################################################################################

mddw_recode_group <- function(vars,
                              .data,
                              food_group = c("staples", "pulses", "nuts_seeds", 
                                             "milk", "meat_fish", "eggs",
                                             "green_leafy", "other_vita",
                                             "vegetables", "fruits")) {
  food_group <- match.arg(food_group)
  
  if (length(vars) > 1) {
    if (!food_group %in% c("staples", "other_vita", "meat_fish")) {
      warning(
        paste0(
          "More than one food group variable specified. Please make sure that
          these variables are specific for the ", food_group, " food group."
        )
      )
    } 
    
    ## Get variables
    df <- .data[vars]
    
    ## Recode each variable to 1 and 0
    for (i in vars) {
      df[i] <- recode_yes_no(df[[i]], na_values = c("8", "9"))
    }
    
    ## Recode food group to 1 and 0
    fg <- recode_yes_no(x = rowSums(df, na.rm = TRUE), detect = "no") |>
      (\(x) { names(x) <- NULL; x })()
  } else {
    if (food_group %in% c("staples", "other_vita", "meat_fish")) {
      warning(
        paste0(
          "Only one food group variable specified. Please make sure that
          there are no other variables that are specific for the ", food_group, 
          " food group."
        )
      )
    }    
    
    ## Calculate indicator
    fg <- recode_yes_no(.data[[vars]], na_values = c("8", "9"))
  }
  
  ## Return
  fg
}

mddw_recode_groups <- function(vars,
                               .data,
                               food_group = c("staples", "pulses", "nuts_seeds", 
                                              "milk", "meat_fish", "eggs",
                                              "green_leafy", "other_vita",
                                              "vegetables", "fruits")) {
  .data_list <- rep(list(.data), 10)
  
  ## Check that length of vars is the same as length of food_group
  if (length(vars) != length(food_group)) {
    stop(
      "Number of variables/variable groups not the same as the number of
      specified food groups. Please verify specified variables."
    )
  }
  
  Map(
    f = mddw_recode_group, 
    vars = vars, 
    .data = .data_list, 
    food_group = food_group
  ) |>
    (\(x) { names(x) <- paste0("mddw_", food_group); x })() |>
    dplyr::bind_rows()

}



################################################################################
#
#'
#' Create named list of food group variables
#'
#
################################################################################

mddw_map_fg_vars <- function(staples, pulses, nuts_seeds, milk, meat_fish, 
                             eggs, green_leafy, other_vita, vegetables, 
                             fruits) {
  list(
    staples = staples,
    pulses = pulses,
    nuts_seeds = nuts_seeds,
    milk = milk,
    meat_fish = meat_fish, 
    eggs = eggs, 
    green_leafy = green_leafy,
    other_vita = other_vita,
    vegetables = vegetables,
    fruits = fruits
  )
} 



################################################################################
#
#'
#' Calculate MDDW
#'
#' @param fg_df A data.frame with 10 columns, one for each food group in WDDS
#' @param add Logical. Should the resulting score be added to fg_df? Default to
#'   TRUE
#' 
#'
#
################################################################################

mddw_calculate_score <- function(fg_df, add = TRUE) {
  ## Check that fg_df has 10 columns
  if (ncol(fg_df) != 10) {
    stop(
      "The food group data.frame needs to have 10 variables for each of the
      MDD-W food groups. Please verify your dataset."
    )
  }
  
  mddw <- rowSums(fg_df, na.rm = TRUE)
  
  if (add) {
    data.frame(
      fg_df,
      mddw_score = mddw,
      mddw = ifelse(mddw >= 5, 1, 0)
    )
  } else {
    data.frame(
      mddw_score = mddw,
      mddw = ifelse(mddw >= 5, 1, 0)
    )
  }
}


mddw_recode <- function(vars,
                        .data,
                        food_group = c("staples", "pulses", "nuts_seeds", 
                                       "milk", "meat_fish", "eggs",
                                       "green_leafy", "other_vita",
                                       "vegetables", "fruits")) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- mddw_recode_groups(
    vars = vars, .data = .data, food_group = food_group
  ) |>
    mddw_calculate_score(add = TRUE)
  
  data.frame(core_vars, recoded_vars)
}

