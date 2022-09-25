################################################################################
#
#'
#' Recode WDDS
#'
#
################################################################################

wdds_recode_group <- function(vars,
                              .data,
                              food_group = c("staples", "grean_leafy", 
                                             "other_vita", "fruits_vegetables", 
                                             "organ_meat", "meat_fish", "eggs", 
                                             "legumes", "milk")) {
  food_group <- match.arg(food_group)
  
  if (length(vars) > 1) {
    if (!food_group %in% c(
      "staples", "other_vita", "fruits_vegetables", "meat_fish", "legumes")
    ) {
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
    if (food_group %in% c(
      "staples", "other_vita", "fruits_vegetables", "meat_fish", "legumes")
    ) {
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

wdds_recode_groups <- function(vars,
                               .data,
                               food_group = c("staples", "grean_leafy", 
                                              "other_vita", "fruits_vegetables", 
                                              "organ_meat", "meat_fish", "eggs", 
                                              "legumes", "milk")) {
  .data_list <- rep(list(.data), 9)
  
  ## Check that length of vars is the same as length of food_group
  if (length(vars) != length(food_group)) {
    stop(
      "Number of variables/variable groups not the same as the number of
      specified food groups. Please verify specified variables."
    )
  }
  
  Map(
    f = wdds_recode_group, 
    vars = vars, 
    .data = .data_list, 
    food_group = food_group
  ) |>
    (\(x) { names(x) <- paste0("wdds_", food_group); x })() |>
    dplyr::bind_rows()

}



################################################################################
#
#'
#' Create named list of food group variables
#'
#
################################################################################

wdds_map_fg_vars <- function(staples, grean_leafy, other_vita, 
                             fruits_vegetables, organ_meat, meat_fish, 
                             eggs, legumes, milk) {
  list(
    staples = staples, 
    grean_leafy = grean_leafy, 
    other_vita = other_vita, 
    fruits_vegetables = fruits_vegetables, 
    organ_meat = organ_meat, 
    meat_fish = meat_fish, 
    eggs = eggs, 
    legumes = legumes, 
    milk = milk
  )
} 



################################################################################
#
#'
#' Calculate WDDS
#'
#' @param fg_df A data.frame with 9 columns, one for each food group in WDDS
#' @param add Logical. Should the resulting score be added to fg_df? Default to
#'   TRUE
#' 
#'
#
################################################################################

wdds_calculate_score <- function(fg_df, add = TRUE) {
  ## Check that fg_df has 9 columns
  if (ncol(fg_df) != 9) {
    stop(
      "The food group data.frame needs to have 9 columns for each of the
      HDDS food groups. Please verify your dataset."
    )
  }
  
  wdds <- rowSums(fg_df, na.rm = TRUE)
  
  if (add) {
    data.frame(
      fg_df,
      wdds = wdds
    )
  } else {
    wdds
  }
}


wdds_recode <- function(vars,
                        .data,
                        food_group = c("staples", "grean_leafy", 
                                       "other_vita", "fruits_vegetables", 
                                       "organ_meat", "meat_fish", "eggs", 
                                       "legumes", "milk")) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- wdds_recode_groups(
    vars = vars, .data = .data, food_group = food_group
  ) |>
    wdds_calculate_score(add = TRUE)
  
  data.frame(core_vars, recoded_vars)
}

