################################################################################
#
#' 
#' Process and recode household dietary diversity score data/indicators
#' 
#' Relevant variables:
#' 
#'  hdds0 Did the members of your household fast yesterday? 1=Yes; 2=No
#'  
#'  hdds1	Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains corn/maize, rice, wheat, sorghum, millet or any 
#'    other grains during the day or night? 1=Yes; 2=No
#'    
#'  hdds2	Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains white potatoes, white yam, white cassava, or any 
#'    other roots during the day or night? 1=Yes; 2=No
#'    
#'  hdds3	Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains pumpkin, carrot, squash, or sweet potato that are 
#'    orange inside during the day or night?	1=Yes; 2=No
#'
#'  hdds4	Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains dark green leafy vegetables, including wild forms 
#'    during the day or night? 1=Yes; 2=No
#'
#'  hdds5	Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains other vegetables (e.g. tomato, onion, eggplant) 
#'    during the day or night?	1=Yes; 2=No
#'
#'  hdds6	Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains ripe mango, cantaloupe, apricot (fresh or dried), 
#'    ripe papaya, dried peach, and 100% fruit juice made from these during the 
#'    day or night?	1=Yes; 2=No
#'
#'  hdds7	Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains other fruits, including wild fruits and 100% fruit 
#'    juice made from these during the day or night? 1=Yes; 2=No
#'
#'  hdds8	Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains liver, kidney, heart or other organ meats or 
#'    blood-based foods during the day or night?	1=Yes; 2=No
#'
#'  hdds9	Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains beef, pork, lamb, goat, rabbit, game, chicken, 
#'    duck, other birds, insects during the day or night? 1=Yes; 2=No
#'
#'  hdds10 Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains eggs from chicken, duck, guinea fowl or any other 
#'    egg during the day or night? 1=Yes; 2=No
#'
#'  hdds11 Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains fresh or dried fish or shellfish during the day or 
#'    night? 1=Yes; 2=No
#'
#'  hdds12 Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains dried beans, dried peas, lentils, nuts, seeds or 
#'    foods made from these (eg. hummus, peanut butter) during the day or night?
#'    1=Yes; 2=No
#'
#'  hdds13 Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains milk, cheese, yogurt or other milk products during 
#'    the day or night? 1=Yes; 2=No
#'
#'  hdds14 Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains oil, fats or butter added to food or used for cooking 
#'    during the day or night? 1=Yes; 2=No
#'
#'  hdds15 Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains sugar, honey, sweetened soda or sweetened juice 
#'    drinks, sugary foods such as chocolates, candies, cookies and cakes during 
#'    the day or night? 1=Yes; 2=No
#'
#'  hdds16 Yesterday, did you eat or drink foods as a meal or snack foods that 
#'    are made of or contains spices (black pepper, salt), condiments 
#'    (soy sauce, hot sauce), coffee, tea, alcoholic beverages during the day 
#'    or night? 1=Yes; 2=No
#'    
#' @param vars A vector of variables to use or a list of vectors of variables
#'   to use for HDDS
#' @param .data A data.frame from which variables should be taken
#' @param food_group A character value specifying food group to recode. Can be
#'   one of 12 food groups used in calculating the HDDS
#'    
#'    
#
################################################################################

hdds_recode_group <- function(vars,
                              .data,
                              food_group = c("cereals", "tubers", "vegetables", 
                                             "fruits", "meat", "eggs", "fish",
                                             "legumes_seeds", "milk", "oils_fats",
                                             "sweets", "spices")) {
  food_group <- match.arg(food_group)
  
  if (length(vars) > 1) {
    if (!food_group %in% c("vegetables", "fruits", "meat")) {
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
    fg <- recode_yes_no(x = rowSums(df, na.rm = TRUE), detect = "no")
  } else {
    if (food_group %in% c("vegetables", "fruits", "meat")) {
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

hdds_recode_groups <- function(vars,
                               .data,
                               food_group = c("cereals", "tubers", "vegetables", 
                                              "fruits", "meat", "eggs", "fish",
                                              "legumes_seeds", "milk", "oils_fats",
                                              "sweets", "spices")) {
  .data_list <- rep(list(.data), 12)
  
  ## Check that length of vars is the same as length of food_group
  if (length(vars) != length(food_group)) {
    stop(
      "Number of variables/variable groups not the same as the number of
      specified food groups. Please verify specified variables."
    )
  }

  Map(
    f = hdds_recode_group, 
    vars = vars, 
    .data = .data_list, 
    food_group = food_group
  ) |>
    (\(x) { names(x) <- paste0("hdds_", food_group); x })() |>
    dplyr::bind_rows()
  
}


################################################################################
#
#'
#' Create named list of food group variables
#'
#
################################################################################

hdds_map_fg_vars <- function(cereals, tubers, vegetables, fruits, meat, 
                             eggs, fish, legumes_seeds, milk, oils_fats,
                             sweets, spices) {
  list(
    cereals = cereals,
    tubers = tubers,
    vegetables = vegetables,
    fruits = fruits,
    meat = meat,
    eggs = eggs,
    fish = fish,
    legumes_seeds = legumes_seeds,
    milk = milk,
    oils_fats = oils_fats,
    sweets = sweets,
    spices = spices
  )
} 


################################################################################
#
#'
#' Calculate HDDS
#'
#' @param fg_df A data.frame with 12 columns, one for each food group in HDDS
#' @param add Logical. Should the resulting score be added to fg_df? Default to
#'   TRUE
#' 
#'
#
################################################################################

hdds_calculate_score <- function(fg_df, add = TRUE) {
  ## Check that fg_df has 12 columns
  if (ncol(fg_df) != 12) {
    stop(
      "The food group data.frame needs to have 12 columns for each of the
      HDDS food groups. Please verify your dataset."
    )
  }
  
  hdds <- rowSums(fg_df, na.rm = TRUE)
  
  if (add) {
    data.frame(
      fg_df,
      hdds = hdds
    )
  } else {
    hdds
  }
}


hdds_recode <- function(vars,
                        .data,
                        food_group = c("cereals", "tubers", "vegetables", 
                                       "fruits", "meat", "eggs", "fish",
                                       "legumes_seeds", "milk", "oils_fats",
                                       "sweets", "spices")) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- hdds_recode_groups(
    vars = vars, .data = .data, food_group = food_group
  ) |>
    hdds_calculate_score(add = TRUE)
  
  data.frame(core_vars, recoded_vars)
}

