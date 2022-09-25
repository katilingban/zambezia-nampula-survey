################################################################################
#
#'
#' Recode ICFI - food groups
#'
#' food_yogurt	Yogurt, other than yogurt drinks?
#' food_yogurt_num	If “yes”: How many times did **${child_random_name}** eat 
#'   yogurt?
#' food_rice	Potatoes, chima, bread, rice, spaghetti, crackers, or other food 
#'   made from cereals
#' food_pumpkin	Yellow or orange squash, carrot, or sweet potato
#' food_potatoes	White pulp potato, white yam, manioc or any other tuber
#' food_green_veg	Any greens of dark green leaves (beans, cassava leaves, kale, 
#'   sweet potato leaves,, Tseke (Nhewe)
#' food_oth_veg	Any other vegetables?
#' food_mango	Ripe mango, ripe papaya, guava red pulp or other fruit rich in 
#'   vitamin A
#' food_oth_fruit	Any other fruits (banana, apple, tomato, lemon, orange, 
#'   tangerine, grapes)
#' food_liver	Liver, kidney, heart or other organs
#' food_proc_meat	Sausages, hot dogs/frankfurters, ham, bacon, salami, 
#'   canned meat or ...?
#' food_meat	Any meat, such as beef, pork, sheep, kid or duck, mouse or 
#'   other game meat
#' food_egg	Eggs
#' food_fish	Fresh or dried fish or shellfish
#' food_bean	Any food made with beans, peas, lentils, almonds or seeds
#' food_cheese	Cheese or other products made from milk
#' food_sweet	Sweet foods such as chocolates, candies, pastries, cakes, 
#'   biscuits, or frozen treats like ice cream and popsicles?
#' food_fast	Chips, crisps, puffs, French fries, fried dough, instant noodles?
#' food_solid	Any other solid, semi-solid or soft food?
#' food_solid_oth	If “yes”: What was the food?
#' food_yest	Did **${child_random_name}** eat any solid, semi-solid or soft 
#'   food yesterday during the day or at night?
#' food_num	How many times did **${child_random_name}** eat any solid, 
#'   semi-solid or soft foods yesterday during the day or night?
#'
#'
#
################################################################################

## Recode responses to specific food group -------------------------------------

fg_recode_response <- function(x, na_values, binary = TRUE) {
  na_type <- get_na_type(x)
  
  if (binary) {
    ifelse(
      x %in% na_values, na_type,
      ifelse(x == 2, 0, 1)
    )
  } else {
    ifelse(x %in% na_values, na_type, x)
  }  
}

## Recode responses to multiple food groups ------------------------------------

fg_recode_responses <- function(vars, .data, 
                                na_values = rep(list(9), 19),
                                binary = TRUE) {
  x <- .data[vars]
  
  Map(
    f = fg_recode_response,
    x = as.list(x),
    na_values = na_values,
    binary = as.list(c(TRUE, FALSE, rep(TRUE, 16), FALSE))
  ) |>
    dplyr::bind_cols() |>
    (\(x) 
     {
       data.frame(
         age_months = .data[["age_months"]],
         x
       ) 
    } 
    )()
}

## Map food intake variables to child-specific food groups ---------------------

fg_map_vars <- function(dairy, starch, vita, other_fruit_veg, 
                        legumes, meat, eggs) {
  list(
    dairy = dairy, 
    starch = starch,
    vita = vita,
    other_fruit_veg = other_fruit_veg,
    legumes = legumes,
    meat = meat,
    eggs = eggs
  )
} 

## Recode dairy food group -----------------------------------------------------

fg_recode_dairy <- function(vars = c("food_yogurt", "food_cheese"), 
                            .data) {
  x <- .data[vars]
  
  ifelse(x[[vars[1]]] == 1 | x[[vars[2]]] == 1, 1, 0)
}

## Recode starch food group ----------------------------------------------------

fg_recode_starch <- function(vars = c("food_rice", "food_potatoes"), 
                            .data) {
  x <- .data[vars]
  
  ifelse(x[[vars[1]]] == 1 | x[[vars[2]]] == 1, 1, 0)
}

## Recode responses to food items for sepcific food group

fg_recode_group <- function(vars,
                            .data,
                            food_group = c("dairy", "starch", "vita", 
                                           "other_fruit_veg", "legumes", 
                                           "meat", "eggs")) {
  food_group <- match.arg(food_group)
  
  if (length(vars) > 1) {
    if (food_group %in% c("eggs", "legumes")) {
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
    # for (i in vars) {
    #   df[i] <- recode_yes_no(df[[i]], na_values = c("8", "9"))
    # }
    
    ## Recode food group to 1 and 0
    fg <- recode_yes_no(x = rowSums(df, na.rm = TRUE), detect = "no")
  } else {
    if (!food_group %in% c("eggs", "legumes")) {
      warning(
        paste0(
          "Only one food group variable specified. Please make sure that
          there are no other variables that are specific for the ", food_group, 
          " food group."
        )
      )
    }    
    
    ## Calculate indicator
    fg <- .data[[vars]]
  }
  
  ## Return
  fg
}

## Recode responses to food itmes for multiple food groups ---------------------

fg_recode_groups <- function(vars,
                             .data,
                             food_group = c("dairy", "starch", "vita", 
                                            "other_fruit_veg", "legumes", 
                                            "meat", "eggs")) {
  .data_list <- rep(list(.data), 7)
  
  ## Check that length of vars is the same as length of food_group
  if (length(vars) != length(food_group)) {
    stop(
      "Number of variables/variable groups not the same as the number of
      specified food groups. Please verify specified variables."
    )
  }
  
  Map(
    f = fg_recode_group, 
    vars = vars, 
    .data = .data_list, 
    food_group = food_group
  ) |>
    (\(x) { names(x) <- paste0("fg_", food_group); x })() |>
    dplyr::bind_rows()
}

## Calculate food group score --------------------------------------------------

fg_calculate_score <- function(fg_df, add = TRUE) {
  ## Check that fg_df has 7 columns
  if (ncol(fg_df) != 7) {
    stop(
      "The food group data.frame needs to have 7 columns for each of the
      ICFI food groups. Please verify your dataset."
    )
  }
  
  fg_score <- rowSums(fg_df, na.rm = TRUE)
  
  if (add) {
    data.frame(
      fg_df,
      fg_score = fg_score
    )
  } else {
    fg_score
  }
}

## Calculate food group component of ICFI --------------------------------------

fg_calculate_icfi <- function(age_months, fg_score) {
  icfi_group <- cut(
    age_months, breaks = c(0, 5, 8, 11, 24), include.lowest = TRUE
  ) |>
    as.numeric()
  
  ifelse(
    icfi_group == 2, (fg_score == 1) + (fg_score > 1) * 2,
    ifelse(
      icfi_group == 3, (fg_score %in% 1:2) + (fg_score > 2) * 2,
      ifelse(
        icfi_group == 4, (fg_score %in% 2:3) + (fg_score > 3) * 2, NA
      )
    )
  )
}

## Overall recode function------------------------------------------------------

fg_recode <- function(vars,
                      .data,
                      food_group = c("dairy", "starch", "vita", 
                                     "other_fruit_veg", "legumes", 
                                     "meat", "eggs")) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- fg_recode_groups(
    vars = vars, .data = .data, food_group = food_group
  ) |>
    fg_calculate_score(add = TRUE)
  
  fg_icfi <- fg_calculate_icfi(
    age_months = .data[["age_months"]], 
    fg_score = recoded_vars[["fg_score"]]
  )
  
  data.frame(core_vars, recoded_vars, fg_icfi)
}