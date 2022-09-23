################################################################################
#
#' 
#' Process and recode Food Consumption Score (FCS) data/indicators
#'
#' Relevant variables:
#'
#'   fcs0 - Did the members of your household fast in the last 7 days?
#'
#'     1=Yes; 2=No
#'
#'   fcs1	How many days in the past week/7 days has your household eaten 
#'     **maize** or any food made from or contains **maize**?
#'
#'   fcs2	How many days in the past week/7 days has your household eaten 
#'     **rice** or any food made from or contains **rice**?
#'
#'   fcs3	How many days in the past week/7 days has your household eaten 
#'     **bread** or any food made from or contains **wheat**?
#'
#'   fcs4	How many days in the past week/7 days has your household eaten 
#'     **tubers** or any food made from or contains **tubers**?
#'
#'   fcs5	How many days in the past week/7 days has your household eaten 
#'     **groundnuts and/or pulses** or any food made from or contains 
#'     **groundnuts and/or pulses**?
#'
#'   fcs6	How many days in the past week/7 days has your household eaten 
#'     **fish** or any food made from or contains **fish**?
#'
#'   fcs7	How many days in the past week/7 days has your household eaten 
#'     **fish powder or fish sauce used as flavouring** or any food made 
#'     from or contains **fish powder or fish sauce as flavouring**?	
#'
#'   fcs8	How many days in the past week/7 days has your household eaten 
#'     **red meat from sheep, goat, or beef** or any food made from or 
#'     contains **red meat from sheep, goat, or beef**?
#'
#'   fcs9	How many days in the past week/7 days has your household eaten 
#'     **white meat from poultry** or any food made from or contains 
#'     **white meat from poultry**?
#'
#'   fcs10	How many days in the past week/7 days has your household eaten 
#'     **vegetable oil and fats** or any food made from or contains 
#'     **vegetable oil and fats**?
#'
#'   fcs11	How many days in the past week/7 days has your household eaten 
#'     **eggs** or any food made from or contains **eggs**?
#'
#'   fcs12	How many days in the past week/7 days has your household eaten 
#'     **milk and dairy products** as a main food?
#'
#'   fcs13	How many days in the past week/7 days has your household taken 
#'     **milk in tea in small amounts**?
#'
#'   fcs14	How many days in the past week/7 days has your household eaten 
#'     **vegetables including leaves** or any food made from or contains 
#'     **vegetables including leaves**?
#'
#'   fcs15	How many days in the past week/7 days has your household eaten 
#'     **fruits** or any food made from or contains **fruits**?
#'
#'   fcs16	How many days in the past week/7days has your household eaten 
#'     **sweets and sugary foods** or any food made from or contains 
#'     **sugar**?
#'     
#' @param vars A vector of variables to use or a list of vectors of variables
#'   to use for FCS
#' @param .data A data.frame from which variables should be taken
#' @param food_group A character value specifying food group to recode. Can be
#'   one of 9 food groups used in calculating the HDDS
#'     
#
################################################################################

fcs_recode_group <- function(vars, 
                             .data, 
                             food_group = c("staples", "pulses", "vegetables",
                                            "fruits", "meat_fish", "milk",
                                            "sugar", "oil", "condiments")) {
  food_group <- match.arg(food_group)
  
  if (length(vars) > 1) {
    if (!food_group %in% c("staples", "meat_fish", "condiments")) {
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
      df[i] <- ifelse(df[[i]] %in% c("88", "99"), NA, df[[i]])
    }
    
    ## Recode food group to 1 and 0
    fg <- rowSums(df, na.rm = TRUE) |>
      (\(x) ifelse(x > 7, 7, x))() |>
      as.integer()
  } else {
    if (food_group %in% c("staples", "meat_fish", "condiments")) {
      warning(
        paste0(
          "Only one food group variable specified. Please make sure that
          there are no other variables that are specific for the ", food_group, 
          " food group."
        )
      )
    }    
    
    ## Calculate indicator
    fg <- ifelse(.data[[vars]] %in% c("88", "99"), NA, .data[[vars]]) |>
      (\(x) ifelse(x > 7, 7, x))() |>
      as.integer()
  }
  
  ## Return
  fg
}


fcs_recode_groups <- function(vars,
                              .data,
                              food_group = c("staples", "pulses", "vegetables",
                                             "fruits", "meat_fish", "milk",
                                             "sugar", "oil", "condiments")) {
  .data_list <- rep(list(.data), 9)
  
  ## Check that length of vars is the same as length of food_group
  if (length(vars) != length(food_group)) {
    stop(
      "Number of variables/variable groups not the same as the number of
      specified food groups. Please verify specified variables."
    )
  }
  
  Map(
    f = fcs_recode_group, 
    vars = vars, 
    .data = .data_list, 
    food_group = food_group
  ) |>
    (\(x) { names(x) <- paste0("fcs_", food_group); x })() |>
    dplyr::bind_rows()
  
}

fcs_map_fg_vars <- function(staples, pulses, vegetables,
                            fruits, meat_fish, milk,
                            sugar, oil, condiments) {
  list(
    staples = staples,
    pulses = pulses,
    vegetables = vegetables,
    fruits = fruits,
    meat_fish = meat_fish,
    milk = milk,
    sugar = sugar,
    oil = oil,
    condiments = condiments
  )
}


################################################################################
#
#'
#' Calculate FCS
#'
#' @param fg_df A data.frame with 9 columns, one for each food group in FCS
#' @param add Logical. Should the resulting score be added to fg_df? Default to
#'   TRUE
#' 
#'
#
################################################################################

fcs_calculate_score <- function(fg_df, add = TRUE, 
                                weights = c(2, 3, 1, 1, 4, 4, 0.5, 0.5, 0)) {
  weights <- weights |>
    lapply(
      FUN = rep,
      times = nrow(fg_df)
    ) |>
    (\(x) { names(x) <- paste0("w", 1:length(weights)); x })() |>
    dplyr::bind_cols()
  
  fg_df_weighted <- fg_df * weights
  
  fcs <- rowSums(fg_df_weighted, na.rm = TRUE)
  
  if (add) {
    data.frame(
      fg_df,
      fcs = fcs
    )
  } else {
    fcs
  }
}


################################################################################
#
#'
#' Classify FCS
#' 
#' @param fcs A vector of food consumption scores
#' @param add Logical. Should classification be column binded to fcs? Default
#'   to FALSE.
#' @param spread Logical. Should classification be spread into columns?
#'   Default to FALSE.
#'
#
################################################################################

fcs_classify <- function(fcs, add = FALSE, spread = FALSE, cutoff = c(21, 35)) {
  fcs_class <- cut(
    x = fcs,
    breaks = c(0, cutoff[1], cutoff[2], Inf),
    labels = c("poor", "borderline", "acceptable"),
    include.lowest = FALSE, right = TRUE
  )
  
  if (spread) {
    fcs_class <- data.frame(
      fcs_class = fcs_class,
      spread_vector_to_columns(x = fcs_class, prefix = "fcs")
    )
  }
  
  if (add) {
    fcs_class <- data.frame(
      fcs, fcs_class
    )
  }
  
  fcs_class
}


fcs_recode <- function(vars,
                       .data,
                       food_group = c("staples", "pulses", "vegetables",
                                      "fruits", "meat_fish", "milk",
                                      "sugar", "oil", "condiments")) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- fcs_recode_groups(
    vars = vars, .data = .data, food_group = food_group
  )
  
  recoded_vars |>
    fcs_calculate_score(add = FALSE) |>
    fcs_classify(add = TRUE, spread = TRUE) |>
    (\(x) data.frame(core_vars, recoded_vars, x))()
}
