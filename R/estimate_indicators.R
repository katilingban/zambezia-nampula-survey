################################################################################
#
#'
#' Perform weighted estimation of indicators
#'
#
################################################################################

## Estimate total --------------------------------------------------------------

estimate_total <- function(vars, design) {
  ## Create formula for vars
  f_vars <- survey::make.formula(vars)
  
  ## Estimate indicators
  survey::svymean(
    x = f_vars,
    design = design,
    na.rm = TRUE,
    deff = TRUE
  )
}

## Estimate by province --------------------------------------------------------

estimate_province <- function(vars, design) {
  ## Make list of formula for vars
  f_vars_list <- rep(list(vars), 2) |>
    lapply(
      FUN = survey::make.formula
    )
  
  ## Make list of designs
  design_list <- list(
    subset(design, subset = prov == 1),
    subset(design, subset = prov == 2)
  )
  
  ## Estimate indicators
  parallel::mcMap(
    f = survey::svymean,
    x = f_vars_list,
    design = design_list,
    na.rm = TRUE,
    deff = TRUE,
    mc.cores = 2
  ) |>
    (\(x) { names(x) <- c("Zambezia", "Nampula"); x } )()
}

## Estimate by strata ----------------------------------------------------------

estimate_strata <- function(vars, design) {
  ## Make list of formula for vars
  f_vars_list <- rep(list(vars), 9) |>
    lapply(
      FUN = survey::make.formula
    )
  
  ## Make list of designs
  design_list <- list(
    subset(design, subset = strata == 1),
    subset(design, subset = strata == 2),
    subset(design, subset = strata == 3),
    subset(design, subset = strata == 4),
    subset(design, subset = strata == 5),
    subset(design, subset = strata == 6),
    subset(design, subset = strata == 7),
    subset(design, subset = strata == 8),
    subset(design, subset = strata == 9)
  )
  
  ## Estimate indicators
  parallel::mcMap(
    f = survey::svymean,
    x = f_vars_list,
    design = design_list,
    na.rm = TRUE,
    deff = TRUE,
    mc.cores = 2
  ) |>
    (\(x)
      {
        names(x) <- c(
          "Gurúè", "Lugela", "Pebane", "Molumbo", "Rest of Zambézia", 
          "Monapo", "Nacala-A-Velha", "Ribáuè", "Rest of Nampula"
        )
        x
      } 
    )()
}

## Estimate by study group (total) ---------------------------------------------

estimate_study_group <- function(vars, design) {
  ## Make list of formula for vars
  f_vars_list <- rep(list(vars), 2) |>
    lapply(
      FUN = survey::make.formula
    )
  
  ## Make list of designs
  design_list <- list(
    subset(design, subset = study_group == "COM"),
    subset(design, subset = study_group == "INT")
  )
  
  ## Estimate indicators
  parallel::mcMap(
    f = survey::svymean,
    x = f_vars_list,
    design = design_list,
    na.rm = TRUE,
    deff = TRUE,
    mc.cores = 2
  ) |>
    (\(x) { names(x) <- c("Control", "Intervention"); x })()
}

## Estimate by study group by province -----------------------------------------

estimate_study_group_province <- function(vars, design) {
  ## Make list of formula for vars
  f_vars_list <- rep(list(vars), 4) |>
    lapply(
      FUN = survey::make.formula
    )
  
  ## Make list of designs
  design_list <- list(
    subset(design, subset = prov == 1 & study_group == "COM"),
    subset(design, subset = prov == 1 & study_group == "INT"),
    subset(design, subset = prov == 2 & study_group == "COM"),
    subset(design, subset = prov == 2 & study_group == "INT")
  )
  
  ## Estimate indicators
  parallel::mcMap(
    f = survey::svymean,
    x = f_vars_list,
    design = design_list,
    na.rm = TRUE,
    deff = TRUE,
    mc.cores = 2
  ) |>
    (\(x) 
      { 
        names(x) <- lapply(
          X = c("Zambezia", "Nampula"),
          FUN = paste,
          c("Control", "Intervention")
        ) |>
          unlist()
        x
      }
    )()
}

