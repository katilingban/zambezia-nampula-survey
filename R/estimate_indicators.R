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
    subset(design, subset = province == "Zambézia"),
    subset(design, subset = province == "Nampula")
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
    (\(x) { names(x) <- c("Zambézia", "Nampula"); x } )()
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
    subset(design, subset = strata == "Gurúè"),
    subset(design, subset = strata == "Lugela"),
    subset(design, subset = strata == "Pebane"),
    subset(design, subset = strata == "Molumbo"),
    subset(design, subset = strata == "Rest of Zambézia"),
    subset(design, subset = strata == "Monapo"),
    subset(design, subset = strata == "Nacala-A-Velha"),
    subset(design, subset = strata == "Ribáuè"),
    subset(design, subset = strata == "Rest of Nampula")
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
    (\(x) { names(x) <- c("Comparison", "Intervention"); x })()
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
    subset(design, subset = province == "Zambézia" & study_group == "COM"),
    subset(design, subset = province == "Zambézia" & study_group == "INT"),
    subset(design, subset = province == "Nampula" & study_group == "COM"),
    subset(design, subset = province == "Nampula" & study_group == "INT")
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
          X = c("Zambézia", "Nampula"),
          FUN = paste,
          c("Comparison", "Intervention")
        ) |>
          unlist()
        x
      }
    )()
}

