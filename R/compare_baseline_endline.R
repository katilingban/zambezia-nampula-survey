################################################################################
#
#'
#' Calculate difference in estimates
#'
#
################################################################################

calculate_difference_in_difference <- function(diff_list,
                                               type = c("province", "overall")) {
  type = match.arg(type)
  
  if (type == "province") {
    c(
      diff_list[[1]][1] - diff_list[[2]][1],
      diff_list[[1]][3] + diff_list[[2]][3],
      sqrt(diff_list[[1]][3] + diff_list[[2]][3]),
      diff_list[[1]][2] - diff_list[[2]][2],
      diff_list[[1]][4] + diff_list[[2]][4],
      sqrt(diff_list[[1]][4] + diff_list[[2]][4])
    ) |>
      matrix(
        nrow = 2, ncol = 3, byrow = TRUE,
        dimnames = list(c("Zambézia", "Nampula"), c("diff", "variance", "se"))
      )
  } else {
    c(
      diff_list[[1]][1] - diff_list[[2]][1],
      diff_list[[1]][2] + diff_list[[2]][2],
      sqrt(diff_list[[1]][2] + diff_list[[2]][2])
    )
  }
}

################################################################################
#
#'
#' Calculate difference in estimates, variance of the difference in estimates,
#' and standard error of difference in estimates
#'
#
################################################################################

calculate_variance_difference <- function(var,
                                          baseline_svystat, 
                                          endline_svystat,
                                          type = c("province", "overall")) {
  type <- match.arg(type)
  
  if (type == "province") {
    baseline_estimates <- lapply(
      X = baseline_svystat,
      FUN = function(x, var) coef(x)[var],
      var = var
    ) |>
      unlist() |>
      matrix(
        nrow = 2, ncol = 2,
        byrow = TRUE,
        dimnames = list(c("Zambézia", "Nampula"), c("Control", "Intervention"))
      )
    
    baseline_variance_matrix <- lapply(
      X = baseline_svystat,
      FUN = function(x, var) vcov(x)[var, var],
      var = var
    ) |>
      unlist() |>
      matrix(
        nrow = 2, ncol = 2,
        byrow = TRUE,
        dimnames = list(c("Zambézia", "Nampula"), c("Control", "Intervention"))
      )
    
    endline_estimates <- lapply(
      X = endline_svystat,
      FUN = function(x, var) coef(x)[var],
      var = var
    ) |>
      unlist() |>
      matrix(
        nrow = 2, ncol = 2,
        byrow = TRUE,
        dimnames = list(c("Zambézia", "Nampula"), c("Control", "Intervention"))
      )
    
    endline_variance_matrix <- lapply(
      X = endline_svystat,
      FUN = function(x, var) vcov(x)[var, var],
      var = var
    ) |>
      unlist() |>
      matrix(
        nrow = 2, ncol = 2,
        byrow = TRUE,
        dimnames = list(c("Zambézia", "Nampula"), c("Control", "Intervention"))
      )
    
    list(
      control = c(
        baseline_estimates[ , 1] - endline_estimates[ , 1],
        baseline_variance_matrix[ , 1] + endline_variance_matrix[ , 1],
        sqrt(baseline_variance_matrix[ , 1] + endline_variance_matrix[ , 1])
      ) |>
        matrix(
          nrow = 2, ncol = 3, 
          dimnames = list(c("Zambézia", "Nampula"), c("diff", "variance", "se"))
        ),
      intervention = c(
        baseline_estimates[ , 2] - endline_estimates[ , 2],
        baseline_variance_matrix[ , 2] + endline_variance_matrix[ , 2],
        sqrt(baseline_variance_matrix[ , 2] + endline_variance_matrix[ , 2])      
      ) |>
        matrix(
          nrow = 2, ncol = 3, 
          dimnames = list(c("Zambézia", "Nampula"), c("diff", "variance", "se"))
        )
    )
  } else {
    baseline_estimates <- lapply(
      X = baseline_svystat,
      FUN = function(x, var) coef(x)[var],
      var = var
    ) |>
      unlist() |>
      (\(x) { names(x) <- c("Control", "Intervention"); x })()
    
    baseline_variance_matrix <- lapply(
      X = baseline_svystat,
      FUN = function(x, var) vcov(x)[var, var],
      var = var
    ) |>
      unlist() |>
      (\(x) { names(x) <- c("Control", "Intervention"); x })()
    
    endline_estimates <- lapply(
      X = endline_svystat,
      FUN = function(x, var) coef(x)[var],
      var = var
    ) |>
      unlist() |>
      (\(x) { names(x) <- c("Control", "Intervention"); x })()
    
    endline_variance_matrix <- lapply(
      X = endline_svystat,
      FUN = function(x, var) vcov(x)[var, var],
      var = var
    ) |>
      unlist() |>
      (\(x) { names(x) <- c("Control", "Intervention"); x })()
    
    list(
      control = c(
        baseline_estimates[1] - endline_estimates[1],
        baseline_variance_matrix[1] + endline_variance_matrix[1],
        sqrt(baseline_variance_matrix[1] + endline_variance_matrix[1])
      ) |>
        (\(x) { names(x) <- c("diff", "variance", "se"); x } )(),
      intervention = c(
        baseline_estimates[2] - endline_estimates[2],
        baseline_variance_matrix[2] + endline_variance_matrix[2],
        sqrt(baseline_variance_matrix[2] + endline_variance_matrix[2])      
      ) |>
        (\(x) { names(x) <- c("diff", "variance", "se"); x } )()
    )
  }
}


################################################################################
#
#'
#' Calculate difference in difference at province level
#'
#
################################################################################

calculate_diff_in_diff_province <- function(baseline, endline) {
  ## Zambezia
  control_diff_zambezia <- data.frame(
    diff_est = coef(endline[["Zambézia Control"]]) - coef(baseline[["Zambézia Control"]]),
    diff_se = sqrt(c(SE(endline[["Zambézia Control"]])) ^ 2 + c(SE(baseline[["Zambézia Control"]])) ^ 2)
  )
  
  intervention_diff_zambezia <- data.frame(
    diff_est = coef(endline[["Zambézia Intervention"]]) - coef(baseline[["Zambézia Intervention"]]),
    diff_se = sqrt(c(SE(endline[["Zambézia Intervention"]])) ^ 2 + c(SE(baseline[["Zambézia Intervention"]])) ^ 2)
  )

  diff_in_diff_zambezia <- data.frame(
    variable = row.names(intervention_diff_zambezia),
    diff_in_diff_est = intervention_diff_zambezia[["diff_est"]] - control_diff_zambezia[["diff_est"]],
    diff_in_diff_se = sqrt((intervention_diff_zambezia[["diff_se"]]) ^ 2 + (control_diff_zambezia[["diff_se"]]) ^ 2)
  ) |>
    dplyr::mutate(
      diff_in_diff_lcl = diff_in_diff_est - 1.96 * diff_in_diff_se,
      diff_in_diff_ucl = diff_in_diff_est + 1.96 * diff_in_diff_se,
      diff_in_diff_z = abs(diff_in_diff_est / diff_in_diff_se),
      diff_in_diff_p = pnorm(diff_in_diff_z, lower.tail = FALSE)
    )
  
  ## Nampula
  control_diff_nampula <- data.frame(
    diff_est = coef(endline[["Nampula Control"]]) - coef(baseline[["Nampula Control"]]),
    diff_se = sqrt(c(SE(endline[["Nampula Control"]])) ^ 2 + c(SE(baseline[["Nampula Control"]])) ^ 2)
  )
  
  intervention_diff_nampula <- data.frame(
    diff_est = coef(endline[["Nampula Intervention"]]) - coef(baseline[["Nampula Intervention"]]),
    diff_se = sqrt(c(SE(endline[["Nampula Intervention"]])) ^ 2 + c(SE(baseline[["Nampula Intervention"]])) ^ 2)
  )
  
  diff_in_diff_nampula <- data.frame(
    variable = row.names(intervention_diff_nampula),
    diff_in_diff_est = intervention_diff_nampula[["diff_est"]] - control_diff_nampula[["diff_est"]],
    diff_in_diff_se = sqrt((intervention_diff_nampula[["diff_se"]]) ^ 2 + (control_diff_nampula[["diff_se"]]) ^ 2)
  ) |>
    dplyr::mutate(
      diff_in_diff_lcl = diff_in_diff_est - 1.96 * diff_in_diff_se,
      diff_in_diff_ucl = diff_in_diff_est + 1.96 * diff_in_diff_se,
      diff_in_diff_z = abs(diff_in_diff_est / diff_in_diff_se),
      diff_in_diff_p = pnorm(diff_in_diff_z, lower.tail = FALSE)
    )

  list(
    Zambézia = diff_in_diff_zambezia, 
    Nampula = diff_in_diff_nampula
  )
}


################################################################################
#
#'
#'
#'
#
################################################################################

calculate_diff_in_diff_overall <- function(baseline, endline) {
  control_diff <- data.frame(
    diff_est = coef(endline[["Control"]]) - coef(baseline[["Control"]]),
    diff_se = sqrt(c(SE(endline[["Control"]])) ^ 2 + c(SE(baseline[["Control"]])) ^ 2)
  )
  
  intervention_diff <- data.frame(
    diff_est = coef(endline[["Intervention"]]) - coef(baseline[["Intervention"]]),
    diff_se = sqrt(c(SE(endline[["Intervention"]])) ^ 2 + c(SE(baseline[["Intervention"]])) ^ 2)
  )
  
  diff_in_diff <- data.frame(
    variable = row.names(intervention_diff),
    diff_in_diff_est = intervention_diff[["diff_est"]] - control_diff[["diff_est"]],
    diff_in_diff_se = sqrt((intervention_diff[["diff_se"]]) ^ 2 + (control_diff[["diff_se"]]) ^ 2)
  ) |>
    dplyr::mutate(
      diff_in_diff_lcl = diff_in_diff_est - 1.96 * diff_in_diff_se,
      diff_in_diff_ucl = diff_in_diff_est + 1.96 * diff_in_diff_se,
      diff_in_diff_z = abs(diff_in_diff_est / diff_in_diff_se),
      diff_in_diff_p = pnorm(diff_in_diff_z, lower.tail = FALSE)
    )
  
  diff_in_diff
}


################################################################################
#
#'
#'
#'
#
################################################################################

create_diff_in_diff_table <- function(diff_province, 
                                      diff_overall, 
                                      indicator_list = survey_indicator_list,
                                      format = c("wide", "long")) {
  format <- match.arg(format)
  
  if (format == "wide") {
    df <- merge(diff_province[[1]], diff_province[[2]], by = "variable") |>
      merge(diff_overall, by = "variable") |>
      (\(x) 
       { 
         names(x) <- c(
           "variable", 
           "zambezia_diff_est", "zambezia_diff_se", "zambezia_diff_lcl", "zambezia_diff_ucl", "zambezia_diff_z", "zambezia_diff_p",
           "nampula_diff_est", "nampula_diff_se", "nampula_diff_lcl", "nampula_diff_ucl", "nampula_diff_z", "nampula_diff_p",
           "overall_diff_est", "overall_diff_se", "overall_diff_lcl", "overall_diff_ucl", "overall_diff_z", "overall_diff_p"
         )
         x
      }
      )()
    
    merge(
      indicator_list, df,
      by.x = "indicator_variable", by.y = "variable",
      all.y = TRUE, sort = FALSE
    )
  } else {
    df <- rbind(
      data.frame(
        strata = "Zambézia",
        diff_province[[1]]
      ),
      data.frame(
        strata = "Nampula",
        diff_province[[2]]
      ),
      data.frame(
        strata = "Overall",
        diff_overall
      )
    ) |>
      (\(x) 
        { 
          names(x) <- c(
            "strata", "variable", "estimate", "se", "lcl", "ucl", "z", "p"
          )
          x
        }
      )() |>
      dplyr::mutate(
        strata = factor(strata, levels = c("Zambézia", "Nampula", "Overall"))
      )
    
    merge(
      indicator_list, df, 
      by.x = "indicator_variable", by.y = "variable", 
      all.y = TRUE, sort = FALSE
    )
  }
}

