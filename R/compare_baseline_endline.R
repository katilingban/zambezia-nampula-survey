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

