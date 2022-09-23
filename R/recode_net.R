################################################################################
#
#'
#' Process and recode mosquito nets coverage data/indicators
#' 
#' 
#' Relevant variables:
#'   cdcg13
#' 
#
################################################################################

## Recode responses to the different mosquito net questions --------------------

net_recode_response <- function(x, na_values) {
  ## Net responses with NAs
  net <- ifelse(x %in% na_values, NA, x)
  
  ## Household with any net
  net_any <- ifelse(
    x %in% na_values, NA,
    ifelse(x == 1, 0, 1)
  )
  
  ## Household with adequate nets
  net_adequate <- ifelse(
    x %in% na_values, NA,
    ifelse(x %in% 3:4, 1, 0)
  )
  
  data.frame(
    net, net_any, net_adequate,
    spread_vector_to_columns(
      x = net,
      fill = 1:4,
      na_rm = FALSE,
      prefix = "net"
    ) |>
      (\(x) { names(x) <- paste0("net", 1:4); x } )()
  )
}

## Overall recode function -----------------------------------------------------

net_recode <- function(vars, .data, na_values) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- net_recode_response(
    x = .data[[vars]],
    na_values = na_values
  )
  
  data.frame(core_vars, recoded_vars)
}

