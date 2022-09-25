################################################################################
#
#' 
#' Process and recode income and occupation data/indicators
#'
#' Relevant variables:
#'
#'   ig1 - What is the main source of income for the family in this household?
#'     integer variables for the following categorical values:
#'     
#'     1=Sale of agricultural products and/or animals; 
#'     2=Self-employed (commercial, service); 
#'     3=Assitência alimentar/ajuda/ganho ganho/biscate; 
#'     4=Fishing; 
#'     5=Salary, pension, remittance; 
#'     6=Other; 
#'     88=Don't know; 
#'     99=No response 
#'
#'  q08 - Adding the income of all the members of the household, including 
#'    remittances from people who are abroad, receipts, or salary of all adults 
#'    and children in the household who work, which of the following categories 
#'    is the closest to the monthly income of the family in this household?	
#'    integer variables for the following categorical values:
#'
#'    1=No income/or remittances not declared in money; 
#'    2=Less than 60-150 Mts per month; 
#'    3=From Mts 150.01 to Mts 500.00 per month; 
#'    4=From Mts 500.01 to Mts 1500.00 per month; 
#'    5=From Mts 1500.01 to Mts 3500.00 per month; 
#'    6=From Mts 3500.01 to Mts 5500.00 per month; 
#'    7=From Mts 5500.01 to Mts 7500.00 per month; 
#'    8=From Mts 7500.01 to Mts 9500.00 per month; 
#'    9=More than Mts 9500.00 per month; 
#'    88=Don't know; 
#'    99=No response
#'
#'  igs1 - What is your primary occupation? integer variables for the following
#'    categorical values:
#'
#'    1=Homemaker; 
#'    2=Your land; 
#'    3=Fishing; 
#'    4=Wage labor; 
#'    5=Business; 
#'    6=Other; 
#'    88=Don't know; 
#'    99=No response
#'
#'  igs2	What is the primary occupation of your husband/partner? integer
#'    variable for the following categorical values:
#'  
#'    1=None; 
#'    2=Your land; 
#'    3=Fishing; 
#'    4=Wage labor; 
#'    5=Business; 
#'    6=Other; 
#'    7=I do not have a partner/husband; 
#'    88=Don't know; 
#'    99=No response
#'    
#
################################################################################

work_recode_response <- function(x, na_values, fill, na_rm = FALSE, 
                                 prefix, label = NULL) {
  x <- ifelse(x %in% na_values, NA, x)
  
  y <- spread_vector_to_columns(
    x = x,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  )
  
  if (!is.null(label)) {
    names(y) <- paste0(prefix, "_", label)
  }
  
  work_df <- data.frame(x, y) |>
    (\(x) { names(x)[1] <- prefix; x } )()
  
  work_df
}

work_recode_responses <- function(vars, .data, na_values, fill, na_rm = FALSE, 
                                  prefix, label = NULL) {
  x <- .data[vars]
  x <- apply(X = x, MARGIN = 2, list) |>
    unlist(recursive = FALSE)
  
  Map(
    f = work_recode_response,
    x = x,
    na_values = na_values,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix,
    label = label
  ) |>
    dplyr::bind_cols()
}


work_recode <- function(vars, .data, na_values, fill, na_rm = FALSE, 
                        prefix, label = NULL) {
  core_vars <- get_core_variables(.data = .data)
  
  recoded_vars <- work_recode_responses(
    vars = vars,
    .data = .data,
    na_values = na_values,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix,
    label = label
  )
  
  data.frame(core_vars, recoded_vars)
}


################################################################################
#
#
#
################################################################################

inc_recode_source <- function(x, na_values, fill, na_rm = FALSE, 
                              prefix = "income_source", label = NULL) {
  x <- ifelse(x %in% na_values, NA, x)

  y <- spread_vector_to_columns(
    x = x,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  )
  
  if (!is.null(label)) {
    names(y) <- paste0(prefix, "_", label)
  }
  
  income_source <- data.frame(x, y)
  
  income_source
}

inc_recode_amount <- function(x, na_values, fill, na_rm = FALSE, 
                              prefix = "income_amount", label) {
  x <- ifelse(x %in% na_values, NA, x)
  
  y <- spread_vector_to_columns(
    x = x,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  )
  
  if (!is.null(label)) {
    names(y) <- paste0(prefix, "_", label)
  }
  
  income_amount <- data.frame(x, y)
  
  income_amount
}


occ_recode_carer <- function(x, na_values, fill, na_rm = FALSE, 
                             prefix = "occ_carer", label) {
  x <- ifelse(x %in% na_values, NA, x)
  
  y <- spread_vector_to_columns(
    x = x,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  )
  
  if (!is.null(label)) {
    names(y) <- paste0(prefix, "_", label)
  }
  
  occ_carer <- data.frame(x, y)
  
  occ_carer
}



occ_recode_partner <- function(x, na_values, fill, na_rm = FALSE, 
                               prefix = "occ_partner", label) {
  x <- ifelse(x %in% na_values, NA, x)
  
  y <- spread_vector_to_columns(
    x = x,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  )
  
  if (!is.null(label)) {
    names(y) <- paste0(prefix, "_", label)
  }
  
  occ_partner <- data.frame(x, y)
  
  occ_partner
}


################################################################################
#
#' 
#' Process and recode income and occupation data/indicators - baseline
#'
#
################################################################################

process_work_baseline <- function(vars = c("ig1", "q08", "igs1", "igs2"), 
                                  .data) {
  respondent_df <- get_respondent_data(.data = .data)
  
  work_df <- get_core_variables(.data = respondent_df) |>
    data.frame(
      respondent_df |>
        subset(select = vars)
    ) |>
    dplyr::mutate(
      ig1 = haven::as_factor(ig1) |>
        (\(x)
          {
            levels(x) <- ifelse(
              levels(x) %in% c("[NÃO LER] Não sabe", 
                               "[NÃO LER] Não responde",
                               "NR", "NS","NSA"),
              NA_character_,
              levels(x)
            )
            x
          }
        )(),
      q08 = haven::as_factor(q08) |>
        (\(x)
         {
           levels(x) <- ifelse(
             levels(x) %in% c("[NÃO LER] Não sabe", 
                              "[NÃO LER] Não responde",
                              "NR", "NS","NSA"),
             NA_character_,
             levels(x)
           )
           x
        }
        )(),
      igs1 = haven::as_factor(igs1) |>
        (\(x)
         {
           levels(x) <- ifelse(
             levels(x) %in% c("[NÃO LER] Não sabe", 
                              "[NÃO LER] Não responde",
                              "NR", "NS","NSA"),
             NA_character_,
             levels(x)
           )
           x
        }
        )(),
      igs2 = haven::as_factor(igs2) |>
        (\(x)
         {
           levels(x) <- ifelse(
             levels(x) %in% c("[NÃO LER] Não sabe", 
                              "[NÃO LER] Não responde",
                              "NR", "NS","NSA"),
             NA_character_,
             levels(x)
           )
           x
        }
        )()
    )
}

