################################################################################
#
#'
#' Recode vitamin A indicators
#'
#
################################################################################

## Recode responses to each vitamin A supplementation questions ----------------

vita_recode <- function(.data) {
  vita_at_least_once <- .data[["vas1"]] |>
    (\(x) 
      {
        ifelse(x %in% 4:5, NA,
          ifelse(
            x == 3, 0, 1
          )
        )
      }
    )()
  
  vita2 <- .data[["vas2"]] |>
    (\(x) 
     {
       ifelse(x %in% 4:5, NA,
              ifelse(
                x == 3, 0, 1
              )
       )
    }
    )() |>
    spread_vector_to_columns(
      fill = 0:2,
      na_rm = FALSE, prefix = "vita"
    )
  
  data.frame(vita_at_least_once, vita2)
}

## Recode responses to the deworming indicator question ------------------------

worm_recode <- function(.data) {
  ifelse(
    .data[["vas3"]] == 8, NA,
    ifelse(
      .data[["vas3"]] == 2, 0, 1
    )
  )
}

## Recode both vitamin A and deworming indicators ------------------------------

vas_recode <- function(.data) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  recoded_vars <- data.frame(
    vita_recode(.data),
    deworm = worm_recode(.data)
  )
  
  data.frame(core_vars, recoded_vars)
}

