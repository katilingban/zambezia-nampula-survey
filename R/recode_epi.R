################################################################################
#
#'
#' Recode immunisation card indicators
#'
#'
#
################################################################################

## Recode responses to card retention question ---------------------------------

imm_recode_card <- function(.data) {
  imm_card1 <- .data[["imm1"]] |>
    (\(x)
      {
        ifelse(
          x %in% c(8, 9), NA,
            ifelse(
            x == 2, 0, x 
          )
        )
      }
     )()  
  
  imm_card2 <- .data[["imm2"]] |>
    (\(x) 
      {
        ifelse(
          x %in% c(88, 99), NA,
          ifelse(
            x == 1, 1, 0
          )
        )
      }
    )()  |>
    (\(x) ifelse(imm_card1 == 1 & x == 1, 1, 0))()

  data.frame(imm_card1, imm_card2)
}


################################################################################
#
#'
#' Recode immunisation indicators
#'
#
################################################################################

## Recode responses per vaccine ------------------------------------------------

imm_recode_vaccine <- function(vars, .data, imm_label = NULL) {
  x <- .data[[vars]]
  
  imm1 <- x |>
    (\(x) 
      {
        ifelse(
          x %in% 4:5, NA,
          ifelse(
            x == 3, 0, 1
          )
        )
      }
    )()
  
  imm2 <- x |>
    (\(x) 
     {
       ifelse(
         x %in% 4:5, NA,
         ifelse(
           x == 1, 1, 0
         )
       )
    }
    )()
  
  if (!is.null(imm_label)) {
    imm_label <- paste(imm_label, c("card_recall", "card_only"), sep = "_")
  } else {
    imm_label <- paste(vars, c("card_recall", "card_only"), sep = "_")
  }
  
  data.frame(imm1, imm2) |>
    (\(x) { names(x) <- imm_label; x })()
}

## Recode responses across multiple vaccines -----------------------------------

imm_recode_vaccines <- function(vars, .data,
                                imm_label = c("bcg", 
                                              "opv1", "opv2", "opv3", "opv4",
                                              "dpt1", "dpt2", "dpt3",
                                              "rub1", "rub2",
                                              "pcv1", "pcv2", "pcv3",
                                              "vorh1", "vorh2")) {
  imm_df <- lapply(
    X = vars,
    FUN = imm_recode_vaccine,
    .data = .data,
    imm_label = NULL
  ) |>
    dplyr::bind_cols()
  
  names(imm_df) <- lapply(
    X = imm_label, 
    FUN = paste, 
    c("card_recall", "card_only"), 
    sep = "_"
  ) |>
    unlist()
  
  imm_df
}

## Recode immunisation coverage indicators -------------------------------------

imm_recode_full <- function(vars, .data) {
  imm_df <- data.frame(
    age_months = .data[["age_months"]],
    imm_recode_vaccines(vars = vars, .data = .data)
  )
  
  imm_full_recall <- with(
    imm_df,
    ifelse(
      age_months < 12 & age_months > 24, NA,
      ifelse(
        bcg_card_recall == 1 & 
          opv3_card_recall == 1 & 
          dpt3_card_recall == 1 &
          rub2_card_recall == 1 & 
          pcv3_card_recall == 1 &
          vorh2_card_recall == 1, 1, 0
      )
    )
  )
      
  imm_full_card <- with(
    imm_df,
    ifelse(
      age_months < 12 & age_months > 24, NA,
      ifelse(
        bcg_card_only == 1 & 
          opv3_card_only == 1 & 
          dpt3_card_only == 1 &
          rub2_card_only == 1 & 
          pcv3_card_only == 1 &
          vorh2_card_only == 1, 1, 0          
      )
    )
  )
      
  imm_appropriate_recall <- with(
    imm_df,
    ifelse(
      age_months >= 0 & age_months < 1.5 & 
        bcg_card_recall == 1 & opv1_card_recall == 1, 1,
      ifelse(
        age_months >= 1.5 & age_months < 2.5 & 
          bcg_card_recall == 1 & opv1_card_recall == 1 &
          opv2_card_recall == 1 & dpt1_card_recall == 1 & 
          pcv1_card_recall == 1 & vorh1_card_recall == 1, 1,
        ifelse(
          age_months >= 2.5 & age_months < 3.5 & 
            bcg_card_recall == 1 & opv1_card_recall == 1 &
            opv2_card_recall == 1 & opv3_card_recall == 1 &
            dpt1_card_recall == 1 & dpt2_card_recall == 1 &
            pcv1_card_recall == 1 & pcv2_card_recall == 1 &
            vorh1_card_recall == 1 & vorh2_card_recall == 1, 1,
          ifelse(
            age_months >= 3.5 & age_months < 9 &
              bcg_card_recall == 1 & opv1_card_recall == 1 &
              opv2_card_recall == 1 & opv3_card_recall == 1 & 
              opv4_card_recall == 1 &
              dpt1_card_recall == 1 & dpt2_card_recall == 1 &
              dpt3_card_recall == 1 &
              pcv1_card_recall == 1 & pcv2_card_recall == 1 &
              pcv3_card_recall == 1 &
              vorh1_card_recall == 1 & vorh2_card_recall == 1, 1,
            ifelse(
              age_months >= 9 & age_months < 18 & 
                bcg_card_recall == 1 & opv1_card_recall == 1 &
                opv2_card_recall == 1 & opv3_card_recall == 1 & 
                opv4_card_recall == 1 &
                dpt1_card_recall == 1 & dpt2_card_recall == 1 &
                dpt3_card_recall == 1 &
                pcv1_card_recall == 1 & pcv2_card_recall == 1 &
                pcv3_card_recall == 1 &
                vorh1_card_recall == 1 & vorh2_card_recall == 1 &
                rub1_card_recall == 1, 1,
              ifelse(
                age_months >= 18 & 
                  bcg_card_recall == 1 & opv1_card_recall == 1 &
                  opv2_card_recall == 1 & opv3_card_recall == 1 & 
                  opv4_card_recall == 1 &
                  dpt1_card_recall == 1 & dpt2_card_recall == 1 &
                  dpt3_card_recall == 1 &
                  pcv1_card_recall == 1 & pcv2_card_recall == 1 &
                  pcv3_card_recall == 1 &
                  vorh1_card_recall == 1 & vorh2_card_recall == 1 &
                  rub1_card_recall == 1 & rub2_card_recall == 1, 1, 0
              )
            )
          )
        )
      )
    )
  )
      
  imm_appropriate_card <- with(
    imm_df,
    ifelse(
      age_months >= 0 & age_months < 1.5 & 
        bcg_card_only == 1 & opv1_card_only == 1, 1,
      ifelse(
        age_months >= 1.5 & age_months < 2.5 &
          bcg_card_only == 1 & opv1_card_only == 1 &
          opv2_card_only == 1 & dpt1_card_only == 1 & 
          pcv1_card_only == 1 & vorh1_card_only == 1, 1,
        ifelse(
          age_months >= 2.5 & age_months < 3.5 & 
            bcg_card_only == 1 & opv1_card_only == 1 &
            opv2_card_only == 1 & opv3_card_only == 1 &
            dpt1_card_only == 1 & dpt2_card_only == 1 &
            pcv1_card_only == 1 & pcv2_card_only == 1 &
            vorh1_card_only == 1 & vorh2_card_only == 1, 1,
          ifelse(
            age_months >= 3.5 & age_months < 9 &
              bcg_card_only == 1 & opv1_card_only == 1 &
              opv2_card_only == 1 & opv3_card_only == 1 & 
              opv4_card_only == 1 &
              dpt1_card_only == 1 & dpt2_card_only == 1 &
              dpt3_card_only == 1 &
              pcv1_card_only == 1 & pcv2_card_only == 1 &
              pcv3_card_only == 1 &
              vorh1_card_only == 1 & vorh2_card_only == 1, 1,
            ifelse(
              age_months >= 9 & age_months < 18 &
                bcg_card_only == 1 & opv1_card_only == 1 &
                opv2_card_only == 1 & opv3_card_only == 1 & 
                opv4_card_only == 1 &
                dpt1_card_only == 1 & dpt2_card_only == 1 &
                dpt3_card_only == 1 &
                pcv1_card_only == 1 & pcv2_card_only == 1 &
                pcv3_card_only == 1 &
                vorh1_card_only == 1 & vorh2_card_only == 1 &
                rub1_card_only == 1, 1,
              ifelse(
                age_months >= 18 & 
                  bcg_card_only == 1 & opv1_card_only == 1 &
                  opv2_card_only == 1 & opv3_card_only == 1 & 
                  opv4_card_only == 1 &
                  dpt1_card_only == 1 & dpt2_card_only == 1 &
                  dpt3_card_only == 1 &
                  pcv1_card_only == 1 & pcv2_card_only == 1 &
                  pcv3_card_only == 1 &
                  vorh1_card_only == 1 & vorh2_card_only == 1 &
                  rub1_card_only == 1 & rub2_card_only == 1, 1, 0
              )
            )
          )
        )
      )
    )
  )

  data.frame(
    imm_full_recall, imm_full_card, 
    imm_appropriate_recall, imm_appropriate_card
  )
}

################################################################################
#
#'
#' Overall immunisation recode
#'
#
################################################################################

imm_recode <- function(vars, .data, 
                       imm_label = c("bcg", "opv1", "opv2", "opv3", "opv4",
                                     "dpt1", "dpt2", "dpt3", "rub1", "rub2",
                                     "pcv1", "pcv2", "pcv3", "vorh1", "vorh2")) {
  core_vars <- get_core_variables(raw_data_clean = .data)
  
  imm_card_df <- imm_recode_card(.data = .data)
  
  imm_vaccines_df <- imm_recode_vaccines(
    vars = vars,
    .data = .data,
    imm_label = imm_label
  )
  
  imm_coverage_df <- imm_recode_full(
    vars = vars, .data = .data
  )
  
  data.frame(core_vars, imm_card_df, imm_vaccines_df, imm_coverage_df)
}

