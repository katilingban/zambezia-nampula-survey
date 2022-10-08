################################################################################
#
#'
#' Access endline data from ONA
#' 
#' @param form_name Short form name of survey.
#' 
#'
#
################################################################################

get_endline_data <- function(form_name = "ins_u5_endline") {
  ## Get form ID
  form_id <- okapi::ona_data_list() |>
    (\(x) x$id[x$id_string == form_name])()
  
  ## Get data
  .data <- okapi::ona_data_get(form_id = form_id)
  
  ## clean-up variable names
  clean_names <- .data |>
    names() |>
    (\(x) gsub(pattern = "^(.*?)/", replacement = "", x = x))() |>
    (\(x) gsub(pattern = "^(.*?)/", replacement = "", x = x))() |>
    (\(x) gsub(pattern = "^_", replacement = "", x = x))()
    
  ## rename data
  names(.data) <- clean_names
  
  ## remove extra uuid
  .data <- .data[ , c(1:102, 104:ncol(.data))]
  
  ## 
  .data
}


################################################################################
#
#'
#' Process household/respondent data
#' 
#' @param df Data.frame produced by function `get_data()`
#'
#
################################################################################

process_respondent_data <- function(df) {
  template_df <- data.frame(
    start = NA_character_,
    end = NA_character_,
    today = NA_character_,
    audit = NA_character_,
    id = NA_integer_,
    tags = NA,
    uuid = NA_character_,
    notes = NA,
    edited = NA,
    status = NA_character_,
    version = NA_character_,
    duration = NA_real_,
    xform_id = NA_integer_,
    attachments = NA,
    geolocation = NA,
    media_count = NA_integer_,
    total_media = NA_integer_,
    submitted_by = NA_character_,
    date_modified = NA_character_,
    instanceID = NA_character_,
    submission_time = NA_character_,
    xform_id_string = NA_character_,
    instanceName = NA_character_,
    bamboo_dataset_id = NA_character_,
    ENUMID = NA_character_,
    ENUMNAME = NA_character_,
    fgh_id = NA_integer_,
    ea_code = NA_character_,
    province = NA_character_,
    district = NA_character_,
    post = NA_character_,
    locality = NA_character_,
    bairro = NA_character_,
    name1 = NA_character_,
    name2 = NA_character_,
    name3 = NA_character_,
    fgh_confirm = NA_character_,
    RURURB = NA_integer_,
    IDIOMAQ = NA_integer_,
    IDIOMAQ_other = NA_integer_,
    GPS = NA_character_,
    SURVEYDATE = NA_character_,
    consent = NA_integer_,
    FAMSIZE = NA_integer_,
    FAMSIZE1 = NA_integer_,
    FAMSIZE2 = NA_integer_,
    RESP_NAME = NA_character_,
    RESP_SEX = NA_integer_,
    Q01 = NA_integer_,
    RESP_MARITAL_STATUS = NA_integer_,
    RESP_EDU = NA_integer_,
    Q02 = NA_integer_,
    Q02a = NA_integer_,
    Q02b = NA_integer_,
    Q02c = NA_integer_,
    Q02d = NA_integer_,
    Q03 = NA_integer_,
    Q05 = NA_integer_,
    Q05_specify = NA_character_,
    Q06 = NA_integer_,
    Q06_content = NA_character_,
    Q06a = NA_integer_,
    Q06b = NA_integer_,
    Q07 = NA_integer_,
    Q07_specify = NA_character_,
    GI1 = NA_integer_,
    GI1_other = NA_character_,
    GI2t = NA_integer_,
    GI2m = NA_integer_,
    GI3t = NA_integer_,
    GI3m = NA_integer_,
    WT1t = NA_integer_,
    WT1m = NA_integer_,
    SDH1 = NA_integer_,
    SDH2 = NA_integer_,
    SDH3 = NA_integer_,
    SDH4 = NA_integer_,
    SDH5 = NA_integer_,
    SDH6 = NA_integer_,
    SDH7 = NA_integer_,
    SDH8 = NA_integer_,
    CDCG1 = NA_integer_,
    CDCG2 = NA_integer_,
    CDCG3 = NA_integer_,
    CDCG4 = NA_integer_,
    CDCG7 = NA_integer_,
    CDCG8 = NA_integer_,
    CDCG9 = NA_integer_,
    CDCG10 = NA_integer_,
    CDCG11 = NA_integer_,
    CDCG11a = NA_integer_,
    CDCG13 = NA_integer_,
    CDCG14 = NA_integer_,
    CFEGS1 = NA_integer_,
    CFEGS2 = NA_integer_,
    CFEGS3 = NA_integer_,
    CFEGS4 = NA_integer_,
    CFEGS5 = NA_integer_,
    CFEGS67 = NA_integer_,
    IG1 = NA_integer_,
    Q08 = NA_integer_,
    IGS1 = NA_integer_,
    IGS2 = NA_integer_,
    IGS3 = NA_integer_,
    IGS6 = NA_integer_,
    IGS7 = NA_integer_,
    IGS8 = NA_integer_,
    IGS8a = NA_integer_,
    IGS8c = NA_integer_,
    IGS8e = NA_integer_,
    IGS8f = NA_integer_,
    GE1 = NA_integer_,
    GE2 = NA_integer_,
    GE3 = NA_integer_,
    GE4 = NA_integer_,
    GE5 = NA_integer_,
    GE6 = NA_integer_,
    GE7 = NA_integer_,
    GE8 = NA_integer_,
    GE9 = NA_integer_,
    GE10 = NA_integer_,
    WT2 = NA_integer_,
    WT2_other = NA_character_,
    WT3 = NA_integer_,
    WT3a = NA_integer_,
    WT3b = NA_integer_,
    WT4 = NA_integer_,
    WT4a = NA_integer_,
    WT5 = NA_integer_,
    WT6 = NA_integer_,
    CAHA1 = NA_integer_,
    CAHA2 = NA_integer_,
    CAHA2_other = NA_character_,
    CAHA3 = NA_integer_,
    LUSD1 = NA_integer_,
    LUSD2 = NA_integer_,
    LUSD3 = NA_integer_,
    LUSD4 = NA_integer_,
    LUSD5 = NA_integer_,
    LUSD6 = NA_integer_,
    LUSD7 = NA_integer_,
    LUSD8 = NA_integer_,
    CCARE1 = NA_integer_,
    CCARE2 = NA_integer_,
    CCARE3 = NA_integer_,
    DES1 = NA_integer_,
    DES2 = NA_integer_,
    WH1 = NA_integer_,
    WH2 = NA_integer_,
    WH3 = NA_integer_,
    WH4 = NA_integer_,
    WH5 = NA_integer_,
    WH6 = NA_integer_,
    WH6a = NA_character_,
    WH7 = NA_integer_,
    WH7a = NA_character_,
    WH8 = NA_integer_,
    PREG1 = NA_integer_,
    PREG2 = NA_integer_,
    PREG3 = NA_integer_,
    PMTCT1 = NA_integer_,
    PMTCT2 = NA_integer_,
    PMTCT3 = NA_integer_,
    IDK1 = NA_integer_,
    IDK2 = NA_integer_,
    SPC1 = NA_integer_,
    SPC2 = NA_integer_,
    SPC2a = NA_integer_,
    SPC2b = NA_integer_,
    SPC3 = NA_integer_,
    SPC4 = NA_integer_,
    SPC5 = NA_integer_,
    SPC5a = NA_integer_,
    SPC6 = NA_integer_,
    SPC6a = NA_integer_,
    SPC6b = NA_integer_,
    SPC7 = NA_integer_,
    SPC7a = NA_integer_,
    SPC7b = NA_integer_,
    THER1 = NA_integer_,
    CHM1 = NA_integer_,
    CHM2 = NA_integer_,
    FANSIDAR1 = NA_integer_,
    FANSIDAR2 = NA_integer_,
    FOL1 = NA_integer_,
    TT1 = NA_integer_,
    TT2 = NA_integer_,
    EB1 = NA_integer_,
    EB2 = NA_integer_,
    EB2_hours = NA_integer_,
    EB2_days = NA_integer_,
    EB2a = NA_integer_,
    EB3 = NA_integer_,
    EB4 = NA_integer_,
    EB5 = NA_integer_,
    EB5a = NA_integer_,
    EB6 = NA_integer_,
    EB6a = NA_integer_,
    PF1 = NA_integer_,
    BS1 = NA_integer_,
    BS1a = NA_integer_,
    BS2 = NA_integer_,
    BS2a = NA_character_,
    BS3 = NA_integer_,
    BS4 = NA_integer_,
    ABOR1 = NA_integer_,
    ABOR1a = NA_integer_,
    NUTMUL1 = NA_integer_,
    NUTMUL2 = NA_integer_,
    NUTMUL3 = NA_integer_,
    NUTMUL4 = NA_integer_,
    NUTMUL5 = NA_integer_,
    NUTMUL6 = NA_integer_,
    NUTMUL7 = NA_integer_,
    NUTMUL8 = NA_integer_,
    NUTMUL9 = NA_integer_,
    NUTMUL10 = NA_integer_,
    NUTMUL11 = NA_integer_,
    NUTMUL12 = NA_integer_,
    NUTMUL13 = NA_integer_,
    NUTMUL14 = NA_integer_,
    NUTMUL15 = NA_integer_,
    NUTMUL16 = NA_integer_,
    NUTMUL17 = NA_integer_,
    NUTMUL18 = NA_integer_,
    NUTMUL19 = NA_integer_,
    VON1 = NA_integer_,
    VON2 = NA_integer_,
    VON3 = NA_integer_,
    VON4 = NA_integer_,
    MENT1 = NA_integer_,
    MENT2 = NA_integer_,
    MENT3 = NA_integer_,
    MENT4 = NA_integer_,
    MENT5 = NA_integer_,
    MENT6 = NA_integer_,
    MENT7 = NA_integer_,
    MENT8 = NA_integer_,
    MENT9 = NA_integer_,
    MALTURA = NA_real_,
    MPESO = NA_real_,
    MBRACO = NA_real_,
    ANIM1 = NA_integer_,
    ANIM2 = NA_integer_,
    AGUA1 = NA_integer_,
    AGUA2 = NA_integer_,
    AGUA3 = NA_integer_,
    COZ1 = NA_integer_,
    COZ2 = NA_integer_,
    COZ3 = NA_integer_,
    QUIN1 = NA_integer_,
    MAO1 = NA_integer_,
    MAO1a = NA_integer_,
    MAO1b = NA_integer_,
    FCS1 = NA_integer_,
    FCS2 = NA_integer_,
    FCS3 = NA_integer_,
    FCS4 = NA_integer_,
    FCS5 = NA_integer_,
    FCS6 = NA_integer_,
    FCS7 = NA_integer_,
    FCS8 = NA_integer_,
    FCS9 = NA_integer_,
    FCS10 = NA_integer_,
    FCS11 = NA_integer_,
    FCS12 = NA_integer_,
    FCS13 = NA_integer_,
    FCS14 = NA_integer_,
    FCS15 = NA_integer_,
    FCS16 = NA_integer_,
    HDDS1 = NA_integer_,
    HDDS2 = NA_integer_,
    HDDS3 = NA_integer_,
    HDDS4 = NA_integer_,
    HDDS5 = NA_integer_,
    HDDS6 = NA_integer_,
    HDDS7 = NA_integer_,
    HDDS8 = NA_integer_,
    HDDS9 = NA_integer_,
    HDDS10 = NA_integer_,
    HDDS11 = NA_integer_,
    HDDS12 = NA_integer_,
    HDDS13 = NA_integer_,
    HDDS14 = NA_integer_,
    HDDS15 = NA_integer_,
    HDDS16 = NA_integer_,
    RCSI1 = NA_integer_,
    RCSI2 = NA_integer_,
    RCSI3 = NA_integer_,
    RCSI4 = NA_integer_,
    RCSI5 = NA_integer_,
    FIES01 = NA_integer_,
    FIES02 = NA_integer_,
    FIES03 = NA_integer_,
    FIES04 = NA_integer_,
    FIES05 = NA_integer_,
    FIES06 = NA_integer_,
    FIES07 = NA_integer_,
    FIES08 = NA_integer_,
    FS1 = NA_integer_,
    FS2 = NA_integer_,
    RESERVE1 = NA_integer_,
    RESERVE1a = NA_integer_,
    RESERVE2 = NA_integer_,
    RESERVE2a = NA_integer_,
    RESERVE3 = NA_integer_,
    RESERVE3a = NA_integer_,
    RESERVE4 = NA_integer_,
    RESERVE4a = NA_integer_,
    RESERVE5 = NA_integer_,
    RESERVE5a = NA_integer_,
    RESERVE6 = NA_integer_,
    RESERVE6a = NA_integer_,
    RESERVE7 = NA_integer_,
    RESERVE7a = NA_integer_,
    PEST1 = NA_integer_,
    PEST2 = NA_integer_,
    PEST2_other = NA_integer_,
    LCS01 = NA_integer_,
    LCS02 = NA_integer_,
    LCS03 = NA_integer_,
    LCS04 = NA_integer_,
    LCS05 = NA_integer_,
    LCS06 = NA_integer_,
    LCS07 = NA_integer_,
    LCS08 = NA_integer_,
    LCS09 = NA_integer_,
    LCS10 = NA_integer_,
    LCS11 = NA_integer_,
    LCS12 = NA_integer_,
    LCS13 = NA_integer_,
    LCS14 = NA_integer_,
    CHILD_ROSTER = NA,
    CHILD_ROSTER_count = NA_character_,
    CHILD_HEALTH = NA,
    CHILD_HEALTH_count = NA_character_,
    BF2 = NA,
    BF2_count = NA_character_,
    CHILD_ANTHRO_REPEAT = NA,
    CHILD_ANTHRO_REPEAT_count = NA_character_
  )
  
  ## Convert simple codes to integers
  integer_vars <- names(template_df)[sapply(X = template_df, FUN = class) == "integer"] |>
    (\(x) names(df)[names(df) %in% x])()
  
  df[ , integer_vars] <- df[ , integer_vars] |>
    apply(MARGIN = 2, FUN = function(x) as.integer(x))
  
  ## Conver to numeric  
  numeric_vars <- names(template_df)[sapply(X = template_df, FUN = class) == "numeric"] |>
    (\(x) names(df)[names(df) %in% x])()
  
  df[ , numeric_vars] <- df[ , numeric_vars] |>
    apply(MARGIN = 2, FUN = function(x) as.numeric(x))
  
  ## Concatenate
  df <- dplyr::bind_rows(template_df, df) |>
    (\(x) x[2:nrow(x), ])() |>
    tibble::tibble()

  df$start <- gsub(pattern = "T", replacement = " ", x = df$start) |> 
    strptime(format = "%Y-%m-%d %H:%M:%S")
  
  df$end <- gsub(pattern = "T", replacement = " ", x = df$end) |> 
    strptime(format = "%Y-%m-%d %H:%M:%S")
  
  names(df) <- tolower(names(df))
  
  ## Return
  df
}


################################################################################
#
#'
#' Process child data
#' 
#' @param df Data.frame produced by function `process_respondent_data()`
#'
#
################################################################################

process_child_data <- function(df) {
  ## Get child roster
  child_roster <- df$child_roster
  names(child_roster) <- df$id
  
  child_roster <- child_roster |>
    lapply(FUN = clean_child_roster_types) |>
    dplyr::bind_rows(.id = "id")
  
  child_health <- df$child_health
  names(child_health) <- df$id
  
  child_health <- child_health |>
    lapply(FUN = clean_child_health_types) |>
    dplyr::bind_rows(.id = "id")
  
  child_bf <- df$bf2
  names(child_bf) <- df$id
  
  child_bf <- child_bf |>
    lapply(FUN = clean_child_bf_types) |>
    dplyr::bind_rows(.id = "id")
  
  child_anthro <- df$child_anthro_repeat
  names(child_anthro) <- df$id
  
  child_anthro <- child_anthro |>
    lapply(FUN = clean_child_anthro_types) |>
    dplyr::bind_rows(.id = "id")
  
  #child_data <- dplyr::bind_cols(child_roster, child_health, child_anthro) |>
  #  subset(select = c(-`id...17`, -`id...109`))
  
  #names(child_data)[1] <- "id"

  #child_data$id <- as.integer(child_data$id)
   
  child_data <- merge(child_roster, child_health, by = c("id", "child_id")) |>
    merge(child_bf, by = c("id", "child_id"), all.x = TRUE) |>
    merge(child_anthro, by = c("id", "child_id"), all.x = TRUE)
   
  df <- merge(df, child_data, by = "id", all.y = TRUE)
  
  names(df) <- tolower(names(df))
  
  df$weight_adj <- ifelse(!is.na(df$cpeso1), df$cpeso1, df$cpeso)
  df$height_adj <- ifelse(!is.na(df$caltura1), df$caltura1, df$caltura)
  df$muac_adj <- ifelse(!is.na(df$cbraco1), df$cbraco1, df$cbraco)
  
  df
}


################################################################################
#
#'
#' Process child roster data
#' 
#' @param df Child roster data.frame
#'
#
################################################################################

clean_child_roster_types <- function(df) {
  ## Check if df is NULL
  if (is.null(df)) {
    df <- data.frame(
      CHILD_NAME = NA_character_, 
      CHILD_RELATIONSHIP = NA_integer_, 
      CHILD_REL_NUMERIC = NA_integer_, 
      CHILD_BIRTHDATE_KNOWN = NA_integer_, 
      CHILD_BIRTHDATE = NA_character_, 
      CHILD_AGE = NA_integer_, 
      CHILD_AGE_CALCULATED = NA_integer_, 
      CHILD_BIO = NA_integer_, 
      RESP_AGE_BIRTH = NA_integer_, 
      CHILD_SEX = NA_integer_, 
      CHILD_BIRTHPLACE = NA_integer_, 
      CHILD_BIRTHWEIGHT = NA_integer_, 
      CHILD_BIRTHORDER = NA_integer_, 
      CHILD_BIRTHCOMP = NA_integer_, 
      CHILD_BIRTHCS = NA_integer_
    )
  } else {
    clean_names <- names(df) |> 
      (\(x) gsub(pattern = "^(.*?)/", replacement = "", x = x))()
  
    names(df) <- clean_names
  
    template_df <- data.frame(
      CHILD_NAME = NA_character_, 
      CHILD_RELATIONSHIP = NA_integer_, 
      CHILD_REL_NUMERIC = NA_integer_, 
      CHILD_BIRTHDATE_KNOWN = NA_integer_, 
      CHILD_BIRTHDATE = NA_character_, 
      CHILD_AGE = NA_integer_, 
      CHILD_AGE_CALCULATED = NA_integer_, 
      CHILD_BIO = NA_integer_, 
      RESP_AGE_BIRTH = NA_integer_, 
      CHILD_SEX = NA_integer_, 
      CHILD_BIRTHPLACE = NA_integer_, 
      CHILD_BIRTHWEIGHT = NA_integer_, 
      CHILD_BIRTHORDER = NA_integer_, 
      CHILD_BIRTHCOMP = NA_integer_, 
      CHILD_BIRTHCS = NA_integer_
    )
  
    ## Convert simple codes to integers
    integer_vars <- names(template_df)[sapply(X = template_df, FUN = class) == "integer"] |>
      (\(x) names(df)[names(df) %in% x])()
  
    df[ , integer_vars] <- df[ , integer_vars] |>
      apply(MARGIN = 2, FUN = function(x) as.integer(x))
  
    ## Conver to numeric  
    numeric_vars <- names(template_df)[sapply(X = template_df, FUN = class) == "numeric"] |>
      (\(x) names(df)[names(df) %in% x])()
  
    df[ , numeric_vars] <- df[ , numeric_vars] |>
      apply(MARGIN = 2, FUN = function(x) as.numeric(x))
  
    ## Concatenate
    df <- dplyr::bind_rows(template_df, df) |>
      (\(x) x[2:nrow(x), ])()  

    df <- data.frame(
      child_id = 1:nrow(df),
      df
    )
  }

  df$CHILD_BIRTHDATE <- as.Date(df$CHILD_BIRTHDATE)
    
  ## Return
  df
}


################################################################################
#
#'
#' Process child health data
#' 
#' @param df Child health data.frame
#'
#
################################################################################

clean_child_health_types <- function(df) {
  ## Check if df is NULL
  if (is.null(df)) {
    df <- data.frame(
      CURRENT_CHILD_NAME = NA_character_, 
      CURRENT_CHILD_AGE = NA_integer_, 
      IMM1 = NA_integer_, 
      IMM2 = NA_integer_, 
      IMM3a = NA_integer_, 
      IMM3b = NA_integer_,
      IMM3c = NA_integer_, 
      IMM3d = NA_integer_, 
      IMM3e = NA_integer_, 
      IMM4a = NA_integer_, 
      IMM4b = NA_integer_, 
      IMM4c = NA_integer_, 
      IMM5 = NA_integer_, 
      IMM5a = NA_integer_,
      IMM6a = NA_integer_, 
      IMM6b = NA_integer_, 
      IMM6c = NA_integer_, 
      IMM7a = NA_integer_, 
      IMM7b = NA_integer_, 
      VAS1 = NA_integer_, 
      VAS2 = NA_integer_, 
      VAS3 = NA_integer_,
      NUT1a = NA_integer_,
      NUT1b = NA_integer_,
      NUT1c = NA_integer_,
      NUT1d = NA_integer_,
      NUT1e = NA_integer_,
      NUT1f = NA_integer_,
      NUT1g = NA_integer_,
      NUT1h = NA_integer_,
      NUT1i = NA_integer_,
      NUT1j = NA_integer_,
      NUT1k = NA_integer_,
      NUT1l = NA_integer_,
      NUT1m = NA_integer_,
      NUT1n = NA_integer_,
      NUT2 = NA_integer_,
      PICA1 = NA_integer_, 
      PICA2 = NA_integer_,
      PICA3 = NA_integer_, 
      LUSD9 = NA_integer_,
      LUSD10 = NA_integer_, 
      LUSD11 = NA_integer_, 
      FEVER1 = NA_integer_, 
      FEVER2 = NA_integer_, 
      FEVER3 = NA_integer_, 
      FEVER4 = NA_integer_, 
      FEVER5 = NA_integer_,
      FEVER6 = NA_integer_, 
      FEVER6a = NA_integer_, 
      FEVER7 = NA_integer_, 
      ORT1 = NA_integer_, 
      ORT1a = NA_integer_, 
      ORT1b = NA_integer_, 
      ORT1c = NA_integer_, 
      ORT2 = NA_integer_,
      ORT3 = NA_integer_, 
      ORT4 = NA_integer_, 
      ORT5a = NA_integer_, 
      ORT5b = NA_integer_, 
      ORT5c = NA_integer_, 
      ORT5d = NA_integer_, 
      ORT5e = NA_integer_, 
      ORT5e_specify = NA_character_,
      ORT6 = NA_integer_, 
      ORT7 = NA_integer_, 
      RI1 = NA_integer_, 
      RI2 = NA_integer_, 
      RI3 = NA_integer_, 
      CH1 = NA_integer_, 
      CH1a = NA_integer_, 
      CH2 = NA_integer_, 
      CH3 = NA_integer_, 
      CH4 = NA_integer_, 
      CH5 = NA_integer_, 
      CH5a = NA_integer_, 
      CH5a_other = NA_character_, 
      PLAY1a = NA_integer_, 
      PLAY1b = NA_integer_, 
      PLAY1c = NA_integer_, 
      PLAY1d = NA_integer_,
      PLAY1e = NA_integer_, 
      PLAY1f = NA_integer_, 
      PLAY1g = NA_integer_, 
      PLAY2 = NA_integer_, 
      PLAY3a = NA_integer_, 
      PLAY3b = NA_integer_, 
      PLAY3c = NA_integer_,
      PLAY3d = NA_integer_, 
      PLAY3e = NA_integer_, 
      PLAY3f = NA_integer_
    )
  } else {
    clean_names <- names(df) |> 
      (\(x) gsub(pattern = "^(.*?)/", replacement = "", x = x))() |>
      (\(x) gsub(pattern = "^(.*?)/", replacement = "", x = x))()
  
    names(df) <- clean_names
  
    template_df <- data.frame(
      CURRENT_CHILD_NAME = NA_character_, 
      CURRENT_CHILD_AGE = NA_integer_, 
      IMM1 = NA_integer_, 
      IMM2 = NA_integer_, 
      IMM3a = NA_integer_, 
      IMM3b = NA_integer_,
      IMM3c = NA_integer_, 
      IMM3d = NA_integer_, 
      IMM3e = NA_integer_, 
      IMM4a = NA_integer_, 
      IMM4b = NA_integer_, 
      IMM4c = NA_integer_, 
      IMM5 = NA_integer_, 
      IMM5a = NA_integer_,
      IMM6a = NA_integer_, 
      IMM6b = NA_integer_, 
      IMM6c = NA_integer_, 
      IMM7a = NA_integer_, 
      IMM7b = NA_integer_, 
      VAS1 = NA_integer_, 
      VAS2 = NA_integer_, 
      VAS3 = NA_integer_,
      NUT1a = NA_integer_,
      NUT1b = NA_integer_,
      NUT1c = NA_integer_,
      NUT1d = NA_integer_,
      NUT1e = NA_integer_,
      NUT1f = NA_integer_,
      NUT1g = NA_integer_,
      NUT1h = NA_integer_,
      NUT1i = NA_integer_,
      NUT1j = NA_integer_,
      NUT1k = NA_integer_,
      NUT1l = NA_integer_,
      NUT1m = NA_integer_,
      NUT1n = NA_integer_,
      NUT2 = NA_integer_,
      PICA1 = NA_integer_, 
      PICA2 = NA_integer_,
      PICA3 = NA_integer_, 
      LUSD9 = NA_integer_,
      LUSD10 = NA_integer_, 
      LUSD11 = NA_integer_, 
      FEVER1 = NA_integer_, 
      FEVER2 = NA_integer_, 
      FEVER3 = NA_integer_, 
      FEVER4 = NA_integer_, 
      FEVER5 = NA_integer_,
      FEVER6 = NA_integer_, 
      FEVER6a = NA_integer_, 
      FEVER7 = NA_integer_, 
      ORT1 = NA_integer_, 
      ORT1a = NA_integer_, 
      ORT1b = NA_integer_, 
      ORT1c = NA_integer_, 
      ORT2 = NA_integer_,
      ORT3 = NA_integer_, 
      ORT4 = NA_integer_, 
      ORT5a = NA_integer_, 
      ORT5b = NA_integer_, 
      ORT5c = NA_integer_, 
      ORT5d = NA_integer_, 
      ORT5e = NA_integer_, 
      ORT5e_specify = NA_character_,
      ORT6 = NA_integer_, 
      ORT7 = NA_integer_, 
      RI1 = NA_integer_, 
      RI2 = NA_integer_, 
      RI3 = NA_integer_, 
      CH1 = NA_integer_, 
      CH1a = NA_integer_, 
      CH2 = NA_integer_, 
      CH3 = NA_integer_, 
      CH4 = NA_integer_, 
      CH5 = NA_integer_, 
      CH5a = NA_integer_, 
      CH5a_other = NA_character_, 
      PLAY1a = NA_integer_, 
      PLAY1b = NA_integer_, 
      PLAY1c = NA_integer_, 
      PLAY1d = NA_integer_,
      PLAY1e = NA_integer_, 
      PLAY1f = NA_integer_, 
      PLAY1g = NA_integer_, 
      PLAY2 = NA_integer_, 
      PLAY3a = NA_integer_, 
      PLAY3b = NA_integer_, 
      PLAY3c = NA_integer_,
      PLAY3d = NA_integer_, 
      PLAY3e = NA_integer_, 
      PLAY3f = NA_integer_
    )
  
    ## Convert simple codes to integers
    integer_vars <- names(template_df)[sapply(X = template_df, FUN = class) == "integer"] |>
      (\(x) names(df)[names(df) %in% x])()
  
    df[ , integer_vars] <- df[ , integer_vars] |>
      apply(MARGIN = 2, FUN = function(x) as.integer(x))
  
    ## Conver to numeric  
    numeric_vars <- names(template_df)[sapply(X = template_df, FUN = class) == "numeric"] |>
      (\(x) names(df)[names(df) %in% x])()
  
    df[ , numeric_vars] <- df[ , numeric_vars] |>
      apply(MARGIN = 2, FUN = function(x) as.numeric(x))
  
    ## Concatenate
    df <- dplyr::bind_rows(template_df, df) |>
      (\(x) x[2:nrow(x), ])()  
  
    df <- data.frame(
      child_id = 1:nrow(df),
      df
    )
  }
  
  ## Return
  df
}


################################################################################
#
#'
#' Process child breastfeeding data
#' 
#' @param df Child breastfeeding data.frame
#'
#
################################################################################

clean_child_bf_types <- function(df) {
  ## Check if df is NULL
  if (is.null(df)) {
    df <- data.frame(
      CURRENT_EBF_NAME = NA_character_,
      CURRENT_EBF_AGE = NA_integer_,
      EB7 = NA_integer_,
      EB9 = NA_integer_,
      EB8a = NA_integer_,
      EB8b = NA_integer_,
      EB8c = NA_integer_,
      EB8d = NA_integer_
    )
  } else {
    clean_names <- names(df) |> 
      (\(x) gsub(pattern = "^(.*?)/", replacement = "", x = x))() |>
      (\(x) gsub(pattern = "^(.*?)/", replacement = "", x = x))()
  
    names(df) <- clean_names
  
    df[ , names(df) != "CURRENT_EBF_NAME"] <- df[ , names(df) != "CURRENT_EBF_NAME"] |>
      apply(MARGIN = 2, FUN = function(x) as.integer(x))  
  }
  
  df <- data.frame(
    child_id = 1:nrow(df),
    df
  )
  
  ## Return
  df
}


################################################################################
#
#'
#' Process child anthro data
#' 
#' @param df Child anthro data.frame
#'
#
################################################################################

clean_child_anthro_types <- function(df) {
  ## Check if df is NULL
  if (is.null(df)) {
    df <- data.frame(
      CURRENT_ANTHRO_NAME = NA_character_, 
      CURRENT_ANTHRO_AGE = NA_integer_, 
      CURRENT_ANTHRO_SEX = NA_integer_,
      CALTURA = NA_real_, 
      POSITION = NA_integer_, 
      CPESO = NA_real_, 
      CBRACO = NA_real_, 
      CMALNUT = NA_integer_,
      height = NA_real_, 
      weight = NA_real_, 
      muac = NA_real_, 
      oedema = NA_integer_, 
      age = NA_integer_, 
      sex = NA_integer_,
      whz_neg3_boy = NA_real_, 
      whz_neg3_girl = NA_real_, 
      flag_whz_neg3 = NA_integer_,
      haz_lower_boy = NA_real_, 
      haz_upper_boy = NA_real_, 
      flag_haz_boy = NA_integer_,
      haz_lower_girl = NA_real_, 
      haz_upper_girl = NA_real_, 
      flag_haz_girl = NA_integer_,
      whz_lower_boy = NA_real_, 
      whz_upper_boy = NA_real_, 
      flag_whz_boy = NA_integer_,
      whz_lower_girl = NA_real_, 
      whz_upper_girl = NA_real_, 
      flag_whz_girl = NA_integer_,
      waz_lower_boy = NA_real_, 
      waz_upper_boy = NA_real_, 
      flag_waz_boy = NA_integer_,
      waz_lower_girl = NA_real_, 
      waz_upper_girl = NA_real_, 
      flag_waz_girl = NA_integer_,
      flag = NA_integer_, 
      sam = NA_integer_,
      CALTURA1 = NA_real_, 
      POSITION1 = NA_integer_, 
      CPESO1 = NA_real_, 
      CBRACO1 = NA_real_, 
      CMALNUT1 = NA_integer_,
      height1 = NA_real_, 
      weight1 = NA_real_, 
      muac1 = NA_real_, 
      oedema1 = NA_integer_, 
      whz_neg3_boy_1 = NA_real_, 
      whz_neg3_girl_1 = NA_real_, 
      flag_whz_neg3_1 = NA_integer_,
      sam1 = NA_integer_,
      rand = NA_real_,
      CALTURA2 = NA_real_, 
      POSITION2 = NA_integer_, 
      CPESO2 = NA_real_, 
      CBRACO2 = NA_real_, 
      CMALNUT2 = NA_integer_,
      height2 = NA_real_, 
      weight2 = NA_real_, 
      muac2 = NA_real_, 
      oedema2 = NA_integer_, 
      whz_neg3_boy_2 = NA_real_, 
      whz_neg3_girl_2 = NA_real_, 
      flag_whz_neg3_2 = NA_integer_,
      sam2 = NA_integer_
    )
  } else {
    clean_names <- names(df) |> 
      (\(x) gsub(pattern = "^(.*?)/", replacement = "", x = x))() |>
      (\(x) gsub(pattern = "^(.*?)/", replacement = "", x = x))() |>
      (\(x) gsub(pattern = "^(.*?)/", replacement = "", x = x))()
  
    names(df) <- clean_names
  
    template_df <- data.frame(
      CURRENT_ANTHRO_NAME = NA_character_, 
      CURRENT_ANTHRO_AGE = NA_integer_, 
      CURRENT_ANTHRO_SEX = NA_integer_,
      CALTURA = NA_real_, 
      POSITION = NA_integer_, 
      CPESO = NA_real_, 
      CBRACO = NA_real_, 
      CMALNUT = NA_integer_,
      height = NA_real_, 
      weight = NA_real_, 
      muac = NA_real_, 
      oedema = NA_integer_, 
      age = NA_integer_, 
      sex = NA_integer_,
      whz_neg3_boy = NA_real_, 
      whz_neg3_girl = NA_real_, 
      flag_whz_neg3 = NA_integer_,
      haz_lower_boy = NA_real_, 
      haz_upper_boy = NA_real_, 
      flag_haz_boy = NA_integer_,
      haz_lower_girl = NA_real_, 
      haz_upper_girl = NA_real_, 
      flag_haz_girl = NA_integer_,
      whz_lower_boy = NA_real_, 
      whz_upper_boy = NA_real_, 
      flag_whz_boy = NA_integer_,
      whz_lower_girl = NA_real_, 
      whz_upper_girl = NA_real_, 
      flag_whz_girl = NA_integer_,
      waz_lower_boy = NA_real_, 
      waz_upper_boy = NA_real_, 
      flag_waz_boy = NA_integer_,
      waz_lower_girl = NA_real_, 
      waz_upper_girl = NA_real_, 
      flag_waz_girl = NA_integer_,
      flag = NA_integer_, 
      sam = NA_integer_,
      CALTURA1 = NA_real_, 
      POSITION1 = NA_integer_, 
      CPESO1 = NA_real_, 
      CBRACO1 = NA_real_, 
      CMALNUT1 = NA_integer_,
      height1 = NA_real_, 
      weight1 = NA_real_, 
      muac1 = NA_real_, 
      oedema1 = NA_integer_, 
      whz_neg3_boy_1 = NA_real_, 
      whz_neg3_girl_1 = NA_real_, 
      flag_whz_neg3_1 = NA_integer_,
      sam1 = NA_integer_,
      rand = NA_real_,
      CALTURA2 = NA_real_, 
      POSITION2 = NA_integer_, 
      CPESO2 = NA_real_, 
      CBRACO2 = NA_real_, 
      CMALNUT2 = NA_integer_,
      height2 = NA_real_, 
      weight2 = NA_real_, 
      muac2 = NA_real_, 
      oedema2 = NA_integer_, 
      whz_neg3_boy_2 = NA_real_, 
      whz_neg3_girl_2 = NA_real_, 
      flag_whz_neg3_2 = NA_integer_,
      sam2 = NA_integer_
    )
  
    ## Convert simple codes to integers
    integer_vars <- names(template_df)[sapply(X = template_df, FUN = class) == "integer"] |>
      (\(x) names(df)[names(df) %in% x])()
  
    df[ , integer_vars] <- df[ , integer_vars] |>
      apply(MARGIN = 2, FUN = function(x) as.integer(x))

    ## Conver to numeric  
    numeric_vars <- names(template_df)[sapply(X = template_df, FUN = class) == "numeric"] |>
      (\(x) names(df)[names(df) %in% x])()

    df[ , numeric_vars] <- df[ , numeric_vars] |>
      apply(MARGIN = 2, FUN = function(x) as.numeric(x))
  
    ## Concatenate
    df <- dplyr::bind_rows(template_df, df) |>
      (\(x) x[2:nrow(x), ])()
  
    df <- data.frame(
      child_id = 1:nrow(df),
      df
    )
  }
  
  ## Return
  df
}


process_endline_data <- function(.data, survey_endline_choices) {
  ## Add filter to remove testing data -----------------------------------------
  dplyr::mutate(
    .data = .data,
    ### Demographics - respondent ----------------------------------------------
    respondent_sex = refactor_var_categorical(
      x = resp_sex,
      y = "sex",
      choices = survey_endline_choices
    ),
    respondent_age_years = q01,
    respondent_age_group = refactor_age_group(
      x = respondent_age_years,
      breaks = c(seq(from = 15, to = 50, by = 5), Inf),
      age_group_labels = c(
        "15 to 19 years", "20 to 24 years", "25 to 29 years", 
        "30 to 34 years", "35 to 39 years", "40 to 44 years", 
        "45 to 49 years", "50 years or more")
    ),
    respondent_language = refactor_var_categorical(
      x = idiomaq,
      y = "language",
      choices = survey_endline_choices
    ),
    respondent_civil_status = refactor_var_categorical(
      x = resp_marital_status, 
      y = "marital_status",
      choices = survey_endline_choices
    ),
    respondent_education_years = ifelse(resp_edu %in% c(88, 99), NA, resp_edu),
    respondent_education_group = refactor_age_group(
      x = respondent_education_years,
      breaks = c(0, 6, 12, Inf),
      age_group_labels = c("0 to 5 years", "6 to 11 years", "12 or more years")
    ),
    respondent_occupation = refactor_var_categorical(
      x = igs1,
      y = "occupation1",
      choices = survey_endline_choices
    ),
    ### Demographics - children ------------------------------------------------
    respondent_child_relationship = refactor_var_categorical(
      x = child_relationship,
      y = "relationship",
      choices = survey_endline_choices
    ),
    child_sex_integer = as.integer(child_sex),
    child_sex = refactor_var_categorical(
      x = child_sex,
      y = "sex",
      choices = survey_endline_choices
    ),
    child_age_days = calculate_age_child(
      date_of_birth = child_birthdate,
      survey_date = today,
      reported_age_months = child_age,
      age_units = "days"
    ),
    child_age_months = calculate_age_child(
      date_of_birth = child_birthdate,
      survey_date = today,
      reported_age_months = child_age,
      age_units = "months"
    ),
    child_age_years = calculate_age_child(
      date_of_birth = child_birthdate,
      survey_date = today,
      reported_age_months = child_age,
      age_units = "years"
    ),
    child_age_group = refactor_age_group(
      x = child_age_months,
      breaks = c(0, 6, 12, 24, 36, 48, 60),
      age_group_labels = c(
        "0 to 5 months", "6-11 months", "12 to 23 months",
        "24 to 35 months", "36 to 47 months", "48 to 59 months"
      )
    ),
    child_currently_breastfeeding = ifelse(eb1 == 2, 1, 0),
    child_parent_age_at_birth = refactor_age_group(
      x = resp_age_birth,
      breaks = c(0, 15, 20, 25, 30, 35, 40, 45, 50, Inf),
      age_group_labels = c(
        "less than 15 years", "15 to 19 years", "20 to 24 years",
        "25 to 29 years", "30 to 34 years", "35 to 39 years",
        "40 to 44 years", "45 to 49 years", "50 years or more"
      )
    ),
    child_location_of_birth = refactor_var_categorical(
      x = child_birthplace,
      y = "birth_location",
      choices = survey_endline_choices
    ),
    child_caesarean_birth = ifelse(child_birthcs == 2, 0, 1),
    child_complications_at_birth = ifelse(child_birthcomp == 2, 0, 1),
    child_low_birth_weight = ifelse(child_birthweight == 2, 0, 1),
    ### Demographics - spouse --------------------------------------------------
    spouse_age_years = q02a,
    spouse_age_group = refactor_age_group(
      x = spouse_age_years,
      breaks = c(seq(from = 15, to = 70, by = 5), Inf),
      age_group_labels = c(
        "15 to 19 years", "20 to 24 years", "25 to 29 years",
        "30 to 34 years", "35 to 39 years", "40 to 44 years",
        "45 to 49 years", "50 to 54 years", "55 to 59 years", 
        "60 to 64 years", "65 to 69 years", "70 years or more"
      )
    ),
    spouse_education_years = ifelse(q02d %in% c(88, 99), NA, q02d),
    spouse_education_group = refactor_age_group(
      x = spouse_education_years,
      breaks = c(0, 6, 12, Inf),
      age_group_labels = c("0 to 5 years", "6 to 11 years", "12 or more years")
    ),
    spouse_occupation = refactor_var_categorical(
      x = igs2,
      y = "occupation2",
      choices = survey_endline_choices
    ),
    spouse_lives_in_home = refactor_var_categorical(
      x = q02,
      y = "yes_no_other",
      choices = survey_endline_choices
    ),
    ### Household income -------------------------------------------------------
    persons_living_in_household = famsize,
    children_under_five_living_in_household = famsize1,
    pregnant_women_living_in_household = famsize2,
    monthly_household_income = refactor_var_categorical(
      x = q08, y = "income4", choices = survey_endline_choices
    ),
    source_of_household_income = refactor_var_categorical(
      x = ig1, y = "income1", choices = survey_endline_choices
    ),
    sufficiency_of_household_income = NA,
    sufficiency_of_family_resource = NA,
    household_income_against_expenses = NA,
    ### Household structure ----------------------------------------------------
    home_ownership_own = ifelse(sdh6 == 2, 0, 1),
    home_ownership_rent = ifelse(sdh7 == 2, 0, 1),
    home_ownership_loan = ifelse(sdh8 == 2, 0, 1),
    #number_of_rooms_in_home = haven::as_factor(sdh3),
    number_of_rooms_in_home = ifelse(sdh3 %in% c(88, 99), NA, sdh3) |>
      factor(),
    number_of_bedrooms_in_home = ifelse(sdh4 %in% c(88, 99), NA, sdh4) |>
      factor(),
    roofing_material = refactor_var_categorical(
      x = sdh1, y = "roof", choices = survey_endline_choices
    ),
    floor_material = refactor_var_categorical(
      x = sdh2, y = "floor", choices = survey_endline_choices
    ),
    time_living_in_location_in_months = ifelse(q03 >=88, NA, q03),
    time_living_in_location_group = refactor_age_group(
      x = time_living_in_location_in_months,
      breaks = c(0, 1, 6, 12, 24, 36, 48, 60, 72, 84, 96, 108, 121, Inf),
      age_group_labels = c(
        "less than 1 month", "1 to 5 months", "6 to 11 months", 
        "12 to 23 months", "24 to 35 months", "36 to 47 months", 
        "48 to 59 months", "60 to 71 months", "72 to 83 monhs", 
        "84 to 95 months", "96 to 107 months", "108 to 120 months",
        "more than 10 years"
      )
    ),
    ### Household amenities ----------------------------------------------------
    electricity = recode_yes_no(as.integer(cdcg1)),
    cellphone = recode_yes_no(as.integer(cdcg4)),
    computer = recode_yes_no(as.integer(cdcg7)),
    landline = NA,
    radio = recode_yes_no(as.integer(cdcg2)),
    television = recode_yes_no(as.integer(cdcg3)),
    housekeeper_childcare_employee = recode_yes_no(as.integer(cdcg14)),
    refrigerator = recode_yes_no(as.integer(cdcg11)),
    refrigerator_alternative = recode_yes_no(as.integer(cdcg11a)),
    number_of_mosquito_nets = ifelse(cdcg13 %in% c(88, 99), NA, cdcg13) |>
      factor(),
    fuel_used_for_cooking = refactor_var_categorical(
      x = cfegs1, y = "fuel1", choices = survey_endline_choices
    ),
    location_of_food_preparation = refactor_var_categorical(
      x = cfegs3, y = "cook_location", choices = survey_endline_choices
    ),
    fuel_used_for_lighting = refactor_var_categorical(
      x = cfegs5, y = "fuel3", choices = survey_endline_choices
    ),
    ### Mode of daily travel ---------------------------------------------------
    usual_mode_of_travel = refactor_var_categorical(
      x = gi1, y = "travel1", choices = survey_endline_choices
    ),
    time_to_travel_to_health_centre = ifelse(gi2t %in% c(888, 998), NA, gi2t),
    mode_of_travel_to_health_centre = refactor_var_categorical(
      x = gi2m, y = "travel2", choices = survey_endline_choices
    ),
    time_to_travel_to_local_markets = ifelse(gi3t %in% c(888, 998), NA, gi3t),
    mode_of_travel_to_local_markets = refactor_var_categorical(
      x = gi3m, y = "travel2", choices = survey_endline_choices
    ),
    time_to_travel_to_primary_school = NA,
    mode_of_travel_to_primary_school = NA,
    time_to_travel_to_secondary_school = NA,
    mode_of_travel_to_secondary_school = NA,
    ### Household decision making ----------------------------------------------
    marrying_age = refactor_var_categorical(
      x = ge1, y = "decisions", choices = survey_endline_choices
    ),
    using_condoms = refactor_var_categorical(
      x = ge2, y = "decisions", choices = survey_endline_choices
    ),
    household_responsibilities = refactor_var_categorical(
      x = ge3, y = "decisions", choices = survey_endline_choices
    ),
    family_planning = refactor_var_categorical(
      x = ge4, y = "decisions", choices = survey_endline_choices
    ),
    agricultural_tasks = refactor_var_categorical(
      x = ge5, y = "decisions", choices = survey_endline_choices
    ),
    household_finances = refactor_var_categorical(
      x = ge6, y = "decisions", choices = survey_endline_choices
    ),
    child_rearing = refactor_var_categorical(
      x = ge7, y = "decisions", choices = survey_endline_choices
    ),
    child_discipline = refactor_var_categorical(
      x = ge8,y = "decisions", choices = survey_endline_choices
    ),
    healthcare_in_pregnancy = refactor_var_categorical(
      x = ge9, y = "decisions", choices = survey_endline_choices
    ),
    healthcare_for_child = refactor_var_categorical(
      x = ge10, y = "decisions", choices = survey_endline_choices
    ),
    ### Community groups participation -----------------------------------------
    group_membership = recode_yes_no(q05),
    #group_membership_type = recode_group_type(q05, q05_spec),
    presentation_participation = recode_yes_no(q06),
    #presentation_topic = recode_presentation_type(q06, q06_spec),
    #presentation_facilitator = recode_var_categorical(q06a) |>
    #  (\(x) ifelse(x == "ONG (especifique)", q06a_spec, x))(),
    information_application = recode_yes_no(q06b),
    health_tasks_participation = recode_yes_no(q07),
    #health_tasks_participation_type = recode_var_categorical(q07) |>
    #  (\(x) ifelse(x == "Sim. Especifique", q07_spec, x))(),
    ### Child anthropometry ----------------------------------------------------
    ### Water ------------------------------------------------------------------
    surface_water_source = ifelse(wt2 == 11, 1, 0),
    unimproved_water_source = ifelse(wt2 %in% c(7, 9), 1, 0),
    limited_water_source = ifelse(
      wt2 %in% c(1:6, 8, 10, 12) & wt3a > 30, 1, 0
    ),
    basic_water_source = ifelse(
      wt2 %in% c(1:6, 8, 10, 12) & wt3a <= 30, 1, 0
    ),
    sufficient_water_source = ifelse(
      wt2 %in% 1:2 & wt4 == 1, 1, 0
    ),
    ### Sanitation -------------------------------------------------------------
    open_defecation = ifelse(lusd1 == 2 | lusd4 == 6, 1, 0),
    unimproved_toilet_facility = ifelse(lusd4 == 5, 1, 0),
    limited_toilet_facility = ifelse(lusd2 == 1 & lusd4 != 5, 1, 0),
    basic_toilet_facility = ifelse(lusd2 == 2 & lusd4 != 5, 1, 0),
    ### Hygiene ----------------------------------------------------------------
    no_handwashing_facility = ifelse(
      mao1 == 2, 1, 
      ifelse(
        mao1 %in% 3:4, NA, 0
      )
    ),
    limited_handwashing_facility = ifelse(
      mao1 == 1 & (mao1a == 2 | mao1b == 3), 1, 0
    ),
    basic_handwashing_facility = ifelse(
      mao1 == 1 & mao1a == 1 & mao1b != 3, 1, 0
    ),
    ### Diarrhoea --------------------------------------------------------------
    diarrhoea_episode = recode_yes_no(ort1),
    diarrhoea_seek_treatment = recode_yes_no(ort3),
    diarrhoea_point_of_care = refactor_var_categorical(
      x = ort4, y = "point_of_care", choices = survey_endline_choices
    ),
    diarrhoea_treatment_with_ors = ifelse(
      ort5a == 1 | ort5b == 1 | ort5c == 1, 1, 0
    ),
    ### Fever ------------------------------------------------------------------
    fever_episode = recode_yes_no(fever1),
    fever_seek_treatment = recode_yes_no(fever2),
    fever_point_of_care = refactor_var_categorical(
      x = fever3, y = "point_of_care", choices = survey_endline_choices
    ),
    fever_malaria_test = ifelse(fever4 == 1 | fever5 == 1, 1, 0),
    fever_malaria_episode = recode_yes_no(fever6),
    fever_treatment = ifelse(fever6a %in% c(88, 99), NA, fever6a),
    fever_malaria_treatment_intake = recode_yes_no(fever7),
    ### RTI --------------------------------------------------------------------
    rti_episode = ifelse(ch1 == 1 & (ch1a == 1 | ch2 == 1), 1, 0),
    rti_seek_treatment = recode_yes_no(ch3),
    rti_point_of_care = refactor_var_categorical(
      x = ch4, y = "point_of_care", choices = survey_endline_choices
    ),
    rti_treatment = ifelse(ch5a %in% c(88, 99), NA, ch5a),
    ### Mental health ----------------------------------------------------------
    ment1 = ifelse(ment1 %in% c(88, 99), NA, ment1),
    ment2 = ifelse(ment2 %in% c(88, 99), NA, ment2),
    ment3 = ifelse(ment3 %in% c(88, 99), NA, ment3),
    ment4 = ifelse(ment4 %in% c(88, 99), NA, ment4),
    ment5 = ifelse(ment5 %in% c(88, 99), NA, ment5),
    ment6 = ifelse(ment6 %in% c(88, 99), NA, ment6),
    ment7 = ifelse(ment7 %in% c(88, 99), NA, ment7),
    ment8 = ifelse(ment8 %in% c(88, 99), NA, ment8),
    #ment9 = ifelse(ment9 %in% c(88, 99), NA, ment9),
    phq8_score = ment1 + ment2 + ment3 + ment4 + ment5 + ment6 + ment7 + ment8,
    major_depression = ifelse(phq8_score > 10 & phq8_score <= 20, 1, 0),
    severe_depression = ifelse(phq8_score > 20, 1, 0),
    at_least_major_depression = ifelse(phq8_score > 10, 1, 0),
    alcohol_consumption = refactor_var_categorical(
      x = ment9, y = "alcohol_frequency", choices = survey_endline_choices
    ),
    .keep = "unused"
  ) |>
    (\(x)
      {
        data.frame(
          x,
          fever_recode_malaria(vars = "fever_treatment", .data = x) |>
            (\(x) 
             { 
               names(x) <- paste0(
                 "fever_malaria_", 
                 c(
                   "coartem", "amodiaquina_artesunato", "fansidar", "quinino", 
                   "quinino_injection", "artesunato", "paracetamol_xarope"
                 )
               )
               x 
            }
            )(),
          rti_recode_treatment(vars = "rti_treatment", .data = x) |>
            (\(x) 
             { 
               names(x) <- paste0(
                 "rti_treatment_", 
                 c(
                   "antibioticos", "paracetamol", "aspirina", 
                   "ibuprofeno", "other"
                 )
               )
               x 
            }
            )()
        )
      }
    )()
}