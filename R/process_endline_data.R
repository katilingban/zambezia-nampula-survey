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
      PREG1 = NA_character_,
      PREG2 = NA_integer_,
      PREG3 = NA_character_,
      PMTCT1 = NA_integer_,
      PMTCT2 = NA_integer_,
      PMTCT3 = NA_integer_,
      IDK1 = NA_integer_,
      IDK2 = NA_integer_,
      SPC1 = NA_integer_,
      SPC2 = NA_integer_,
      SPC2a = NA_integer_,
      SPC2b = NA_integer_,
      SPC3 = NA_character_,
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
      BS2 = NA_character_,
      BS2a = NA_character_,
      BS3 = NA_character_,
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
      AGUA2 = NA_character_,
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
        FEVER6a = NA_character_, 
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
        CH5a = NA_character_, 
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
        FEVER6a = NA_character_, 
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
        CH5a = NA_character_, 
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
  
  
  clean_endline_ea_ids <- function(.data, survey_sampling_list) {
    x <- .data
    x[x$id == 95385641 & x$child_id == 1, "fgh_id"] <- 42109
    x[x$id == 95385641 & x$child_id == 2, "fgh_id"] <- 42109
    x[x$id == 95386444 & x$child_id == 1, "fgh_id"] <- 42109
    x[x$id == 95385646 & x$child_id == 1, "fgh_id"] <- 42109
    
    x[x$id == 95288090 & x$child_id == 1, "fgh_id"] <- 42117
    x[x$id == 95288090 & x$child_id == 2, "fgh_id"] <- 42117
    x[x$id == 95238955 & x$child_id == 1, "fgh_id"] <- 42117
    x[x$id == 95288325 & x$child_id == 1, "fgh_id"] <- 42117
    x[x$id == 95238958 & x$child_id == 1, "fgh_id"] <- 42117
    x[x$id == 95288092 & x$child_id == 1, "fgh_id"] <- 42117
    x[x$id == 95288092 & x$child_id == 2, "fgh_id"] <- 42117
    
    x[x$id == 95800686 & x$child_id == 1, "fgh_id"] <- 42101
    x[x$id == 95800683 & x$child_id == 1, "fgh_id"] <- 42101
    
    x[x$id == 96560307 & x$child_id == 1, "fgh_id"] <- 40807
    x[x$id == 96560331 & x$child_id == 1, "fgh_id"] <- 40807
    
    x[x$id == 96625523 & x$child_id == 1, "fgh_id"] <- 40825
    x[x$id == 96625510 & x$child_id == 1, "fgh_id"] <- 40825
    
    x[x$id == 95247921 & x$child_id == 1, "fgh_id"] <- 40809
    x[x$id == 95247921 & x$child_id == 2, "fgh_id"] <- 40809
    x[x$id == 95247911 & x$child_id == 1, "fgh_id"] <- 40809
    x[x$id == 95191746 & x$child_id == 1, "fgh_id"] <- 40809
    
    x[x$id == 95782295 & x$child_id == 1, "fgh_id"] <- 40828
    
    x[x$id == 95348870 & x$child_id == 1, "fgh_id"] <- 40810
    
    x[x$id == 95351149 & x$child_id == 1, "fgh_id"] <- 40801
    x[x$id == 95348853 & x$child_id == 1, "fgh_id"] <- 40801
    x[x$id == 95348855 & x$child_id == 1, "fgh_id"] <- 40801
    x[x$id == 95348859 & x$child_id == 1, "fgh_id"] <- 40801
    x[x$id == 95348860 & x$child_id == 1, "fgh_id"] <- 40801
    
    x[x$id == 95656498 & x$child_id == 1, "fgh_id"] <- 40820
    x[x$id == 95656499 & x$child_id == 1, "fgh_id"] <- 40820
    x[x$id == 95656500 & x$child_id == 1, "fgh_id"] <- 40820
    x[x$id == 95656501 & x$child_id == 1, "fgh_id"] <- 40820
    x[x$id == 95656503 & x$child_id == 1, "fgh_id"] <- 40820
    x[x$id == 95656505 & x$child_id == 1, "fgh_id"] <- 40820
    
    x[x$id == 95945743 & x$child_id == 1, "fgh_id"] <- 40802
    
    x[x$id == 95950443 & x$child_id == 1, "fgh_id"] <- 40826
    
    x[x$id == 96035312 & x$child_id == 1, "fgh_id"] <- 40804
    x[x$id == 96035317 & x$child_id == 1, "fgh_id"] <- 40804
    
    x[x$id == 95281556 & x$child_id == 1, "fgh_id"] <- 41451
    x[x$id == 95281559 & x$child_id == 1, "fgh_id"] <- 41451
    x[x$id == 95334821 & x$child_id == 1, "fgh_id"] <- 41451
    x[x$id == 95334821 & x$child_id == 2, "fgh_id"] <- 41451
    x[x$id == 95334821 & x$child_id == 3, "fgh_id"] <- 41451
    x[x$id == 95281557 & x$child_id == 1, "fgh_id"] <- 41451
    x[x$id == 95281555 & x$child_id == 1, "fgh_id"] <- 41451
    x[x$id == 95281553 & x$child_id == 1, "fgh_id"] <- 41451
    x[x$id == 95281550 & x$child_id == 1, "fgh_id"] <- 41451
    x[x$id == 95334753 & x$child_id == 1, "fgh_id"] <- 41451
    x[x$id == 95334753 & x$child_id == 2, "fgh_id"] <- 41451
    x[x$id == 95281554 & x$child_id == 1, "fgh_id"] <- 41451
    x[x$id == 95281554 & x$child_id == 2, "fgh_id"] <- 41451
    x[x$id == 95281551 & x$child_id == 1, "fgh_id"] <- 41451
    
    x[x$id == 95231324 & x$child_id == 1, "fgh_id"] <- 41452
    x[x$id == 95230831 & x$child_id == 1, "fgh_id"] <- 41452
    x[x$id == 95231336 & x$child_id == 1, "fgh_id"] <- 41452
    x[x$id == 95230838 & x$child_id == 1, "fgh_id"] <- 41452
    x[x$id == 95231327 & x$child_id == 1, "fgh_id"] <- 41452
    x[x$id == 95231321 & x$child_id == 1, "fgh_id"] <- 41452
    x[x$id == 95230834 & x$child_id == 1, "fgh_id"] <- 41452
    x[x$id == 95230828 & x$child_id == 1, "fgh_id"] <- 41452
    x[x$id == 95230767 & x$child_id == 1, "fgh_id"] <- 41452
    x[x$id == 95231331 & x$child_id == 1, "fgh_id"] <- 41452
    
    x[x$id == 95334827 & x$child_id == 1, "fgh_id"] <- 41453
    x[x$id == 95334823 & x$child_id == 1, "fgh_id"] <- 41453
    x[x$id == 95334829 & x$child_id == 1, "fgh_id"] <- 41453
    x[x$id == 95334761 & x$child_id == 1, "fgh_id"] <- 41453
    x[x$id == 95334757 & x$child_id == 1, "fgh_id"] <- 41453
    x[x$id == 95334759 & x$child_id == 1, "fgh_id"] <- 41453
    x[x$id == 95334835 & x$child_id == 1, "fgh_id"] <- 41453
    x[x$id == 95334831 & x$child_id == 1, "fgh_id"] <- 41453
    x[x$id == 95334766 & x$child_id == 1, "fgh_id"] <- 41453
    x[x$id == 95334766 & x$child_id == 2, "fgh_id"] <- 41453
    
    x[x$id == 96038112 & x$child_id == 1, "fgh_id"] <- 42052
    
    x[x$id == 95339104 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95339104 & x$child_id == 2, "fgh_id"] <- 42118
    x[x$id == 95339226 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95339103 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95339205 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95339223 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95339222 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95339222 & x$child_id == 2, "fgh_id"] <- 42118
    x[x$id == 95339096 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95339102 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95339206 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95339093 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95339206 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95385637 & x$child_id == 1, "fgh_id"] <- 42118
    x[x$id == 95385625 & x$child_id == 1, "fgh_id"] <- 42118
    
    x[x$id == 95751980 & x$child_id == 1, "fgh_id"] <- 42114
    x[x$id == 95751980 & x$child_id == 2, "fgh_id"] <- 42114
    
    x[x$id == 95238906 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238903 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238903 & x$child_id == 2, "fgh_id"] <- 41852
    x[x$id == 95238465 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238909 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238909 & x$child_id == 2, "fgh_id"] <- 41852
    x[x$id == 95238466 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238896 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238900 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238898 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238894 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238461 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238463 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238464 & x$child_id == 1, "fgh_id"] <- 41852
    x[x$id == 95238464 & x$child_id == 2, "fgh_id"] <- 41852
    
    x[x$id == 95289757 & x$child_id == 1, "fgh_id"] <- 41853
    x[x$id == 95289705 & x$child_id == 1, "fgh_id"] <- 41853
    x[x$id == 95289699 & x$child_id == 1, "fgh_id"] <- 41853
    x[x$id == 95289750 & x$child_id == 1, "fgh_id"] <- 41853
    x[x$id == 95289701 & x$child_id == 1, "fgh_id"] <- 41853
    x[x$id == 95289701 & x$child_id == 2, "fgh_id"] <- 41853
    x[x$id == 95289754 & x$child_id == 1, "fgh_id"] <- 41853
    x[x$id == 95289752 & x$child_id == 1, "fgh_id"] <- 41853
    x[x$id == 95289703 & x$child_id == 1, "fgh_id"] <- 41853
    x[x$id == 95289703 & x$child_id == 2, "fgh_id"] <- 41853
    x[x$id == 95289755 & x$child_id == 1, "fgh_id"] <- 41853
    x[x$id == 95289755 & x$child_id == 2, "fgh_id"] <- 41853
    
    x[x$id == 95288393 & x$child_id == 1, "fgh_id"] <- 41731
    
    x[x$id == 95288426 & x$child_id == 1, "fgh_id"] <- 41730
    
    x[x$id == 96316632 & x$child_id == 1, "fgh_id"] <- 41722
    x[x$id == 96316630 & x$child_id == 1, "fgh_id"] <- 41722
    x[x$id == 96316651 & x$child_id == 1, "fgh_id"] <- 41722
    x[x$id == 96316636 & x$child_id == 1, "fgh_id"] <- 41722
    x[x$id == 96316641 & x$child_id == 1, "fgh_id"] <- 41722
    x[x$id == 96316641 & x$child_id == 2, "fgh_id"] <- 41722
    x[x$id == 96316652 & x$child_id == 1, "fgh_id"] <- 41722
    
    x[x$id == 96316661 & x$child_id == 1, "fgh_id"] <- 41719
    x[x$id == 96316662 & x$child_id == 1, "fgh_id"] <- 41719
    x[x$id == 96316659 & x$child_id == 1, "fgh_id"] <- 41719
    x[x$id == 96316658 & x$child_id == 1, "fgh_id"] <- 41719
    x[x$id == 96316044 & x$child_id == 1, "fgh_id"] <- 41719
    x[x$id == 96316041 & x$child_id == 1, "fgh_id"] <- 41719
    x[x$id == 96316050 & x$child_id == 1, "fgh_id"] <- 41719
    x[x$id == 96316665 & x$child_id == 1, "fgh_id"] <- 41719
    x[x$id == 96316655 & x$child_id == 1, "fgh_id"] <- 41719
    x[x$id == 96316043 & is.na(x$child_id), "fgh_id"] <- 41719
    x[x$id == 96316027 & x$child_id == 1, "fgh_id"] <- 41719
    x[x$id == 96316032 & x$child_id == 1, "fgh_id"] <- 41719
    
    x[x$id == 95731768 & x$child_id == 1, "fgh_id"] <- 40252
    
    x[x$id == 98347696 & x$child_id == 1, "fgh_id"] <- 32127
    x[x$id == 98305779 & x$child_id == 1, "fgh_id"] <- 32127
    x[x$id == 98305782 & x$child_id == 1, "fgh_id"] <- 32127
    x[x$id == 98347692 & x$child_id == 1, "fgh_id"] <- 32127
    x[x$id == 98347691 & x$child_id == 1, "fgh_id"] <- 32127
  
    x[x$id == 98274458 & x$child_id == 1, "fgh_id"] <- 32128
    x[x$id == 98347769 & x$child_id == 1, "fgh_id"] <- 32128
    x[x$id == 98347765 & is.na(x$child_id), "fgh_id"] <- 32128
    x[x$id == 98347767 & x$child_id == 1, "fgh_id"] <- 32128
    x[x$id == 98348972 & x$child_id == 1, "fgh_id"] <- 32128
    x[x$id == 98348974 & x$child_id == 1, "fgh_id"] <- 32128
    x[x$id == 98347704 & x$child_id == 1, "fgh_id"] <- 32128
    x[x$id == 98347701 & x$child_id == 1, "fgh_id"] <- 32128
    x[x$id == 98347706 & x$child_id == 1, "fgh_id"] <- 32128
    x[x$id == 98348975 & x$child_id == 1, "fgh_id"] <- 32128
    
    x[x$id == 100141572 & x$child_id == 1, "fgh_id"] <- 31823
    
    x[x$id == 100068619 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 99959150 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 100068615 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 99959155 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 100068611 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 99959161 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 99959160 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 100068604 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 100068607 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 100068621 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 99959164 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 99959166 & x$child_id == 1, "fgh_id"] <- 30754
    x[x$id == 99959166 & x$child_id == 2, "fgh_id"] <- 30754
    
    x[x$id == 100068596 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 100068592 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 99959091 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 100068601 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 100068601 & x$child_id == 2, "fgh_id"] <- 30755
    x[x$id == 100068589 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 100068587 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 100068580 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 99959098 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 99959095 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 99959102 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 99959111 & x$child_id == 1, "fgh_id"] <- 30755
    x[x$id == 99959106 & x$child_id == 1, "fgh_id"] <- 30755
    
    x[x$id == 95337740 & x$child_id == 1, "fgh_id"] <- 41851
    x[x$id == 95337740 & x$child_id == 2, "fgh_id"] <- 41851
    x[x$id == 95337739 & x$child_id == 1, "fgh_id"] <- 41851
    x[x$id == 95337739 & x$child_id == 2, "fgh_id"] <- 41851
    x[x$id == 95337001 & x$child_id == 1, "fgh_id"] <- 41851
    x[x$id == 95337002 & x$child_id == 1, "fgh_id"] <- 41851
    x[x$id == 95337735 & x$child_id == 1, "fgh_id"] <- 41851
    x[x$id == 95337744 & x$child_id == 1, "fgh_id"] <- 41851
    x[x$id == 95337744 & x$child_id == 2, "fgh_id"] <- 41851
    x[x$id == 95337745 & x$child_id == 1, "fgh_id"] <- 41851
    x[x$id == 95336997 & x$child_id == 1, "fgh_id"] <- 41851
    x[x$id == 95336997 & x$child_id == 2, "fgh_id"] <- 41851
    x[x$id == 95336999 & x$child_id == 1, "fgh_id"] <- 41851
    x[x$id == 95336996 & x$child_id == 1, "fgh_id"] <- 41851
    x[x$id == 95336996 & x$child_id == 2, "fgh_id"] <- 41851
    x[x$id == 95337000 & x$child_id == 1, "fgh_id"] <- 41851
    
    x[x$id == 96282953 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96282953 & x$child_id == 2, "fgh_id"] <- 40822
    x[x$id == 96282952 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96282949 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96282948 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96282955 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96282954 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96261270 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96261266 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96261268 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96261267 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96261264 & x$child_id == 1, "fgh_id"] <- 40822
    x[x$id == 96261262 & x$child_id == 1, "fgh_id"] <- 40822
    
    x
  }
  
  clean_endline_identifiers <- function(endline_raw_data, 
                                        survey_sampling_list_endline) {
    endline_raw_data |>
      dplyr::left_join(
        y = survey_sampling_list_endline |>
          subset(
            select = c(
              UNIQUE_ID, FGH_ID, `Cod..Concatenado.(AREA)`, 
              Provincia, Distrito, Paridade
            )
          ),
        by = c("fgh_id" = "FGH_ID")
      ) |>
      dplyr::mutate(
        hh_id = id,
        ch_id = paste0(
          id, stringr::str_pad(child_id, width = 2, side = "left", pad = "0")
        ),
        province = factor(Provincia, levels = c("Zambézia", "Nampula")),
        district = Distrito,
        ea_code = `Cod..Concatenado.(AREA)`,
        ea_id = UNIQUE_ID,
        strata = ifelse(
          Paridade == "COM", paste0("Rest of ", as.character(province)), district
        ) |>
          factor(
            levels = c(
              "Gurúè", "Lugela", "Pebane", "Molumbo", "Rest of Zambézia",
              "Monapo", "Nacala-A-Velha", "Ribáuè", "Rest of Nampula"
            )
          ),
        longitude = do.call(rbind, geolocation)[ , 2],
        latitude = do.call(rbind, geolocation)[ , 1]
      ) |>
      subset(
        select = c(
          -UNIQUE_ID, -`Cod..Concatenado.(AREA)`, -Provincia, -Distrito, -Paridade
        )
      )
  }
  
  add_ea_info <- function(survey_sampling_list) {
    x <- matrix(
      nrow = 2, ncol = ncol(survey_sampling_list), byrow = TRUE
    ) |>
      data.frame() |>
      (\(x) { names(x) <- names(survey_sampling_list); x })()
    
    x[1, "UNIQUE_ID"] <- max(survey_sampling_list$UNIQUE_ID) + 1
    x[2, "UNIQUE_ID"] <- max(survey_sampling_list$UNIQUE_ID) + 2
    
    x[1, "FGH_ID"] <- 30754
    x[2, "FGH_ID"] <- 30755
    
    x[1, "Paridade"] <- "COM"
    x[2, "Paridade"] <- "COM"
    
    x[1, "Provincia"] <- "Nampula"
    x[2, "Provincia"] <- "Nampula"
    
    x[1, "Distrito"] <- "Meconta"
    x[2, "Distrito"] <- "Meconta"
    
    x[1, "Posto"] <- "Corrane"
    x[2, "Posto"] <- "Corrane"
    
    x[1, "População.Total.-.preliminar"] <- 593
    x[2, "População.Total.-.preliminar"] <- 317
    
    rbind(survey_sampling_list, x)
  }
  
  
  process_endline_data <- function(.data, 
                                   survey_endline_choices) {
    n_row <- nrow(.data)
    
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
      sufficiency_of_household_income = vector(
        mode = "character", length = n_row
      ) |>
        factor(
          levels = survey_endline_choices |>
            subset(list_name == "income2", select = `label::English (en)`) |>
            (\(x) x$`label::English (en)`)()
        ),
      sufficiency_of_family_resource = vector(
        mode = "character", length = n_row
      ) |>
        factor(
          levels = survey_endline_choices |>
            subset(list_name == "income2", select = `label::English (en)`) |>
            (\(x) x$`label::English (en)`)()
        ),
      household_income_against_expenses = vector(
        mode = "character", length = n_row
      ) |>
        factor(
          levels = survey_endline_choices |>
            subset(list_name == "income3", select = `label::English (en)`) |>
            (\(x) x$`label::English (en)`)()
        ),
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
      landline = NA_integer_,
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
      time_to_travel_to_primary_school = NA_integer_,
      mode_of_travel_to_primary_school = vector(
        mode = "character", length = n_row
      ) |>
        factor(
          levels = survey_endline_choices |>
            subset(list_name == "travel2", select = `label::English (en)`) |>
            (\(x) x$`label::English (en)`)()
        ),
      time_to_travel_to_secondary_school = NA_integer_,
      mode_of_travel_to_secondary_school = vector(
        mode = "character", length = n_row
      ) |>
        factor(
          levels = survey_endline_choices |>
            subset(list_name == "travel2", select = `label::English (en)`) |>
            (\(x) x$`label::English (en)`)()
        ),
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
      ### Community groups participation ---------------------------------------
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
      ### Child anthropometry --------------------------------------------------
      child_height_length = height,
      child_standing = position,
      child_weight = weight,
      child_muac = clean_child_muac_data(muac, muac1, muac2),
      global_wasting_by_muac = ifelse(child_muac < 12.5, 1, 0),
      moderate_wasting_by_muac = ifelse(child_muac < 12.5 & child_muac >= 11.5, 1, 0),
      severe_wasting_by_muac = ifelse(child_muac < 11.5, 1, 0),
      severe_wasting_by_oedema = ifelse(cmalnut == 1, 1, 0),
      ### Water ----------------------------------------------------------------
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
      ### Sanitation -----------------------------------------------------------
      open_defecation = ifelse(lusd1 == 2 | lusd4 == 6, 1, 0),
      unimproved_toilet_facility = ifelse(lusd4 == 5, 1, 0),
      limited_toilet_facility = ifelse(lusd2 == 1 & lusd4 != 5, 1, 0),
      basic_toilet_facility = ifelse(lusd2 == 2 & lusd4 != 5, 1, 0),
      ### Hygiene --------------------------------------------------------------
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
      ### Diarrhoea ------------------------------------------------------------
      diarrhoea_episode = recode_yes_no(ort1, na_values = c(8, 9)),
      diarrhoea_seek_treatment = recode_yes_no(ort3),
      diarrhoea_point_of_care = refactor_var_categorical(
        x = ort4, y = "point_of_care", choices = survey_endline_choices
      ),
      diarrhoea_treatment_with_ors = ifelse(
        ort5a %in% c(8, 9) | ort5b %in% c(8, 9) | ort5c %in% c(8, 9), NA,
        ifelse(
          ort5a == 1 | ort5b == 1 | ort5c == 1, 1, 0
        )
      ),
      ### Fever ----------------------------------------------------------------
      fever_episode = recode_yes_no(fever1, na_values = c(8, 9)),
      fever_seek_treatment = recode_yes_no(fever2),
      fever_point_of_care = refactor_var_categorical(
        x = fever3, y = "point_of_care", choices = survey_endline_choices
      ),
      fever_malaria_test = ifelse(
        fever4 %in% c(8, 9) | fever5 %in% c(8, 9), NA,
        ifelse(
          fever4 == 1 | fever5 == 1, 1, 0
        )
      ),
      fever_malaria_episode = recode_yes_no(fever6, na_values = c(8, 9)),
      fever_treatment = ifelse(fever6a %in% c(88, 99), NA, fever6a),
      fever_malaria_treatment_intake = recode_yes_no(fever7, na_values = c(8, 9)),
      ### RTI ------------------------------------------------------------------
      rti_episode = ifelse(ch1 == 1 & (ch1a == 1 | ch2 == 1), 1, 0),
      rti_seek_treatment = recode_yes_no(ch3),
      rti_point_of_care = refactor_var_categorical(
        x = ch4, y = "point_of_care", choices = survey_endline_choices
      ),
      rti_treatment = ifelse(ch5a %in% c(88, 99), NA, ch5a),
      ### Mental health --------------------------------------------------------
      ment1 = ifelse(ment1 %in% c(88, 99), NA, ment1),
      ment2 = ifelse(ment2 %in% c(88, 99), NA, ment2),
      ment3 = ifelse(ment3 %in% c(88, 99), NA, ment3),
      ment4 = ifelse(ment4 %in% c(88, 99), NA, ment4),
      ment5 = ifelse(ment5 %in% c(88, 99), NA, ment5),
      ment6 = ifelse(ment6 %in% c(88, 99), NA, ment6),
      ment7 = ifelse(ment7 %in% c(88, 99), NA, ment7),
      ment8 = ifelse(ment8 %in% c(88, 99), NA, ment8),
      phq8_score = ment1 + ment2 + ment3 + ment4 + ment5 + ment6 + ment7 + ment8,
      major_depression = ifelse(phq8_score > 10 & phq8_score <= 20, 1, 0),
      severe_depression = ifelse(phq8_score > 20, 1, 0),
      at_least_major_depression = ifelse(phq8_score > 10, 1, 0),
      alcohol_consumption = refactor_var_categorical(
        x = ment9, y = "alcohol_frequency", choices = survey_endline_choices
      ),
      ### Pregnant -------------------------------------------------------------
      currently_pregnant = recode_yes_no(wh1),
      #weeks_of_gestation_self_report = NA,
      prenatal_card_self_report = recode_yes_no(wh2),
      prenatal_card_available = recode_yes_no(wh3),
      malaria_during_pregnancy = recode_yes_no(wh4),
      anemia_during_pregnancy = recode_yes_no(wh5),
      excluded_foods_from_diet = recode_yes_no(wh6),
      included_foods_from_diet = recode_yes_no(wh7),
      wants_more_children = recode_yes_no(wh8),
      pregnancy_danger_signs = ifelse(preg1 %in% c(88, 99), NA, preg1),
      plans_when_labor_begins = refactor_var_categorical(
        x = preg2, y = "labor_action", choices = survey_endline_choices
      ),
      ### PMTCT and malaria prevention -----------------------------------------
      offered_voluntary_counselling_and_testing = recode_yes_no(
        pmtct1, na_values = 8:9
      ),
      received_vct_results = recode_yes_no(pmtct2, na_values = 8:9),
      offered_medication_to_reduce_child_risk = recode_yes_no(
        pmtct3, na_values = 8:9
      ),
      received_mosquito_net = recode_yes_no(idk1, na_values = 8:9),
      slept_under_mosquito_net = recode_yes_no(idk2, na_values = 8:9),
      ### Natal care -----------------------------------------------------------
      location_of_last_delivery = refactor_var_categorical(
        x = spc1, y = "delivery_location", choices = survey_endline_choices
      ),
      number_of_prenatal_visits = ifelse(spc2 %in% c(88, 99), NA, spc2) |>
        cut(
          breaks = c(-1, 0, 1, 2, 3, 4, Inf), 
          labels = c(
            "No visits", "1 visit", "2 visits", "3 visits", 
            "4 visits", "more than 4 visits"
          )
        ) |>
        as.character() |>
        (\(x) { ifelse(spc2 %in% c(88, 99), "No response", x) })() |>
        factor(
          levels = c(
            "No visits", "1 visit", "2 visits", "3 visits", 
            "4 visits", "more than 4 visits", "No response"
          )
        ),
      at_least_four_anc_visits = ifelse(
        number_of_prenatal_visits %in% c("4 visits", "more than 4 visits"), 1, 0
      ),
      treated_well_during_anc = recode_yes_no(spc2a, na_values = 8:9),
      treated_well_at_delivery = recode_yes_no(spc2b, na_values = 8:9),
      spc3 = ifelse(spc3 %in% c(88, 99), NA, spc3),
      spc5a = ifelse(spc5a %in% c(88, 99), NA, spc5a),
      spc6 = ifelse(spc6 %in% c(88, 99), NA, spc6),
      spc7 = ifelse(spc7 %in% c(88, 99), NA, spc7),
      given_malaria_treatment_during_pregnancy = recode_yes_no(
        fansidar1, na_values = 8:9
      ),
      took_malaria_treatment_during_pregnancy = ifelse(fansidar2 == 1, 0, 1),
      completed_malaria_treatment_during_pregnancy = ifelse(
        fansidar2 %in% c(88, 99), NA,
        ifelse(
          fansidar2 == 4, 1, 0
        )
      ),
      at_least_one_tetanus_toxoid_vaccination = recode_yes_no(
        tt1, na_values = 8:9
      ),
      two_or_more_tetanus_toxoid_vaccination = ifelse(
        tt2 %in% c(88, 99), NA,
        ifelse(
          tt2 != 1, 1, 0
        )
      ),
      ferrous_sulfate_supplementation = recode_yes_no(fol1, na_values = 8:9),
      vitamin_a_supplementation_during_pregnancy = NA_integer_,
      ### Family planning ------------------------------------------------------
      attempted_to_delay_or_prevent_pregnancy = recode_yes_no(pf1),
      bs2 = ifelse(bs2 %in% c(88, 99), NA, bs2),
      bs3 = ifelse(bs3 %in% c(88, 99), NA, bs3),
      bs4 = ifelse(bs4 %in% c(88, 99), NA, bs4),
      benefit_of_waiting_until_18_years_of_age = ifelse(bs3 == 6, 0, 1),
      problem_with_having_more_than_4_children = ifelse(bs4 == 6, 0, 1),
      ### EPI ------------------------------------------------------------------
      # immunisation_card_retention_self_report = recode_yes_no(
      #   imm1, na_values = 8:9
      # ),
      immunisation_card_retention_self_report = ifelse(
        imm2 %in% c(88, 99), NA,
        ifelse(
          imm2 == 3, 0, 1
        )
      ),
      immunisation_card_retention = recode_yes_no(imm2, na_values = c(88, 99)),
      immunisation_bcg = recode_yes_no(imm3a, na_values = 4:5),
      immunisation_polio_first_dose = recode_yes_no(imm3b, na_values = 4:5),
      immunisation_polio_second_dose = recode_yes_no(imm3c, na_values = 4:5),
      immunisation_polio_third_dose = recode_yes_no(imm3d, na_values = 4:5),
      immunisation_polio_fourth_dose = recode_yes_no(imm3e, na_values = 4:5),
      immunisation_pentavalent_first_dose = recode_yes_no(imm4a, na_values = 4:5),
      immunisation_pentavalent_second_dose = recode_yes_no(imm4b, na_values = 4:5),
      immunisation_pentavalent_third_dose = recode_yes_no(imm4c, na_values = 4:5),
      immunisation_measles_first_dose = recode_yes_no(imm5, na_values = 4:5),
      immunisation_measles_second_dose = recode_yes_no(imm5a, na_values = 4:5),
      immunisation_pneumococcal_first_dose = recode_yes_no(imm6a, na_values = 4:5),
      immunisation_pneumococcal_second_dose = recode_yes_no(imm6b, na_values = 4:5),
      immunisation_pneumococcal_third_dose = recode_yes_no(imm6c, na_values = 4:5),
      immunisation_rotavirus_first_dose = recode_yes_no(imm7a, na_values = 4:5),
      immunisation_rotavirus_second_dose = recode_yes_no(imm7b, na_values = 4:5),
      immunisation_fully_immunised = ifelse(
        child_age_months >= 12 & child_age_months < 24 &
          immunisation_bcg + 
          immunisation_polio_first_dose +
          immunisation_polio_second_dose + 
          immunisation_polio_third_dose +
          immunisation_polio_fourth_dose + 
          immunisation_pentavalent_first_dose +
          immunisation_pentavalent_second_dose + 
          immunisation_pentavalent_third_dose +
          immunisation_measles_first_dose + 
          immunisation_measles_second_dose +
          immunisation_pneumococcal_first_dose + 
          immunisation_pneumococcal_second_dose +
          immunisation_pneumococcal_third_dose + 
          immunisation_rotavirus_first_dose +
          immunisation_rotavirus_second_dose == 15, 1, 0
      ),
      immunisation_age_appropriate_immunisation = ifelse(
        child_age_months >= 0 & child_age_months < 1.5 & 
          immunisation_bcg == 1 &
          immunisation_polio_first_dose == 1, 1,
        ifelse(
          child_age_months >= 1.5 & child_age_months < 2.5 &
            immunisation_bcg == 1 & 
            immunisation_polio_first_dose == 1 &
            immunisation_polio_second_dose == 1 & 
            immunisation_pentavalent_first_dose == 1 &
            immunisation_pneumococcal_first_dose == 1 &
            immunisation_rotavirus_first_dose == 1, 1,
          ifelse(
            child_age_months >= 2.5 & child_age_months < 3.5 &
              immunisation_bcg == 1 & 
              immunisation_polio_first_dose == 1 &
              immunisation_polio_second_dose == 1 & 
              immunisation_polio_third_dose == 1 &
              immunisation_pentavalent_first_dose == 1 &
              immunisation_pentavalent_second_dose == 1 &
              immunisation_pneumococcal_first_dose == 1 &
              immunisation_pneumococcal_second_dose == 1 &
              immunisation_rotavirus_first_dose == 1 &
              immunisation_rotavirus_second_dose == 1, 1,
            ifelse(
              child_age_months >= 3.5 & child_age_months < 9 &
                immunisation_bcg == 1 & 
                immunisation_polio_first_dose == 1 &
                immunisation_polio_second_dose == 1 & 
                immunisation_polio_third_dose == 1 &
                immunisation_polio_fourth_dose == 1 &
                immunisation_pentavalent_first_dose == 1 &
                immunisation_pentavalent_second_dose == 1 &
                immunisation_pentavalent_third_dose == 1 &
                immunisation_pneumococcal_first_dose == 1 &
                immunisation_pneumococcal_second_dose == 1 &
                immunisation_pentavalent_third_dose == 1 &
                immunisation_rotavirus_first_dose == 1 &
                immunisation_rotavirus_second_dose == 1, 1,
              ifelse(
                child_age_months >= 9 & child_age_months < 18 &
                  immunisation_bcg == 1 & 
                  immunisation_polio_first_dose == 1 &
                  immunisation_polio_second_dose == 1 & 
                  immunisation_polio_third_dose == 1 &
                  immunisation_polio_fourth_dose == 1 &
                  immunisation_pentavalent_first_dose == 1 &
                  immunisation_pentavalent_second_dose == 1 &
                  immunisation_pentavalent_third_dose == 1 &
                  immunisation_pneumococcal_first_dose == 1 &
                  immunisation_pneumococcal_second_dose == 1 &
                  immunisation_pentavalent_third_dose == 1 &
                  immunisation_rotavirus_first_dose == 1 &
                  immunisation_rotavirus_second_dose == 1 &
                  immunisation_measles_first_dose == 1, 1,
                ifelse(
                  child_age_months >= 18 &
                    immunisation_bcg == 1 & 
                    immunisation_polio_first_dose == 1 &
                    immunisation_polio_second_dose == 1 & 
                    immunisation_polio_third_dose == 1 &
                    immunisation_polio_fourth_dose == 1 &
                    immunisation_pentavalent_first_dose == 1 &
                    immunisation_pentavalent_second_dose == 1 &
                    immunisation_pentavalent_third_dose == 1 &
                    immunisation_pneumococcal_first_dose == 1 &
                    immunisation_pneumococcal_second_dose == 1 &
                    immunisation_pentavalent_third_dose == 1 &
                    immunisation_rotavirus_first_dose == 1 &
                    immunisation_rotavirus_second_dose == 1 &
                    immunisation_measles_first_dose == 1 &
                    immunisation_measles_second_dose == 1, 1, 0
                )
              )
            )
          )
        )
      ),
      ### Vitamin A and deworming ------------------------------------------------
      vas1 = ifelse(vas1 %in% 4:5, NA, vas1),
      vas2 = ifelse(vas2 %in% 4:5, NA, vas2),
      vitamin_a_supplementation_coverage = ifelse(
        child_age_months >= 6 & child_age_months < 12 & vas1 == 1, 1,
        ifelse(
          child_age_months >= 12 & child_age_months < 60 & vas2 == 2, 1, 0
        )
      ),
      deworming_coverage = recode_yes_no(vas3, na_values = 8:9),
      ### IYCF/ICFI - 6-23 months - MDD ------------------------------------------
      eb7 = ifelse(eb7 %in% 8:9, NA, eb7),
      nut1a = ifelse(nut1a %in% c(88, 99), NA, nut1a),
      nut1b = ifelse(nut1b %in% c(88, 99), NA, nut1b),
      nut1c = ifelse(nut1c %in% c(88, 99), NA, nut1c),
      nut1d = ifelse(nut1d %in% c(88, 99), NA, nut1d),
      nut1e = ifelse(nut1e %in% c(88, 99), NA, nut1e),
      nut1f = ifelse(nut1f %in% c(88, 99), NA, nut1f),
      nut1g = ifelse(nut1g %in% c(88, 99), NA, nut1g),
      nut1h = ifelse(nut1h %in% c(88, 99), NA, nut1h),
      nut1i = ifelse(nut1i %in% c(88, 99), NA, nut1i),
      nut1j = ifelse(nut1j %in% c(88, 99), NA, nut1j),
      nut1k = ifelse(nut1k %in% c(88, 99), NA, nut1k),
      nut1l = ifelse(nut1l %in% c(88, 99), NA, nut1l),
      nut1m = ifelse(nut1m %in% c(88, 99), NA, nut1m),
      nut1n = ifelse(nut1n %in% c(88, 99), NA, nut1n),
      food_group_breastmilk = ifelse(eb7 == 2, 1, 0),
      food_group_dairy = ifelse(nut1l > 0, 1, 0),
      food_group_starch = ifelse(nut1a > 0 | nut1c > 0, 1, 0),
      food_group_vitamin_a_rich = ifelse(
        nut1b > 0 | nut1d > 0 | nut1e > 0 | nut1n > 0, 1, 0
      ),
      food_group_other_fruits_vegetables = ifelse(nut1f > 0, 1, 0),
      food_group_legumes = ifelse(nut1k > 0, 1, 0),
      food_group_meat = ifelse(nut1g > 0 | nut1h > 0 | nut1j > 0, 1, 0),
      food_group_eggs = ifelse(nut1i > 0, 1, 0),
      food_groups_score = food_group_breastmilk + food_group_dairy + 
        food_group_starch + food_group_vitamin_a_rich + 
        food_group_other_fruits_vegetables +
        food_group_legumes + food_group_meat + food_group_eggs,
      minimum_dietary_diversity = ifelse(
        child_age_months < 6 | child_age_months >= 24, NA,
        ifelse(
          food_groups_score >= 5, 1, 0
        )
      ),
      ### Breastfeeding (less than 24 months) ------------------------------------
      eb1 = ifelse(eb1 %in% c(88, 99), NA, eb1),
      eb2 = ifelse(eb2 %in% c(88, 99), NA, eb2),
      nut2 = ifelse(nut2 %in% c(88, 99), NA, nut2),
      ever_breastfed = ifelse(eb1 %in% 1:2, 1, 0),
      early_initiation_of_breastfeeding = ifelse(eb2 == 1 | eb2_hours <= 1, 1, 0),
      ### Exclusive breastfeeding (less than 6 months) ---------------------------
      exclusive_breastfeeding = ifelse(eb7 == 1 & nut2 == 0, 1, 0),
      ### Women's decision making ------------------------------------------------
      freedom_and_control = refactor_var_categorical(
        x = von1, y = "freedom_choice", choices = survey_endline_choices
      ),
      control_over_destiny = refactor_var_categorical(
        x = von2, y = "decision_level", choices = survey_endline_choices
      ),
      make_decision_without_husband = refactor_var_categorical(
        x = von3, y = "decision_frequency", choices = survey_endline_choices
      ),
      willingly_participate_in_survey = refactor_var_categorical(
        x = von4, y = "survey_participation", choices = survey_endline_choices
      ),
      ### Mother anthropometry -------------------------------------------------
      maltura = ifelse(
        maltura == 0, NA,
        ifelse(
          maltura < 2, maltura * 100, maltura
        )
      ),
      mpeso = ifelse(mpeso == 0, NA, mpeso),
      mbraco = clean_woman_muac_data(mbraco),
      body_mass_index = mpeso / ((maltura / 100) ^ 2),
      bmi_class = cut(
        x = body_mass_index,
        breaks = c(0, 18.5, 25, 30, Inf),
        labels = c("Underweight", "Healthy weight", "Overweight", "Obese"),
        include.lowest = TRUE, right = FALSE
      ),
      ## Food stocks
      corn_reserve = refactor_var_categorical(
        x = reserve1a,
        y = "stock_frequency",
        choices = survey_endline_choices
      ) |>
        (\(x)
          ifelse(
            as.character(x) %in% c("Don't know", "No response"), 
            "Don't know/No response", as.character(x)
          )
        )() |>
        factor(levels = c(
          "Menos de um mês", "1 a 3 meses", "4 a 6 meses", "Mais de 6 meses",
          "Don't know/No response"
        )),
      rice_reserve = refactor_var_categorical(
        x = reserve2a,
        y = "stock_frequency",
        choices = survey_endline_choices
      ) |>
        (\(x)
         ifelse(
           as.character(x) %in% c("Don't know", "No response"), 
           "Don't know/No response", as.character(x)
         )
        )() |>
        factor(levels = c(
          "Menos de um mês", "1 a 3 meses", "4 a 6 meses", "Mais de 6 meses",
          "Don't know/No response"
        )),
      millet_reserve = refactor_var_categorical(
        x = reserve3a,
        y = "stock_frequency",
        choices = survey_endline_choices
      ) |>
        (\(x)
         ifelse(
           as.character(x) %in% c("Don't know", "No response"), 
           "Don't know/No response", as.character(x)
         )
        )() |>
        factor(levels = c(
          "Menos de um mês", "1 a 3 meses", "4 a 6 meses", "Mais de 6 meses",
          "Don't know/No response"
        )),
      sorghum_reserve = refactor_var_categorical(
        x = reserve4a,
        y = "stock_frequency",
        choices = survey_endline_choices
      ) |>
        (\(x)
         ifelse(
           as.character(x) %in% c("Don't know", "No response"), 
           "Don't know/No response", as.character(x)
         )
        )() |>
        factor(levels = c(
          "Menos de um mês", "1 a 3 meses", "4 a 6 meses", "Mais de 6 meses",
          "Don't know/No response"
        )),
      cassava_reserve = refactor_var_categorical(
        x = reserve5a,
        y = "stock_frequency",
        choices = survey_endline_choices
      ) |>
        (\(x)
         ifelse(
           as.character(x) %in% c("Don't know", "No response"), 
           "Don't know/No response", as.character(x)
         )
        )() |>
        factor(levels = c(
          "Menos de um mês", "1 a 3 meses", "4 a 6 meses", "Mais de 6 meses",
          "Don't know/No response"
        )),
      sweet_potato_reserve = refactor_var_categorical(
        x = reserve6a,
        y = "stock_frequency",
        choices = survey_endline_choices
      ) |>
        (\(x)
         ifelse(
           as.character(x) %in% c("Don't know", "No response"), 
           "Don't know/No response", as.character(x)
         )
        )() |>
        factor(levels = c(
          "Menos de um mês", "1 a 3 meses", "4 a 6 meses", "Mais de 6 meses",
          "Don't know/No response"
        )),
      legumes_reserve = refactor_var_categorical(
        x = reserve7a,
        y = "stock_frequency",
        choices = survey_endline_choices
      ) |>
        (\(x)
         ifelse(
           as.character(x) %in% c("Don't know", "No response"), 
           "Don't know/No response", as.character(x)
         )
        )() |>
        factor(levels = c(
          "Menos de um mês", "1 a 3 meses", "4 a 6 meses", "Mais de 6 meses",
          "Don't know/No response"
        )),
      ## Observation module
      clean_yard_house = ifelse(anim2 == 2, 1, 0),
      water_storage_system = ifelse(agua1 == 1, 1, 0),
      # water_storage_small_mouth_covered = refactor_var_categorical(
      #   x = agua2,
      #   y = 
      # ),
      # water_storage_small_mouth_uncoverd = agua2_2,
      # water_storage_wide_mouth = agua2_3,
      eating_utensils_storage_system = ifelse(
        coz1 %in% 3:4, NA,
        ifelse(
          coz1 == 1, 1, 0
        )
      ),
      dry_food_storage_system = ifelse(
        coz2 %in% 3:4, NA,
        ifelse(
          coz2 == 1, 1, 0
        )
      ),
      mortar_hanger = ifelse(
        coz3 %in% 3:4, NA,
        ifelse(
          coz3 == 1, 1, 0
        )
      ),
      trash_system = ifelse(
        quin1 %in% 3:4, NA,
        ifelse(
          quin1 == 1, 1, 0
        )
      ),
      ## PICA
      pica_frequency = refactor_var_categorical(
        x = pica1,
        y = "pica_frequency",
        choices = survey_endline_choices
      ) |>
        (\(x)
          ifelse(
            as.character(x) %in% c("Don't know", "No response"), 
            NA, as.character(x)
          )
        )() |>
        factor(levels = c(
          "0 times", "<1 time per day", "Once per day", "2-5 times per day",
          "More than 5 times per day"
        )),
      pica_bad = ifelse(
        pica3 %in% 8:9, NA,
        ifelse(1, 1, 0)
      ),
      .keep = "unused"
    ) |>
      ## Child anthropometry
      zscorer::addWGSR(
        sex = "child_sex_integer",
        firstPart = "child_weight",
        secondPart = "child_age_days",
        index = "wfa"
      ) |>
      zscorer::addWGSR(
        sex = "child_sex_integer",
        firstPart = "child_height_length",
        secondPart = "child_age_days",
        standing = "child_standing",
        index = "hfa"
      ) |>
      zscorer::addWGSR(
        sex = "child_sex_integer",
        firstPart = "child_weight",
        secondPart = "child_height_length",
        standing = "child_standing",
        index = "wfh"
      ) |>
      zscorer::addWGSR(
        sex = "child_sex_integer",
        firstPart = "child_muac",
        secondPart = "child_age_days",
        index = "mfa"
      ) |>
      (\(x) { x$hfaz <- ifelse(x$hfaz > 6 | x$hfaz < -6, NA, x$hfaz); x })() |>
      (\(x) { x$wfaz <- ifelse(x$wfaz > 5 | x$wfaz < -6, NA, x$wfaz); x })() |>
      (\(x) { x$wfhz <- ifelse(x$wfhz > 5 | x$wfhz < -5, NA, x$wfhz); x })() |>
      (\(x) { x$mfaz <- ifelse(x$mfaz > 5 | x$mfaz < -6, NA, x$mfaz); x })() |>
      (\(x) 
       {
         ## Stunting
         x$global_stunting = ifelse(x$hfaz < -2, 1, 0)
         x$moderate_stunting = ifelse(x$hfaz >= -3 & x$hfaz < -2, 1, 0)
         x$severe_stunting = ifelse(x$hfaz < -3, 1, 0)
         ## Underweight
         x$global_underweight = ifelse(x$wfaz < -2, 1, 0)
         x$moderate_underweight = ifelse(x$wfaz >= -3 & x$wfaz < -2, 1, 0)
         x$severe_underweight = ifelse(x$wfaz < -3, 1, 0)
         ## Wasting
         x$global_wasting_by_weight_for_height = ifelse(x$wfhz < -2, 1, 0)
         x$moderate_wasting_by_weight_for_height = ifelse(x$wfhz >= -3 & x$wfhz < -2, 1, 0)
         x$severe_wasting_by_weight_for_height = ifelse(x$wfhz < -3, 1, 0)
         x
      }
      )() |>
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
                     "quinino_injection", "artesunato", "paracetamol_comprimido_xarope"
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
              )(),
            preg_recode_danger(
              vars = "pregnancy_danger_signs",
              .data = x,
              prefix = "danger"
            ) |>
              (\(x) 
                { 
                  names(x) <- c(
                    "vaginal_bleeding", "severe_headache", "blurry_vision",
                    "swollen_extremities", "convulsions", "fever",
                    "intense_abdominal_pain", "loss_of_consciousness",
                    "fatigue", "accelerated_diminished_fetal_movement",
                    "danger_all", "danger_prop"
                  )
                  x 
                }
              )(),
            nc_recode_assist(vars = "spc3", .data = x, na_rm = FALSE) |>
              (\(x)
                {
                  names(x) <- c(
                    "delivery_assisted_by",
                    paste0(
                      "delivery_assisted_by_", 
                      c(
                        "doctor", "nurse", "midwife", "other_person",
                        "traditional_midwife", "community_health_worker",
                        "relative_or_friend", "other", "nobody"
                      )
                    )
                  )
                  x
                }
              )(),
            nc_recode_difficulties(vars = "spc5a", .data = x, na_rm = FALSE) |>
              (\(x)
               {
                 names(x) <- c(
                   "difficulty_reaching_faciity",
                   paste0(
                     "difficulty_reaching_facility_",
                     c(
                       "due_to_cost", "due_to_distance", "due_to_stigma", 
                       "due_to_poor_roads", "due_to_other_reasons", "no_difficulty"
                     )
                   )
                 )
                 x
               }
              )(),
            nc_recode_pnc(
              vars = c("spc6", "spc6a", "spc6b"), 
              .data = x, 
              prefix = "pnc_child"
            ) |>
              (\(x)
                {
                  names(x)[1:2] <- paste0("pnc_child_", names(x)[1:2])
                  x
                }
              )() |>
              (\(x)
                {
                  x$time_to_postnatal_check_for_child <- ifelse(
                    is.na(x$pnc_child_days_to_pnc), "No response",
                    ifelse(
                      x$pnc_child_days_to_pnc > 0 & 
                        x$pnc_child_days_to_pnc < 3, "Menos de 3 dias",
                      ifelse(
                        x$pnc_child_days_to_pnc >= 3 &
                          x$pnc_child_days_to_pnc <= 6, "De 3 a 6 dias",
                        ifelse(
                          x$pnc_child_days_to_pnc >= 7, "Mais de uma semana", 
                          "Não recebeu cuidados"
                        )
                      )
                    )
                  )
                  x
                }
              )(),
            nc_recode_pnc(
              vars = c("spc7", "spc7a", "spc7b"), 
              .data = x, 
              prefix = "pnc_mother"
            ) |>
              (\(x)
               {
                 names(x)[1:2] <- paste0("pnc_mother_", names(x)[1:2])
                 x
              }
              )() |>
              (\(x)
               {
                 x$time_to_postnatal_check_for_mother <- ifelse(
                   is.na(x$pnc_mother_days_to_pnc), "No response",
                   ifelse(
                     x$pnc_mother_days_to_pnc > 0 & 
                       x$pnc_mother_days_to_pnc < 3, "Menos de 3 dias",
                     ifelse(
                       x$pnc_mother_days_to_pnc >= 3 &
                         x$pnc_mother_days_to_pnc <= 6, "De 3 a 6 dias",
                       ifelse(
                         x$pnc_mother_days_to_pnc >= 7, "Mais de uma semana", 
                         "Não recebeu cuidados"
                       )
                     )
                   )
                 )
                 x
               }
             )(),
            fp_recode_benefit_next(vars = "bs2", .data = x, fill = 1:6) |>
              (\(x)
               {
                 names(x) <- paste0(
                   "benefit_of_waiting_for_next_pregnancy_",
                   c(
                     "less_danger_to_health_of_mother",
                     "less_danger_to_health_of_baby",
                     "avoid_poverty",
                     "more_likely_that_children_are_educated",
                     "other_reasons",
                     "none"
                   )
                 )
                 x
               }
              )(),
            fp_recode_benefit_first(vars = "bs3", .data = x, fill = 1:6) |>
              (\(x)
                {
                  names(x) <- paste0(
                    "benefit_of_waiting_until_18_years_of_age_",
                    c(
                      "less_danger_to_health_of_mother",
                      "less_danger_to_health_of_baby",
                      "avoid_poverty",
                      "more_likely_that_children_are_educated",
                      "other_reasons",
                      "none"
                    )
                  )
                  x
                }
              )(),
            fp_recode_multiparity(vars = "bs4", .data = x, fill = 1:6) |>
              (\(x)
               {
                 names(x) <- paste0(
                   "problem_with_having_more_than_4_children_",
                   c(
                     "maternal_mortality",
                     "child_mortality",
                     "poverty",
                     "more_likely_that_children_are_not_educated",
                     "other_reasons",
                     "none"
                   )
                 )
                 x
               }
              )()
          )
        }
      )() |>
      (\(x)
        {
          data.frame(
            x,
            wdds_recode_groups(
              vars = wdds_map_fg_vars(
                staples = c("nutmul1", "nutmul2"),
                green_leafy = "nutmul10",
                other_vita = c("nutmul11", "nutmul12"),
                fruits_vegetables = c("nutmul13", "nutmul14"),
                organ_meat = "nutmul6",
                meat_fish = c("nutmul7", "nutmul8"),
                eggs = "nutmul9",
                legumes = c("nutmul3", "nutmul4"),
                milk = "nutmul5"
              ),
              .data = x
            ) |>
              wdds_calculate_score(add = TRUE)
          )
        }
      )() |>
      (\(x)
        {
          data.frame(
            x,
            mddw_recode_groups(
              vars = mddw_map_fg_vars(
                staples = c("nutmul1", "nutmul2"),
                pulses = "nutmul3",
                nuts_seeds = "nutmul4",
                milk = "nutmul5",
                meat_fish = c("nutmul6", "nutmul7", "nutmul8"), 
                eggs = "nutmul9", 
                green_leafy = "nutmul10",
                other_vita = c("nutmul11", "nutmul12"),
                vegetables = "nutmul13",
                fruits = "nutmul14"
              ),
              .data = x
            ) |>
            mddw_calculate_score(add = TRUE)
          )
        }
      )() |>
      (\(x)
        {
          play_df <- data.frame(
            x,
            play_recode_responses(
              vars = paste0(
                "play", 
                c(paste0(1, letters[1:7]), 2, paste0(3, letters[1:6]))
              ),
              .data = x,
              na_values = c(8, 9)
            ) |>
            (\(x)
              {
                names(x) <- c(
                  "sing_to_or_with_child", "take_child_for_a_walk", 
                  "play_a_game_with_child", "read_books_or_look_at_photos",
                  "tell_stories_to_child", "identify_objects_with_child",
                  "draw_things_to_or_with_child", "child_has_place_for_toys",
                  "play_with_child_during_bath", "play_with_child_while_feeding_child",
                  "play_with_child_while_changing_clothes", 
                  "play_with_child_while_working_at_home",
                  "play_with_child_while_working_in_the_field",
                  "play_with_child_during_free_time"
                )
                x
              }
            )()
          )
        }
      )() |>
      ## Household dietary diversity score
      (\(x)
        {
          data.frame(
            x,
            hdds_recode_groups(
              vars = hdds_map_fg_vars(
                cereals = "hdds1", 
                tubers = "hdds2", 
                vegetables = c("hdds3", "hdds4", "hdds5"), 
                fruits = c("hdds6", "hdds7"), 
                meat = c("hdds8", "hdds9"), 
                eggs = "hdds10", fish = "hdds11", 
                legumes_seeds = "hdds12", 
                milk = "hdds13", 
                oils_fats = "hdds14", 
                sweets = "hdds15", 
                spices = "hdds16"
              ),
              .data = x
            ) |>
              hdds_calculate_score()
          )
        }
      )() |>
      ## Food consumption score
      (\(x)
        {
          data.frame(
            x,
            fcs_recode_groups(
              vars = fcs_map_fg_vars(
                staples = paste0("fcs", 1:4), 
                pulses = "fcs5", 
                vegetables = "fcs14", 
                fruits = "fcs15", 
                meat_fish = paste0("fcs", c(6, 8:9, 11)), 
                milk = "fcs12", 
                sugar = "fcs16", 
                oil = "fcs10", 
                condiments = paste0("fcs", c(7, 13))
              ),
              .data = x
            ) |>
              fcs_calculate_score(add = TRUE) |>
              (\(x)
                {
                  names(x) <- c(
                    paste0(
                      "fcs_", 
                      c("staples", "pulses", "vegetables",
                        "fruits", "meat_fish", "milk",
                        "sugar", "oil", "condiments")
                    ),
                    "fcs_score"
                  )
                  x
                }
              )(),
            fcs_class = fcs_recode_groups(
              vars = fcs_map_fg_vars(
                staples = paste0("fcs", 1:4), 
                pulses = "fcs5", 
                vegetables = "fcs14", 
                fruits = "fcs15", 
                meat_fish = paste0("fcs", c(6, 8:9, 11)), 
                milk = "fcs12", 
                sugar = "fcs16", 
                oil = "fcs10", 
                condiments = paste0("fcs", c(7, 13))
              ),
              .data = x
            ) |>
              fcs_calculate_score(add = FALSE) |>
              fcs_classify(add = FALSE)
          )
        }
      )() |>
      ## Reduced coping strategy index (rCSI)
      (\(x)
        {
          data.frame(
            x,
            rcsi_score = rcsi_recode_strategies(
              vars = paste0("rcsi", 1:5),
              .data = x,
              na_values = c(88, 99)
            ) |>
              rcsi_calculate_index(add = FALSE),
            rcsi_class = rcsi_recode_strategies(
              vars = paste0("rcsi", 1:5),
              .data = x,
              na_values = c(88, 99)
            ) |>
              rcsi_calculate_index(add = FALSE) |>
              rcsi_classify(add = FALSE)
          )
        }
      )() |>
      ## Livelihoods coping strategy index (LCSI)
      (\(x)
        {
          data.frame(
            x,
            lcsi_recode_strategies(
              vars = c(
                "lcs01", "lcs02", "lcs03", "lcs04", "lcs05", "lcs06", "lcs07", 
                "lcs08", "lcs09", "lcs10", "lcs11", "lcs12", "lcs13", "lcs14"
              ),
              .data = x,
              na_values = c(5, 8, 9)
            ) |>
              lcsi_calculate_index(add = FALSE) |>
              lcsi_classify(add = TRUE) |>
              (\(x) { names(x) <- c("lcsi_score", "lcsi_class"); x })()
          )  
        }
      )() |>
      ## FIES
      (\(x)
        {
          data.frame(
            x,
            fies_recode_responses(
              vars = paste0("fies0", 1:8),
              .data = x
            ) |>
              fies_calculate_score() |>
              (\(x)
                {
                  names(x) <- c(
                    paste0("fies_", 1:8),
                    "fies_score"
                  )
                  x
                }
              )()
          )
        }
      )() |>
      ## observation module
      (\(x)
        {
           data.frame(
             x,
            split_select_multiples(
              x = x$agua2,
              fill = 1:4,
              prefix = "water_storage"
            ) |>
              (\(x) 
                { 
                  names(x) <- c(
                    "water_storage_small_mouth_covered", 
                    "water_storage_small_mouth_uncovered", 
                    "water_storage_wide_mouth", 
                    "water_storage_none"
                  ) 
                  x 
                } 
              )()
           )
        }
      )() |>
      ## PICA
      (\(x)
        {
          data.frame(
            x,
            spread_vector_to_columns(
              x = x$pica2,
              fill = 1:5,
              prefix = "pica"
            ) |>
              (\(x)
                {
                  names(x) <- c(
                    "pica_stop_child", "pica_remove_dirt",
                    "pica_wash_with_water", "pica_wash_with_water_and_soap",
                    "pica_do_nothing"
                  ) 
                  x
                }
              )()
          )
        }
      )()
  }
  
  
clean_child_muac_data <- function(muac, muac1, muac2) {
  muac <- ifelse(
    muac > 100, muac / 10, muac
  )
    
  muac <- ifelse(muac == 1, NA, muac)
  muac <- ifelse(muac == 23.4, muac1, muac)
  muac <- ifelse(muac == 26, NA, muac)
  muac <- ifelse(muac == 27.9, muac1, muac)
  muac <- ifelse(muac == 69.8, muac1, muac)
  muac <- ifelse(muac == 77.1, NA, muac)
  muac <- ifelse(muac == 77.8, muac2, muac)
  muac <- ifelse(muac == 22222, NA, muac)
}
  
clean_woman_muac_data <- function(muac) {
  muac <- ifelse(
    muac > 100 , muac / 10, muac
  )
    
  muac[muac < 10] <- NA
  muac[muac == 60.1] <- NA
}
  
  
create_flagged_child_anthro_data <- function(.data = endline_raw_data) {
  actions <- c(
    "No flags",
    "Check height and age",
    "Check weight and height",
    "Check height",
    "Check weight and age",
    "Check age ",
    "Check weight",
    "Check age, height and weight",
    "Check MUAC and age"
  )
  
  x <- .data |>
    subset(
      select = c(
        today, child_sex, child_birthdate, child_age, 
        position, height, height1, height2,
        weight, weight1, weight2,
        muac, muac1, muac2
      )
    ) |>
    dplyr::mutate(
      child_age_months = calculate_age_child(
        date_of_birth = child_birthdate,
        survey_date = today,
        reported_age_months = child_age,
        age_units = "months"
      ),
      child_age_days = calculate_age_child(
        date_of_birth = child_birthdate,
        survey_date = today,
        reported_age_months = child_age,
        age_units = "days"
      ),
      child_age = child_age,
      child_standing = position,
      child_muac = clean_child_muac_data(muac, muac1, muac2),
      .keep = "unused"
    ) |>
    ## Child anthropometry
    zscorer::addWGSR(
      sex = "child_sex",
      firstPart = "weight",
      secondPart = "child_age_days",
      index = "wfa"
    ) |>
    zscorer::addWGSR(
      sex = "child_sex",
      firstPart = "height",
      secondPart = "child_age_days",
      standing = "child_standing",
      index = "hfa"
    ) |>
    zscorer::addWGSR(
      sex = "child_sex",
      firstPart = "weight",
      secondPart = "height",
      standing = "child_standing",
      index = "wfh"
    ) |>
    zscorer::addWGSR(
      sex = "child_sex",
      firstPart = "child_muac",
      secondPart = "child_age_days",
      index = "mfa"
    ) |>
    dplyr::mutate(
      flag = 0,
      flag = ifelse(hfaz < -6 | hfaz > 6, flag + 1, flag),
      flag = ifelse(wfhz < -5 | wfhz > 5, flag + 2, flag),
      flag = ifelse(wfaz < -6 | wfaz > 5, flag + 4, flag),
      flag = ifelse(mfaz < -6 | mfaz > 5, flag + 8, flag),
      flag_action = actions[flag + 1]
    )
  
  
}
  
create_flagged_woman_anthro_data <- function(.data = endline_raw_data) {
  actions <- c(
    "No flags", "Check height", "Check weight", "Check height and weight", 
    "Check MUAC", "Check height and MUAC", "Check weight and MUAC", 
    "Check height, weight, and MUAC", "Check height and weight"
  )
  
  x <- .data |>
    subset(
      select = c(resp_sex, q01, maltura, mpeso, mbraco)
    ) |>
    dplyr::mutate(
      maltura = ifelse(
        maltura == 0, NA,
        ifelse(
          maltura < 2, maltura * 100, maltura
        )
      ),
      mpeso = ifelse(mpeso == 0, NA, mpeso),
      mbraco = clean_woman_muac_data(mbraco),
      bmi = round(mpeso / (maltura / 100) ^ 2, 2),
      flag = 0,
      flag = ifelse(nipnTK::outliersUV(maltura, fence = 3), flag + 1, flag),
      flag = ifelse(nipnTK::outliersUV(mpeso, fence = 3), flag + 2, flag),
      flag = ifelse(nipnTK::outliersUV(mbraco, fence = 3), flag + 4, flag),
      flag = ifelse(nipnTK::outliersUV(bmi, fence = 3), flag + 8, flag),
      flag_action = actions[flag + 1]
    )
  
  x
}
  
  # outBMI <- ma.anth[outliersUV(x = ma.anth$BMI, fence = 3), ]
  # 
  # table(outliersUV(x = ma.anth$BMI, fence = 3))
  # prop.table(table(outliersUV(x = ma.anth$BMI, fence = 3)))  * 100
  # 
  # outBMI <- outBMI[with(outBMI, order(BMI)), ]
  # 
  # (outBMI)
  # ma.anth$height <- ifelse(ma.anth$height < 2, ma.anth$height * 100, ma.anth$height)
  # 
  # ## Univariate outliers
  # outWeight <- ma.anth[outliersUV(x = ma.anth$weight), ]
  # 
  # outHeight <- ma.anth[outliersUV(x = ma.anth$height, fence = 3), ]
  # outHeight <- outHeight[with(outHeight, order(height)), ]
  # ## Bottom half
  # head(outHeight,n = trunc(nrow(outHeight) / 2))
  # ## Top health
  # tail(outHeight,n = trunc(nrow(outHeight) / 2))
  # 
  # ma.anth$height <- ifelse(ma.anth$height < 2, ma.anth$height * 100, ma.anth)
  
  
  
  
  
clean_anthro_data <- function(.data) {
  ## Clean up child anthro
  .data[  54, "height"] <- .data$height1[54]
  .data[  75, "height"] <- .data$height1[75]
  .data[  78, "height"] <- .data$height1[78]
  .data[ 133, "height"] <- .data$height1[133]
  .data[ 148, "weight"] <- .data$weight[148] / 100
  .data[ 159, "height"] <- .data$height1[159]
  .data[ 302, "height"] <- .data$height1[302]
  .data[ 303, "height"] <- .data$height1[303]
  .data[ 304, "height"] <- .data$height1[304]
  .data[ 306, "height"] <- .data$height1[306]
  .data[ 309, "height"] <- .data$height1[309]
  .data[ 403, "height"] <- .data$height1[403]
  
  .data[ 440, "height"] <- .data$height1[440]
  .data[ 440, "weight"] <- .data$weight1[440]
  
  .data[ 473, "height"] <- .data$height1[473]
  .data[ 527, "height"] <- .data$height1[527]
  
  .data[ 539, "height"] <- .data$height1[539]
  .data[ 539, "weight"] <- .data$weight1[539]
  
  .data[ 540, "height"] <- .data$height[540]
  .data[ 540, "weight"] <- .data$weight[540]
  
  .data[ 658, "height"] <- .data$height1[658]
  
  .data[ 749, "height"] <- .data$height1[749]
  .data[1040, "height"] <- .data$height1[1040]
  
  .data[1047, "height"] <- .data$height1[1047]
  
  .data[1055, "height"] <- .data$height1[1055]
  .data[1055, "weight"] <- .data$weight1[1055]
  
  .data[1057, "height"] <- .data$height1[1057]
  .data[1057, "weight"] <- .data$weight1[1057]
  
  
  .data[1254, "height"] <- .data$height1[1254]
  .data[1254, "weight"] <- .data$weight1[1254]
  
  .data[1274, "height"] <- .data$height1[1274]
  
  .data[1283, "height"] <- .data$height1[1283]
  
  .data[1288, "height"] <- .data$height1[1288]
  
  .data[1291, "height"] <- .data$height1[1291]
  
  .data[1327, "weight"] <- .data$weight1[1327]
  
  .data[1365, "height"] <- .data$height1[1365]
  
  .data[1383, "height"] <- .data$height1[1383]
  .data[1383, "weight"] <- .data$weight1[1383]
  
  .data[1446, "height"] <- .data$height1[1446]
  
  .data[1455, "height"] <- .data$height1[1455]
  
  .data[1468, "height"] <- .data$height1[1468]
  .data[1468, "weight"] <- .data$weight1[1468]
  
  .data[1497, "height"] <- .data$height1[1497]
  
  .data[1825, "height"] <- .data$height1[1825]
  .data[1825, "weight"] <- .data$weight1[1825]
  
  .data[1949, "height"] <- .data$height1[1949]
  .data[1949, "weight"] <- .data$weight1[1949]
  
  .data[2046, "height"] <- .data$height1[2046]
  
  .data[2154, "height"] <- .data$height1[2154]
  
  .data[2158, "height"] <- .data$height1[2158]
  .data[2158, "weight"] <- .data$weight1[2158]
  
  .data[2162, "height"] <- .data$height1[2162]
  .data[2162, "weight"] <- .data$weight1[2162]
  
  .data[2170, "height"] <- .data$height1[2170]
  .data[2170, "weight"] <- .data$weight1[2170]
  
  .data[2171, "height"] <- .data$height1[2171]
  .data[2171, "weight"] <- .data$weight1[2171]
  
  .data[2186, "height"] <- .data$height1[2186]
  
  .data[2188, "height"] <- .data$height1[2188]
  
  .data[2412, "height"] <- .data$height[2412]
  .data[2412, "weight"] <- .data$weight[2412]
  
  .data[2456, "height"] <- .data$height1[2456]
  
  .data[2657, "height"] <- .data$height1[2657]
  .data[2657, "weight"] <- .data$weight1[2657]
  
  .data[2750, "weight"] <- .data$weight1[2750]
  
  .data[2852, "height"] <- .data$height1[2852]
  
  .data[2894, "height"] <- .data$height1[2894]
  
  .data[2971, "height"] <- .data$height1[2971]
  
  .data[2972, "height"] <- .data$height1[2972]
  
  .data[2981, "height"] <- .data$height1[2981]
  .data[2981, "weight"] <- .data$weight1[2981]
  
  .data[3030, "height"] <- .data$height1[3030]
  .data[3030, "weight"] <- .data$weight1[3030]
  
  .data[3061, "height"] <- .data$height1[3061]
  .data[3061, "weight"] <- .data$weight1[3061]
  
  .data[3137, "height"] <- .data$height1[3137]
  
  .data[3140, "height"] <- .data$height1[3140]
  
  .data[3225, "height"] <- .data$height1[3225]
  
  .data[3278, "height"] <- .data$height1[3278]
  
  .data[3356, "height"] <- .data$height1[3356]
  .data[3356, "weight"] <- .data$weight1[3356]
  
  .data[3437, "height"] <- .data$height1[3437]
  
  .data[3453, "height"] <- .data$height[3453]
  
  .data[3483, "height"] <- .data$height1[3483]
  
  .data[3596, "height"] <- .data$height1[3596]
  
  .data[3615, "height"] <- .data$height1[3615]
  
  .data[3751, "height"] <- .data$height1[3751]
  
  .data[4030, "height"] <- .data$height1[4030]
  
  .data[4304, "weight"] <- .data$weight1[4304]
  
  .data[4480, "height"] <- .data$height[4480]
  
  .data[4550, "height"] <- .data$height1[4550]
  
  .data[4708, "height"] <- .data$height[4708]
  
  .data[4805, "height"] <- .data$height1[4805]
  
  .data[4916, "height"] <- .data$height1[4916]
  
  .data[5026, "height"] <- .data$height1[5026]
  
  .data[5032, "height"] <- .data$height1[5032]
  
  .data[5120, "weight"] <- .data$weight1[5120]
  
  .data[5173, "weight"] <- .data$weight1[5173]
  
  .data[5387, "height"] <- .data$height1[5387]
  
  .data[5412, "height"] <- .data$height1[5412]
  
  .data[5418, "height"] <- .data$height1[5418]
  
  .data[5559, "height"] <- .data$height1[5559]
 
  ## Clean up mother anthro
  .data[   8, "maltura"] <- 159.0
  .data[  61, "maltura"] <- 142.0
  .data[  64, "mpeso"]   <- 49.8
  .data[  66, "maltura"] <- 108.0
  .data[  67, "maltura"] <- 105.0
  .data[  70, "maltura"] <- 158.0
  .data[ 183, "maltura"] <- 158.0
  .data[ 204, "maltura"] <- 147.2
  .data[ 260, "maltura"] <- 156.0
  .data[ 261, "maltura"] <- 156.0
  .data[ 272, "maltura"] <- 150.2
  .data[ 498, "maltura"] <- 99.6
  .data[ 688, "maltura"] <- 143.0
  .data[ 698, "maltura"] <- NA
  .data[ 698, "mpeso"]   <- NA
  .data[1045, "maltura"] <- 152.9
  .data[1045, "mpeso"]   <- 53.2
  .data[1131, "mpeso"]   <- 84.5
  .data[1454, "maltura"] <- 156.7
  .data[1575, "maltura"] <- 183.8
  .data[1576, "maltura"] <- 183.8
  .data[1792, "maltura"] <- 145.3
  .data[1820, "maltura"] <- 153.5
  .data[1893, "maltura"] <- NA
  .data[1893, "mpeso"]   <- NA
  .data[1945, "maltura"] <- NA
  .data[1998, "maltura"] <- 152.3
  .data[2321, "maltura"] <- 126.0
  .data[2720, "maltura"] <- 176.9
  .data[2931, "maltura"] <- 156.0
  .data[2947, "maltura"] <- 154.1
  .data[2978, "maltura"] <- 161.2
  .data[3132, "maltura"] <- 156.0
  .data[3133, "maltura"] <- 156.0
  .data[3147, "maltura"] <- 163.1
  .data[3147, "mpeso"]   <- 55.2
  .data[3148, "maltura"] <- 163.1
  .data[3148, "mpeso"]   <- 55.2
  .data[3251, "maltura"] <- 147.6
  .data[3252, "maltura"] <- 147.6
  .data[3280, "maltura"] <- 161.8
  .data[3891, "maltura"] <- 165.0
  .data[3892, "maltura"] <- 165.0
  .data[4434, "mpeso"]   <- 43.8
  .data[4485, "maltura"] <- 156.6
  .data[4607, "maltura"] <- 159.2
  .data[4608, "maltura"] <- 159.2
  .data[5250, "maltura"] <- 137.2
  .data[5329, "maltura"]   <- NA
  .data[5567, "maltura"] <- NA
  .data[5568, "maltura"] <- NA

  .data
}  