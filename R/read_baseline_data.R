################################################################################
#
#'
#' Get specified dataset
#' 
#' @param filename Filename, with extension, of file to download from Google
#'   Drive.
#' @param path Character. Path for output file. If absent, the default file name 
#'   is the file's name on Google Drive and the default location is 
#'   working directory, possibly with an added file extension.
#' @param overwrite Logical. If local path already exists, do you want to
#'   overwrite it?
#'   
#' @return Files downloaded in the specified path.
#' 
#'
#
################################################################################

download_baseline_data <- function(filename, 
                                   path = paste0("data/", filename),
                                   overwrite = FALSE) {
  ## Authenticate
  googledrive::drive_auth(
    email = Sys.getenv("GOOGLE_AUTH_EMAIL"),
    path = Sys.getenv("GOOGLE_AUTH_FILE")
  )
  
  ## 
  googledrive::drive_download(
    file = filename,
    path = path,
    overwrite = overwrite
  )
}


################################################################################
#
#'
#' Download all baseline data
#' 
#' @param pattern A regex for files to search for to download
#' @param overwrite Logical. Should files be overwritten if they have already
#'   been downloaded? Default is FALSE
#' 
#' @return A tibble containing downloaded file information including local path
#'   to which desired file has been downloaded to
#'   
#'
#
################################################################################

download_all_baseline_data <- function(pattern = "\\.do$|\\.sav|\\.dta|anthrofin|dataset.csv|README.txt",
                                       overwrite = FALSE) {
  ## Authenticate
  googledrive::drive_auth(
    email = Sys.getenv("GOOGLE_AUTH_EMAIL"),
    path = Sys.getenv("GOOGLE_AUTH_FILE")
  )
  
  googledrive::drive_find(pattern = pattern) |> 
    dplyr::select(name) |> 
    c() |> 
    unlist() |>
    lapply(FUN = download_baseline_data, overwrite = overwrite) |>
    dplyr::bind_rows()
}


################################################################################
#
#'
#' Read SPSS data
#' 
#' @param file_list A tibble downloaded file information including local path
#'   to which files has been downloaded to created when using the
#'   `download_all_baseline_data` function
#'
#' @return A tibble of SPSS data
#'
#
################################################################################

read_spss_data <- function(file_list = data_download) {
  ## Get local paths for SPSS data
  spss_file <- file_list |>
    dplyr::select(local_path) |>
    (\(x) x$local_path)() |>
    (\(x) x[stringr::str_detect(string = x, pattern = ".sav")])()
    
  ## Read SPSS file
  haven::read_spss(file = spss_file)
}


################################################################################
#
#'
#' Read Stata data
#'
#' @param file_list A tibble downloaded file information including local path
#'   to which files has been downloaded to created when using the
#'   `download_all_baseline_data` function
#' @param filename Filename of file to read. This can either be the full
#'   filename or a unique search pattern that identifies the file
#'
#' @return A tibble of Stata data
#'
#
################################################################################

read_stata_data <- function(file_list = data_download, filename) {
  ## Get local paths for SPSS data
  stata_file <- file_list |>
    dplyr::select(local_path) |>
    (\(x) x$local_path)() |>
    (\(x) x[stringr::str_detect(string = x, pattern = filename)])()
  
  ## Read SPSS file
  haven::read_stata(file = stata_file)
}


################################################################################
#
#'
#' Read CSV data
#' 
#' @param file_list A tibble downloaded file information including local path
#'   to which files has been downloaded to created when using the
#'   `download_all_baseline_data` function
#' @param filename Filename of file to read. This can either be the full
#'   filename or a unique search pattern that identifies the file
#'
#' @return A tibble of CSV data
#' 
#
################################################################################

read_csv_data <- function(file_list = data_download, 
                          filename, 
                          fileEncoding = "") {
  ## Get local paths for CSV data
  csv_file <- file_list |>
    dplyr::select(local_path) |>
    (\(x) x$local_path)() |>
    (\(x) x[stringr::str_detect(string = x, pattern = filename)])()
  
  ## Read CSV file
  read.csv(file = csv_file, fileEncoding = fileEncoding) |>
    tibble::tibble()
}


################################################################################
#
#'
#' Read text and code data
#' 
#' @param file_list A tibble downloaded file information including local path
#'   to which files has been downloaded to created when using the
#'   `download_all_baseline_data` function
#' @param filename Filename of file to read. This can either be the full
#'   filename or a unique search pattern that identifies the file
#'
#'
#
################################################################################

read_text_data <- function(file_list = data_download, filename, widths = 80) {
  ## Get local paths for text data
  text_file <- file_list |>
    dplyr::select(local_path) |>
    (\(x) x$local_path)() |>
    (\(x) x[stringr::str_detect(string = x, pattern = filename)])()
  
  ## Read text file and format
  read.fwf(file = text_file, widths = widths) |>
    tibble::tibble()
}