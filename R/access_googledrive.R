################################################################################
#
#'
#' Download files from Google Drive
#'
#
################################################################################

download_googledrive <- function(filename, 
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


read_googlesheet <- function(filename) {
  ## Authenticate
  # googledrive::drive_auth(
  #   email = Sys.getenv("GOOGLE_AUTH_EMAIL"),
  #   path = Sys.getenv("GOOGLE_AUTH_FILE")
  # )
  
  ## Authenticate
  # googlesheets4::gs4_auth(
  #   email = Sys.getenv("GOOGLE_AUTH_EMAIL"),
  #   path = Sys.getenv("GOOGLE_AUTH_FILE")
  # )
  
  ##
  file_id <- googlesheets4::gs4_find() |>
    subset(name == filename) |>
    (\(x) x$id)()
  
  ##
  googlesheets4::read_sheet(ss = file_id)
}
