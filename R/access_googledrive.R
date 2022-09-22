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