## Load .env if present
if (file.exists(".env")) {
  try(readRenviron(".env"), silent = TRUE)
}


if (Sys.info()[['sysname']] %in% c('Linux', 'Windows')) {
  options(
    repos = c(RSPM = "https://packagemanager.rstudio.com/all/latest")
  )
} else {
  ## For Mac users, we'll default to installing from CRAN/MRAN instead, since
  ## RSPM does not yet support Mac binaries.
  options(
    repos = c(CRAN = "https://cran.rstudio.com/"), pkgType = "both"
  )
}


options(
  renv.config.repos.override = getOption("repos"),
  renv.config.auto.snapshot = FALSE, 
  renv.config.rspm.enabled = TRUE, 
  renv.config.install.shortcuts = TRUE, 
  renv.config.cache.enabled = TRUE
)


source("renv/activate.R")
