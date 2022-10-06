################################################################################
#
#'
#' Add a worksheet to workbook
#'
#
################################################################################

add_worksheet <- function(wb, sheet, x) {
  openxlsx::addWorksheet(wb = wb, sheetName = sheet)
  
  openxlsx::writeData(
    wb = wb,
    sheet = sheet,
    x = x
  )
}


add_worksheets <- function(wb, sheet, x) {
  Map(
    f = add_worksheet,
    wb = wb,
    sheet = sheet,
    x = x
  )
}


create_results_spreadsheet <- function(sheet, x, filename) {
  wb <- openxlsx::createWorkbook()
  
  add_worksheets(
    wb = rep(list(wb), length(sheet)),
    sheet = sheet,
    x = x
  )
  
  Map(
    f = openxlsx::setColWidths,
    wb = rep(list(wb), length(sheet)),
    sheet = sheet,
    cols = rep(list(seq_len(ncol(x[[1]]))), length(sheet)),
    widths = "auto"
  )
  
  openxlsx::saveWorkbook(
    wb = wb,
    file = filename,
    overwrite = TRUE
  )
  
  filename
}
  
  