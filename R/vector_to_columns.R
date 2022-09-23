################################################################################
#
#'
#' Convert character vector of categorical responses into unique variables
#' 
#' Function transforms a vector of categorical responses into `n` number of
#' new columns/variables equal to the number of unique categorical values.
#' 
#' @param x Vector of categorical values 
#' @param prefix A character string to prepend to the names of the new columns
#'   to be created
#' 
#'  
#'
#
################################################################################

# spread_vector_to_columns <- function(x, fill = NULL, na_rm = FALSE, prefix) {
#   values <- sort(unique(x), na.last = NA)
#   
#   if (!is.null(fill)) {
#     values <- c(values, fill[!fill %in% values]) |>
#       sort(na.last = NA)
#   }
#   
#   if (na_rm) {
#     values <- c(values, NA_integer_)
#   }
#   
#   values <- values |>
#     stringr::str_replace_all(
#       pattern = " ", replacement = "_"
#     )
#   
#   col_names <- paste(prefix, values, sep = "_")
#   
#   lapply(
#     X = x,
#     FUN = function(x, y) ifelse(x == y, 1, 0),
#     y = values
#   ) |>
#     (\(x) do.call(rbind, x))() |>
#     data.frame() |>
#     (\(x) { names(x) <- col_names; x })()
# }

