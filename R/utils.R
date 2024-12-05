#'
#' Convert character vector of categorical responses into unique variables
#' 
#' Function transforms a vector of categorical responses into `n` number of
#' new columns/variables equal to the number of unique categorical values.
#' 
#' @param x Vector of categorical values.
#' @param fill Vector of all possible unique categorical values for `x`.
#' @param na_rm Logical. Should NA values in `x` be included as a category?
#'   Default to FALSE.
#' @param prefix A character string to prepend to the names of the new columns
#'   to be created
#'
#' @returns A data.frame with number of rows equal to the length of `x`. If
#'   `fill` is not NULL, number of columns is equal to length of `fill` plus
#'   one if `na_rm = TRUE`. Otherwise, number of columns is equal to length
#'   of unique categorical values in `x` plus one if `na_rm = TRUE`. Variable 
#'   names of output data.frame is a concatenation of the `prefix` and the 
#'   unique categorical values in `x` or the values in `fill` if `fill = TRUE`.  
#'
#' @keywords internal
#' 

spread_vector_to_columns <- function(x, fill = NULL, na_rm = FALSE, prefix) {
  values <- sort(unique(x), na.last = NA)
  
  if (!is.null(fill)) {
    values <- c(values, fill[!fill %in% values]) |>
      sort(na.last = NA)
  }
  
  if (na_rm) values <- c(values, NA)
  
  values <- gsub(pattern = " ", x = values, replacement = "_")
  
  col_names <- paste(prefix, values, sep = "_")
  
  lapply(
    X = x,
    FUN = function(x, y) ifelse(x == y, 1, 0),
    y = values
  ) |>
    (\(x) do.call(rbind, x))() |>
    data.frame() |>
    stats::setNames(col_names)
}
