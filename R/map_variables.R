#'
#' Map data variables to corresponding indicators
#' 
#' @param ... Name-value pairs. Name gives the labels for indicators. The value
#'   should be the corresponding variable name in a dataset used for that
#'   indicator.
#' @param indicators A character vector of indicator labels for a specific
#'   dietary intake indicator set.
#' 
#' @returns A named list of variable name/s for corresponding indicators.
#' 
#' @examples 
#' ## Variable names in fcs01 mapped to corresponding food group indicators
#' map_variables(
#'   staples = "FCSStap",
#'   pulses = "FCSPulse",
#'   vegetables = "FCSVeg",
#'   fruits = "FCSFruit",
#'   meatfish = "FCSPr",
#'   milk = "FCSDairy",
#'   sugar = "FCSSugar",
#'   oil = "FCSFat",
#'   condiment = "FCSCond"
#' )
#' 
#' @rdname map_variables
#' @export
#'  

map_variables <- function(...) {
  list(...)
}

#'
#' @rdname map_variables
#' @export 
#' 

fcs_fg_map_variables <- function(..., 
                                 indicators = c("staples", "pulses", 
                                                "vegetables", "fruits", 
                                                "meatfish", "milk", "sugar",
                                                "oil", "condiment")) {
  ## Create variables map ----
  var_map <- map_variables(...)

  ## Check that variables map names are correct ----
  missing_indicators <- indicators[!indicators %in% names(var_map)]

  if (length(missing_indicators) > 0)
    stop (
      "Variable map is missing values for ",
      paste(missing_indicators, collapse = ", "), 
      ". Please check that you have specified variables for all ",
      "FCS indicators or that you have spelled the FCS indicators correctly."
    )
  
  ## Return var_map ----
  var_map
}