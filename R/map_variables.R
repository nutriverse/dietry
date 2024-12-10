#'
#' Map data variables to corresponding indicators
#' 
#' @param ... Name-value pairs. Name gives the labels for indicators. The value
#'   should be the corresponding variable name in a dataset used for that
#'   indicator.
#' @param foodgroups A character vector of food group labels for a specific
#'   dietary intake indicator set.
#' 
#' @returns A named list of variable name/s for corresponding food groups.
#' 
#' @examples 
#' ## Variable names in fcs01 mapped to corresponding food group labels
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
                                 foodgroups = c("staples", "pulses", 
                                                "vegetables", "fruits", 
                                                "meatfish", "milk", "sugar",
                                                "oil", "condiment")) {
  ## Create variables map ----
  var_map <- map_variables(...)

  ## Check that variables map names are correct ----
  missing_foodgroups <- foodgroups[!foodgroups %in% names(var_map)]

  if (length(missing_foodgroups) > 0)
    stop (
      "Variable map is missing values for ",
      paste(missing_foodgroups, collapse = ", "), 
      ". Please check that you have specified variables for all ",
      "FCS food groups or that you have spelled the FCS food groups correctly."
    )
  
  ## Return var_map ----
  var_map
}


#'
#' @rdname map_variables
#' @export
#' 

hdds_fg_map_variables <- function(...,
                                  foodgroups = c("cereals", "roots_tubers",
                                                 "vegetables", "fruits",
                                                 "meat", "eggs", "fish",
                                                 "pulses", "milk", "oil",
                                                 "sugar", "condiments")) {
  ## Create variables map ----
  var_map <- map_variables(...)

  ## Check that variables map names are correct ----
  missing_foodgroups <- foodgroups[!foodgroups %in% names(var_map)]

  if (length(missing_foodgroups) > 0)
    stop (
      "Variable map is missing values for ",
      paste(missing_foodgroups, collapse = ", "), 
      ". Please check that you have specified variables for all ",
      "HDDS food groups or that you have spelled the HDDS food groups correctly."
    )
  
  ## Return var_map ----
  var_map
}


#'
#' @rdname map_variables
#' @export
#' 

mddw_fg_map_variables <- function(...,
                                  foodgroups = c("staples", "pulses", "nuts",
                                                 "milk", "meat_fish", "eggs",
                                                 "green_leafy", "other_vita",
                                                 "other_vegetables", 
                                                 "other_fruits")) {
    ## Create variables map ----
    var_map <- map_variables(...)

    ## Check that variables map names are correct ----
    missing_foodgroups <- foodgroups[!foodgroups %in% names(var_map)]
  
    if (length(missing_foodgroups) > 0)
      stop (
        "Variable map is missing values for ",
        paste(missing_foodgroups, collapse = ", "), 
        ". Please check that you have specified variables for all ",
        "HDDS food groups or that you have spelled the HDDS food groups correctly."
      )
    
    ## Return var_map ----
    var_map
}