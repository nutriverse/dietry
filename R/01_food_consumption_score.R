#' 
#' Recode Food Consumption Score (FCS) data
#'     
#' @param x A vector of numeric values that can range from 0 to 7 for the number
#'   of days in a week that a food group is eaten by a household as per FCS 
#'   guidelines.
#' @param na_values A value or a vector of values that are to be considered as
#'   NA. Default to NA.
#' 
#' @returns An integer vector with possible values ranging from 0 to 7.
#' 
#' @examples
#' fcs_recode(fcs01$FCSStap)
#' 
#' @author Ernest Guevarra
#' 
#' @export
#'     

fcs_recode <- function(x, na_values = NULL) {
  ## Recode NA values to NA ----
  if (!is.null(na_values)) x[x %in% na_values] <- NA_integer_

  ## Recode x to 0 if NA ----
  x[is.na(x)] <- 0L

  ## Recode x if x > 7 ----
  x <- ifelse(x > 7L, 7L, x)
  x[x > 7L] <- 7L

  ## Set x as integer ----
  x <- as.integer(x)

  ## Return x ----
  x
}


#'
#' Calculate Food Consumption Score (FCS)
#'
#' @param df A data.frame with FCS data.
#' @param var_map A named list of FCS food groups mapped to corresponding 
#'   variable names in `df`. This can be produced using 
#' [fcs_fg_map_variables()].
#' @param weights A numeric vector of FCS weights applied to corresponding food
#'   groups. The weights should be ordered as that for staples, pulses,
#'   vegetables, fruits, meat and fish, dairy, sugar, oil, and condiments.
#'   Default to NULL which uses the weights based on current FCS 
#'   recommendations. Only change this if new recommendations have been provided
#'   or for testing/studying new/experimental FCS weighting systems.
#' @param add Logical. Should the resulting FCS scores be added to `df`? 
#'   Default to TRUE.
#' 
#' @returns If `add = TRUE`, a data.frame based on `df` with a new variable
#'   named `fcs` for the calculated food consumption scores. Otherwise, a
#'   numeric vector of the calculated food consumption scores.
#' 
#' @examples
#' var_map <- fcs_fg_map_variables(
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
#' fcs_calculate(df = fcs01, var_map = var_map)
#'
#' @author Ernest Guevarra
#' 
#' @export
#' 

fcs_calculate <- function(df, 
                          var_map,  
                          weights = NULL,
                          add = TRUE) {
  ## Determine weights to use ----
  if (is.null(weights)) weights <- c(2, 3, 1, 1, 4, 4, 0.5, 0.5, 0)

  ## Recode food groups
  df_recoded <- df[ , unlist(var_map)] |>
    apply(MARGIN = 2, FUN = fcs_recode, simplify = TRUE)
  
  ## Create a weights matrix compatible to fg_df ----
  weights_matrix <- lapply(
    X = weights,
    FUN = rep,
    times = nrow(df)
  ) |>
    stats::setNames(paste0("w", seq_len(length(weights)))) |>
    do.call(cbind, args = _)

  ## Weight food consumption ----
  df_weighted <- df_recoded * weights_matrix
  
  ## Calculate FCS ----
  fcs <- rowSums(df_weighted, na.rm = TRUE)
  
  ## Should score be added to fg_df_recoded? ----
  if (add) {
    fcs <- data.frame(df_recoded, fcs = fcs)
  } else {
    fcs <- fcs
  }

  ## Return fcs ----
  fcs
}


#'
#' Classify Food Consumption Score (FCS)
#' 
#' @param fcs A vector of food consumption scores.
#' @param cutoff A numeric vector of length 2 for the cut-offs to use for
#'   classifying FCS. Default to NULL in which case standard recommended cut-off
#'   values for FCS are used.
#' @param add Logical. Should classification be column bound to fcs? Default
#'   to FALSE.
#' @param spread Logical. Should classification be spread into columns?
#'   Default to FALSE.
#' 
#' @returns If `spread = TRUE`, a data.frame with number of rows equal to the 
#'   length of `fcs` and number of columns equal to length of `fill` plus an
#'   initial column named `fcs` containing the FCS values provided by `fcs`
#'   argument if `add = TRUE`. Otherwise, a vector of class factor containing
#'   FCS classifications. If `add = TRUE`, this vector is concatenated with the
#'   `fcs` values in a data.frame.
#' 
#' @examples
#' var_map <- fcs_fg_map_variables(
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
#' fcs <- fcs_calculate(df = fcs01, var_map = var_map)
#' 
#' fcs_classify(fcs$fcs)
#'
#' @author Ernest Guevarra
#' 
#' @export
#' 

fcs_classify <- function(fcs, cutoff = NULL, add = FALSE, spread = FALSE) {
  ## Set cut-offs ----
  if (is.null(cutoff)) cutoff <- c(21, 35)

  ## Classify FCS ----
  fcs_class <- cut(
    x = fcs,
    breaks = c(0, cutoff[1], cutoff[2], 112),
    labels = c("poor", "borderline", "acceptable"),
    include.lowest = FALSE, right = TRUE
  )
  
  ## Should classifications be spread to columns ----
  if (spread) {
    fcs_class <- data.frame(
      fcs_class = fcs_class,
      spread_vector_to_columns(
        x = fcs_class,
        fill = c("poor", "borderline", "acceptable"), 
        prefix = "fcs"
      )
    )
  }
  
  ## Should classifications be concatenated with fcs ----
  if (add) fcs_class <- data.frame(fcs, fcs_class)
  
  ## Return fcs_class ----
  fcs_class
}


#'
#' Get Food Consumption Score (FCS) colours
#' 
#' @returns A named character vector of recommended FCS classification colours
#' 
#' @examples
#' fcs_get_colours()
#'
#' @author Ernest Guevarra
#'  
#' @export
#' 

fcs_get_colours <- function() {
  c(acceptable = "#ECE1B1", borderline = "#E67536", poor = "#D70000")
}
