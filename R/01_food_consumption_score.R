#'
#' F
#' 

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
#' fcs_recode_group(fcs01$FCSStap)
#' 
#' @export
#'     

fcs_recode_group <- function(x, na_values = NA) {
  ## Recode NA values to NA ----
  x <- ifelse(x %in% na_values, NA, x)

  ## Recode x to 0 if NA ----
  x <- ifelse(is.na(x), 0, x)

  ## Recode x if x > 7 ----
  x <- ifelse(x > 7, 7, x)

  ## Return x ----
  x
}


#'
#' Map Food Consumption Score data variables to corresponding FCS food groups
#' 
#' @param staples,pulses,vegetables,fruits,meat_fish,milk,sugar,oil,condiments
#'   A character value or vector of values for variables name/s for 
#'   corresponding food groups.
#' 
#' @returns A named list of variable names for corresponding food groups.
#' 
#' @examples
#' vars <- paste0(
#'   "FCS", 
#'   c("Stap", "Pulse", "Veg", "Fruit", "Pr", "Dairy", "Sugar", "Fat", "Cond")
#' ) |>
#'   as.list()
#' 
#' do.call(fcs_map_fg_vars, vars) 
#' 
#' @export
#' 

fcs_map_fg_vars <- function(staples, 
                            pulses, 
                            vegetables,
                            fruits, 
                            meat_fish, 
                            milk,
                            sugar, 
                            oil, 
                            condiments) {
  list(
    staples = staples,
    pulses = pulses,
    vegetables = vegetables,
    fruits = fruits,
    meat_fish = meat_fish,
    milk = milk,
    sugar = sugar,
    oil = oil,
    condiments = condiments
  )
}


#'
#' Calculate Food Consumption Score (FCS)
#'
#' @param fg_df A data.frame with FCS data.
#' @param var_map A named list of FCS food groups mapped to corresponding 
#'   variable names in `fg_df`. This can be produced using [fcs_map_fg_vars()].
#' @param weights A numeric vector of FCS weights applied to corresponding food
#'   groups. The weights should be ordered as that for staples, pulses,
#'   vegetables, fruits, meat and fish, dairy, sugar, oil, and condiments.
#'   Default to NULL which uses the weights based on current FCS 
#'   recommendations. Only change this if new recommendations have been provided
#'   or for testing/studying new/experimental FCS weighting systems.
#' @param add Logical. Should the resulting FCS scores be added to `fg_df`? 
#'   Default to TRUE.
#' 
#' @returns If `add = TRUE`, a data.frame based on `fg_df` with a new variable
#'   named `fcs` for the calculated food consumption scores. Otherwise, a
#'   numeric vector of the calculated food consumption scores.
#' 
#' @examples
#' vars <- paste0(
#'   "FCS", 
#'   c("Stap", "Pulse", "Veg", "Fruit", "Pr", "Dairy", "Sugar", "Fat", "Cond")
#' ) |>
#'   as.list()
#' 
#' var_map <- do.call(fcs_map_fg_vars, vars) 
#' 
#' fcs_calculate_score(fg_df = fcs01, var_map = var_map)
#'
#' @author Ernest Guevarra
#' 
#' @export
#' 

fcs_calculate_score <- function(fg_df, 
                                var_map,  
                                weights = NULL,
                                add = TRUE) {
  ## Determine weights to use ----
  if (is.null(weights)) weights <- c(2, 3, 1, 1, 4, 4, 0.5, 0.5, 0)

  ## Recode food groups
  fg_df_recoded <- fg_df[ , unlist(var_map)] |>
    apply(MARGIN = 2, FUN = fcs_recode_group, simplify = TRUE)
  
  ## Create a weights matrix compatible to fg_df ----
  weights_matrix <- lapply(
    X = weights,
    FUN = rep,
    times = nrow(fg_df)
  ) |>
    stats::setNames(paste0("w", seq_len(length(weights)))) |>
    do.call(cbind, args = _)

  ## Weight food consumption ----
  fg_df_weighted <- fg_df_recoded * weights_matrix
  
  ## Calculate FCS ----
  fcs <- rowSums(fg_df_weighted, na.rm = TRUE)
  
  ## Should score be added to fg_df_recoded? ----
  if (add) {
    fcs <- data.frame(fg_df_recoded, fcs = fcs)
  } else {
    fcs <- fcs
  }

  ## Return fcs ----
  fcs
}


#'
#' Classify Food Consumption Score
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
#' vars <- paste0(
#'   "FCS", 
#'   c("Stap", "Pulse", "Veg", "Fruit", "Pr", "Dairy", "Sugar", "Fat", "Cond")
#' ) |>
#'   as.list()
#' 
#' var_map <- do.call(fcs_map_fg_vars, vars) 
#' 
#' fcs <- fcs_calculate_score(fg_df = fcs01, var_map = var_map)
#' 
#' fcs_classify(fcs$fcs)
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
#' Food Consumption Score colours
#' 

fcs_colours <- c(
  acceptable = "#ECE1B1", borderline = "#E67536", poor = "#D70000"
)