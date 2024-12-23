% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_food_consumption_score.R
\name{fcs_classify}
\alias{fcs_classify}
\title{Classify Food Consumption Score (FCS)}
\usage{
fcs_classify(fcs, cutoff = NULL, add = FALSE, spread = FALSE)
}
\arguments{
\item{fcs}{A vector of food consumption scores.}

\item{cutoff}{A numeric vector of length 2 for the cut-offs to use for
classifying FCS. Default to NULL in which case standard recommended cut-off
values for FCS are used.}

\item{add}{Logical. Should classification be column bound to fcs? Default
to FALSE.}

\item{spread}{Logical. Should classification be spread into columns?
Default to FALSE.}
}
\value{
If \code{spread = TRUE}, a data.frame with number of rows equal to the
length of \code{fcs} and number of columns equal to length of \code{fill} plus an
initial column named \code{fcs} containing the FCS values provided by \code{fcs}
argument if \code{add = TRUE}. Otherwise, a vector of class factor containing
FCS classifications. If \code{add = TRUE}, this vector is concatenated with the
\code{fcs} values in a data.frame.
}
\description{
Classify Food Consumption Score (FCS)
}
\examples{
var_map <- fcs_fg_map_variables(
  staples = "FCSStap",
  pulses = "FCSPulse",
  vegetables = "FCSVeg",
  fruits = "FCSFruit",
  meatfish = "FCSPr",
  milk = "FCSDairy",
  sugar = "FCSSugar",
  oil = "FCSFat",
  condiment = "FCSCond"
) 

fcs <- fcs_calculate(df = fcs01, var_map = var_map)

fcs_classify(fcs$fcs)

}
\author{
Ernest Guevarra
}
