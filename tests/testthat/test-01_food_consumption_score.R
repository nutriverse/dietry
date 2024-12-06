# Tests for FCS functions ------------------------------------------------------

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

fcs_df <- fcs_calculate(fcs01, var_map)
fcs_vector <- fcs_calculate(fcs01, var_map, add = FALSE)

fcs_class_df <- fcs_classify(fcs_vector, add = TRUE)
fcs_class_expanded_df <- fcs_classify(fcs_vector, add = TRUE, spread = TRUE)
fcs_class_vector <- fcs_classify(fcs_vector, add = FALSE)
fcs_class_only_df <- fcs_classify(fcs_vector, add = FALSE, spread = TRUE)


test_that("fcs_recode works as expected", {
  expect_type(fcs_recode(fcs01$FCSStap), "integer")
  expect_true(all(fcs_recode(fcs01$FCSStap) %in% seq(from = 0, to = 7, by = 1)))
})


test_that("fcs_calculate works as expected", {
  expect_s3_class(fcs_df, "data.frame")
  expect_vector(fcs_vector, ptype = double(), size = nrow(fcs01))
})


test_that("fcs_classify works as expected", {
  expect_s3_class(fcs_class_df, "data.frame")
  expect_s3_class(fcs_class_expanded_df, "data.frame")
  expect_s3_class(fcs_class_only_df, "data.frame")
  expect_vector(
    fcs_class_vector, 
    factor(levels = c("poor", "borderline", "acceptable")), 
    length(fcs_vector)
  )
})
