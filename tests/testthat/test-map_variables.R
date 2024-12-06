# Tests for indicator variable mapping -----------------------------------------

var_map <- map_variables(
  staples = "FCSStap", meatfish = "FCSPr", pulses = "FCSPulse"
)

test_that("map_variables works as expected", {
  expect_type(var_map, "list")
  expect_named(var_map, c("staples", "meatfish", "pulses"))
})

test_that("fcs_fg_map_variables works as expected", {
  expect_error(
    fcs_var_map <- fcs_fg_map_variables(
      staples = "FCSStap", meatfish = "FCSPr", pulses = "FCSPulse"
    ),
    regexp = paste0(
      "Variable map is missing values for vegetables, fruits, milk, sugar, ",
      "oil, condiment. Please check that you have specified variables for all ",
      "FCS indicators or that you have spelled the FCS indicators correctly."
    )
  )
})
