test_that("Valid lavaan model syntax returns TRUE", {
  good_model <- "visual =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 speed =~ x7 + x8 + x9
                 visual ~ speed
                 textual ~ speed"
  data(HolzingerSwineford1939, package = "lavaan")
  expect_true(is_valid_lav_syntax(good_model, HolzingerSwineford1939))
})

test_that("Invalid lavaan model syntax returns FALSE", {
  bad_model <- "y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9"
  data(HolzingerSwineford1939, package = "lavaan")
  expect_false(is_valid_lav_syntax(bad_model, HolzingerSwineford1939))
})

test_that("Error thrown for non-data.frame data argument", {
  bad_data_input <- "not_a_dataframe"
  good_model <- "visual =~ x1 + x2 + x3"
  expect_error(is_valid_lav_syntax(good_model, bad_data_input),
               "'data' must be a data frame.")
})
