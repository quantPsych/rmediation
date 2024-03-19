test_that("Valid lavaan model syntax returns TRUE", {
  good_model <- "visual =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 speed =~ x7 + x8 + x9
                 visual ~ speed
                 textual ~ speed"
  # data(HolzingerSwineford1939, package = "lavaan")
  expect_true(is_lav_syntax(good_model))
})

test_that("Invalid lavaan model syntax returns FALSE", {
  bad_model <- "y = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9"
  # data(HolzingerSwineford1939, package = "lavaan")
  expect_false(is_lav_syntax(bad_model))
})

test_that("Rturn FALSE when the model is not character string", {
  str_model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
  expect_false(is_lav_syntax(str_model))
})
