require(testthat)
library(testthat)

context("multiply_across")

test_that("column skipped is the disaggregation",{
  expect_that(disagg, is_identical_to(colnames(data[, -1])))
})

#make sure that all percents are in .05 or .15 form, if they are greater than 1 will really mess up the results.
test_that("percents are less than 1"){
  expect_true(any(percent_to_1 < 2))
}


