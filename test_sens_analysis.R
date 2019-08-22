require(testthat)
library(testthat)

context("sens_analysis")

#test that market basket columns are in the dataset
test_that("goods in dataset", {
  expect_true(goods %in% colnames(data))
  })

#test that market basket contains numerics
test_that("goods are numeric", {
  expect_that(is.numeric(goods))
})
