
library(testthat)
library(dplyr)

# load the calcMELDna function
source("../../R/calcMELDna.R")

context("Testing calcMELDna function")

# Test 1: Input Validation
test_that("Input validation for dialysisTwoTimeWeek", {
  expect_error(calcMELDna('InvalidValue', 1.3, 1.2, 1.1, 140))
  expect_error(calcMELDna('True', 1.3, 1.2, 1.1, 140))  # Different case TRUE
  expect_error(calcMELDna('False', 1.3, 1.2, 1.1, 140))  # Different case FALSE
})

test_that("Input validation for creatinine, bilirubin, inr, and sodium", {
  expect_error(calcMELDna('FALSE', 'Invalid', 1.2, 1.1, 140))
  expect_error(calcMELDna('FALSE', 1.3, 'Invalid', 1.1, 140))
  expect_error(calcMELDna('FALSE', 1.3, 1.2, 'Invalid', 140))
  expect_error(calcMELDna('FALSE', 1.3, 1.2, 1.1, 'Invalid'))
})

# Test 2: Correct MELDna Score Calculation
test_that("Correct MELDna Score Calculation", {
  expect_equal(calcMELDna('FALSE', 2.3, 4, 4, 145), 35)
  # Same test as above with dialysisTwoTimeWeek as TRUE
  expect_equal(calcMELDna('TRUE', 2.3, 4, 4, 145), 40)
})

# Test 3: MELDna Score Boundary Conditions
test_that("MELDna Score Boundary Conditions", {
  expect_equal(calcMELDna('FALSE', 1, 1, 1, 125), 6)  # Minimum boundary condition
  expect_equal(calcMELDna('FALSE', 100, 100, 100, 137), 40)  # Maximum boundary condition
})

# Test 4: Dialysis Adjustment
test_that("Dialysis adjustment in MELDna calculation", {
  expect_equal(calcMELDna('TRUE', 2.3, 1.9, 2, 145), calcMELDna('FALSE', 4, 1.9, 2, 145))
})

# Test 5: Sodium Effect on MELDna Score
test_that("Sodium effect on MELDna score", {
  expect_equal(calcMELDna('FALSE', 1.3, 1.2, 1.1, 125), calcMELDna('FALSE', 1.3, 1.2, 1.1, 137))
})

# Test 6: Check default values
test_that("default input values check out", {
  # checks if the default input values work as they should when no explicit input is provided
  expect_equal(calcMELDna(), calcMELDna('FALSE', 1.3, 1.2, 1.1, 140))
})
