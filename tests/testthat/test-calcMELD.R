
library(testthat)
library(dplyr)

# Load the calcMELD function file
source("../../R/calcMELD.R")

context("Testing calcMELD function")

# Test 1: Input Validation
test_that("Input validation for dialysisTwoTimeWeek", {
  expect_error(calcMELD('InvalidValue', 1.3, 1.2, 1.1))
  expect_error(calcMELD('True', 1.3, 1.2, 1.1))  #  different case TRUE
  expect_error(calcMELD('False', 1.3, 1.2, 1.1))  # different case FALSE
})

test_that("Input validation for creatinine, bilirubin, and INR", {
  expect_error(calcMELD('FALSE', 'Invalid', 1.2, 1.1))
  expect_error(calcMELD('FALSE', 1.3, 'Invalid', 1.1))
  expect_error(calcMELD('FALSE', 1.3, 1.2, 'Invalid'))
})

# Test 2: Correct MELD Score Calculation
test_that("Correct MELD Score Calculation", {
  # Example values in the function's example
  expect_equal(calcMELD('TRUE', 2.3, 1.9, 2), 30)
  # Same previous example but dialysisInPastWeek is FALSE
  expect_equal(calcMELD('FALSE', 2.3, 1.9, 2), 25)
})

# Test 3: MELD Score Boundary Conditions
test_that("MELD Score Boundary Conditions", {
  expect_equal(calcMELD('FALSE', 0, 0, 0), 1)  # Minimum boundary condition
  expect_equal(calcMELD('FALSE', 100, 100, 100), 40)  # Maximum boundary condition
})

# Test 4: Dialysis Adjustment
test_that("Dialysis adjustment in MELD calculation", {
  expect_equal(calcMELD('TRUE', 2.3, 1.9, 2), calcMELD('FALSE', 4, 1.9, 2))
})

# Test 5: Check default values
test_that("default input values check out", {
  # checks if the default input values work as they should when no explicit input is provided
  expect_equal(calcMELD(), calcMELDna('FALSE', 1.3, 1.2, 1.1))
})
