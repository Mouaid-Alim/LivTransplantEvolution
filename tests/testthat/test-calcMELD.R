
library(LivTransplantEvolution)

test_that("calcMELD returns correct low score", {
  expected_low_score = 6
  expect_equal(calcMELD(0, 1.0, 1.0, 1.0), expected_low_score)
})

test_that("calcMELD returns correct high score", {
  expected_high_score = 40
  expect_equal(calcMELD(0, 5, 8, 3), expected_high_score)
})

test_that("calcMELD adjusts for dialysis", {
  expected_score_with_dialysis = 40
  expect_equal(calcMELD(1, 1, 8, 3), expected_score_with_dialysis)
})
