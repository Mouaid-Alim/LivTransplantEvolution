
library(LivTransplantEvolution)
library(dplyr)

test_that("prepareData returns correct processed data", {
  # Create a temporary CSV file for testing
  temp_file <- tempfile(fileext = ".csv")
  data.frame(Date = as.Date('2020-01-01'),
             Sex = "F",
             DialysisTwiceInPastWeek = TRUE,
             Creatinine = 1.0,
             INR = 1.2,
             Total_Bilirubin = 0.5,
             ALP = 100,
             ALT = 50,
             AST = 40,
             Sodium = 140,
             Albumin = 4.5) %>%
    write.csv(temp_file, row.names = FALSE)

  # Test the function
  result <- prepareData(temp_file)

  # Check if the function returns a data frame
  expect_true(is.data.frame(result))

  # Check if the correct columns are present
  expected_cols <- c("Date", "Sex", "DialysisTwiceInPastWeek", "Creatinine", "INR", "Total_Bilirubin", "Sodium", "Albumin")
  expect_true(all(expected_cols %in% names(result)))

  # Clean up
  unlink(temp_file)
})
