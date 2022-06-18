
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Load test data
path <- system.file("tests/testthat/data/data_frame.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001

# Test: data.frame() ----------------------------------------------------------

test_that("data.frame works", {
  model <- t.test(extra ~ 1, data = sleep)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$t_test_one_sample

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
