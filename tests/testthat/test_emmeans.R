
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(emmeans)

# Load test data
path <- system.file("tests/testthat/data/emmeans.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001


# Test: marginaleffects() ----------------------------------------------------------

test_that("Estimated marginal means (single set) works", {
  model <- summary(emmeans(lm(breaks ~ wool, data = warpbreaks),  ~ wool))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_summary_single

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
test_that("Estimated marginal means (multiple sets) works", {
  model <- summary(emmeans(warp.lm,  ~ wool | tension))

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_summary_multi

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

emmeans_summary_adjust <- summary(emmeans(warp.lm, poly ~ tension | wool, adjust = "sidak"))

test_that("Estimated marginal means (from list) works", {
  model <- emmeans_summary_adjust$emmeans

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$`emmeans_summary_adjust$emmeans`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Estimated marginal means (contrasts) works", {
  model <- emmeans_summary_adjust$contrasts

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$`emmeans_summary_adjust$contrasts`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: confint(emmeans()) ----------------------------------------------------------

