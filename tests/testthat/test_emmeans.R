
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


# Test: emmeans() ----------------------------------------------------------

test_that("Estimated marginal means (single set) works", {
  model <- emmeans(lm(breaks ~ wool, data = warpbreaks),  ~ wool)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_single

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
test_that("Estimated marginal means (multiple sets) works", {
  model <- emmeans(warp.lm,  ~ wool | tension)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_multi

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

emmeans_adjust <- emmeans(warp.lm, poly ~ tension | wool, adjust = "sidak")

test_that("Estimated marginal means (from list) works", {
  model <- emmeans_adjust

  tidy_model <- add_stats(list(), model)$`model$emmeans`
  tidy_model_test <- test_results$`emmeans_adjust$emmeans`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Estimated marginal means (contrasts) works", {
  model <- emmeans_adjust

  tidy_model <- add_stats(list(), model)$`model$contrasts`
  tidy_model_test <- test_results$`emmeans_adjust$contrasts`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: summary(emmeans()) ----------------------------------------------------------

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
  model <- emmeans_summary_adjust

  tidy_model <- add_stats(list(), model)$`model$emmeans`
  tidy_model_test <- test_results$`emmeans_summary_adjust$emmeans`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})

test_that("Estimated marginal means (contrasts) works", {
  model <- emmeans_summary_adjust

  tidy_model <- add_stats(list(), model)$`model$contrasts`
  tidy_model_test <- test_results$`emmeans_summary_adjust$contrasts`

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: confint(emmeans()) ----------------------------------------------------------

warp.emm <- emmeans(warp.lm, ~ tension | wool)

test_that("Confidence interval for estimated marginal means works", {
  model <- confint(warp.emm, level = .85)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_confint

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


test_that("Confidence interval for estimated marginal means (single set) works", {
  model <- confint(warp.emm, by = NULL, level = .90)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_confint_single

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: test(emmeans()) ----------------------------------------------------------

pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.emm <- emmeans(pigs.lm, "percent", type = "response")

test_that("Test for estimated marginal means works", {
  model <- test(pigs.emm, null = log(35), delta = log(1.10), side = ">")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_test

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
test_that("Test (joint) for estimated marginal means works", {
  model <- test(pigs.emm, joint = TRUE)

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_test_joint

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


# Test: contrast(emmeans()) ----------------------------------------------------------

test_that("Contrast for estimated marginal means works", {
  set.seed(1234)
  pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
  pigs.emm <- emmeans(pigs.lm, "percent", type = "response")
  model <- contrast(pigs.emm, "consec")

  tidy_model <- tidy_stats(model)
  tidy_model_test <- test_results$emmeans_contrast

  tidy_model$package <- NULL
  tidy_model_test$package <- NULL

  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
