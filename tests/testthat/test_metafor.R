
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(metafor)

# Load test data
path <- system.file("tests/testthat/data/metafor.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001

# Function to compare models
# model: the model to be passed to tidy_stats
# tidy_model_test: test results to compare to
models_equal = function(model, tidy_model_test) {
  tidy_model <- tidy_stats(model)
  tidy_model$package <- NULL
  tidy_model_test$package <- NULL
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
}

# Test: rma.uni ----------------------------------------------------------------

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

test_that("Regular random-effects model works",
  {
    models_equal(
      rma(yi, vi, data=dat, method="REML"),
      test_results$rma_uni)
  })

test_that("Mixed-effects model with moderators works",
  {
    models_equal(
      rma(yi ~ ablat + year, vi, data=dat),
      test_results$rma_uni_mods)
  })


test_that("RMA for pairwise differences (with Holm's method) works",
  {
    models_equal(
      rma(yi, vi, mods = ~ factor(alloc) - 1, data=dat),
      test_results$rma_uni_pairwise)
  })

test_that("RMA with Q_Total for fixed-effects works",
  {
    models_equal(
      rma(yi, vi, data=dat, method="FE"),
      test_results$rma_uni_qtotal)
  })

test_that("RMA with Q_E + Q_M for fixed-effects works",
  {
    models_equal(
      rma(yi, vi, mods = ~ ablat + year, data=dat, method="FE"),
      test_results$rma_uni_qs)
  })

dat <- dat.bangertdrowns2004

test_that("RMA for location-scale model works",
  {
    models_equal(
      rma(yi, vi, scale = ~ 1, data=dat),
      test_results$rma_uni_ls)
  })

test_that("RMA for location-scale model with scale predictor works",
  {
    dat$ni100 <- dat$ni/100
    models_equal(
      rma(yi, vi, mods = ~ ni100, scale = ~ ni100, data=dat),
      test_results$rma_uni_ls_pred)
  })

test_that("RMA for location-scale model with differing location and scale parts works",
  {
    dat$ni100 <- dat$ni/100
    models_equal(
      suppressWarnings(rma(yi, vi, mods = ~ ni100 + meta, scale = ~ ni100 + imag, data=dat)),
      test_results$rma_uni_ls_diff)
  })

# Test: rma.mh ----------------------------------------------------------------

test_that("Equal-Effects Model (OR) model works",
  {
    models_equal(
      rma.mh(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg),
      test_results$rma_mh_test_or)
  })
test_that("Equal-Effects Model (RR) model works",
  {
    models_equal(
      rma.mh(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg),
      test_results$rma_mh_test_rr)
  })