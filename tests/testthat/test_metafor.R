
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
      test_results$rma_results)
  })

### fit a mixed-effects model with two moderators (absolute latitude and publication year)
rma_results_mods <- rma(yi ~ ablat + year, vi, data=dat)

### test all pairwise differences with Holm's method (using the 'multcomp' package if installed)
rma_results_pairwise <- rma(yi, vi, mods = ~ factor(alloc) - 1, data=dat)

### demonstrating that Q_E + Q_M = Q_Total for fixed-effects models
rma_results_qtotal <- rma(yi, vi, data=dat, method="FE")
### Q_E + Q_M
rma_results_qs <- rma(yi, vi, mods = ~ ablat + year, data=dat, method="FE")

### an example of a location-scale model
dat <- dat.bangertdrowns2004

### fit as location-scale model
rma_results_ls <- rma(yi, vi, scale = ~ 1, data=dat)

### add the total sample size (per 100) as a location and scale predictor
dat$ni100 <- dat$ni/100
rma_results_ls_pred <- rma(yi, vi, mods = ~ ni100, scale = ~ ni100, data=dat)

### variables in the location and scale parts can differ
rma_results_ls_diff <- rma(yi, vi, mods = ~ ni100 + meta, scale = ~ ni100 + imag, data=dat)












