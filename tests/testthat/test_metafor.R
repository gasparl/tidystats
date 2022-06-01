
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

# Test: rma.peto ----------------------------------------------------------------

test_that("Equal-Effects Model (Peto's method) model works",
  {
    models_equal(
      rma.peto(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg),
      test_results$rma_peto_test)
  })

# Test: rma.glmm ----------------------------------------------------------------
test_that("Random-Effects Model (Unconditional Model with Fixed Study Effects) works",
  {
    models_equal(
      rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="UM.FS"),
      test_results$rma_glmm_umfs)
  })
test_that("Random-Effects Model (Unconditional Model with Random Study Effects) works",
  {
    models_equal(
      suppressWarnings(rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, 
        di=cneg, data=dat.bcg, model="UM.RS")),
      test_results$rma_glmm_umrs)
  })
test_that("Random-Effects Model (Conditional Model with Approximate Likelihood) works",
  {
    models_equal(
      rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="CM.AL"),
      test_results$rma_glmm_cmal)
  })


# Test: rma.mv ----------------------------------------------------------------

test_that("Multivariate Meta-Analysis Model (REML) works",
  {
    dat <- escalc(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
    models_equal(
      rma.mv(yi, vi, random = ~ 1 | trial, data=dat),
      test_results$rma_mv)
  })
test_that("Multivariate Meta-Analysis Model (multilevel REML) works",
  {
    models_equal(
      rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011),
      test_results$rma_mv_mm)
  })
test_that("Multivariate Meta-Analysis Model (bivariate REML) works",
  {
    dat.long <- to.long(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
    levels(dat.long$group) <- c("exp", "con")
    dat.long$group <- relevel(dat.long$group, ref="con")
    dat.long <- escalc(measure="PLO", xi=out1, mi=out2, data=dat.long)
    models_equal(
      rma.mv(yi, vi, mods = ~ group, random = ~ group | study, struct="UN", data=dat.long),
      test_results$rma_mv_biv)
  })

# Test: confint.rma ----------------------------------------------------------------

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

test_that("RMA confidence intervals (uni) works",
  {
    models_equal(
      confint(res <- rma(yi, vi, data=dat, method="REML")),
      test_results$confint_rma_uni)
  })
test_that("RMA confidence intervals (mv) works",
  {
    models_equal(
      confint(rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011)),
      test_results$confint_rma_mv)
  })
res = rma.mv(yi, vi, random = ~ school | district, data=dat.konstantopoulos2011)
test_that("RMA confidence intervals (mv, parameterization) works",
  {
    models_equal(
      confint(res),
      test_results$confint_rma_mv_para)
  })
test_that("RMA confidence intervals (mv, single output) works",
  {
    models_equal(
      confint(res, tau2=1),
      test_results$confint_rma_mv_single)
  })
test_that("RMA confidence intervals (mv, custom CI level) works", {
  model <- confint(res, level = .8)
  
  tidy_model <- tidy_stats(model, args = .8)
  tidy_model_test <- test_results$confint_rma_mv_ci80
  
  tidy_model$package$version <- NULL
  tidy_model_test$package$version <- NULL
  
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})
test_that("RMA confidence intervals (mh",
  {
    models_equal(
      confint(rma.mh(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)),
      test_results$confint_rma_mh)
  })

# Test: anova.mv ----------------------------------------------------------------

dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
res1 <- rma(yi, vi, data=dat, method="ML")
res2 <- rma(yi, vi, mods = ~ ablat + year, data=dat, method="ML")
test_that("Wald-Type Tests for 'rma' Objects works",
  {
    models_equal(
      anova(res2),
      test_results$anova_rma_wald)
  })
test_that("Wald-Type Tests for 'rma' Objects (with moderators) works",
  {
    models_equal(
      anova(res2, X=rbind(c(0,1,0), c(0,0,1))),
      test_results$anova_rma_wald_est)
  })
test_that("Likelihood Ratio Tests for 'rma' Objects works",
  {
    models_equal(
      anova(res1, res2),
      test_results$anova_rma_lrt)
  })
test_that("Likelihood Ratio Tests (of linear combination) works",
  {
    models_equal(
      anova(res2, X=c(1,35,1970)),
      test_results$anova_rma_wald_comb)
  })
test_that("Likelihood Ratio Tests (for component) works",
  {
    dat <- dat.konstantopoulos2011
    res <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat)
    res0 <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat, sigma2=c(0,NA))
    models_equal(
      anova(res, res0),
      test_results$anova_rma_lrt_complex)
  })

