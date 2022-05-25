
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(metafor)

# Create an empty list
results <- list()

# rma.uni() --------------------------------------------------------------------

# Get data
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

### fit a random-effects model using the log risk ratios and sampling variances as input
rma_uni <- rma(yi, vi, data=dat, method="REML")

### fit a mixed-effects model with two moderators (absolute latitude and publication year)
rma_uni_mods <- rma(yi ~ ablat + year, vi, data=dat)

### test all pairwise differences with Holm's method (using the 'multcomp' package if installed)
rma_uni_pairwise <- rma(yi, vi, mods = ~ factor(alloc) - 1, data=dat)

### demonstrating that Q_E + Q_M = Q_Total for fixed-effects models
rma_uni_qtotal <- rma(yi, vi, data=dat, method="FE")
### Q_E + Q_M
rma_uni_qs <- rma(yi, vi, mods = ~ ablat + year, data=dat, method="FE")

### an example of a location-scale model
dat <- dat.bangertdrowns2004

### fit as location-scale model
rma_uni_ls <- rma(yi, vi, scale = ~ 1, data=dat)

### add the total sample size (per 100) as a location and scale predictor
dat$ni100 <- dat$ni/100
rma_uni_ls_pred <- rma(yi, vi, mods = ~ ni100, scale = ~ ni100, data=dat)

### variables in the location and scale parts can differ
rma_uni_ls_diff <- rma(yi, vi, mods = ~ ni100 + meta, scale = ~ ni100 + imag, data=dat)

# Add stats
results = results %>%
  add_stats(rma_uni) %>%
  add_stats(rma_uni_mods) %>%
  add_stats(rma_uni_pairwise) %>%
  add_stats(rma_uni_qtotal) %>%
  add_stats(rma_uni_qs) %>%
  add_stats(rma_uni_ls) %>%
  add_stats(rma_uni_ls_pred) %>%
  add_stats(rma_uni_ls_diff)

# Inspect output
rma_uni
rma_uni_mods
rma_uni_pairwise
rma_uni_qtotal
rma_uni_qs
rma_uni_ls
rma_uni_ls_pred
rma_uni_ls_diff


# rma.mh() --------------------------------------------------------------------

# Run analyses

### meta-analysis of the (log) odds ratios using the Mantel-Haenszel method
rma_mh_test_or <- rma.mh(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

### meta-analysis of the (log) risk ratios using the Mantel-Haenszel method
rma_mh_test_rr <- rma.mh(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

# Add stats
results <- results %>%
  add_stats(rma_mh_test_or) %>%
  add_stats(rma_mh_test_rr)

# Inspect output
rma_mh_test_or
rma_mh_test_rr

# rma.peto() --------------------------------------------------------------------

# Run analyses

### meta-analysis of the (log) odds ratios using Peto's method
rma_peto_test <- rma.peto(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

# Add stats
results <- results %>%
  add_stats(rma_peto_test)

# Inspect output
rma_peto_test


# rma.glmm() --------------------------------------------------------------------

# Run analyses: random-effects models using rma.glmm() (require 'lme4' package)

### unconditional model with fixed study effects
rma_glmm_umfs = rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="UM.FS")

### unconditional model with random study effects
rma_glmm_umrs = rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="UM.RS")

### conditional model with approximate likelihood
rma_glmm_cmal = rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="CM.AL")

### conditional model with exact likelihood (takes too long)
#rma_glmm_cmel = rma.glmm(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, model="CM.EL")

# Add stats
results <- results %>%
  add_stats(rma_glmm_umfs) %>%
  add_stats(rma_glmm_umrs) %>%
  add_stats(rma_glmm_cmal) # %>% add_stats(rma_glmm_cmel)

# Inspect output
rma_glmm_umfs
rma_glmm_umrs
rma_glmm_cmal
#rma_glmm_cmel


# rma.mv() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# confint.rma() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# anova.rma() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# permutest() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# tes() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# matreg() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# ranktest() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# regtest() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# trimfill() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# selmodel() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# fsn() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# hc() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# robust() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# cumul() --------------------------------------------------------------------

# Get data

# Run analyses
new_test <- 99

# Add stats
results <- results %>%
  add_stats(new_test)

# Inspect output



# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/metafor.json")
