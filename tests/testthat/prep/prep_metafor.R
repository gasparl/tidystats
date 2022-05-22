
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(metafor)

# Create an empty list
results <- list()

# rma() --------------------------------------------------------------------

# Get data
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

### fit a random-effects model using the log risk ratios and sampling variances as input
rma_results <- rma(yi, vi, data=dat, method="REML")

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
 
# Add stats
results = results %>%
  add_stats(rma_results) %>%
  add_stats(rma_results_mods) %>%
  add_stats(rma_results_pairwise) %>%
  add_stats(rma_results_qtotal) %>%
  add_stats(rma_results_qs) %>%
  add_stats(rma_results_ls) %>%
  add_stats(rma_results_ls_pred) %>%
  add_stats(rma_results_ls_diff)

#add_stats(results, rma_results_ls)
#add_stats(results, rma_results_ls_diff)

# Inspect output
rma_results
rma_results_mods
rma_results_pairwise
rma_results_qtotal
rma_results_qs
rma_results_ls
rma_results_ls_pred
rma_results_ls_diff

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/metafor.json")
