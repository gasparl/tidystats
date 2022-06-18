
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(marginaleffects)

# Create an empty list
results <- list()

# rma.uni() --------------------------------------------------------------------

# Get data
mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
mfx <- marginaleffects(mod)
head(mfx)

# Average Marginal Effect (AME)
mfx_sum = summary(mfx)
tidy(mfx)

# Add stats
results = results %>%
  add_stats(rma_uni)

# Inspect output

dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ hp + cyl + am, data = dat)

# Compute and summarize marginal means
mm <- marginalmeans(mod)
summary(mm)

?marginaleffects::comparisons

mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
pred <- predictions(mod)





# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/marginaleffects.json")
