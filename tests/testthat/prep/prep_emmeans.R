
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(emmeans)

# Create an empty list
results <- list()

# summary(emmeans()) --------------------------------------------------------------------

# Run analysis
emmeans_summary_single <- summary(emmeans(lm(breaks ~ wool, data = warpbreaks),  ~ wool))

warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
emmeans_summary_multi <- summary(emmeans(warp.lm,  ~ wool | tension))

# with 'adjust' argument
emmeans_summary_adjust <- summary(emmeans(warp.lm, poly ~ tension | wool, adjust = "sidak"))

# Add stats
results = results %>%
  add_stats(emmeans_summary_single) %>%
  add_stats(emmeans_summary_multi) %>%
  add_stats(emmeans_summary_adjust)

# Inspect output
emmeans_summary_single
emmeans_summary_multi
emmeans_summary_adjust$emmeans
emmeans_summary_adjust$contrasts

# confint(emmeans()) --------------------------------------------------------------------


# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/emmeans.json")
