
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(neatStats)

# Create an empty list
results <- list()

# anova_neat() --------------------------------------------------------------------

# Run analysis
anova_neat_between = anova_neat(
    iris,
    values = c("Sepal.Length"),
    between_vars = "Species"
)

anova_neat_between_interaction = anova_neat(
    ToothGrowth,
    values = c("len"),
    between_vars = c("supp", "dose")
)

anova_neat_within = anova_neat(
    iris,
    values = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
)

anova_neat_within_interaction = anova_neat(
    iris,
    values = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    within_ids = list(
        Organ = c("Sepal", "Petal"),
        Dimension = c("Length", "Width")
    )
)

anova_neat_between_within = anova_neat(
    iris,
    values = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    between_vars = "Species"
)

anova_neat_between_within_interacts = anova_neat(
    iris,
    values = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    within_ids = list(
        Organ = c("Sepal", "Petal"),
        Dimension = c("Length", "Width")
    ),
    between_vars = "Species"
)

# Add stats
results = results %>%
  add_stats(anova_neat_between) %>%
  add_stats(anova_neat_between_interaction) %>%
  add_stats(anova_neat_within) %>%
  add_stats(anova_neat_within_interaction) %>%
  add_stats(anova_neat_between_within) %>%
  add_stats(anova_neat_between_within_interacts)


# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/neatStats.json")
