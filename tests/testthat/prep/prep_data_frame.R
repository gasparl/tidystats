
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)

# Create an empty list
results <- list()

# data.frame() ------------------------------------------------------------------

# Get data
data_frame_mtcars = mtcars
data_frame_toothgrowth = ToothGrowth
data_frame_mix = data.frame(
  subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  strings = c('K', 'L', 'K', 'L', 'L', 'K', 'K', 'K', 'L', 'K'),
  facts = as.factor(c('K1', 'L1', 'K1', 'L2', 'L2', 'K2', 'K2', 'K1', 'L1', 'K2')),
  nums1 = c(2, 2, NA, NA, 1, 1, 1, 2, NA, NA),
  nums2 = c(6, 7, 8.5, NA, 5, 16, NA, 16, 45, 77)
)

# Add stats
results = results %>%
  add_stats(data_frame_mtcars) %>%
  add_stats(data_frame_toothgrowth) %>%
  add_stats(data_frame_mix) %>%
  add_stats(data_frame_mix, 
    identifier = "data frame with custom symbol", 
    symbols = list("facts" = "my_factors", "nums2" = "numbers 2")) %>%
  add_stats(data_frame_mix, 
    identifier = "data frame with subscripts", 
    subscripts = c("rings", "2"),
    method_name = "Table from df with subscripts",
    table_name = "Headers with subscripts")


# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_stats(results, "tests/testthat/data/data_frame.json")
