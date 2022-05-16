
# Todos -------------------------------------------------------------------

#TODO: Rename certain terms to contrasts?
#TODO: # Call deviance dfs df numerator and df denominator?
#TODO: Replace some of the statistics extraction code with loops over all
#      columns and then fix the names after
#TODO: Use pipes when adding statistics via add_statistic()?

# Update ------------------------------------------------------------------


# Load packages etc
library(tidystats)
library(testthat)
tolerance <- 0.001
models_equal = function(model, tidy_model_test) {
  tidy_model <- tidy_stats(model)
  tidy_model$package <- NULL
  tidy_model_test$package <- NULL
  expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
}

# Set path (in RStudio)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

results = list()

test_results <- read_stats("tests/testthat/data/pairwise_htest.json")
devtools::load_all()
devtools::test()

# Update documentation
devtools::document()

# Load the package

# Install the dev version
#devtools::install()
#.rs.restartR()

# Add dependency ----------------------------------------------------------

#usethis::use_package("lavaan", "Suggests")

# Testing -----------------------------------------------------------------

# Add a test
# usethis::use_test("add_stats")

# Test a specific test
testthat::test_file("tests/testthat/test_main.R")

# Test all tests
devtools::test()

# Create a vignette -------------------------------------------------------

# usethis::use_vignette("read-and-use-a-tidystats-file")

# Add a data set ----------------------------------------------------------

# usethis::use_data(quote_source, overwrite = TRUE)

# README ------------------------------------------------------------------

# Update README
knitr::knit(input = "README.Rmd")

# Build website -----------------------------------------------------------

# Run to build the website
pkgdown::build_site_github_pages()

# Preview the site
pkgdown::preview_site()

# Delete website files
pkgdown::clean_site()

# CRAN submission ---------------------------------------------------------

# Update README
knitr::knit(input = "README.Rmd")

# Update website
pkgdown::build_site_github_pages()

# Check examples
devtools::run_examples()

# Check tests
devtools::test()

# Check package
# devtools::load_all()
devtools::check()
devtools::check(args = c('--run-donttest')) # Without examples test
devtools::check(args = c('--as-cran'))

# run R CMD check on CRAN’s servers
devtools::check_win_devel()
devtools::check_win_release()

# Submit
devtools::release()

# Setup -------------------------------------------------------------------

# Create README file
usethis::use_readme_rmd(open = rlang::is_interactive())

# Create pkgdown website
usethis::use_pkgdown()