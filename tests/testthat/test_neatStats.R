
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(neatStats)

# Load test data
path <-
    system.file("tests/testthat/data/neatStats.json", package = "tidystats")
test_results <- read_stats(path)

# Set options
tolerance <- 0.001


# Test: anova_neat() ----------------------------------------------------------

test_that("Analysis of Variance (via neatStats, between design) works", {
    model <- anova_neat(
        iris,
        values = c("Sepal.Length"),
        between_vars = "Species",
        hush = TRUE
    )

    tidy_model <- tidy_stats(model)
    tidy_model_test <- test_results$anova_neat_between

    tidy_model$package <- NULL
    tidy_model_test$package <- NULL

    expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


test_that("Analysis of Variance (via neatStats, between design with interaction) works",
          {
              model <- anova_neat(
                  ToothGrowth,
                  values = c("len"),
                  between_vars = c("supp", "dose"),
                  hush = TRUE
              )

              tidy_model <- tidy_stats(model)
              tidy_model_test <-
                  test_results$anova_neat_between_interaction

              tidy_model$package <- NULL
              tidy_model_test$package <- NULL

              expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
          })


test_that("Analysis of Variance (via neatStats, within design) works", {
    model <- anova_neat(
        iris,
        values = c(
            "Sepal.Length",
            "Sepal.Width",
            "Petal.Length",
            "Petal.Width"
        ),
        hush = TRUE
    )

    tidy_model <- tidy_stats(model)
    tidy_model_test <- test_results$anova_neat_within

    tidy_model$package <- NULL
    tidy_model_test$package <- NULL

    expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})



test_that("Analysis of Variance (via neatStats, within design with interaction) works",
          {
              model <- anova_neat(
                  iris,
                  values = c(
                      "Sepal.Length",
                      "Sepal.Width",
                      "Petal.Length",
                      "Petal.Width"
                  ),
                  within_ids = list(
                      Organ = c("Sepal", "Petal"),
                      Dimension = c("Length", "Width")
                  ),
                  hush = TRUE
              )

              tidy_model <- tidy_stats(model)
              tidy_model_test <-
                  test_results$anova_neat_within_interaction

              tidy_model$package <- NULL
              tidy_model_test$package <- NULL

              expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
          })


test_that("Analysis of Variance (via neatStats, mixed design) works", {
    model <- anova_neat(
        iris,
        values = c(
            "Sepal.Length",
            "Sepal.Width",
            "Petal.Length",
            "Petal.Width"
        ),
        between_vars = "Species",
        hush = TRUE
    )

    tidy_model <- tidy_stats(model)
    tidy_model_test <- test_results$anova_neat_between_within

    tidy_model$package <- NULL
    tidy_model_test$package <- NULL

    expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
})


test_that(
    "Analysis of Variance (via neatStats, mixed-design with within-subject interaction) works",
    {
        model <- anova_neat(
            iris,
            values = c(
                "Sepal.Length",
                "Sepal.Width",
                "Petal.Length",
                "Petal.Width"
            ),
            within_ids = list(
                Organ = c("Sepal", "Petal"),
                Dimension = c("Length", "Width")
            ),
            between_vars = "Species",
            hush = TRUE
        )

        tidy_model <- tidy_stats(model)
        tidy_model_test <-
            test_results$anova_neat_between_within_interacts

        tidy_model$package <- NULL
        tidy_model_test$package <- NULL

        expect_equal(tidy_model, tidy_model_test, tolerance = tolerance)
    }
)
