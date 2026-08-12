#!/usr/bin/env Rscript
# run_tests.R
# Script to run all tests for the gc-wildlife-viewer app

cat("\n")
cat("================================================================================\n")
cat("  Running Tests for GC Wildlife Viewer\n")
cat("================================================================================\n")
cat("\n")

# Load required packages
required_packages <- c("testthat", "dplyr", "janitor")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
    cat("Installing missing test packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages, dependencies = TRUE)
}

library(testthat)
library(dplyr)
library(janitor)

# Source the functions to test
cat("Loading utils.r...\n")
source("utils.r")

cat("\n")
cat("Running unit tests...\n")
cat("--------------------------------------------------------------------------------\n")

# Run tests
test_results <- test_dir(
    "tests/testthat",
    reporter = "progress",
    stop_on_failure = FALSE
)

cat("\n")
cat("================================================================================\n")
cat("  Test Summary\n")
cat("================================================================================\n")

# Print summary
print(test_results)

cat("\n")
# 
# # Check if all tests passed
# if (any(test_results$failed > 0)) {
#     cat("TESTS FAILED\n")
#     quit(status = 1)
# } else {
#     cat("ALL TESTS PASSED\n")
#     quit(status = 0)
# }
