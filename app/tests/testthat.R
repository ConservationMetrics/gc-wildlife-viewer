# tests/testthat.R
# This file is part of the standard testthat structure
# It's the entry point for running tests

library(testthat)
library(dplyr)
library(janitor)

# Source the functions to test
source("utils.r")

# Run all tests
test_check("gc-wildlife-viewer")
