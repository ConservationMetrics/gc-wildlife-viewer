# Tests for GC Wildlife Viewer

This directory contains tests for the GC Wildlife Viewer application.

## Test Structure

```
tests/
├── testthat.R                          # Test runner entry point
└── testthat/
    ├── test-{function_name}.R          # Tests for functions each function gets its own file.
    └── test-app-integration.R          # Integration tests for Shiny app
```

## Running Tests

### Run all tests

```r
# From R console
source("tests/run_tests.R")
```

### Run specific test file

```r
library(testthat)
source("utils.r")
test_file("tests/testthat/test-get_media_type.R")
```

### Run tests for a specific function

```r
library(testthat)
source("utils.r")
test_file("tests/testthat/test-load_metadata.R", 
          filter = "handles missing lat/lon")
```

## Test Data Requirements

Some tests require actual files to be present:

### For `test-generate_thumbnails.R`
- Tests create temporary images using the `magick` package
- No external test data required

### For `test-app-integration.R`
- Requires `data/ImageData.csv` to exist
- Requires corresponding image/video files in the expected locations
- These tests are automatically skipped if test data is not available

## Dependencies

### Required R Packages
- `testthat` - Testing framework
- `dplyr` - Data manipulation (used in utils.r)
- `janitor` - Column name cleaning (used in utils.r)
- `magick` - Image processing (for thumbnail tests)

### Optional Dependencies
- `shinytest2` - For app integration tests
- `ffmpeg` - For video thumbnail tests (system dependency)

## Continuous Integration

Tests are automatically run via GitHub Actions on:
- Every push to `main` or `develop` branches
- Every pull request to these branches

See `.github/workflows/test.yml` for configuration.

## Writing New Tests

### Test File Naming
- Unit test files: `test-<function_name>.R`
- Integration tests: `test-<feature>-integration.R`

### Test Structure
```r
# tests/testthat/test-my_function.R

test_that("my_function does something correctly", {
  result <- my_function(input)
  expect_equal(result, expected_output)
})

test_that("my_function handles edge cases", {
  expect_error(my_function(NULL), "specific error message")
  expect_warning(my_function(bad_input))
})
```

## Troubleshooting

### "ffmpeg not found" warnings
Install ffmpeg on your system:
- **Ubuntu/Debian**: `sudo apt-get install ffmpeg`
- **macOS**: `brew install ffmpeg`
- **Windows**: Download from https://ffmpeg.org/

### Magick package installation issues
Install system dependencies:
- **Ubuntu/Debian**: `sudo apt-get install libmagick++-dev`
- **macOS**: `brew install imagemagick`

### Tests fail in CI but pass locally
- Check that all system dependencies are installed in the CI environment
- Verify file paths work across different OS (use `file.path()`)
- Ensure tests don't rely on local-only data

## Test Metrics

Run tests with coverage report:

```r
# Install covr package
install.packages("covr")

# Generate coverage report
library(covr)
cov <- package_coverage()
report(cov)
```

This will show which lines of code are covered by tests.
