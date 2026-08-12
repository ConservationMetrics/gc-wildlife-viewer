# tests/testthat/test-unflatten_paths.R

test_that("unflatten_paths creates directory structure", {
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_images")
  dir.create(test_image_dir, showWarnings = FALSE)
  
  # Create test metadata
  meta <- data.frame(
    image_path_flat = c("Site01.IMG_0001.JPG", "Site02.IMG_0002.JPG"),
    image_path = c("Site01/IMG_0001.JPG", "Site02/IMG_0002.JPG"),
    stringsAsFactors = FALSE
  )
  
  # Create flattened files
  file.create(file.path(test_image_dir, "Site01.IMG_0001.JPG"))
  file.create(file.path(test_image_dir, "Site02.IMG_0002.JPG"))
  
  # Run unflatten
  unflatten_paths(meta, test_image_dir, verbose = FALSE)
  
  # Check that hierarchical directories were created
  expect_true(dir.exists(file.path(test_image_dir, "Site01")))
  expect_true(dir.exists(file.path(test_image_dir, "Site02")))
  
  # Check that files were moved
  expect_true(file.exists(file.path(test_image_dir, "Site01/IMG_0001.JPG")))
  expect_true(file.exists(file.path(test_image_dir, "Site02/IMG_0002.JPG")))
  
  # Check that original flat files no longer exist
  expect_false(file.exists(file.path(test_image_dir, "Site01.IMG_0001.JPG")))
  expect_false(file.exists(file.path(test_image_dir, "Site02.IMG_0002.JPG")))
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
})

test_that("unflatten_paths handles nested directories", {
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_nested")
  dir.create(test_image_dir, showWarnings = FALSE)
  
  # Create test metadata with nested paths
  meta <- data.frame(
    image_path_flat = c("Site01.SubFolder.IMG_0001.JPG"),
    image_path = c("Site01/SubFolder/IMG_0001.JPG"),
    stringsAsFactors = FALSE
  )
  
  # Create flattened file
  file.create(file.path(test_image_dir, "Site01.SubFolder.IMG_0001.JPG"))
  
  # Run unflatten
  unflatten_paths(meta, test_image_dir, verbose = FALSE)
  
  # Check nested directories were created
  expect_true(dir.exists(file.path(test_image_dir, "Site01/SubFolder")))
  expect_true(file.exists(file.path(test_image_dir, "Site01/SubFolder/IMG_0001.JPG")))
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
})

test_that("unflatten_paths skips files that already exist", {
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_skip")
  dir.create(test_image_dir, showWarnings = FALSE)
  dir.create(file.path(test_image_dir, "Site01"), showWarnings = FALSE)
  
  # Create test metadata
  meta <- data.frame(
    image_path_flat = c("Site01.IMG_0001.JPG"),
    image_path = c("Site01/IMG_0001.JPG"),
    stringsAsFactors = FALSE
  )
  
  # Create both flat and hierarchical files
  file.create(file.path(test_image_dir, "Site01.IMG_0001.JPG"))
  file.create(file.path(test_image_dir, "Site01/IMG_0001.JPG"))
  
  # Run unflatten - should skip since hierarchical file exists
  unflatten_paths(meta, test_image_dir, verbose = FALSE)
  
  # Both files should still exist
  expect_true(file.exists(file.path(test_image_dir, "Site01.IMG_0001.JPG")))
  expect_true(file.exists(file.path(test_image_dir, "Site01/IMG_0001.JPG")))
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
})

test_that("unflatten_paths handles missing files gracefully", {
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_missing")
  dir.create(test_image_dir, showWarnings = FALSE)
  
  # Create test metadata for files that don't exist
  meta <- data.frame(
    image_path_flat = c("NonExistent.IMG_0001.JPG"),
    image_path = c("NonExistent/IMG_0001.JPG"),
    stringsAsFactors = FALSE
  )
  
  # Run unflatten - should not error
  expect_silent(unflatten_paths(meta, test_image_dir, verbose = FALSE))
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
})

test_that("unflatten_paths verbose parameter works", {
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_verbose")
  dir.create(test_image_dir, showWarnings = FALSE)
  
  # Create test metadata
  meta <- data.frame(
    image_path_flat = c("Site01.IMG_0001.JPG"),
    image_path = c("Site01/IMG_0001.JPG"),
    stringsAsFactors = FALSE
  )
  
  # Create flattened file
  file.create(file.path(test_image_dir, "Site01.IMG_0001.JPG"))
  
  # Test verbose = TRUE produces messages
  expect_message(
    unflatten_paths(meta, test_image_dir, verbose = TRUE),
    "Unflattening"
  )
  
  # Cleanup and recreate for next test
  unlink(test_image_dir, recursive = TRUE)
  dir.create(test_image_dir, showWarnings = FALSE)
  file.create(file.path(test_image_dir, "Site01.IMG_0001.JPG"))
  
  # Test verbose = FALSE suppresses messages
  expect_silent(
    unflatten_paths(meta, test_image_dir, verbose = FALSE)
  )
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
})
