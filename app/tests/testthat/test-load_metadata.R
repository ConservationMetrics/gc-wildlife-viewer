# tests/testthat/test-load_metadata.R

test_that("load_metadata fails when CSV doesn't exist", {
  expect_error(
    load_metadata("nonexistent_file.csv", verbose = FALSE),
    "Metadata CSV not found"
  )
})

test_that("load_metadata loads and processes CSV correctly", {
  # Create temporary test CSV
  test_csv <- tempfile(fileext = ".csv")
  
  test_data <- data.frame(
    Camera = c("Site01", "Site01", "Site02"),
    Relative_Path = c("Site01", "Site01", "Site02"),
    File = c("IMG_0001.JPG", "VID_0001.MP4", "IMG_0002.JPG"),
    Date_Time = c("2024-01-15T10:30:00", "2024-01-15T11:00:00", "2024-01-16T09:00:00"),
    Latitude = c(0.95, 0.95, 1.05),
    Longitude = c(34.65, 34.65, 34.55),
    stringsAsFactors = FALSE
  )
  
  write.csv(test_data, test_csv, row.names = FALSE)
  
  # Load metadata
  meta <- load_metadata(test_csv, verbose = FALSE)
  
  # Test basic structure
  expect_s3_class(meta, "data.frame")
  expect_equal(nrow(meta), 3)
  
  # Test column names are cleaned
  expect_true("camera" %in% names(meta))
  expect_true("relative_path" %in% names(meta))
  expect_true("file" %in% names(meta))
  expect_true("date_time" %in% names(meta))
  
  # Test site_name is created
  expect_true("site_name" %in% names(meta))
  expect_equal(unique(meta$site_name), c("Site01", "Site02"))
  
  # Test image_path is created
  expect_true("image_path" %in% names(meta))
  expect_equal(meta$image_path[1], "Site01/IMG_0001.JPG")
  
  # Test thumb_path is created
  expect_true("thumb_path" %in% names(meta))
  
  # Test video thumbnail path adjustment
  expect_true("is_video" %in% names(meta))
  expect_true(meta$is_video[2])  # VID_0001.MP4 should be TRUE
  expect_false(meta$is_video[1]) # IMG_0001.JPG should be FALSE
  expect_equal(meta$thumb_path[2], "Site01/VID_0001.jpg") # Video thumb should be .jpg
  expect_equal(meta$thumb_path[1], "Site01/IMG_0001.JPG") # Image thumb unchanged
  
  # Test date_time parsing
  expect_s3_class(meta$date_time, "POSIXct")
  expect_false(any(is.na(meta$date_time)))
  
  # Test image_path_flat
  expect_true("image_path_flat" %in% names(meta))
  expect_true(grepl("\\.", meta$image_path_flat[1]))
  
  # Cleanup
  unlink(test_csv)
})

test_that("load_metadata handles missing lat/lon columns", {
  # Create temporary test CSV without lat/lon
  test_csv <- tempfile(fileext = ".csv")
  
  test_data <- data.frame(
    Camera = c("Site01", "Site02"),
    Relative_Path = c("Site01", "Site02"),
    File = c("IMG_0001.JPG", "IMG_0002.JPG"),
    stringsAsFactors = FALSE
  )
  
  write.csv(test_data, test_csv, row.names = FALSE)
  
  # Load metadata - should generate placeholder lat/lon
  meta <- load_metadata(test_csv, verbose = FALSE)
  
  expect_true("latitude" %in% names(meta))
  expect_true("longitude" %in% names(meta))
  expect_false(any(is.na(meta$latitude)))
  expect_false(any(is.na(meta$longitude)))
  
  # Cleanup
  unlink(test_csv)
})

test_that("load_metadata handles missing date_time column", {
  # Create temporary test CSV without date_time
  test_csv <- tempfile(fileext = ".csv")
  
  test_data <- data.frame(
    Camera = c("Site01"),
    Relative_Path = c("Site01"),
    File = c("IMG_0001.JPG"),
    stringsAsFactors = FALSE
  )
  
  write.csv(test_data, test_csv, row.names = FALSE)
  
  # Load metadata
  meta <- load_metadata(test_csv, verbose = FALSE)
  
  expect_true("date_time" %in% names(meta))
  expect_true(is.na(meta$date_time[1]))
  
  # Cleanup
  unlink(test_csv)
})

test_that("load_metadata handles backslashes in camera names", {
  # Create temporary test CSV
  test_csv <- tempfile(fileext = ".csv")
  
  test_data <- data.frame(
    Camera = c("Path\\To\\Site01"),
    Relative_Path = c("Site01"),
    File = c("IMG_0001.JPG"),
    stringsAsFactors = FALSE
  )
  
  write.csv(test_data, test_csv, row.names = FALSE)
  
  # Load metadata
  meta <- load_metadata(test_csv, verbose = FALSE)
  
  # Camera should be cleaned to just "Site01"
  expect_equal(meta$camera[1], "Site01")
  expect_equal(meta$site_name[1], "Site01")
  
  # Cleanup
  unlink(test_csv)
})

test_that("load_metadata verbose parameter works", {
  # Create temporary test CSV
  test_csv <- tempfile(fileext = ".csv")
  
  test_data <- data.frame(
    Camera = c("Site01"),
    Relative_Path = c("Site01"),
    File = c("IMG_0001.JPG"),
    Date_Time = c("2024-01-15T10:30:00"),
    stringsAsFactors = FALSE
  )
  
  write.csv(test_data, test_csv, row.names = FALSE)
  
  # Test verbose = TRUE produces messages
  expect_message(
    load_metadata(test_csv, verbose = TRUE),
    "Loading metadata"
  )
  
  # Test verbose = FALSE suppresses messages
  expect_silent(
    load_metadata(test_csv, verbose = FALSE)
  )
  
  # Cleanup
  unlink(test_csv)
})
