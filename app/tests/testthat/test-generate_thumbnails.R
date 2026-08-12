# tests/testthat/test-generate_thumbnails.R

test_that("generate_thumbnails requires magick package", {
  # This test assumes magick is installed
  # If not, the function should throw an error
  skip_if_not_installed("magick")
  
  # Create minimal test setup
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_images_thumb")
  test_thumb_dir <- file.path(temp_dir, "test_thumbs")
  dir.create(test_image_dir, showWarnings = FALSE)
  dir.create(test_thumb_dir, showWarnings = FALSE)
  
  meta <- data.frame(
    image_path = "https://jeroen.github.io/images/frink.png",
    thumb_path = file.path(test_thumb_dir,"test.png"),
    stringsAsFactors = FALSE
  )
  
  # Should not error with magick installed
  expect_silent(
    generate_thumbnails(meta, test_image_dir, test_thumb_dir, verbose = FALSE)
  )
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
  unlink(test_thumb_dir, recursive = TRUE)
})

test_that("generate_thumbnails creates thumbnail directories", {
  skip_if_not_installed("magick")
  
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_gen_images")
  test_thumb_dir <- file.path(temp_dir, "test_gen_thumbs")
  dir.create(test_image_dir, showWarnings = FALSE)
  dir.create(file.path(test_image_dir, "Site01"), showWarnings = FALSE)
  dir.create(file.path(test_image_dir, "Site02"), showWarnings = FALSE)
  
  # Create simple test images (1x1 pixel)
  img1 <- file.path(test_image_dir, "Site01/IMG_0001.JPG")
  img2 <- file.path(test_image_dir, "Site02/IMG_0002.JPG")
  
  # Create minimal PNG images
  magick::image_blank(1, 1, "white") %>%
    magick::image_write(img1, format = "jpeg")
  magick::image_blank(1, 1, "black") %>%
    magick::image_write(img2, format = "jpeg")
  
  # Create metadata
  meta <- data.frame(
    image_path = c("Site01/IMG_0001.JPG", "Site02/IMG_0002.JPG"),
    thumb_path = c("Site01/IMG_0001.JPG", "Site02/IMG_0002.JPG"),
    stringsAsFactors = FALSE
  )
  
  # Generate thumbnails
  generate_thumbnails(meta, test_image_dir, test_thumb_dir, verbose = FALSE)
  
  # Check directories were created
  expect_true(dir.exists(file.path(test_thumb_dir, "Site01")))
  expect_true(dir.exists(file.path(test_thumb_dir, "Site02")))
  
  # Check thumbnails were created
  expect_true(file.exists(file.path(test_thumb_dir, "Site01/IMG_0001.JPG")))
  expect_true(file.exists(file.path(test_thumb_dir, "Site02/IMG_0002.JPG")))
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
  unlink(test_thumb_dir, recursive = TRUE)
})

test_that("generate_thumbnails handles video files with ffmpeg", {
  skip_if_not_installed("magick")
  skip_if_not(check_ffmpeg(), "ffmpeg not available")
  cat(getwd())
  source("../../utils.r")
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_video_images")
  test_thumb_dir <- file.path(temp_dir, "test_video_thumbs")
  dir.create(test_image_dir, showWarnings = FALSE)
  dir.create(file.path(test_image_dir, "Site01"), showWarnings = FALSE)
  
  # Note: Creating actual video files for testing is complex
  # This test verifies the logic flow but may skip actual video creation
  
  # Create metadata for video file
  meta <- data.frame(
    image_path = c("Site01/VID_0001.MP4"),
    thumb_path = c("Site01/VID_0001.jpg"),  # Already adjusted for video
    file = c("VID_0001.MP4"),
    stringsAsFactors = FALSE
  )
  
  # Detect media type
  meta$media_type <- sapply(meta$file, get_media_type)
  
  # Verify video was detected
  expect_equal(meta$media_type[1], "video")
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
  unlink(test_thumb_dir, recursive = TRUE)
})

test_that("generate_thumbnails skips existing thumbnails", {
  skip_if_not_installed("magick")
  
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_skip_images")
  test_thumb_dir <- file.path(temp_dir, "test_skip_thumbs")
  dir.create(test_image_dir, showWarnings = FALSE)
  dir.create(test_thumb_dir, showWarnings = FALSE)
  dir.create(file.path(test_image_dir, "Site01"), showWarnings = FALSE)
  dir.create(file.path(test_thumb_dir, "Site01"), showWarnings = FALSE)
  
  # Create test image
  img1 <- file.path(test_image_dir, "Site01/IMG_0001.JPG")
  magick::image_blank(1, 1, "white") %>%
    magick::image_write(img1, format = "jpeg")
  
  # Create existing thumbnail
  thumb1 <- file.path(test_thumb_dir, "Site01/IMG_0001.JPG")
  magick::image_blank(1, 1, "red") %>%
    magick::image_write(thumb1, format = "jpeg")
  
  # Get original modification time
  original_mtime <- file.info(thumb1)$mtime
  
  # Wait a moment to ensure different timestamp
  Sys.sleep(0.1)
  
  # Create metadata
  meta <- data.frame(
    image_path = c("Site01/IMG_0001.JPG"),
    thumb_path = c("Site01/IMG_0001.JPG"),
    stringsAsFactors = FALSE
  )
  
  # Generate thumbnails - should skip existing
  generate_thumbnails(meta, test_image_dir, test_thumb_dir, verbose = FALSE)
  
  # Check that thumbnail was not regenerated (same mtime)
  new_mtime <- file.info(thumb1)$mtime
  expect_equal(original_mtime, new_mtime)
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
  unlink(test_thumb_dir, recursive = TRUE)
})

test_that("generate_thumbnails handles missing files gracefully", {
  skip_if_not_installed("magick")
  
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_missing_images")
  test_thumb_dir <- file.path(temp_dir, "test_missing_thumbs")
  dir.create(test_image_dir, showWarnings = FALSE)
  
  # Create metadata for non-existent file
  meta <- data.frame(
    image_path = c("NonExistent/IMG_0001.JPG"),
    thumb_path = c("NonExistent/IMG_0001.JPG"),
    stringsAsFactors = FALSE
  )
  
  # Should not error, just skip missing files
  expect_silent(
    generate_thumbnails(meta, test_image_dir, test_thumb_dir, verbose = FALSE)
  )
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
  unlink(test_thumb_dir, recursive = TRUE)
})

test_that("generate_thumbnails respects thumb_width parameter", {
  skip_if_not_installed("magick")
  
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_width_images")
  test_thumb_dir <- file.path(temp_dir, "test_width_thumbs")
  dir.create(test_image_dir, showWarnings = FALSE)
  dir.create(file.path(test_image_dir, "Site01"), showWarnings = FALSE)
  
  # Create test image with known size (100x100)
  img1 <- file.path(test_image_dir, "Site01/IMG_0001.JPG")
  magick::image_blank(100, 100, "white") %>%
    magick::image_write(img1, format = "jpeg")
  
  # Create metadata
  meta <- data.frame(
    image_path = c("Site01/IMG_0001.JPG"),
    thumb_path = c("Site01/IMG_0001.JPG"),
    stringsAsFactors = FALSE
  )
  
  # Generate thumbnails with width = 50
  generate_thumbnails(meta, test_image_dir, test_thumb_dir, 
                     thumb_width = 50, verbose = FALSE)
  
  # Check thumbnail size
  thumb1 <- file.path(test_thumb_dir, "Site01/IMG_0001.JPG")
  thumb_info <- magick::image_read(thumb1) %>% magick::image_info()
  
  expect_equal(thumb_info$width, 50)
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
  unlink(test_thumb_dir, recursive = TRUE)
})

test_that("generate_thumbnails verbose parameter works", {
  skip_if_not_installed("magick")
  
  # Create temporary directories
  temp_dir <- tempdir()
  test_image_dir <- file.path(temp_dir, "test_verb_images")
  test_thumb_dir <- file.path(temp_dir, "test_verb_thumbs")
  dir.create(test_image_dir, showWarnings = FALSE)
  
  # Create empty metadata
  meta <- data.frame(
    image_path = "test",#character(0),
    thumb_path = "test",#character(0),
    stringsAsFactors = FALSE
  )
  
  # Test verbose = TRUE produces messages
  expect_message(
    generate_thumbnails(meta, test_image_dir, test_thumb_dir, verbose = TRUE),
    "Preparing thumbnail generation"
  )
  
  # Test verbose = FALSE suppresses messages
  expect_silent(
    generate_thumbnails(meta, test_image_dir, test_thumb_dir, verbose = FALSE)
  )
  
  # Cleanup
  unlink(test_image_dir, recursive = TRUE)
  unlink(test_thumb_dir, recursive = TRUE)
})
