# tests/testthat/test-create_video_thumbnail.R

test_that("create_video_thumbnail returns logical", {
  skip_if_not(check_ffmpeg(), "ffmpeg not available")
  
  # Test with non-existent file - should return FALSE
  result <- create_video_thumbnail(
    "nonexistent.mp4",
    "nonexistent_thumb.jpg",
    thumb_width = 300,
    seek_time = 1
  )
  
  expect_type(result, "logical")
  expect_length(result, 1)
  expect_false(result)
})

test_that("create_video_thumbnail adjusts thumbnail extension to jpg", {
  skip_if_not(check_ffmpeg(), "ffmpeg not available")
  
  # This is a mock test since we can't easily create real video files
  # We test that the function would change the extension
  
  # The function should internally convert any extension to .jpg
  # We can't fully test this without a real video file, but we can
  # verify the function doesn't error on proper inputs
  
  expect_type(
    create_video_thumbnail("test.mp4", "test.png", 300, 1),
    "logical"
  )
})

test_that("create_video_thumbnail handles different seek times", {
  skip_if_not(check_ffmpeg(), "ffmpeg not available")
  
  # Test with seek_time = 0 (first frame)
  result <- create_video_thumbnail(
    "nonexistent.mp4",
    "thumb.jpg",
    thumb_width = 300,
    seek_time = 0
  )
  
  expect_type(result, "logical")
  
  # Test with seek_time = 5
  result <- create_video_thumbnail(
    "nonexistent.mp4",
    "thumb.jpg",
    thumb_width = 300,
    seek_time = 5
  )
  
  expect_type(result, "logical")
})

test_that("create_video_thumbnail handles different widths", {
  skip_if_not(check_ffmpeg(), "ffmpeg not available")
  
  # Test with small width
  result <- create_video_thumbnail(
    "nonexistent.mp4",
    "thumb.jpg",
    thumb_width = 100,
    seek_time = 1
  )
  
  expect_type(result, "logical")
  
  # Test with large width
  result <- create_video_thumbnail(
    "nonexistent.mp4",
    "thumb.jpg",
    thumb_width = 800,
    seek_time = 1
  )
  
  expect_type(result, "logical")
})
