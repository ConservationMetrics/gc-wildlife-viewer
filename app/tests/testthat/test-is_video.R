# tests/testthat/test-is_video.R

test_that("is_video returns TRUE for video files", {
  expect_true(is_video("clip.mp4"))
  expect_true(is_video("clip.MP4"))
  expect_true(is_video("clip.mov"))
  expect_true(is_video("clip.avi"))
  expect_true(is_video("clip.mkv"))
  expect_true(is_video("clip.webm"))
  expect_true(is_video("path/to/video.mp4"))
})

test_that("is_video returns FALSE for image files", {
  expect_false(is_video("photo.jpg"))
  expect_false(is_video("photo.png"))
  expect_false(is_video("photo.gif"))
  expect_false(is_video("photo.bmp"))
  expect_false(is_video("path/to/image.jpeg"))
})

test_that("is_video returns FALSE for unknown files", {
  expect_false(is_video("document.pdf"))
  expect_false(is_video("data.csv"))
  expect_false(is_video(NA))
  expect_false(is_video(""))
})
