# tests/testthat/test-get_media_type.R

test_that("get_media_type correctly identifies image files", {
  expect_equal(get_media_type("photo.jpg"), "image")
  expect_equal(get_media_type("photo.JPG"), "image")
  expect_equal(get_media_type("photo.jpeg"), "image")
  expect_equal(get_media_type("photo.JPEG"), "image")
  expect_equal(get_media_type("photo.png"), "image")
  expect_equal(get_media_type("photo.PNG"), "image")
  expect_equal(get_media_type("photo.gif"), "image")
  expect_equal(get_media_type("photo.bmp"), "image")
})

test_that("get_media_type correctly identifies video files", {
  expect_equal(get_media_type("clip.mp4"), "video")
  expect_equal(get_media_type("clip.MP4"), "video")
  expect_equal(get_media_type("clip.mov"), "video")
  expect_equal(get_media_type("clip.MOV"), "video")
  expect_equal(get_media_type("clip.avi"), "video")
  expect_equal(get_media_type("clip.mkv"), "video")
  expect_equal(get_media_type("clip.webm"), "video")
})

test_that("get_media_type returns unknown for unrecognized extensions", {
  expect_equal(get_media_type("document.pdf"), "unknown")
  expect_equal(get_media_type("data.csv"), "unknown")
  expect_equal(get_media_type("script.R"), "unknown")
  expect_equal(get_media_type("file.txt"), "unknown")
  expect_equal(get_media_type("noextension"), "unknown")
})

test_that("get_media_type handles edge cases", {
  expect_equal(get_media_type(NA), "unknown")
  expect_equal(get_media_type(""), "unknown")
  expect_equal(get_media_type(NA_character_), "unknown")
})

test_that("get_media_type works with full paths", {
  expect_equal(get_media_type("path/to/camera/IMG_0001.JPG"), "image")
  expect_equal(get_media_type("path/to/camera/VID_0001.mp4"), "video")
  expect_equal(get_media_type("C:/Users/data/photo.png"), "image")
  expect_equal(get_media_type("/home/user/videos/clip.avi"), "video")
})

test_that("get_media_type handles multiple dots in filename", {
  expect_equal(get_media_type("my.photo.with.dots.jpg"), "image")
  expect_equal(get_media_type("my.video.with.dots.mp4"), "video")
})
