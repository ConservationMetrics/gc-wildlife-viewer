# tests/testthat/test-check_ffmpeg.R

test_that("check_ffmpeg returns a logical value", {
  result <- check_ffmpeg()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("check_ffmpeg returns TRUE when ffmpeg is available", {
  skip_if_not(Sys.which("ffmpeg") != "", "ffmpeg not installed")
  expect_true(check_ffmpeg())
})
