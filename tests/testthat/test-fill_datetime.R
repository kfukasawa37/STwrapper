test_that("parse_video_datetime reads yymmdd_HHMMSS from file names", {
  x <- parse_video_datetime(c("c15_240501_105240_05010001.MOV", "bad.MOV"))
  expect_equal(format(x[1], "%Y/%m/%d %H:%M:%S"), "2024/05/01 10:52:40")
  expect_true(is.na(x[2]))
  jst <- parse_video_datetime("c15_240501_105240_05010001.MOV", tz = "Asia/Tokyo")
  expect_equal(attr(jst, "tzone"), "Asia/Tokyo")
  expect_equal(format(jst, "%H:%M:%S"), "10:52:40")
})

test_that("empty date-times are filled from file names", {
  d <- data.frame(
    video_name = c("c15_240501_105240_05010001.MOV", "c15_240501_110228_05010002.MOV"),
    DateTime = c(NA, "2024/05/01 11:00:00"),
    other = 1:2
  )
  expect_message(res <- fill_datetime(d), "1 row")
  expect_equal(res$DateTime, c("2024/05/01 10:52:40", "2024/05/01 11:00:00"))
  expect_equal(res$other, 1:2)

  expect_message(res <- fill_datetime(d, overwrite = TRUE), "2 row")
  expect_equal(res$DateTime[2], "2024/05/01 11:02:28")
})

test_that("empty strings count as empty and the column is created", {
  d <- data.frame(video_name = "c15_240501_105240_05010001.MOV", DateTime = "")
  expect_equal(suppressMessages(fill_datetime(d))$DateTime, "2024/05/01 10:52:40")
  d$DateTime <- NULL
  expect_equal(suppressMessages(fill_datetime(d))$DateTime, "2024/05/01 10:52:40")
})

test_that("file names without a date-time are reported", {
  d <- data.frame(video_name = c("c15_240501_105240_05010001.MOV", "IMG_0001.MOV"))
  expect_warning(res <- suppressMessages(fill_datetime(d)), "row[(]s[)] 2 ")
  expect_true(is.na(res$DateTime[2]))
  expect_error(fill_datetime(data.frame(x = 1)), "video_name")
})

test_that("filling from file names reproduces the example date-times", {
  d <- read.csv(system.file("extdata", "example_stay.csv", package = "STwrapper"))
  empty <- d
  empty$DateTime <- NA
  filled <- suppressMessages(fill_datetime(empty))
  expect_equal(filled$DateTime, d$DateTime)
  expect_equal(convert_stay(filled), convert_stay(d))
})
