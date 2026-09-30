example_data <- function() {
  read.csv(system.file("extdata", "example_stay.csv", package = "STwrapper"))
}

test_that("parse_video_datetime reads yymmdd_HHMMSS from file names", {
  x <- parse_video_datetime(c("c15_240501_105240_05010001.MOV", "bad.MOV"))
  expect_equal(format(x[1], "%Y/%m/%d %H:%M:%S"), "2024/05/01 10:52:40")
  expect_true(is.na(x[2]))
})

test_that("output has the ctrest detection_data columns and types", {
  res <- convert_stay(example_data())
  expect_named(res, c("Station", "DateTime", "Term", "Species", "y", "Stay", "Cens"))
  expect_s3_class(res$DateTime, "POSIXct")
  expect_type(res$y, "integer")
  expect_type(res$Stay, "double")
  expect_type(res$Cens, "double")
})

test_that("events spanning several videos are merged", {
  res <- convert_stay(example_data())
  deer <- res[res$Species == "deer", ]
  expect_equal(format(deer$DateTime, "%Y/%m/%d %H:%M:%S"),
               c("2024/06/01 23:59:50", "2024/06/02 00:00:30", "2024/06/02 00:01:00"))
  expect_equal(deer$y, c(1L, 0L, 0L))
  # 23:59:40 -> 00:00:40 on the next day
  expect_equal(deer$Stay, c(60, NA, NA))
  expect_equal(deer$Cens, c(0, NA, NA))
})

test_that("several entries in one video give one y and extra stay rows", {
  res <- convert_stay(example_data())
  boar <- res[res$Species == "boar", ]
  expect_equal(boar$y, c(2L, NA, 0L))
  # the second event continues into the next video with the same sp_ID
  expect_equal(boar$Stay, c(49, 5, NA))
  expect_equal(boar$Cens, c(1, 0, NA))
  expect_equal(boar$DateTime[1], boar$DateTime[2])
})

test_that("detections without an entry get y 0 or NA", {
  res <- convert_stay(example_data())
  expect_true(is.na(res$y[res$Species == "hito"]))
  fox <- res[res$Species == "fox", ]
  expect_equal(fox$y, c(0L, 1L))
  expect_equal(fox$Stay, c(NA, 8))
})

test_that("term can be a constant or a column", {
  res <- convert_stay(example_data(), term = "term1")
  expect_true(all(res$Term == "term1"))
  d <- example_data()
  d$period <- "T1"
  expect_true(all(convert_stay(d, term = "period")$Term == "T1"))
})

test_that("unmatched Enter_cont warns and starts a new event", {
  d <- example_data()
  d <- d[d$id != 2, ]
  expect_warning(res <- convert_stay(d), "no open event")
  deer <- res[res$Species == "deer", ]
  expect_equal(deer$y, c(1L, 0L))
  expect_equal(deer$Stay, c(38, NA))
})

test_that("a file path is rejected", {
  expect_error(convert_stay("example_stay.csv"), "must be a data frame")
})

test_that("only the minimal columns are needed", {
  d <- example_data()
  minimal <- d[, c("deploymentID", "video_name", "species1", "enter", "out",
                   "stayingTimeCensoringtype", "Enter_cont")]
  res <- convert_stay(minimal)
  deer <- res[res$Species == "deer", ]
  expect_equal(deer$Stay, c(60, NA, NA))
  expect_equal(deer$y, c(1L, 0L, 0L))
})

test_that("extra columns are ignored", {
  d <- example_data()
  d$extra <- "x"
  expect_equal(convert_stay(d), convert_stay(example_data()))
})

test_that("video_name is not needed when DateTime is given", {
  d <- example_data()
  d$DateTime <- format(parse_video_datetime(d$video_name), "%Y/%m/%d %H:%M:%S")
  d$video_name <- NULL
  expect_equal(convert_stay(d), convert_stay(example_data()))
})

test_that("DateTime or video_name is required", {
  d <- example_data()
  d$video_name <- NULL
  d$DateTime <- NULL
  expect_error(convert_stay(d), "date-time column")
})

test_that("missing required columns give an error", {
  d <- example_data()
  d$Enter_cont <- NULL
  expect_error(convert_stay(d), "Enter_cont")
})

test_that("sp_ID is optional", {
  d <- example_data()
  d$sp_ID <- NULL
  res <- convert_stay(d)
  # without sp_ID the earliest open event is continued
  boar <- res[res$Species == "boar" & !is.na(res$Stay), ]
  expect_equal(sort(boar$Stay), c(5, 49))
})
