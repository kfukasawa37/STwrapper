example_path <- function() {
  system.file("extdata", "example_stay.csv", package = "STwrapper")
}

test_that("parse_video_datetime reads yymmdd_HHMMSS from file names", {
  x <- parse_video_datetime(c("c15_240501_105240_05010001.MOV", "bad.MOV"))
  expect_equal(format(x[1], "%Y/%m/%d %H:%M:%S"), "2024/05/01 10:52:40")
  expect_true(is.na(x[2]))
})

test_that("output has the ctrest detection data columns", {
  res <- convert_stay(example_path())
  expect_named(res, c("Season", "Station", "File", "DateTimeCorrected",
                      "Species", "Enter", "Stay", "RightCens"))
})

test_that("events spanning several videos are merged", {
  res <- convert_stay(example_path())

  deer <- res[res$Species == "deer", ]
  expect_equal(deer$File, c("camA_240601_235950_00010002.MOV",
                            "camA_240602_000030_00020003.MOV",
                            "camA_240602_000100_00020004.MOV"))
  expect_equal(deer$Enter, c(1L, 0L, 0L))
  # 23:59:40 -> 00:00:40 on the next day
  expect_equal(deer$Stay, c(60, NA, NA))
  expect_equal(deer$RightCens, c(FALSE, NA, NA))
  expect_equal(deer$DateTimeCorrected[1], "2024/06/01 23:59:50")
})

test_that("continuations join the event with the same sp_ID", {
  res <- convert_stay(example_path())
  boar <- res[res$Species == "boar" & res$Enter %in% 1L, ]
  boar <- boar[order(boar$Stay), ]
  expect_equal(boar$Stay, c(5, 49))
  expect_equal(boar$RightCens, c(FALSE, TRUE))
})

test_that("detections without an entry get Enter 0 or NA", {
  res <- convert_stay(example_path())
  expect_true(is.na(res$Enter[res$Species == "hito"]))
  fox <- res[res$Species == "fox", ]
  expect_equal(fox$Enter, c(0L, 1L))
  expect_equal(fox$Stay, c(NA, 8))
})

test_that("season can be a constant or a column", {
  res <- convert_stay(example_path(), season = "S2024")
  expect_true(all(res$Season == "S2024"))
  d <- utils::read.csv(example_path(), colClasses = "character")
  d$term <- "T1"
  expect_true(all(convert_stay(d, season = "term")$Season == "T1"))
})

test_that("unmatched Enter_cont warns and starts a new event", {
  d <- utils::read.csv(example_path(), colClasses = "character")
  d <- d[d$id != "2", ]
  expect_warning(res <- convert_stay(d), "no open event")
  deer <- res[res$Species == "deer", ]
  expect_equal(deer$Enter, c(1L, 0L))
  expect_equal(deer$Stay, c(38, NA))
})
