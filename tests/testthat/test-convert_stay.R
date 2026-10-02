example_data <- function() {
  read.csv(system.file("extdata", "example_stay.csv", package = "STwrapper"))
}

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
               c("2024/06/01 23:59:50", "2024/06/02 00:00:15", "2024/06/02 00:00:40"))
  expect_equal(deer$y, c(1L, 0L, 0L))
  # 23:59:40 -> 00:00:30 on the next day
  expect_equal(deer$Stay, c(50, NA, NA))
  expect_equal(deer$Cens, c(0, NA, NA))
})

test_that("several entries in one video give one row each with y = 1", {
  res <- convert_stay(example_data())
  boar <- res[res$Species == "boar", ]
  expect_equal(boar$y, c(1L, 1L, 0L))
  # the first event continues into the next video with the same sp_ID
  expect_equal(boar$Stay, c(43, 5, NA))
  expect_equal(boar$Cens, c(1, 0, NA))
  expect_equal(boar$DateTime[1], boar$DateTime[2])
})

test_that("rows with a staying time always have y = 1", {
  set.seed(5)
  res <- convert_stay(simulate_stay_data(n_station = 5, days = 30))
  expect_true(all(res$y[!is.na(res$Stay)] == 1L))
  expect_true(all(is.na(res$Stay[res$y %in% 0L])))
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
  expect_equal(deer$Stay, c(35, NA))
})

test_that("a file path is rejected", {
  expect_error(convert_stay("example_stay.csv"), "must be a data frame")
})

test_that("only the minimal columns are needed", {
  d <- example_data()
  minimal <- d[, c("deploymentID", "DateTime", "species", "enter", "out",
                   "stayingTimeCensoringType", "Enter_cont")]
  res <- convert_stay(minimal)
  deer <- res[res$Species == "deer", ]
  expect_equal(deer$Stay, c(50, NA, NA))
  expect_equal(deer$y, c(1L, 0L, 0L))
})

test_that("extra columns are ignored", {
  d <- example_data()
  d$extra <- "x"
  expect_equal(convert_stay(d), convert_stay(example_data()))
})

test_that("video_name is not needed", {
  d <- example_data()
  d$video_name <- NULL
  expect_equal(convert_stay(d), convert_stay(example_data()))
})

test_that("DateTime is required and must be filled", {
  d <- example_data()
  d$DateTime <- NULL
  expect_error(convert_stay(d), "DateTime")

  # convert_stay() does not read date-times from file names
  d <- example_data()
  d$DateTime[3] <- NA
  expect_error(convert_stay(d), "empty in row[(]s[)] 3 .*fill_datetime")

  d <- example_data()
  d$DateTime[2] <- "June 1"
  expect_error(convert_stay(d), "could not be read in row[(]s[)] 2 ")
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
  expect_equal(sort(boar$Stay), c(5, 43))
})

test_that("consistent data give no warnings", {
  expect_no_warning(convert_stay(example_data()))
  set.seed(6)
  expect_no_warning(convert_stay(simulate_stay_data(n_station = 10, days = 60)))
})

test_that("Enter_cont rows that are not left or both censored are reported", {
  d <- example_data()
  d$stayingTimeCensoringType[d$id == 6] <- "right"
  expect_match(capture_warnings(convert_stay(d)), "not left or both: row[(]s[)] 7 of 'data'", all = FALSE)
})

test_that("inconsistent records are reported with their rows", {
  d <- example_data()
  d$stayingTimeCensoringType[2] <- "rihgt"
  expect_match(capture_warnings(convert_stay(d)), "Unknown stayingTimeCensoringType.*row[(]s[)] 2 ", all = FALSE)

  d <- example_data()
  d$out[9] <- NA
  expect_match(capture_warnings(convert_stay(d)), "Only one of 'enter' and 'out'.*row[(]s[)] 9 ", all = FALSE)

  d <- example_data()
  d$out[9] <- "10:10:40"
  expect_match(capture_warnings(convert_stay(d)), "'out' is earlier than 'enter'.*row[(]s[)] 9 ", all = FALSE)

  d <- example_data()
  d$Enter_new[3] <- TRUE
  expect_match(capture_warnings(convert_stay(d)), "both TRUE.*row[(]s[)] 3 ", all = FALSE)

  d <- example_data()
  d$Enter_new[9] <- FALSE
  expect_match(capture_warnings(convert_stay(d)), "both FALSE.*row[(]s[)] 9 ", all = FALSE)

  d <- example_data()
  d$Enter_cont[1] <- TRUE
  expect_match(capture_warnings(convert_stay(d)), "without 'enter' and 'out'.*row[(]s[)] 1 ", all = FALSE)
})

test_that("an animal entering between videos is accepted without a warning", {
  d <- example_data()
  d$stayingTimeCensoringType[9] <- "left"
  expect_no_warning(res <- convert_stay(d))
  expect_equal(res$Stay[res$Species == "fox"], c(NA, 8))
})
