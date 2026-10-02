elapsed_data <- function() {
  d <- data.frame(
    id = 1:4,
    deploymentID = "camA",
    video_name = c("camA_240601_235950_06010001.MOV",
                   "camA_240602_000015_06020002.MOV",
                   "camA_240602_000040_06020003.MOV",
                   "camA_240602_120000_06020004.MOV"),
    species = c("deer", "deer", "deer", "hito"),
    enter_elapsed = c(0, 0, 0, NA),
    out_elapsed = c(20, 20, 10, NA),
    video_length = c(20, 20, 20, NA),
    stayingTimeCensoringType = c("right", "both", "left", NA),
    Enter_cont = c(FALSE, TRUE, TRUE, NA)
  )
  suppressMessages(fill_datetime(d))
}

test_that("elapsed times are added to the recording date-time", {
  res <- transform_elapsed(elapsed_data())
  expect_equal(res$enter, c("23:59:50", "0:00:15", "0:00:40", NA))
  expect_equal(res$out, c("0:00:10", "0:00:35", "0:00:50", NA))
  # other columns are kept
  expect_equal(res$species, elapsed_data()$species)
})

test_that("offset_videolength = TRUE subtracts the video length column", {
  res <- transform_elapsed(elapsed_data(), offset_videolength = TRUE)
  expect_equal(res$enter, c("23:59:30", "23:59:55", "0:00:20", NA))
  expect_equal(res$out, c("23:59:50", "0:00:15", "0:00:30", NA))
})

test_that("NA video length counts as 0", {
  d <- elapsed_data()
  d$video_length[1] <- NA
  res <- transform_elapsed(d, offset_videolength = TRUE)
  expect_equal(res$enter[1:2], c("23:59:50", "23:59:55"))
})

test_that("a number subtracts the same offset from every row", {
  expect_equal(transform_elapsed(elapsed_data(), offset_videolength = 20),
               transform_elapsed(elapsed_data(), offset_videolength = TRUE))
  res <- suppressWarnings(transform_elapsed(elapsed_data(), offset_videolength = 5))
  expect_equal(res$enter[1], "23:59:45")
})

test_that("the result can be converted with convert_stay()", {
  res <- convert_stay(transform_elapsed(elapsed_data(), offset_videolength = TRUE))
  deer <- res[res$Species == "deer", ]
  # 23:59:30 -> 00:00:30 on the next day
  expect_equal(deer$Stay, c(60, NA, NA))
  expect_equal(deer$y, c(1L, 0L, 0L))
})

test_that("elapsed times may be strings and have decimals", {
  d <- elapsed_data()
  d$enter_elapsed <- c("0:01.5", "0", "0:00:00", NA)
  d$out_elapsed <- c("12.25", "20", "0:00:10", NA)
  res <- transform_elapsed(d)
  expect_equal(res$enter[1:3], c("23:59:51.5", "0:00:15", "0:00:40"))
  expect_equal(res$out[1], "0:00:02.25")
})

test_that("existing enter/out are kept where elapsed times are missing", {
  d <- elapsed_data()
  d$enter <- c(NA, NA, NA, "12:00:00")
  d$out <- c(NA, NA, NA, "12:00:05")
  res <- transform_elapsed(d)
  expect_equal(res$enter[4], "12:00:00")
  expect_equal(res$enter[1], "23:59:50")
})

test_that("only the date-time column is used", {
  d <- elapsed_data()
  d$video_name <- NULL
  expect_equal(transform_elapsed(d)$enter, transform_elapsed(elapsed_data())$enter)

  d <- elapsed_data()
  d$DateTime[1] <- NA
  expect_error(transform_elapsed(d), "empty.*fill_datetime")
  # rows without elapsed times need no date-time
  d <- elapsed_data()
  d$DateTime[4] <- NA
  expect_no_error(transform_elapsed(d))
})

test_that("invalid input gives errors or warnings", {
  expect_error(transform_elapsed(elapsed_data(), offset_videolength = -1),
               "offset_videolength")
  expect_error(transform_elapsed(elapsed_data(), offset_videolength = "yes"),
               "offset_videolength")
  d <- elapsed_data()
  d$video_length <- NULL
  expect_error(transform_elapsed(d, offset_videolength = TRUE), "video_length")
  d <- elapsed_data()
  d$out_elapsed <- NULL
  expect_error(transform_elapsed(d), "out_elapsed")
  d <- elapsed_data()
  d$enter_elapsed[1] <- "abc"
  expect_error(transform_elapsed(d), "Elapsed times must be")
  expect_warning(transform_elapsed(elapsed_data(), offset_videolength = 5),
                 "longer than the video length")
  expect_no_warning(transform_elapsed(elapsed_data(), offset_videolength = TRUE))
  d <- elapsed_data()
  d$enter_elapsed[1] <- -1
  expect_warning(transform_elapsed(d), "Negative elapsed time")
  d <- elapsed_data()
  d$out_elapsed[1] <- 0
  d$enter_elapsed[1] <- 5
  expect_warning(transform_elapsed(d), "smaller than")
})
