test_that("simulated data can be converted", {
  set.seed(1)
  sim <- simulate_stay_data(n_station = 5, days = 30)
  expect_true(all(c("deploymentID", "video_name", "DateTime", "species",
                    "enter", "out", "stayingTimeCensoringType",
                    "Enter_new", "Enter_cont") %in% names(sim)))
  det <- convert_stay(sim, term = "term1")
  truth <- attr(sim, "truth")

  # every entry is counted once and gets one staying time
  expect_equal(sum(det$y, na.rm = TRUE), sum(sim$Enter_new, na.rm = TRUE))
  expect_equal(sum(!is.na(det$Stay)), sum(det$y, na.rm = TRUE))
  expect_lte(sum(det$y, na.rm = TRUE), truth$n_entry)
  # videos without a new entry have no staying time
  expect_true(all(is.na(det$Stay[det$y %in% 0L])))
  expect_true(all(det$Cens %in% c(0, 1, NA)))
})

test_that("each camera has set-up and retrieval videos", {
  set.seed(2)
  sim <- simulate_stay_data(n_station = 3, days = 10)
  people <- sim[sim$species == "hito", ]
  expect_equal(nrow(people), 6)
  span <- tapply(as.POSIXct(people$DateTime, format = "%Y/%m/%d %H:%M:%S", tz = "UTC"),
                 people$deploymentID, function(x) as.numeric(diff(range(x)), units = "days"))
  expect_equal(as.vector(span), rep(10, 3))
})

test_that("stays spanning videos are merged into one event", {
  set.seed(3)
  sim <- simulate_stay_data(n_station = 3, days = 30, stay_mean = 30, stay_sd = 20)
  expect_gt(sum(sim$Enter_cont, na.rm = TRUE), 0)
  det <- convert_stay(sim)
  expect_gt(max(det$Stay, na.rm = TRUE), 20)  # longer than one video
})

test_that("the number of entries follows the REST model", {
  set.seed(4)
  sim <- simulate_stay_data(n_station = 40, days = 60)
  truth <- attr(sim, "truth")
  expected <- truth$density / 1e6 * truth$focal_area / truth$mean_stay *
    truth$activity_level * truth$effort * 86400
  expect_lt(abs(truth$n_entry - expected) / expected, 0.15)
})
