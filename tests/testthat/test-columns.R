example_data <- function() {
  read.csv(system.file("extdata", "example_stay.csv", package = "STwrapper"))
}

rename <- function(d, from, to) {
  names(d)[match(from, names(d))] <- to
  d
}

test_that("convert_stay() reads columns with other names", {
  d <- rename(example_data(),
              c("deploymentID", "species", "stayingTimeCensoringType", "Enter_cont",
                "enter", "out", "DateTime", "sp_ID", "Enter_new", "video_name"),
              c("camera", "species1", "stayingTimeCensoringtype", "cont",
                "t_in", "t_out", "date", "id_in_video", "new", "file"))
  res <- convert_stay(d, col_station = "camera", col_species = "species1",
                      col_cens = "stayingTimeCensoringtype", col_enter_cont = "cont",
                      col_enter = "t_in", col_out = "t_out", col_datetime = "date",
                      col_sp_id = "id_in_video", col_enter_new = "new",
                      col_file = "file")
  expect_equal(res, convert_stay(example_data()))
})

test_that("missing columns are reported with the names given", {
  d <- rename(example_data(), "species", "species1")
  expect_error(convert_stay(d), "species")
  expect_error(convert_stay(d, col_species = "sp"), "Column[(]s[)] not found in 'data': sp")
})

test_that("a standard column is replaced by the column given", {
  d <- example_data()
  d$species1 <- toupper(d$species)
  expect_warning(res <- convert_stay(d, col_species = "species1"),
                 "'species' is replaced by column 'species1'")
  expect_true(all(res$Species %in% c("HITO", "DEER", "BOAR", "FOX")))
})

test_that("preprocessing functions return standard column names", {
  d <- data.frame(
    file = "camA_240601_235950_06010001.MOV",
    date = NA,
    t_in = 0, t_out = 15, len = 20
  )
  filled <- suppressMessages(fill_datetime(d, col_datetime = "date", col_file = "file"))
  expect_true(all(c("DateTime", "video_name") %in% names(filled)))
  expect_false(any(c("date", "file") %in% names(filled)))
  expect_equal(filled$DateTime, "2024/06/01 23:59:50")

  res <- transform_elapsed(filled, col_enter_elapsed = "t_in", col_out_elapsed = "t_out",
                           col_video_length = "len", offset_videolength = TRUE)
  expect_true(all(c("enter_elapsed", "out_elapsed", "video_length",
                    "enter", "out") %in% names(res)))
  expect_equal(c(res$enter, res$out), c("23:59:30", "23:59:45"))
})

test_that("one column given for two items stops", {
  d <- example_data()
  d$t <- d$enter
  expect_error(convert_stay(d, col_enter = "t", col_out = "t"),
               "'t' is given for more than one item [(]enter, out[)]")
  # a non-default name that equals another item's default
  expect_error(convert_stay(d, col_species = "sp_ID"), "more than one item")
})

test_that("the standard column of another item cannot be given", {
  d <- example_data()
  expect_error(convert_stay(d, col_species = "enter", col_enter = "enter_time"),
               "'enter' is given for 'species'")
  # swapping two columns is refused as well
  expect_error(convert_stay(d, col_enter = "out", col_out = "enter"),
               "standard column for another item")
  # transform_elapsed() writes enter / out itself
  e <- data.frame(DateTime = "2024/06/01 23:59:50", enter = 0, out_elapsed = 10)
  expect_error(transform_elapsed(e, col_enter_elapsed = "enter"),
               "'enter' is given for 'enter_elapsed'")
})
