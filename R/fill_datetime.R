#' Fill the recording date-time from video file names
#'
#' Fills empty recording date-times with the date-time in the video file name,
#' for use before [transform_elapsed()] and [convert_stay()], which take the
#' recording date-time only from the date-time column. The file names must be
#' of the form `<camera>_yymmdd_HHMMSS_...` (see [parse_video_datetime()]).
#'
#' The filled values are written to the date-time column as text
#' (`yyyy/mm/dd HH:MM:SS`), so they can be checked before the conversion. A
#' message reports how many rows were filled, and a warning lists the rows
#' whose file name has no date-time.
#'
#' Columns given with `col_datetime` and `col_file` are returned under the
#' standard names `DateTime` and `video_name`, so the following functions need
#' no column arguments for them.
#'
#' @param data A data frame, as read by [utils::read.csv()].
#' @param col_datetime Column with the recording date-time of each video. It
#'   is created when missing. Default `"DateTime"`.
#' @param col_file Column with the video file name. Default `"video_name"`.
#' @param overwrite If `TRUE`, date-times that are already filled are also
#'   replaced with the date-time in the file name. Default `FALSE`.
#'
#' @return `data` with the date-time column filled, and the date-time and
#'   file name columns named `DateTime` and `video_name`. All other columns are
#'   returned unchanged.
#'
#' @examples
#' d <- data.frame(
#'   video_name = c("c15_240501_105240_05010001.MOV", "c15_240501_110228_05010002.MOV"),
#'   DateTime = c(NA, "2024/05/01 11:02:28")
#' )
#' fill_datetime(d)
#' @export
fill_datetime <- function(data,
                          col_datetime = "DateTime",
                          col_file = "video_name",
                          overwrite = FALSE) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  data <- standardize_columns(data, c(DateTime = col_datetime, video_name = col_file),
                              required = "video_name")
  current <- if ("DateTime" %in% names(data)) {
    as_chr(data$DateTime)
  } else {
    rep(NA_character_, nrow(data))
  }
  target <- if (isTRUE(overwrite)) rep(TRUE, nrow(data)) else is.na(current)

  file <- as_chr(data$video_name)
  parsed <- format(parse_video_datetime(file, tz = "UTC"), "%Y/%m/%d %H:%M:%S",
                   tz = "UTC")
  filled <- target & !is.na(parsed)
  current[filled] <- parsed[filled]
  data$DateTime <- current

  message(sprintf("Filled 'DateTime' of %d row(s) from '%s'.", sum(filled), col_file))
  warn_rows(which(target & !is.na(file) & is.na(parsed)),
            "No date-time could be read from the file name")
  data
}

#' Parse the recording date-time from a video file name
#'
#' Extracts the date-time from file names of the form
#' `<camera>_yymmdd_HHMMSS_...` (for example
#' `c15_240501_105240_05010001.MOV` gives 2024-05-01 10:52:40).
#'
#' @param x Character vector of file names.
#' @param tz Time zone of the returned date-times. Default `"UTC"`.
#' @return A `POSIXct` vector; `NA` where the name does not match.
#' @examples
#' parse_video_datetime("c15_240501_105240_05010001.MOV", tz = "Asia/Tokyo")
#' @export
parse_video_datetime <- function(x, tz = "UTC") {
  m <- regmatches(x, regexec("^[^_]+_(\\d{6})_(\\d{6})_", x))
  s <- vapply(m, function(v) {
    if (length(v) == 3) paste(v[2], v[3]) else NA_character_
  }, character(1))
  as.POSIXct(s, format = "%y%m%d %H%M%S", tz = tz)
}
