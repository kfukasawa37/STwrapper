#' Convert elapsed times within videos to clock times
#'
#' Prepares records in which the times of entering and leaving the focal area
#' are given as elapsed seconds within each video, for use with
#' [convert_stay()]. The clock times are the recording date-time of the video
#' plus the elapsed seconds:
#' * `enter` = recording date-time + `enter_elapsed`
#' * `out` = recording date-time + `out_elapsed`
#'
#' The recording date-time is taken from the date-time column. Use
#' [fill_datetime()] beforehand to fill empty date-times from the file names.
#'
#' When the recording date-time is the time at which recording ended (as in
#' file names written at the end of a video), use `offset_videolength` to
#' subtract the length of the video so that the elapsed times count from the
#' start of the video:
#' * `FALSE` (default): nothing is subtracted.
#' * `TRUE`: the number of seconds in the video length column is subtracted
#'   (`NA` is taken as 0).
#' * A number of 0 or more: that number of seconds is subtracted from every
#'   row.
#'
#' @param data A data frame, as read by [utils::read.csv()].
#' @param col_enter_elapsed,col_out_elapsed Columns with the elapsed times of
#'   entering and leaving the focal area, counted from the recording
#'   date-time. Seconds as numbers (decimals allowed) or as `"M:SS"` /
#'   `"H:MM:SS"`. Defaults `"enter_elapsed"` and `"out_elapsed"`.
#' @param offset_videolength `FALSE`, `TRUE` or a number of seconds of 0 or
#'   more; see Details.
#' @param col_video_length Column with the length of each video in seconds,
#'   used when `offset_videolength = TRUE`. Default `"video_length"`.
#' @param col_datetime Column with the recording date-time of each video
#'   (`yyyy/mm/dd HH:MM:SS`). Default `"DateTime"`.
#' @param tz Time zone used to interpret the date-times. Default `"UTC"`.
#'
#' A warning lists the rows with a negative elapsed time, with `out_elapsed`
#' smaller than `enter_elapsed`, or, when an offset is subtracted, with an
#' elapsed time longer than the offset (the video length).
#'
#' @return `data` with the columns `enter` and `out` set to clock times
#'   (`"H:MM:SS"`, with decimals when the elapsed times have them). Rows whose
#'   elapsed time is `NA` keep the value already in `enter` / `out`, or `NA`
#'   when there is no such column. All other columns are returned unchanged.
#'
#' @examples
#' d <- data.frame(
#'   DateTime = "2024/06/01 23:59:50",
#'   enter_elapsed = 0,
#'   out_elapsed = 15,
#'   video_length = 20
#' )
#' # file name time is the end of the video: subtract its length
#' transform_elapsed(d, offset_videolength = TRUE)
#' transform_elapsed(d, offset_videolength = 20)
#' @export
transform_elapsed <- function(data,
                              col_enter_elapsed = "enter_elapsed",
                              col_out_elapsed = "out_elapsed",
                              offset_videolength = FALSE,
                              col_video_length = "video_length",
                              col_datetime = "DateTime",
                              tz = "UTC") {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  missing <- setdiff(c(col_enter_elapsed, col_out_elapsed, col_datetime), names(data))
  if (length(missing) > 0) {
    stop("Column(s) not found in 'data': ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  # Seconds to subtract from the recording date-time
  if (isTRUE(offset_videolength)) {
    if (!col_video_length %in% names(data)) {
      stop(sprintf("Column '%s' not found in 'data' (needed for offset_videolength = TRUE).",
                   col_video_length), call. = FALSE)
    }
    offset <- elapsed_seconds(data[[col_video_length]])
    if (any(!is.na(as_chr(data[[col_video_length]])) & is.na(offset))) {
      stop(sprintf("Column '%s' must contain seconds.", col_video_length), call. = FALSE)
    }
    offset[is.na(offset)] <- 0
  } else if (isFALSE(offset_videolength)) {
    offset <- rep(0, nrow(data))
  } else if (is.numeric(offset_videolength) && length(offset_videolength) == 1 &&
             is.finite(offset_videolength) && offset_videolength >= 0) {
    offset <- rep(offset_videolength, nrow(data))
  } else {
    stop("'offset_videolength' must be FALSE, TRUE or a single number of 0 or more.",
         call. = FALSE)
  }

  enter_sec <- elapsed_seconds(data[[col_enter_elapsed]])
  out_sec <- elapsed_seconds(data[[col_out_elapsed]])
  bad <- (!is.na(as_chr(data[[col_enter_elapsed]])) & is.na(enter_sec)) |
    (!is.na(as_chr(data[[col_out_elapsed]])) & is.na(out_sec))
  if (any(bad)) {
    stop("Elapsed times must be seconds or 'M:SS' / 'H:MM:SS': row(s) ",
         paste(utils::head(which(bad), 10), collapse = ", "), " of 'data'.",
         call. = FALSE)
  }

  has_time <- !is.na(enter_sec) | !is.na(out_sec)
  video_dt <- video_datetime(data[[col_datetime]], tz)
  check_datetime(video_dt, data[[col_datetime]], col_datetime,
                 rows = which(has_time & is.na(video_dt)))
  warn_rows(which(enter_sec < 0 | out_sec < 0), "Negative elapsed time")
  warn_rows(which(offset > 0 & (enter_sec > offset | out_sec > offset)),
            "Elapsed time longer than the video length (offset)")
  warn_rows(which(out_sec < enter_sec), "'out_elapsed' is smaller than 'enter_elapsed'")

  start <- video_dt - offset
  data$enter <- fill_clock(data$enter, start + enter_sec, enter_sec, tz)
  data$out <- fill_clock(data$out, start + out_sec, out_sec, tz)
  data
}

# Seconds from numbers or "M:SS" / "H:MM:SS" strings
elapsed_seconds <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- as_chr(x)
  vapply(strsplit(x, ":", fixed = TRUE), function(p) {
    if (length(p) == 0 || anyNA(p)) return(NA_real_)
    p <- suppressWarnings(as.numeric(p))
    if (anyNA(p) || length(p) > 3) return(NA_real_)
    sum(p * c(3600, 60, 1)[(4 - length(p)):3])
  }, numeric(1))
}

# Clock times where elapsed seconds are given, existing values elsewhere
fill_clock <- function(old, time, sec, tz) {
  res <- if (is.null(old)) rep(NA_character_, length(sec)) else as_chr(old)
  ok <- !is.na(sec)
  if (!any(ok)) return(res)
  lt <- as.POSIXlt(time[ok], tz = tz)
  s <- round(lt$sec, 3)
  s_txt <- ifelse(s == round(s), sprintf("%02d", as.integer(round(s))),
                  sub("0+$", "", sprintf("%06.3f", s)))
  res[ok] <- sprintf("%d:%02d:%s", lt$hour, lt$min, s_txt)
  res
}
