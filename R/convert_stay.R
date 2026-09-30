#' Convert staying time records to 'ctrest' detection data
#'
#' Converts a per-video staying time table (one row per animal and video, with
#' `enter` / `out` clock times) into the detection data format used by the
#' 'ctrest' package (`Season`, `Station`, `File`, `DateTimeCorrected`,
#' `Species`, `Enter`, `Stay`, `RightCens`).
#'
#' Required columns are `enter`, `out`, `stayingTimeCensoringtype`,
#' `Enter_cont`, the station and species columns, and at least one of the
#' date-time column (`DateTime`) and the file name column (`video_name`).
#' Other columns such as `Enter_new`, `sp_ID`, `note` and `memo` are used when
#' present, and any further columns are ignored. When the date-time column is
#' missing or empty, the date-time is read from the file name (see
#' [parse_video_datetime()]). When the file name column is missing, videos are
#' identified by their date-time and `File` is `NA`.
#'
#' A staying event that spans several videos is recorded as several rows in
#' the input: the first row has `Enter_new = TRUE` and each row in a following
#' video has `Enter_cont = TRUE`. For each station and species, a row with
#' `Enter_cont = TRUE` is merged into an event that was still in view
#' (`stayingTimeCensoringtype` of `"right"` or `"both"`) at the end of the
#' previous video in which the same species appeared. When several such events
#' are open, the one with the same `sp_ID` is preferred, then the earliest.
#'
#' Each merged event becomes one output row:
#' * `File` and `DateTimeCorrected` are those of the first video of the event.
#' * `Enter` is 1.
#' * `Stay` is the time in seconds from `enter` of the first row to `out` of
#'   the last row of the event.
#' * `RightCens` is `TRUE` when the last row of the event has
#'   `stayingTimeCensoringtype` `"right"` or `"both"`.
#'
#' Videos in which a species was detected but no event started (for example
#' `"noentry"` in `note`, `memo` or `stayingTimeCensoringtype`, or only
#' continuations of earlier events) are output as one row with `Enter = 0`
#' and `Stay` / `RightCens` set to `NA`. Detections
#' without any entry information (no `enter`/`out` and no `"noentry"`)
#' get `Enter = NA`.
#'
#' The `enter` and `out` columns are clock times (`H:MM:SS`). They are placed
#' on the calendar day (the video's date, or the day before or after) that
#' puts them closest to the video's recording time, so events around midnight
#' are handled correctly.
#'
#' @param data A data frame in the staying time input format, as read by
#'   [utils::read.csv()].
#' @param col_station Column used as `Station`. Default `"deploymentID"`.
#' @param col_species Column used as `Species`. Default `"species1"`.
#' @param col_datetime Column with the recording date-time of each video
#'   (`yyyy/mm/dd HH:MM:SS`). Default `"DateTime"`.
#' @param col_file Column with the video file name. Default `"video_name"`.
#' @param season Value for the `Season` column: either a single value used for
#'   all rows or the name of a column in `data`. Default `NA`.
#' @param tz Time zone used to interpret the date-times. Default `"UTC"`.
#'
#' @return A data frame with columns `Season`, `Station`, `File`,
#'   `DateTimeCorrected` (character, `yyyy/mm/dd HH:MM:SS`), `Species`,
#'   `Enter` (integer), `Stay` (numeric, seconds) and `RightCens` (logical),
#'   ordered by station and date-time.
#'
#' @examples
#' path <- system.file("extdata", "example_stay.csv", package = "STwrapper")
#' convert_stay(read.csv(path))
#' @export
convert_stay <- function(data,
                         col_station = "deploymentID",
                         col_species = "species1",
                         col_datetime = "DateTime",
                         col_file = "video_name",
                         season = NA,
                         tz = "UTC") {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }

  required <- c("enter", "out", "stayingTimeCensoringtype", "Enter_cont",
                col_station, col_species)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Column(s) not found in 'data': ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  has_file <- col_file %in% names(data)
  if (!has_file && !col_datetime %in% names(data)) {
    stop(sprintf("'data' needs a date-time column ('%s') or a file name column ('%s').",
                 col_datetime, col_file), call. = FALSE)
  }
  optional_col <- function(col) {
    if (col %in% names(data)) as_chr(data[[col]]) else rep(NA_character_, nrow(data))
  }

  d <- data.frame(
    row = seq_len(nrow(data)),
    Station = as_chr(data[[col_station]]),
    File = optional_col(col_file),
    Species = as_chr(data[[col_species]]),
    enter = as_chr(data$enter),
    out = as_chr(data$out),
    cens = tolower(as_chr(data$stayingTimeCensoringtype)),
    enter_cont = as_lgl(data$Enter_cont),
    sp_ID = optional_col("sp_ID"),
    stringsAsFactors = FALSE
  )
  # "noentry" marks a detection without entry into the focal area
  d$noentry <- Reduce(`|`, lapply(
    intersect(c("note", "memo", "stayingTimeCensoringtype"), names(data)),
    function(col) tolower(as_chr(data[[col]])) %in% "noentry"
  ), rep(FALSE, nrow(d)))
  d$Season <- if (length(season) == 1 && is.character(season) &&
                  season %in% names(data)) {
    as_chr(data[[season]])
  } else {
    rep(season, length.out = nrow(d))
  }

  # Recording date-time of each video: DateTime column if filled, else file name
  video_dt <- as.POSIXct(optional_col(col_datetime), tz = tz,
                         tryFormats = c("%Y/%m/%d %H:%M:%OS", "%Y-%m-%d %H:%M:%OS",
                                        "%Y/%m/%d %H:%M", "%Y-%m-%d %H:%M"),
                         optional = TRUE)
  from_name <- is.na(video_dt)
  video_dt[from_name] <- parse_video_datetime(d$File[from_name], tz = tz)
  if (anyNA(video_dt)) {
    stop("Could not determine the date-time of row(s) ",
         paste(d$row[is.na(video_dt)], collapse = ", "), call. = FALSE)
  }
  d$video_dt <- video_dt
  # Key identifying a video: file name, or date-time when there is none
  d$video <- if (has_file) d$File else format(video_dt, "%Y-%m-%d %H:%M:%S", tz = tz)

  d <- d[!is.na(d$Species), , drop = FALSE]
  d$is_stay <- !is.na(d$enter) & !is.na(d$out)
  d$t_enter <- clock_to_datetime(d$enter, d$video_dt, tz)
  d$t_out <- clock_to_datetime(d$out, d$video_dt, tz)
  d$still_in <- d$cens %in% c("right", "both")

  d <- d[order(d$Station, d$Species, d$video_dt, d$video, d$row), , drop = FALSE]

  # Assign an event id to every stay row
  d$event <- NA_integer_
  n_event <- 0L
  for (key in unique(paste(d$Station, d$Species, sep = "\r"))) {
    idx <- which(paste(d$Station, d$Species, sep = "\r") == key)
    open <- integer(0)  # events still in view at the end of the previous video
    for (v in unique(d$video[idx])) {
      vid <- idx[d$video[idx] == v]
      used <- integer(0)
      for (i in vid[d$is_stay[vid]]) {
        ev <- NA_integer_
        if (isTRUE(d$enter_cont[i])) {
          cand <- setdiff(open, used)
          if (length(cand) > 0) {
            last <- vapply(cand, function(e) max(which(d$event == e)), integer(1))
            same_id <- cand[d$sp_ID[last] %in% d$sp_ID[i] & !is.na(d$sp_ID[i])]
            ev <- if (length(same_id) > 0) same_id[1] else cand[1]
          } else {
            warning(sprintf(
              "Enter_cont = TRUE in '%s' (%s) but no open event in the previous video; treated as a new event.",
              d$video[i], d$Species[i]), call. = FALSE)
          }
        }
        if (is.na(ev)) {
          n_event <- n_event + 1L
          ev <- n_event
        }
        d$event[i] <- ev
        used <- c(used, ev)
      }
      open <- unique(d$event[vid][d$is_stay[vid] & d$still_in[vid]])
    }
  }

  # One row per event
  stays <- d[d$is_stay, , drop = FALSE]
  ev_rows <- lapply(split(seq_len(nrow(stays)), stays$event), function(r) {
    first <- r[1]
    last <- r[length(r)]
    data.frame(
      Season = stays$Season[first],
      Station = stays$Station[first],
      File = stays$File[first],
      video = stays$video[first],
      video_dt = stays$video_dt[first],
      Species = stays$Species[first],
      Enter = 1L,
      Stay = as.numeric(difftime(stays$t_out[last], stays$t_enter[first],
                                 units = "secs")),
      RightCens = stays$still_in[last],
      stringsAsFactors = FALSE
    )
  })
  events <- do.call(rbind, c(list(empty_output(tz)), ev_rows))
  if (any(events$Stay < 0, na.rm = TRUE)) {
    warning("Negative staying time in: ",
            paste(events$video[events$Stay < 0], collapse = ", "), call. = FALSE)
  }

  # Detections (video x species) in which no event started
  det_key <- paste(d$Station, d$video, d$Species, sep = "\r")
  new_key <- paste(events$Station, events$video, events$Species, sep = "\r")
  rest <- d[!det_key %in% new_key & !duplicated(det_key), , drop = FALSE]
  has_info <- vapply(det_key[!det_key %in% new_key & !duplicated(det_key)],
                     function(k) {
                       s <- det_key == k
                       any(d$is_stay[s] | d$noentry[s])
                     }, logical(1))
  others <- data.frame(
    Season = rest$Season,
    Station = rest$Station,
    File = rest$File,
    video = rest$video,
    video_dt = rest$video_dt,
    Species = rest$Species,
    Enter = ifelse(has_info, 0L, NA_integer_),
    Stay = rep(NA_real_, nrow(rest)),
    RightCens = rep(NA, nrow(rest)),
    stringsAsFactors = FALSE
  )

  out <- rbind(events, others)
  out <- out[order(out$Station, out$video_dt, out$video, out$Species), , drop = FALSE]
  out$DateTimeCorrected <- format(out$video_dt, "%Y/%m/%d %H:%M:%S", tz = tz)
  out <- out[, c("Season", "Station", "File", "DateTimeCorrected", "Species",
                 "Enter", "Stay", "RightCens")]
  rownames(out) <- NULL
  out
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
#' parse_video_datetime("c15_240501_105240_05010001.MOV")
#' @export
parse_video_datetime <- function(x, tz = "UTC") {
  m <- regmatches(x, regexec("^[^_]+_(\\d{6})_(\\d{6})_", x))
  s <- vapply(m, function(v) {
    if (length(v) == 3) paste(v[2], v[3]) else NA_character_
  }, character(1))
  as.POSIXct(s, format = "%y%m%d %H%M%S", tz = tz)
}

# Place clock times ("H:MM:SS") on the day closest to the reference date-time
clock_to_datetime <- function(clock, ref, tz) {
  res <- rep(as.POSIXct(NA, tz = tz), length(clock))
  ok <- !is.na(clock)
  if (!any(ok)) return(res)
  parts <- strsplit(clock[ok], ":", fixed = TRUE)
  secs <- vapply(parts, function(p) {
    p <- as.numeric(p)
    if (length(p) == 2) p <- c(p, 0)
    if (length(p) != 3 || anyNA(p)) NA_real_ else sum(p * c(3600, 60, 1))
  }, numeric(1))
  if (anyNA(secs)) {
    stop("Invalid clock time(s): ", paste(clock[ok][is.na(secs)], collapse = ", "),
         call. = FALSE)
  }
  day0 <- as.POSIXct(format(ref[ok], "%Y-%m-%d", tz = tz), tz = tz)
  best <- day0 + secs
  for (shift in c(-86400, 86400)) {
    cand <- day0 + shift + secs
    closer <- abs(as.numeric(cand - ref[ok], units = "secs")) <
      abs(as.numeric(best - ref[ok], units = "secs"))
    best[closer] <- cand[closer]
  }
  res[ok] <- best
  res
}

empty_output <- function(tz) {
  data.frame(Season = character(0), Station = character(0), File = character(0),
             video = character(0), video_dt = as.POSIXct(character(0), tz = tz), Species = character(0),
             Enter = integer(0), Stay = numeric(0), RightCens = logical(0),
             stringsAsFactors = FALSE)
}

as_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA")] <- NA_character_
  x
}

as_lgl <- function(x) {
  x <- toupper(as_chr(x))
  ifelse(x %in% c("TRUE", "T", "1"), TRUE, ifelse(x %in% c("FALSE", "F", "0"), FALSE, NA))
}
