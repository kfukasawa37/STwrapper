#' Convert staying time records to 'ctrest' detection data
#'
#' Converts a per-video staying time table (one row per animal and video, with
#' `enter` / `out` clock times) into the detection data format used by the
#' 'ctrest' package (`Station`, `DateTime`, `Term`, `Species`, `y`, `Stay`,
#' `Cens`), as in `ctrest::detection_data`.
#'
#' Required columns are `enter`, `out`, `stayingTimeCensoringtype`,
#' `Enter_cont`, the station and species columns, and the recording date-time
#' (`DateTime`), which must be filled on every row. Use [fill_datetime()]
#' beforehand to fill empty date-times from the file names. Other columns such
#' as `video_name`, `Enter_new`, `sp_ID`, `note` and `memo` are used when
#' present, and any further columns are ignored. Videos are identified by the
#' file name when the file name column is present, and by their date-time
#' otherwise.
#'
#' A staying event that spans several videos is recorded as several rows in
#' the input: the first row has `Enter_new = TRUE` and each row in a following
#' video has `Enter_cont = TRUE`. For each station and species, a row with
#' `Enter_cont = TRUE` is merged into an event that was still in view
#' (`stayingTimeCensoringtype` of `"right"` or `"both"`) at the end of the
#' previous video in which the same species appeared. When several such events
#' are open, the one with the same `sp_ID` is preferred, then the earliest.
#'
#' The output has one row per staying event:
#' * `DateTime` is the recording date-time of the video in which the event
#'   started.
#' * `y` is 1 (one entry into the focal area).
#' * `Stay` is the staying time in seconds, from `enter` of the first row of
#'   the event to `out` of its last row, which may be in a later video.
#' * `Cens` is 1 when the last row of the event has
#'   `stayingTimeCensoringtype` `"right"` or `"both"`, and 0 otherwise.
#'
#' When several animals enter in the same video, each gets its own row with
#' `y = 1`. In addition, a video in which the species was detected but no
#' event started gives one row with `Stay` and `Cens` set to `NA`: `y` is 0
#' when there was no new entry (`"noentry"` in `note`, `memo` or
#' `stayingTimeCensoringtype`, or only continuations of earlier events), and
#' `NA` when there is no entry information at all (no `enter`/`out` and no
#' `"noentry"`).
#'
#' The sum of `y` per station is the number of entries used by the REST model.
#' Because a video with several entries gives several rows, the output is not
#' suited to the RAD-REST model, which needs the number of entries per video.
#'
#' The input is checked for inconsistent records, and a warning lists the rows
#' of `data` concerned:
#' * `stayingTimeCensoringtype` other than `"complete"`, `"left"`, `"right"`
#'   or `"both"` on a row with `enter` and `out` (the row is treated as not
#'   right-censored);
#' * only one of `enter` and `out` filled (the row is not used as a stay);
#' * `out` earlier than `enter`;
#' * `Enter_cont = TRUE` without `enter` / `out`;
#' * `Enter_cont = TRUE` but `stayingTimeCensoringtype` not `"left"` or
#'   `"both"` (an animal continuing from the previous video is already in view
#'   when the video starts);
#' * `Enter_new` and `Enter_cont` both `TRUE`, or both `FALSE` on a row with
#'   `enter` and `out` (only when the `Enter_new` column is present);
#' * `Enter_cont = TRUE` but no event of the same species was still in view
#'   (`"right"` or `"both"`) at the end of the previous video (the row is
#'   treated as a new event).
#'
#' Rows that start with `"left"` or `"both"` and `Enter_cont = FALSE` (an
#' animal that entered between two videos) are accepted without a warning; the
#' staying time then starts at `enter` of that row.
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
#' @param col_file Column with the video file name, used to tell videos apart
#'   when present. Default `"video_name"`.
#' @param term Value for the `Term` column: either a single value used for
#'   all rows or the name of a column in `data`. Default `NA`.
#' @param tz Time zone used to interpret the date-times. Default `"UTC"`.
#'
#' @return A data frame with columns `Station` (character), `DateTime`
#'   (POSIXct), `Term`, `Species` (character), `y` (integer), `Stay`
#'   (numeric, seconds) and `Cens` (numeric, 0 or 1), ordered by station and
#'   date-time.
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
                         term = NA,
                         tz = "UTC") {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }

  required <- c("enter", "out", "stayingTimeCensoringtype", "Enter_cont",
                col_station, col_species, col_datetime)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Column(s) not found in 'data': ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  has_file <- col_file %in% names(data)
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
    enter_new = if ("Enter_new" %in% names(data)) as_lgl(data$Enter_new) else NA,
    sp_ID = optional_col("sp_ID"),
    stringsAsFactors = FALSE
  )
  # "noentry" marks a detection without entry into the focal area
  d$noentry <- Reduce(`|`, lapply(
    intersect(c("note", "memo", "stayingTimeCensoringtype"), names(data)),
    function(col) tolower(as_chr(data[[col]])) %in% "noentry"
  ), rep(FALSE, nrow(d)))
  d$Term <- if (length(term) == 1 && is.character(term) &&
                term %in% names(data)) {
    as_chr(data[[term]])
  } else {
    rep(term, length.out = nrow(d))
  }

  video_dt <- video_datetime(data[[col_datetime]], tz)
  check_datetime(video_dt, data[[col_datetime]], col_datetime)
  d$video_dt <- video_dt
  # Key identifying a video: file name, or date-time when there is none
  d$video <- if (has_file) d$File else format(video_dt, "%Y-%m-%d %H:%M:%S", tz = tz)

  d <- d[!is.na(d$Species), , drop = FALSE]
  d$is_stay <- !is.na(d$enter) & !is.na(d$out)
  d$t_enter <- clock_to_datetime(d$enter, d$video_dt, tz)
  d$t_out <- clock_to_datetime(d$out, d$video_dt, tz)
  d$still_in <- d$cens %in% c("right", "both")

  # Consistency checks on the input records
  warn_rows(d$row[d$is_stay & !d$cens %in% c("complete", "left", "right", "both")],
            "Unknown stayingTimeCensoringtype (not complete/left/right/both)")
  warn_rows(d$row[xor(is.na(d$enter), is.na(d$out))],
            "Only one of 'enter' and 'out' is given; the row is not used as a stay")
  warn_rows(d$row[d$is_stay & d$t_out < d$t_enter], "'out' is earlier than 'enter'")
  warn_rows(d$row[d$enter_cont %in% TRUE & !d$is_stay],
            "Enter_cont = TRUE without 'enter' and 'out'")
  warn_rows(d$row[d$is_stay & d$enter_cont %in% TRUE & !d$cens %in% c("left", "both")],
            "Enter_cont = TRUE but stayingTimeCensoringtype is not left or both")
  warn_rows(d$row[d$enter_new %in% TRUE & d$enter_cont %in% TRUE],
            "Enter_new and Enter_cont are both TRUE")
  warn_rows(d$row[d$is_stay & d$enter_new %in% FALSE & d$enter_cont %in% FALSE],
            "Enter_new and Enter_cont are both FALSE")

  d <- d[order(d$Station, d$Species, d$video_dt, d$video, d$row), , drop = FALSE]

  # Assign an event id to every stay row
  d$event <- NA_integer_
  n_event <- 0L
  unmatched <- integer(0)
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
            unmatched <- c(unmatched, d$row[i])
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

  warn_rows(unmatched, paste(
    "Enter_cont = TRUE but no open event (right or both) in the previous video",
    "of the same species; treated as a new event"))

  # One record per event, attached to the video in which it started
  stays <- d[d$is_stay, , drop = FALSE]
  ev_rows <- lapply(split(seq_len(nrow(stays)), stays$event), function(r) {
    first <- r[1]
    last <- r[length(r)]
    data.frame(
      Station = stays$Station[first],
      video = stays$video[first],
      video_dt = stays$video_dt[first],
      Term = stays$Term[first],
      Species = stays$Species[first],
      y = 1L,
      Stay = as.numeric(difftime(stays$t_out[last], stays$t_enter[first],
                                 units = "secs")),
      Cens = as.numeric(stays$still_in[last]),
      stringsAsFactors = FALSE
    )
  })
  events <- do.call(rbind, c(list(empty_output(tz)), ev_rows))
  if (any(events$Stay < 0, na.rm = TRUE)) {
    warning("Negative staying time in: ",
            paste(events$video[events$Stay < 0], collapse = ", "), call. = FALSE)
  }

  # Detections (video x species) in which no event started
  new_key <- paste(events$Station, events$video, events$Species, sep = "\r")
  det_key <- paste(d$Station, d$video, d$Species, sep = "\r")
  keep <- !det_key %in% new_key & !duplicated(det_key)
  rest <- d[keep, , drop = FALSE]
  has_info <- vapply(det_key[keep], function(k) {
    s <- det_key == k
    any(d$is_stay[s] | d$noentry[s])
  }, logical(1))
  others <- data.frame(
    Station = rest$Station,
    video = rest$video,
    video_dt = rest$video_dt,
    Term = rest$Term,
    Species = rest$Species,
    y = ifelse(has_info, 0L, NA_integer_),
    Stay = rep(NA_real_, nrow(rest)),
    Cens = rep(NA_real_, nrow(rest)),
    stringsAsFactors = FALSE
  )

  out <- rbind(events, others)
  out <- out[order(out$Station, out$video_dt, out$video, out$Species),
             , drop = FALSE]
  out$DateTime <- out$video_dt
  out <- out[, c("Station", "DateTime", "Term", "Species", "y", "Stay", "Cens")]
  rownames(out) <- NULL
  out
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
  data.frame(Station = character(0), video = character(0),
             video_dt = as.POSIXct(character(0), tz = tz), Term = character(0),
             Species = character(0), y = integer(0), Stay = numeric(0),
             Cens = numeric(0), stringsAsFactors = FALSE)
}

# Recording date-time of each video from the date-time column
video_datetime <- function(x, tz) {
  x <- as_chr(x)
  dt <- rep(as.POSIXct(NA, tz = tz), length(x))
  for (f in c("%Y/%m/%d %H:%M:%OS", "%Y-%m-%d %H:%M:%OS",
              "%Y/%m/%d %H:%M", "%Y-%m-%d %H:%M")) {
    todo <- is.na(dt) & !is.na(x)
    if (!any(todo)) break
    dt[todo] <- as.POSIXct(x[todo], format = f, tz = tz)
  }
  dt
}

# Stop when a recording date-time is empty or cannot be read
check_datetime <- function(dt, x, col_datetime, rows = which(is.na(dt))) {
  if (length(rows) == 0) return(invisible())
  empty <- rows[is.na(as_chr(x)[rows])]
  bad <- setdiff(rows, empty)
  msg <- character(0)
  if (length(empty) > 0) {
    msg <- c(msg, sprintf(
      "'%s' is empty in row(s) %s of 'data'. Fill it first, for example from the file names with fill_datetime().",
      col_datetime, paste(utils::head(empty, 10), collapse = ", ")))
  }
  if (length(bad) > 0) {
    msg <- c(msg, sprintf(
      "'%s' could not be read in row(s) %s of 'data' (use yyyy/mm/dd HH:MM:SS).",
      col_datetime, paste(utils::head(bad, 10), collapse = ", ")))
  }
  stop(paste(msg, collapse = "
"), call. = FALSE)
}

# Warn once for a set of problem rows of the input
warn_rows <- function(rows, msg) {
  rows <- sort(unique(rows))
  if (length(rows) == 0) return(invisible())
  shown <- paste(utils::head(rows, 10), collapse = ", ")
  if (length(rows) > 10) shown <- paste0(shown, ", ... (", length(rows), " rows)")
  warning(sprintf("%s: row(s) %s of 'data'.", msg, shown), call. = FALSE)
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
