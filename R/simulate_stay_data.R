#' Simulate staying time records from camera-trap videos
#'
#' Simulates animals passing through the focal area in front of camera traps
#' and the videos recorded by the cameras, and returns per-video staying time
#' records in the input format of [convert_stay()]. Because every video is
#' annotated from the simulated animal movements, the number of entries and the
#' staying times are consistent with each other, and staying events that span
#' several videos are marked with `Enter_new` / `Enter_cont`.
#'
#' The simulation follows the assumptions of the REST model:
#' * Animals enter the focal area of each camera as a Poisson process whose
#'   rate follows a daily activity pattern (a mixture of von Mises
#'   distributions with peaks at `activity_peaks`). The mean rate is
#'   `density * focal_area / stay_mean * activity_level`, where the activity
#'   level is `1 / (2 * pi * max(f))` for the activity density `f`.
#' * Animals come in groups: the group size is `1 + Poisson(group_mean - 1)`,
#'   and members of a group enter one after another at exponentially
#'   distributed intervals with mean `group_lag` seconds. The rate of groups is
#'   the rate of entries divided by `group_mean`.
#' * Each animal stays in the focal area for a log-normally distributed time
#'   with mean `stay_mean` and standard deviation `stay_sd` (seconds).
#' * Animals that are detected but do not enter the focal area occur at
#'   `noentry_ratio` times the rate of entries.
#'
#' The camera starts a video of `video_length` seconds when an animal enters
#' the focal area or is detected nearby. After each video it waits `interval`
#' seconds; if an animal is still in the focal area at that time, it records
#' the next video straight away, otherwise it waits for the next trigger.
#' Entries during the waiting time of animals that leave before the next video
#' are missed, and an animal that is in view at the end of a video but gone
#' when the next one starts gives a right-censored staying time.
#'
#' Within a video, `sp_ID` numbers the animals; an animal that continues from
#' the previous video keeps its number, and new animals get the smallest free
#' numbers.
#'
#' Each video is named `<station>_yymmdd_HHMMSS_<mmdd><nnnn>.MOV` with the
#' date-time at which recording ended, and `enter` / `out` are clock times
#' rounded to seconds. A video of people (`species = "hito"`) is added when
#' each camera is set up and retrieved, so that the survey period can be
#' taken from the first and last videos.
#'
#' @param n_station Number of camera stations.
#' @param days Survey period of each camera (days).
#' @param density Animal density (individuals per km^2).
#' @param focal_area Focal area of each camera (m^2).
#' @param stay_mean,stay_sd Mean and standard deviation of the staying time
#'   in the focal area (seconds).
#' @param activity_peaks Hours of the day at which activity peaks.
#' @param activity_kappa Concentration of each activity peak (von Mises
#'   kappa); larger values give narrower peaks.
#' @param group_mean Mean group size (at least 1).
#' @param group_lag Mean time between entries of group members (seconds).
#' @param noentry_ratio Rate of detections without entry into the focal area,
#'   relative to the rate of entries.
#' @param video_length Length of each video (seconds).
#' @param interval Time between the end of a video and the start of the next
#'   one (seconds).
#' @param species Species name of the simulated animal.
#' @param start Date-time at which the first camera is set up, as a
#'   character string (`"yyyy-mm-dd HH:MM:SS"`, read in the time zone `tz`) or
#'   a `POSIXct`. The other cameras are set up within the following 3 hours.
#' @param tz Time zone of the date-times, for example `"Asia/Tokyo"`. The
#'   activity pattern, file names and clock times follow this time zone.
#'
#' @return A data frame with columns `id`, `deploymentID`, `video_name`,
#'   `DateTime` (`yyyy/mm/dd HH:MM:SS`), `species`, `sp_ID`, `enter`, `out`,
#'   `stayingTimeCensoringType`, `memo`, `Enter_new` and `Enter_cont`, one
#'   row per animal and video. The true parameter values are attached as the
#'   attribute `"truth"`: a list with `density`, `focal_area`, `mean_stay`
#'   (seconds), `activity_level`, `effort` (camera days) and `n_entry` (number
#'   of entries into the focal area).
#'
#' @examples
#' set.seed(1)
#' sim <- simulate_stay_data(n_station = 5, days = 30)
#' head(sim)
#' attr(sim, "truth")
#' @export
simulate_stay_data <- function(n_station = 30,
                               days = 60,
                               density = 30,
                               focal_area = 2,
                               stay_mean = 10,
                               stay_sd = 8,
                               activity_peaks = c(5.5, 18),
                               activity_kappa = 4,
                               group_mean = 1.5,
                               group_lag = 3,
                               noentry_ratio = 0.5,
                               video_length = 20,
                               interval = 5,
                               species = "deer",
                               start = "2024-05-01 09:00:00",
                               tz = "UTC") {
  mu <- activity_peaks / 24 * 2 * pi
  kappa <- rep(activity_kappa, length.out = length(mu))
  f_act <- function(theta) {
    dens <- vapply(seq_along(mu), function(j) {
      exp(kappa[j] * cos(theta - mu[j])) / (2 * pi * besselI(kappa[j], 0))
    }, numeric(length(theta)))
    rowMeans(matrix(dens, nrow = length(theta)))
  }
  f_max <- max(f_act(seq(0, 2 * pi, length.out = 2881)))
  activity_level <- 1 / (2 * pi * f_max)

  # Entries per second at the activity peak
  rate_max <- density / 1e6 * focal_area / stay_mean
  sdlog <- sqrt(log(1 + (stay_sd / stay_mean)^2))
  meanlog <- log(stay_mean) - sdlog^2 / 2

  station <- sprintf("ST%02d", seq_len(n_station))
  setup <- as.POSIXct(start, tz = tz) + round(stats::runif(n_station, 0, 3 * 3600))
  duration <- days * 86400

  n_entry <- 0L
  out <- vector("list", n_station)
  for (s in seq_len(n_station)) {
    t0 <- as.numeric(setup[s])
    t1 <- t0 + duration

    # Poisson process thinned by the activity pattern
    thinned <- function(rate) {
      n <- stats::rpois(1, rate * duration)
      t <- sort(stats::runif(n, t0, t1))
      lt <- as.POSIXlt(as.POSIXct(t, origin = "1970-01-01", tz = tz))
      theta <- (lt$hour * 3600 + lt$min * 60 + lt$sec) / 86400 * 2 * pi
      t[stats::runif(n) < f_act(theta) / f_max]
    }
    group_t <- thinned(rate_max / group_mean)
    size <- 1 + stats::rpois(length(group_t), group_mean - 1)
    group <- rep(seq_along(size), size)
    lag <- stats::rexp(length(group), 1 / group_lag)
    lag[!duplicated(group)] <- 0  # first member of each group
    enter <- rep(group_t, size) + unlist(lapply(split(lag, group), cumsum), use.names = FALSE)
    exit <- enter + stats::rlnorm(length(enter), meanlog, sdlog)
    noentry <- thinned(rate_max * noentry_ratio)
    n_entry <- n_entry + length(enter)

    # Videos: (start, end) pairs
    # cameras record whole seconds
    triggers <- floor(sort(c(enter, noentry)))
    v_start <- numeric(0)
    next_free <- -Inf
    i <- 1
    while (i <= length(triggers)) {
      t <- max(triggers[i], next_free)
      repeat {
        v_start <- c(v_start, t)
        t <- t + video_length + interval
        if (!any(enter < t & exit > t)) break
      }
      # triggers while recording or waiting are not used; an animal that
      # entered while waiting and is still there started the next video above
      next_free <- t
      while (i <= length(triggers) && triggers[i] < next_free) i <- i + 1
    }
    v_end <- v_start + video_length

    rows <- list()
    prev <- integer(0)     # animals seen in the previous video
    prev_id <- integer(0)  # and their sp_ID
    for (v in seq_along(v_start)) {
      vs <- v_start[v]
      ve <- v_end[v]
      inside <- which(enter < ve & exit > vs)
      inside <- inside[order(enter[inside])]
      # videos follow each other directly while an animal stays in view, so an
      # animal also seen in the previous video continues its stay
      cont <- inside %in% prev
      ids <- integer(length(inside))
      ids[cont] <- prev_id[match(inside[cont], prev)]
      free <- setdiff(seq_along(inside), ids[cont])
      ids[!cont] <- free[seq_len(sum(!cont))]
      left <- enter[inside] < vs
      right <- exit[inside] > ve
      rows[[v]] <- data.frame(
        v = rep(v, length(inside)),
        sp_ID = ids,
        enter = clock_time(pmax(enter[inside], vs), tz),
        out = clock_time(pmin(exit[inside], ve), tz),
        cens = ifelse(left & right, "both",
                      ifelse(left, "left", ifelse(right, "right", "complete"))),
        memo = rep(NA_character_, length(inside)),
        Enter_new = !cont,
        Enter_cont = cont,
        stringsAsFactors = FALSE
      )
      if (any(noentry >= vs & noentry < ve)) {
        rows[[v]] <- rbind(rows[[v]], data.frame(
          v = v, sp_ID = NA_integer_, enter = NA_character_, out = NA_character_,
          cens = NA_character_, memo = "noentry", Enter_new = NA, Enter_cont = NA,
          stringsAsFactors = FALSE
        ))
      }
      prev <- inside
      prev_id <- ids
    }
    rec <- do.call(rbind, rows)
    if (is.null(rec)) {
      rec <- data.frame(v = integer(0), sp_ID = integer(0), enter = character(0),
                        out = character(0), cens = character(0), memo = character(0),
                        Enter_new = logical(0), Enter_cont = logical(0))
    }
    rec$species <- rep(species, nrow(rec))
    rec$v_end <- v_end[rec$v]

    # People setting up and retrieving the camera
    people <- data.frame(
      v = NA_integer_, sp_ID = NA_integer_, enter = NA_character_,
      out = NA_character_, cens = NA_character_, memo = NA_character_,
      Enter_new = NA, Enter_cont = NA, species = "hito",
      v_end = c(t0, t1), stringsAsFactors = FALSE
    )
    rec <- rbind(people[1, ], rec, people[2, ])

    end_dt <- as.POSIXct(rec$v_end, origin = "1970-01-01", tz = tz)
    video_no <- cumsum(!duplicated(rec$v_end))
    rec$video_name <- sprintf("%s_%s_%s%04d.MOV", station[s],
                              format(end_dt, "%y%m%d_%H%M%S", tz = tz),
                              format(end_dt, "%m%d", tz = tz), video_no)
    rec$DateTime <- format(end_dt, "%Y/%m/%d %H:%M:%S", tz = tz)
    rec$deploymentID <- station[s]
    out[[s]] <- rec
  }

  res <- do.call(rbind, out)
  res <- data.frame(
    id = seq_len(nrow(res)),
    deploymentID = res$deploymentID,
    video_name = res$video_name,
    DateTime = res$DateTime,
    species = res$species,
    sp_ID = res$sp_ID,
    enter = res$enter,
    out = res$out,
    stayingTimeCensoringType = res$cens,
    memo = res$memo,
    Enter_new = res$Enter_new,
    Enter_cont = res$Enter_cont,
    stringsAsFactors = FALSE
  )
  attr(res, "truth") <- list(
    density = density,
    focal_area = focal_area,
    mean_stay = stay_mean,
    activity_level = activity_level,
    effort = n_station * days,
    n_entry = n_entry
  )
  res
}

# Clock time "H:MM:SS" rounded to seconds
clock_time <- function(t, tz) {
  if (length(t) == 0) return(character(0))
  lt <- as.POSIXlt(as.POSIXct(round(t), origin = "1970-01-01", tz = tz))
  sprintf("%d:%02d:%02d", lt$hour, lt$min, as.integer(lt$sec))
}
