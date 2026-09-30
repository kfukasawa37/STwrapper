# STwrapper

STwrapper converts per-video staying time annotations from camera traps into the
detection data format of [ctrest](https://github.com/YoshihiroNakashima/ctrest)
(REST / RAD-REST models).

カメラトラップ動画ごとの動物滞在時間の記録を、ctrest パッケージの
`detection_data`（`ctrest::detection_data`）形式に変換する R パッケージです。

## Installation

```r
# install.packages("remotes")
remotes::install_github("kfukasawa37/STwrapper")
```

## Usage

```r
library(STwrapper)

path <- system.file("extdata", "example_stay.csv", package = "STwrapper")
stay <- read.csv(path)
detection <- convert_stay(stay, term = "term1")
write.csv(detection, "detection_data.csv", row.names = FALSE)
```

## Simulated data and a full REST workflow

`simulate_stay_data()` simulates animals entering the focal areas of camera
traps under the REST model (with a daily activity pattern, group arrivals and
log-normal staying times), records 20-second videos and annotates them in the
input format of `convert_stay()`. Stays longer than one video are split over
consecutive videos with `Enter_cont = TRUE`, so the number of entries and the
staying times are consistent.

```r
set.seed(123)
sim <- simulate_stay_data(n_station = 30, days = 60, density = 30, focal_area = 2)
attr(sim, "truth")        # true density, mean staying time, activity level, ...
detection <- convert_stay(sim, term = "term1")
```

The vignette `vignette("rest-workflow", package = "STwrapper")` runs the
whole analysis: simulation, merging with STwrapper, and density estimation
with `ctrest::bayes_rest()`, compared with the true values. To build it,
install with `remotes::install_github("kfukasawa37/STwrapper", build_vignettes = TRUE)`.

## Input format

A data frame (for example from `read.csv()`) with one row per animal (or
species) and video. Other columns may be present and are ignored.

Required columns:

| Column | Content |
|---|---|
| `deploymentID` | Station (change with `col_station`) |
| `species1` | Species (change with `col_species`) |
| `enter`, `out` | Clock times (`H:MM:SS`) when the animal entered and left the focal area |
| `stayingTimeCensoringtype` | `complete`, `left`, `right` or `both` |
| `Enter_cont` | `TRUE` when the animal continues staying from the previous video |
| `DateTime` and/or `video_name` | At least one of them (see below) |

Optional columns:

| Column | Content |
|---|---|
| `DateTime` | Recording date-time `yyyy/mm/dd HH:MM:SS` (change with `col_datetime`). If missing or empty, it is read from `video_name`. |
| `video_name` | File name `<camera>_yymmdd_HHMMSS_....MOV` (change with `col_file`). If missing, videos are identified by `DateTime`. |
| `Enter_new` | `TRUE` on the first row of a staying event (not needed for the conversion) |
| `sp_ID` | Individual number within the video, used to pick the right event when several are continued |
| `note`, `memo` | `noentry` for a detection without entry into the focal area |

## Output format

The same format as `ctrest::detection_data`, with one row per video and species:

| Column | Type | Content |
|---|---|---|
| `Station` | chr | Station |
| `DateTime` | POSIXct | Recording date-time of the video |
| `Term` | | Value of `term` (a constant or a column name) |
| `Species` | chr | Species |
| `y` | int | Number of staying events that started in the video; 0 for a detection without a new entry, `NA` when there is no entry information |
| `Stay` | dbl | Staying time in seconds of the event that started in the video, from `enter` of its first video to `out` of its last video |
| `Cens` | dbl | 1 when the last video of the event is `right` or `both` censored, else 0 |

## How events are merged

For each station and species, a row with `Enter_cont = TRUE` is joined to an
event that was still in view (`right` or `both`) at the end of the previous
video of the same species. If several events are open, the one with the same
`sp_ID` is used first (or the earliest one when there is no `sp_ID`). A
staying event therefore gives one staying time even if it spans many videos,
and is counted in `y` only for the video in which it started.

When several events start in the same video, the first row of that video has
`y` equal to the number of events, and the other events are added as rows with
`y = NA` and their own `Stay` and `Cens`. `ctrest::format_stay()` uses all of
them for staying time, while `ctrest::format_station_data()` drops the
`y = NA` rows and counts each video once.

## License

MIT
