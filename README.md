# STwrapper

STwrapper converts per-video staying time annotations from camera traps into the
detection data format of [ctrest](https://github.com/YoshihiroNakashima/ctrest)
(REST / RAD-REST models).

カメラトラップ動画ごとの動物滞在時間の記録を、ctrest パッケージの
`detection_data` 形式に変換する R パッケージです。

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
detection <- convert_stay(stay, season = "2024")
write.csv(detection, "detection_data.csv", row.names = FALSE)
```

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
| `video_name` | File name `<camera>_yymmdd_HHMMSS_....MOV` (change with `col_file`). If missing, videos are identified by `DateTime` and `File` is `NA`. |
| `Enter_new` | `TRUE` on the first row of a staying event (not needed for the conversion) |
| `sp_ID` | Individual number within the video, used to pick the right event when several are continued |
| `note`, `memo` | `noentry` for a detection without entry into the focal area |

## Output format

| Column | Content |
|---|---|
| `Season` | Value of `season` (a constant or a column name) |
| `Station` | Station |
| `File` | First video of the staying event |
| `DateTimeCorrected` | Date-time of the first video of the event |
| `Species` | Species |
| `Enter` | 1 for a staying event, 0 for a detection without a new entry, `NA` when there is no entry information |
| `Stay` | Staying time in seconds, from `enter` of the first video to `out` of the last video |
| `RightCens` | `TRUE` when the last video of the event is `right` or `both` censored |

## How events are merged

For each station and species, a row with `Enter_cont = TRUE` is joined to an
event that was still in view (`right` or `both`) at the end of the previous
video of the same species. If several events are open, the one with the same
`sp_ID` is used first (or the earliest one when there is no `sp_ID`). A staying event therefore becomes a single row even if
it spans many videos. Each event gets its own row, so a video with two entries
gives two rows with `Enter = 1`; summing `Enter` per video gives the number of
passes.

## License

MIT
