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

One row per animal (or species) and video. Columns used:

| Column | Content |
|---|---|
| `video_name` | File name `<camera>_yymmdd_HHMMSS_....MOV` |
| `DateTime` | Recording date-time `yyyy/mm/dd HH:MM:SS`. If empty, it is taken from `video_name`. |
| `deploymentID` | Station (change with `col_station`) |
| `species1` | Species (change with `col_species`) |
| `sp_ID` | Individual number within the video |
| `enter`, `out` | Clock times (`H:MM:SS`) when the animal entered and left the focal area |
| `stayingTimeCensoringtype` | `complete`, `left`, `right` or `both` |
| `Enter_new` | `TRUE` on the first row of a staying event |
| `Enter_cont` | `TRUE` when the animal continues staying from the previous video |
| `note` / `memo` | `noentry` for a detection without entry into the focal area |

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
`sp_ID` is used first. A staying event therefore becomes a single row even if
it spans many videos. Each event gets its own row, so a video with two entries
gives two rows with `Enter = 1`; summing `Enter` per video gives the number of
passes.

## License

MIT
