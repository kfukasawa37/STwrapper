# STwrapper

STwrapper converts per-video staying time annotations from camera traps into the
detection data format of [ctrest](https://github.com/YoshihiroNakashima/ctrest)
(REST model).

カメラトラップ動画ごとの動物滞在時間の記録を、ctrest パッケージの
`detection_data`（`ctrest::detection_data`）形式に変換する R パッケージです。

## Installation

```r
# install.packages("remotes")
remotes::install_github("YoshihiroNakashima/ctrest")  # used in the vignette
remotes::install_github("kfukasawa37/STwrapper", build_vignettes = TRUE)
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
with `ctrest::bayes_rest()`, compared with the true values. It is built at
installation (see above); building takes a few minutes because of the MCMC.

### 日本語チュートリアル

`vignette("tutorial-ja", package = "STwrapper")` は、この流れを初心者向けに
日本語で説明したチュートリアルです。Rtools と依存パッケージのインストールから始めて、
入力データの形、`convert_stay()` による滞在のまとめ方、ダミーデータの作成、
WAIC による滞在時間分布の選択、ctrest での密度推定と真の値との比較までを、
コードと図で順に説明しています。

A step-by-step tutorial in Japanese, from installing Rtools to REST density
estimation with ctrest, is available as `vignette("tutorial-ja", package = "STwrapper")`.

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

The same columns as `ctrest::detection_data`, with one row per staying event
(plus one row for each video in which the species was detected without a new
entry):

| Column | Type | Content |
|---|---|---|
| `Station` | chr | Station |
| `DateTime` | POSIXct | Recording date-time of the video in which the event started |
| `Term` | | Value of `term` (a constant or a column name) |
| `Species` | chr | Species |
| `y` | int | 1 for a staying event; 0 for a detection without a new entry, `NA` when there is no entry information |
| `Stay` | dbl | Staying time in seconds, from `enter` of the first video of the event to `out` of its last video |
| `Cens` | dbl | 1 when the last video of the event is `right` or `both` censored, else 0 |

## How events are merged

For each station and species, a row with `Enter_cont = TRUE` is joined to an
event that was still in view (`right` or `both`) at the end of the previous
video of the same species. If several events are open, the one with the same
`sp_ID` is used first (or the earliest one when there is no `sp_ID`). A
staying event therefore gives one staying time even if it spans many videos,
and is counted in `y` only for the video in which it started.

When several animals enter in the same video, each gets its own row with
`y = 1`, so every row with a staying time has `y = 1` and the sum of `y` per
station is the number of entries used by the REST model. Because the number
of entries per video is spread over several rows, the output is meant for the
REST model, not for RAD-REST.

## License

MIT
