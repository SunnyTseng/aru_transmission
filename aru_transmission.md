ARU cardinal transmission
================
Sunny Tseng
2023-02-07

## Library

``` r
library(here)
library(tidyverse)
```

## Import data

``` r
data <- read_csv(here("cardinal_playback.csv"))
```

    ## Rows: 1626 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): location, ARU, plot_type, direction
    ## dbl  (4): beaufort, distance, song_exp, confidence
    ## date (1): date
    ## time (1): time
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data
```

    ## # A tibble: 1,626 × 10
    ##    date       time   location beaufort ARU   plot_type direction dista…¹ song_…²
    ##    <date>     <time> <chr>       <dbl> <chr> <chr>     <chr>       <dbl>   <dbl>
    ##  1 2022-05-01 08:00  PB              0 PB    PB        N               0       1
    ##  2 2022-05-01 08:00  PB              0 PB    PB        N               0       2
    ##  3 2022-05-01 08:00  PB              0 PB    PB        N               0       3
    ##  4 2022-05-01 08:00  PB              0 PB    PB        N               0       4
    ##  5 2022-05-01 08:00  PB              0 PB    PB        N               0       6
    ##  6 2022-05-01 08:00  PB              0 PB    PB        N               0       7
    ##  7 2022-06-14 11:48  14_35           0 NEW   F         E              50       1
    ##  8 2022-06-14 11:48  14_35           0 NEW   F         E              50       2
    ##  9 2022-06-14 11:48  14_35           0 NEW   F         E              50       3
    ## 10 2022-06-14 11:48  14_35           0 NEW   F         E              50       4
    ## # … with 1,616 more rows, 1 more variable: confidence <dbl>, and abbreviated
    ## #   variable names ¹​distance, ²​song_exp
