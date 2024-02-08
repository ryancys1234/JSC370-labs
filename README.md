Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
# rm(list=ls())
install.packages('R.utils')
install.packages('webshot')
library(data.table)
library(dtplyr)
library(dplyr)
library(mgcv)
library(ggplot2)
library(leaflet)
library(kableExtra)
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")

met$lat <- met$lat/1000
met$lon <- met$lon/1000
met$wind.sp <- met$wind.sp/10
met$temp <- met$temp/10
met$dew.point <- met$dew.point/10
met$atm.press <- met$atm.press/10
met$rh <- 100*((112-0.1*met$temp+met$dew.point)/(112+0.9*met$temp))^8

met <- na.omit(met)
```

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
met_2 <- merge(
  # Data
  x     = met,
  y     = stations,
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF",
  # Which obs to keep?
  all.x = TRUE,
  all.y = FALSE
  )
```

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
quantile(met_2$temp, na.rm=TRUE)["50%"]
```

    ##  50% 
    ## 22.2

``` r
quantile(met_2$wind.sp, na.rm=TRUE)["50%"]
```

    ## 50% 
    ## 3.6

``` r
quantile(met_2$atm.press, na.rm=TRUE)["50%"]
```

    ##    50% 
    ## 1011.5

Next identify the stations have these median values.

``` r
station_med <- met_2[, .(
  temp_ID=quantile(temp, probs=.5,na.rm=TRUE),
  wind.sp_ID=quantile(wind.sp, probs=.5,na.rm=TRUE),
  atm.press_ID=quantile(atm.press, probs=.5,na.rm=TRUE)
), by=list(USAFID,STATE)]

station_med[, temp_dist:=abs(temp_ID-21.7)]
median_temp_stations <- station_med[temp_dist==0]

station_med[, wind.sp_dist:=abs(wind.sp_ID-3.1)]
median_wind.sp_stations <- station_med[wind.sp_dist==0]

station_med[, atm.press_dist:=abs(atm.press_ID-1011.7)]
median_atm.press_stations <- station_med[atm.press_dist==0]

station_med[atm.press_dist== 0 & wind.sp_dist==0 & temp_dist==0]
```

    ##    USAFID STATE temp_ID wind.sp_ID atm.press_ID temp_dist wind.sp_dist
    ## 1: 720379    KY    21.7        3.1       1011.7         0            0
    ##    atm.press_dist
    ## 1:              0

**The station with USAFID 723119 has all three median values (or have
values closest to them).**

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
euc <- function(a, b) sqrt(sum((a - b)^2))

station_med_state <- met_2[, .(
  temp_ID_state=quantile(temp, probs=.5,na.rm=TRUE),
  wind.sp_ID_state=quantile(wind.sp, probs=.5,na.rm=TRUE),
  atm.press_ID_state=quantile(atm.press, probs=.5,na.rm=TRUE)
), by=STATE]

merged <- merge(
  x     = station_med,
  y     = station_med_state,
  by.x  = "STATE",
  by.y  = "STATE",
  all.x = TRUE,
  all.y = TRUE
)

merged[, temp_dist:=euc(temp_ID,temp_ID_state)]
median_temp_stations <- merged[ , .SD[which.min(temp_dist)], by = STATE]

merged[, wind.sp_dist:=euc(wind.sp_ID,3.1)]
median_wind.sp_stations <- merged[ , .SD[which.min(wind.sp_dist)], by = STATE]

merged[, atm.press_dist:=euc(atm.press_ID,1011.7)]
median_atm.press_stations <- merged[ , .SD[which.min(atm.press_dist)], by = STATE]

median_temp_stations
```

    ##     STATE USAFID temp_ID wind.sp_ID atm.press_ID temp_dist wind.sp_dist
    ##  1:    AL 720361   27.20        2.6      1011.80    71.382          0.5
    ##  2:    AR 720175   27.80        2.6      1011.00    71.382          0.5
    ##  3:    AZ 722720   26.10        4.1      1008.80    71.382          1.0
    ##  4:    CA 690150   25.60        4.6      1009.40    71.382          1.5
    ##  5:    CO 722817   15.00        4.6      1013.90    71.382          1.5
    ##  6:    CT 725027   21.10        3.1      1009.20    71.382          0.0
    ##  7:    DE 724088   21.00        4.1      1010.80    71.382          1.0
    ##  8:    FL 720383   26.10        2.6      1010.10    71.382          0.5
    ##  9:    GA 722070   24.40        3.6      1011.60    71.382          0.5
    ## 10:    IA 725420   22.20        3.1      1012.10    71.382          0.0
    ## 11:    ID 720369   11.10        2.6      1015.40    71.382          0.5
    ## 12:    IL 724335   25.00        3.1      1012.40    71.382          0.0
    ## 13:    IN 724320   23.90        3.1      1011.50    71.382          0.0
    ## 14:    KS 724468   25.00        3.1      1011.60    71.382          0.0
    ## 15:    KY 720379   21.70        3.1      1011.70    71.382          0.0
    ## 16:    LA 722310   28.30        3.1      1011.50    71.382          0.0
    ## 17:    MA 725059   19.40        3.1      1010.30    71.382          0.0
    ## 18:    MD 720334   22.80        3.1      1010.30    71.382          0.0
    ## 19:    ME 726060   16.10        3.6      1010.00    71.382          0.5
    ## 20:    MI 722093   18.60        3.1      1012.00    71.382          0.0
    ## 21:    MN 726440   20.60        3.6      1013.10    71.382          0.5
    ## 22:    MO 720306   23.90        3.1      1011.80    71.382          0.0
    ## 23:    MS 720708   26.10        2.6      1004.00    71.382          0.5
    ## 24:    MT 726676   18.90        3.1      1012.60    71.382          0.0
    ## 25:    NC 723010   21.10        2.6      1010.40    71.382          0.5
    ## 26:    ND 727530   23.30        4.1      1011.30    71.382          1.0
    ## 27:    NE 725500   23.90        3.6      1011.40    71.382          0.5
    ## 28:    NH 726050   18.30        3.1      1009.40    71.382          0.0
    ## 29:    NJ 720407   20.60        4.1      1012.20    71.382          1.0
    ## 30:    NM 722677   17.80        5.1      1012.10    71.382          2.0
    ## 31:    NV 722096   26.10        4.6      1009.80    71.382          1.5
    ## 32:    NY 724988   19.40        2.6      1011.75    71.382          0.5
    ## 33:    OH 724200   18.90        3.6      1012.30    71.382          0.5
    ## 34:    OK 720558   23.40        3.6      1041.40    71.382          0.5
    ## 35:    OR 720365   13.30        2.6      1016.30    71.382          0.5
    ## 36:    PA 720378   20.00        3.1      1014.10    71.382          0.0
    ## 37:    RI 722151   19.40        3.1      1010.80    71.382          0.0
    ## 38:    SC 722080   25.00        3.6      1011.30    71.382          0.5
    ## 39:    SD 726510   24.40        4.1      1011.70    71.382          1.0
    ## 40:    TN 723240   24.40        3.1      1010.70    71.382          0.0
    ## 41:    TX 720269   29.35        4.6      1005.60    71.382          1.5
    ## 42:    UT 724700   17.20        3.1      1010.40    71.382          0.0
    ## 43:    VA 720498   16.10        2.6      1012.30    71.382          0.5
    ## 44:    VT 725165   17.20        3.1      1009.70    71.382          0.0
    ## 45:    WA 726988   20.60        6.2      1013.65    71.382          3.1
    ## 46:    WI 726400   20.00        4.1      1012.80    71.382          1.0
    ## 47:    WV 724120   18.05        3.6      1011.80    71.382          0.5
    ## 48:    WY 725640   14.40        4.1      1011.50    71.382          1.0
    ##     STATE USAFID temp_ID wind.sp_ID atm.press_ID temp_dist wind.sp_dist
    ##     atm.press_dist temp_ID_state wind.sp_ID_state atm.press_ID_state
    ##  1:           0.10          25.6              2.6            1010.90
    ##  2:           0.70          26.1              2.6            1011.40
    ##  3:           2.90          26.7              3.6            1008.50
    ##  4:           2.30          18.3              3.6            1012.80
    ##  5:           2.20          16.7              3.6            1011.50
    ##  6:           2.50          20.0              3.1            1010.00
    ##  7:           0.90          21.7              3.6            1010.60
    ##  8:           1.60          27.2              3.6            1012.60
    ##  9:           0.10          24.4              3.1            1011.10
    ## 10:           0.40          22.2              3.6            1012.40
    ## 11:           3.70          16.1              3.1            1012.20
    ## 12:           0.70          22.8              3.6            1011.90
    ## 13:           0.20          21.7              3.6            1011.90
    ## 14:           0.10          23.3              3.6            1011.10
    ## 15:           0.00          22.2              3.1            1011.20
    ## 16:           0.20          28.8              3.1            1011.20
    ## 17:           1.40          18.3              3.1            1010.20
    ## 18:           1.40          22.1              3.6            1011.00
    ## 19:           1.70          15.6              3.1            1011.30
    ## 20:           0.30          19.4              3.6            1012.70
    ## 21:           1.40          21.7              3.6            1013.00
    ## 22:           0.10          24.4              3.1            1011.50
    ## 23:           7.70          27.2              3.1            1011.00
    ## 24:           0.90          16.7              3.1            1013.40
    ## 25:           1.30          23.3              3.1            1011.70
    ## 26:           0.40          21.0              3.6            1012.10
    ## 27:           0.30          21.7              3.6            1011.80
    ## 28:           2.30          18.3              2.6            1009.70
    ## 29:           0.50          20.6              3.6            1009.70
    ## 30:           0.40          22.2              4.1            1009.30
    ## 31:           1.90          22.8              3.6            1009.80
    ## 32:           0.05          18.9              3.1            1009.80
    ## 33:           0.60          20.6              3.6            1011.80
    ## 34:          29.70          25.0              3.6            1010.75
    ## 35:           4.60          16.1              3.1            1015.60
    ## 36:           2.40          19.4              3.1            1011.10
    ## 37:           0.90          17.8              3.6            1010.60
    ## 38:           0.40          23.9              3.1            1011.10
    ## 39:           0.00          21.1              3.6            1011.60
    ## 40:           1.00          24.4              3.1            1011.20
    ## 41:           6.10          28.3              4.1            1009.40
    ## 42:           1.30          19.4              3.6            1010.10
    ## 43:           0.60          22.3              3.6            1011.50
    ## 44:           2.00          18.9              2.6            1009.50
    ## 45:           1.95          16.7              3.6            1015.40
    ## 46:           1.10          20.0              3.1            1013.00
    ## 47:           0.10          20.0              3.1            1011.60
    ## 48:           0.20          14.4              3.6            1012.40
    ##     atm.press_dist temp_ID_state wind.sp_ID_state atm.press_ID_state

``` r
median_wind.sp_stations
```

    ##     STATE USAFID temp_ID wind.sp_ID atm.press_ID temp_dist wind.sp_dist
    ##  1:    AL 720361   27.20        2.6      1011.80    71.382      25.4389
    ##  2:    AR 720175   27.80        2.6      1011.00    71.382      25.4389
    ##  3:    AZ 722720   26.10        4.1      1008.80    71.382      25.4389
    ##  4:    CA 690150   25.60        4.6      1009.40    71.382      25.4389
    ##  5:    CO 722817   15.00        4.6      1013.90    71.382      25.4389
    ##  6:    CT 725027   21.10        3.1      1009.20    71.382      25.4389
    ##  7:    DE 724088   21.00        4.1      1010.80    71.382      25.4389
    ##  8:    FL 720383   26.10        2.6      1010.10    71.382      25.4389
    ##  9:    GA 722070   24.40        3.6      1011.60    71.382      25.4389
    ## 10:    IA 725420   22.20        3.1      1012.10    71.382      25.4389
    ## 11:    ID 720369   11.10        2.6      1015.40    71.382      25.4389
    ## 12:    IL 724335   25.00        3.1      1012.40    71.382      25.4389
    ## 13:    IN 724320   23.90        3.1      1011.50    71.382      25.4389
    ## 14:    KS 724468   25.00        3.1      1011.60    71.382      25.4389
    ## 15:    KY 720379   21.70        3.1      1011.70    71.382      25.4389
    ## 16:    LA 722310   28.30        3.1      1011.50    71.382      25.4389
    ## 17:    MA 725059   19.40        3.1      1010.30    71.382      25.4389
    ## 18:    MD 720334   22.80        3.1      1010.30    71.382      25.4389
    ## 19:    ME 726060   16.10        3.6      1010.00    71.382      25.4389
    ## 20:    MI 722093   18.60        3.1      1012.00    71.382      25.4389
    ## 21:    MN 726440   20.60        3.6      1013.10    71.382      25.4389
    ## 22:    MO 720306   23.90        3.1      1011.80    71.382      25.4389
    ## 23:    MS 720708   26.10        2.6      1004.00    71.382      25.4389
    ## 24:    MT 726676   18.90        3.1      1012.60    71.382      25.4389
    ## 25:    NC 723010   21.10        2.6      1010.40    71.382      25.4389
    ## 26:    ND 727530   23.30        4.1      1011.30    71.382      25.4389
    ## 27:    NE 725500   23.90        3.6      1011.40    71.382      25.4389
    ## 28:    NH 726050   18.30        3.1      1009.40    71.382      25.4389
    ## 29:    NJ 720407   20.60        4.1      1012.20    71.382      25.4389
    ## 30:    NM 722677   17.80        5.1      1012.10    71.382      25.4389
    ## 31:    NV 722096   26.10        4.6      1009.80    71.382      25.4389
    ## 32:    NY 724988   19.40        2.6      1011.75    71.382      25.4389
    ## 33:    OH 724200   18.90        3.6      1012.30    71.382      25.4389
    ## 34:    OK 720558   23.40        3.6      1041.40    71.382      25.4389
    ## 35:    OR 720365   13.30        2.6      1016.30    71.382      25.4389
    ## 36:    PA 720378   20.00        3.1      1014.10    71.382      25.4389
    ## 37:    RI 722151   19.40        3.1      1010.80    71.382      25.4389
    ## 38:    SC 722080   25.00        3.6      1011.30    71.382      25.4389
    ## 39:    SD 726510   24.40        4.1      1011.70    71.382      25.4389
    ## 40:    TN 723240   24.40        3.1      1010.70    71.382      25.4389
    ## 41:    TX 720269   29.35        4.6      1005.60    71.382      25.4389
    ## 42:    UT 724700   17.20        3.1      1010.40    71.382      25.4389
    ## 43:    VA 720498   16.10        2.6      1012.30    71.382      25.4389
    ## 44:    VT 725165   17.20        3.1      1009.70    71.382      25.4389
    ## 45:    WA 726988   20.60        6.2      1013.65    71.382      25.4389
    ## 46:    WI 726400   20.00        4.1      1012.80    71.382      25.4389
    ## 47:    WV 724120   18.05        3.6      1011.80    71.382      25.4389
    ## 48:    WY 725640   14.40        4.1      1011.50    71.382      25.4389
    ##     STATE USAFID temp_ID wind.sp_ID atm.press_ID temp_dist wind.sp_dist
    ##     atm.press_dist temp_ID_state wind.sp_ID_state atm.press_ID_state
    ##  1:           0.10          25.6              2.6            1010.90
    ##  2:           0.70          26.1              2.6            1011.40
    ##  3:           2.90          26.7              3.6            1008.50
    ##  4:           2.30          18.3              3.6            1012.80
    ##  5:           2.20          16.7              3.6            1011.50
    ##  6:           2.50          20.0              3.1            1010.00
    ##  7:           0.90          21.7              3.6            1010.60
    ##  8:           1.60          27.2              3.6            1012.60
    ##  9:           0.10          24.4              3.1            1011.10
    ## 10:           0.40          22.2              3.6            1012.40
    ## 11:           3.70          16.1              3.1            1012.20
    ## 12:           0.70          22.8              3.6            1011.90
    ## 13:           0.20          21.7              3.6            1011.90
    ## 14:           0.10          23.3              3.6            1011.10
    ## 15:           0.00          22.2              3.1            1011.20
    ## 16:           0.20          28.8              3.1            1011.20
    ## 17:           1.40          18.3              3.1            1010.20
    ## 18:           1.40          22.1              3.6            1011.00
    ## 19:           1.70          15.6              3.1            1011.30
    ## 20:           0.30          19.4              3.6            1012.70
    ## 21:           1.40          21.7              3.6            1013.00
    ## 22:           0.10          24.4              3.1            1011.50
    ## 23:           7.70          27.2              3.1            1011.00
    ## 24:           0.90          16.7              3.1            1013.40
    ## 25:           1.30          23.3              3.1            1011.70
    ## 26:           0.40          21.0              3.6            1012.10
    ## 27:           0.30          21.7              3.6            1011.80
    ## 28:           2.30          18.3              2.6            1009.70
    ## 29:           0.50          20.6              3.6            1009.70
    ## 30:           0.40          22.2              4.1            1009.30
    ## 31:           1.90          22.8              3.6            1009.80
    ## 32:           0.05          18.9              3.1            1009.80
    ## 33:           0.60          20.6              3.6            1011.80
    ## 34:          29.70          25.0              3.6            1010.75
    ## 35:           4.60          16.1              3.1            1015.60
    ## 36:           2.40          19.4              3.1            1011.10
    ## 37:           0.90          17.8              3.6            1010.60
    ## 38:           0.40          23.9              3.1            1011.10
    ## 39:           0.00          21.1              3.6            1011.60
    ## 40:           1.00          24.4              3.1            1011.20
    ## 41:           6.10          28.3              4.1            1009.40
    ## 42:           1.30          19.4              3.6            1010.10
    ## 43:           0.60          22.3              3.6            1011.50
    ## 44:           2.00          18.9              2.6            1009.50
    ## 45:           1.95          16.7              3.6            1015.40
    ## 46:           1.10          20.0              3.1            1013.00
    ## 47:           0.10          20.0              3.1            1011.60
    ## 48:           0.20          14.4              3.6            1012.40
    ##     atm.press_dist temp_ID_state wind.sp_ID_state atm.press_ID_state

``` r
median_atm.press_stations
```

    ##     STATE USAFID temp_ID wind.sp_ID atm.press_ID temp_dist wind.sp_dist
    ##  1:    AL 720361   27.20        2.6      1011.80    71.382      25.4389
    ##  2:    AR 720175   27.80        2.6      1011.00    71.382      25.4389
    ##  3:    AZ 722720   26.10        4.1      1008.80    71.382      25.4389
    ##  4:    CA 690150   25.60        4.6      1009.40    71.382      25.4389
    ##  5:    CO 722817   15.00        4.6      1013.90    71.382      25.4389
    ##  6:    CT 725027   21.10        3.1      1009.20    71.382      25.4389
    ##  7:    DE 724088   21.00        4.1      1010.80    71.382      25.4389
    ##  8:    FL 720383   26.10        2.6      1010.10    71.382      25.4389
    ##  9:    GA 722070   24.40        3.6      1011.60    71.382      25.4389
    ## 10:    IA 725420   22.20        3.1      1012.10    71.382      25.4389
    ## 11:    ID 720369   11.10        2.6      1015.40    71.382      25.4389
    ## 12:    IL 724335   25.00        3.1      1012.40    71.382      25.4389
    ## 13:    IN 724320   23.90        3.1      1011.50    71.382      25.4389
    ## 14:    KS 724468   25.00        3.1      1011.60    71.382      25.4389
    ## 15:    KY 720379   21.70        3.1      1011.70    71.382      25.4389
    ## 16:    LA 722310   28.30        3.1      1011.50    71.382      25.4389
    ## 17:    MA 725059   19.40        3.1      1010.30    71.382      25.4389
    ## 18:    MD 720334   22.80        3.1      1010.30    71.382      25.4389
    ## 19:    ME 726060   16.10        3.6      1010.00    71.382      25.4389
    ## 20:    MI 722093   18.60        3.1      1012.00    71.382      25.4389
    ## 21:    MN 726440   20.60        3.6      1013.10    71.382      25.4389
    ## 22:    MO 720306   23.90        3.1      1011.80    71.382      25.4389
    ## 23:    MS 720708   26.10        2.6      1004.00    71.382      25.4389
    ## 24:    MT 726676   18.90        3.1      1012.60    71.382      25.4389
    ## 25:    NC 723010   21.10        2.6      1010.40    71.382      25.4389
    ## 26:    ND 727530   23.30        4.1      1011.30    71.382      25.4389
    ## 27:    NE 725500   23.90        3.6      1011.40    71.382      25.4389
    ## 28:    NH 726050   18.30        3.1      1009.40    71.382      25.4389
    ## 29:    NJ 720407   20.60        4.1      1012.20    71.382      25.4389
    ## 30:    NM 722677   17.80        5.1      1012.10    71.382      25.4389
    ## 31:    NV 722096   26.10        4.6      1009.80    71.382      25.4389
    ## 32:    NY 724988   19.40        2.6      1011.75    71.382      25.4389
    ## 33:    OH 724200   18.90        3.6      1012.30    71.382      25.4389
    ## 34:    OK 720558   23.40        3.6      1041.40    71.382      25.4389
    ## 35:    OR 720365   13.30        2.6      1016.30    71.382      25.4389
    ## 36:    PA 720378   20.00        3.1      1014.10    71.382      25.4389
    ## 37:    RI 722151   19.40        3.1      1010.80    71.382      25.4389
    ## 38:    SC 722080   25.00        3.6      1011.30    71.382      25.4389
    ## 39:    SD 726510   24.40        4.1      1011.70    71.382      25.4389
    ## 40:    TN 723240   24.40        3.1      1010.70    71.382      25.4389
    ## 41:    TX 720269   29.35        4.6      1005.60    71.382      25.4389
    ## 42:    UT 724700   17.20        3.1      1010.40    71.382      25.4389
    ## 43:    VA 720498   16.10        2.6      1012.30    71.382      25.4389
    ## 44:    VT 725165   17.20        3.1      1009.70    71.382      25.4389
    ## 45:    WA 726988   20.60        6.2      1013.65    71.382      25.4389
    ## 46:    WI 726400   20.00        4.1      1012.80    71.382      25.4389
    ## 47:    WV 724120   18.05        3.6      1011.80    71.382      25.4389
    ## 48:    WY 725640   14.40        4.1      1011.50    71.382      25.4389
    ##     STATE USAFID temp_ID wind.sp_ID atm.press_ID temp_dist wind.sp_dist
    ##     atm.press_dist temp_ID_state wind.sp_ID_state atm.press_ID_state
    ##  1:       77.84827          25.6              2.6            1010.90
    ##  2:       77.84827          26.1              2.6            1011.40
    ##  3:       77.84827          26.7              3.6            1008.50
    ##  4:       77.84827          18.3              3.6            1012.80
    ##  5:       77.84827          16.7              3.6            1011.50
    ##  6:       77.84827          20.0              3.1            1010.00
    ##  7:       77.84827          21.7              3.6            1010.60
    ##  8:       77.84827          27.2              3.6            1012.60
    ##  9:       77.84827          24.4              3.1            1011.10
    ## 10:       77.84827          22.2              3.6            1012.40
    ## 11:       77.84827          16.1              3.1            1012.20
    ## 12:       77.84827          22.8              3.6            1011.90
    ## 13:       77.84827          21.7              3.6            1011.90
    ## 14:       77.84827          23.3              3.6            1011.10
    ## 15:       77.84827          22.2              3.1            1011.20
    ## 16:       77.84827          28.8              3.1            1011.20
    ## 17:       77.84827          18.3              3.1            1010.20
    ## 18:       77.84827          22.1              3.6            1011.00
    ## 19:       77.84827          15.6              3.1            1011.30
    ## 20:       77.84827          19.4              3.6            1012.70
    ## 21:       77.84827          21.7              3.6            1013.00
    ## 22:       77.84827          24.4              3.1            1011.50
    ## 23:       77.84827          27.2              3.1            1011.00
    ## 24:       77.84827          16.7              3.1            1013.40
    ## 25:       77.84827          23.3              3.1            1011.70
    ## 26:       77.84827          21.0              3.6            1012.10
    ## 27:       77.84827          21.7              3.6            1011.80
    ## 28:       77.84827          18.3              2.6            1009.70
    ## 29:       77.84827          20.6              3.6            1009.70
    ## 30:       77.84827          22.2              4.1            1009.30
    ## 31:       77.84827          22.8              3.6            1009.80
    ## 32:       77.84827          18.9              3.1            1009.80
    ## 33:       77.84827          20.6              3.6            1011.80
    ## 34:       77.84827          25.0              3.6            1010.75
    ## 35:       77.84827          16.1              3.1            1015.60
    ## 36:       77.84827          19.4              3.1            1011.10
    ## 37:       77.84827          17.8              3.6            1010.60
    ## 38:       77.84827          23.9              3.1            1011.10
    ## 39:       77.84827          21.1              3.6            1011.60
    ## 40:       77.84827          24.4              3.1            1011.20
    ## 41:       77.84827          28.3              4.1            1009.40
    ## 42:       77.84827          19.4              3.6            1010.10
    ## 43:       77.84827          22.3              3.6            1011.50
    ## 44:       77.84827          18.9              2.6            1009.50
    ## 45:       77.84827          16.7              3.6            1015.40
    ## 46:       77.84827          20.0              3.1            1013.00
    ## 47:       77.84827          20.0              3.1            1011.60
    ## 48:       77.84827          14.4              3.6            1012.40
    ##     atm.press_dist temp_ID_state wind.sp_ID_state atm.press_ID_state

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
