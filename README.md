# Allie <img src="images/Allie_logo.jpg" width = 110px align="right" vertical-align="top">

This is an R package featuring various utilities and convenient functions for data processing and summary statistics.

## Features
- Suitable for manipulating datasets involving timeseries or repeated measures
- Well suited to New Zealand national health datasets
- Functions work on both data.frame and data.table objects

## Functions

#### Cleaning
- `AutoBreaks` Easily label continuous data into banded categories
- `FindNearestDate` Quickly find and prioritise time-points
- `FreqTable` Quickly create summary tables containing multiple subgroups
- `GroupIntervalDates` Group overlapping time intervals and provide a sequential index

## Installation
The R package "devtools" is required to enable things from GitHub to be installed directly into your R environment.

If devtools is not yet installed, then:
```
install.packages("devtools")
```

To install package, when devtools is available:
```
devtools::install_github("billy-nz/Allie")
```

To load the package:
```
library(PredictRiskScores)
```

## Maintainer / Developer 
- Billy Wu / billy.wu@auckland.ac.nz
