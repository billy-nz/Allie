# Allie <img src="images/Allie_logo.jpg" width = 110px align="right" vertical-align="top">

This is an R package featuring various utilities and convenient functions for data processing and summary statistics.

## Features
-	Designed to work on large scale health datasets that contain multiple individuals with multiple observations
-	Dynamic correction of input parameters rather than returning an error
-	Does not require installing other packages (dependencies)

## Functions

#### Cleaning
- `AutoBands` Easily label continuous data into banded categories
- `FindNearest` Quickly find and prioritise time-points

#### Blending
- `CVDRisk` Combine variables to create a risk score
- `CMIndex` Combine variables to create a multi-morbidity index

#### Aggregation
- `FreqTable` Quickly create summary tables containing multiple subgroups

## Installation
The R package "devtools" is required to enable things from GitHub to be installed directly into your R environment.
```
# If devtools is not yet installed, then:
install.packages("devtools")

# When devtools is installed, then:
devtools::install_github("billy-nz/Allie")
```

## Author
- Billy Wu - University of Auckland
