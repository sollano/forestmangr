
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/sollano/forestmangr.svg?branch=master)](https://travis-ci.org/sollano/forestmangr)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/forestmangr)](https://cran.r-project.org/package=forestmangr)
[![Coverage
Status](https://codecov.io/gh/sollano/forestmangr/branch/master/graph/badge.svg)](https://codecov.io/gh/sollano/forestmangr?branch=master)

# forestmangr

Set of functions for processing forest inventory data with methods such
as simple random sampling, stratified random sampling and systematic
sampling. There are also functions for yield and growth predictions and
model fitting, linear and non linear grouped data fitting, and
statistical tests.

## Installation

You can install forestmangr from github with:

``` r
# install.packages("devtools")
devtools::install_github("sollano/forestmangr")
```

## Example

``` r
library(forestmangr)
data("exfm17")
head(exfm17)
```

Now, we can fit a model for S estimatation. With nls\_table, we can fit
a non-linear model, extract it’s coefficients, and merge it with the
original data in one line. Here we’ll use Chapman & Richards model:

``` r
age_i <- 64
exfm16_fit <- exfm17 %>%
  nls_table(DH ~ b0 * (1-exp(-b1* age))^b2, mod_start = c( b0=23, b1=0.03, b2 = 1.3), output="merge") %>% 
  mutate(site = DH *( ( (1- exp( -b1/age ))^b2 ) / (( 1 - exp(-b1/age_i))^b2 ))) %>% 
  select(-b0,-b1,-b2)
head(exfm16_fit)
```

Now, to fit Clutter’s model, we can use the fit\_clutter function,
indicating the DH, B, V, S and Plot variable
names:

``` r
coefs_clutter <- fit_clutter(exfm17_fit, "age", "DH", "B", "V", "site", "plot")
coefs_clutter
```

Now let’s say we wanted to do a Simple Random Sampling Forest Inventory,
with 20% as a accepted error. First, let’s load the package and some
data:

``` r
library(forestmangr)
data("exfm2")
data("exfm3")
data("exfm4")
head(exfm3,10)
```

First we should try a pilot inventory, to see if the number of plots
sampled is enough for reaching the desired error:

``` r
sprs(exfm3, "VWB", "PLOT_AREA", "TOTAL_AREA", error = 20, pop = "fin")
```

We can see that we have 10 plots, but 15 more are needed if we want a
minimum of 20% error. The exfm4 data has new samples, that we now can
use to run a definitive inventory:

``` r
sprs(exfm4, "VWB", "PLOT_AREA", "TOTAL_AREA", error = 20, pop = "fin")
```

The exfm2 data has a strata variable. Say we wanted to run a SRS
inventory for every stand. We can do this with the grupos argument:

``` r
head(exfm2,10)
sprs(exfm2, "VWB", "PLOT_AREA", "STRATA_AREA", "STRATA", error = 20, pop = "fin")
```

We can also run a stratified random sampling inventory with this
data:

``` r
strs(exfm2, "VWB", "PLOT_AREA", "STRATA_AREA", "STRATA", error = 20, pop = "fin")
```

## License

This project is licensed under the MIT License - see the
[LICENSE](LICENSE) file for details

## Acknowledgments

  - This project is being done on the Forest Management Lab, DEF, UFVJM
    - Diamantina/Minas Gerais - Brazil.

  - This project came to be as a mean to make the life of a forestry
    engeneer a little easier and pratical. We’d like to thank everyone
    at UFVJM that has in anyway helped this project grow.

  - We’d like to thank UFVJM, FAPEMIG, CNPq e CAPES fo rthe support.
