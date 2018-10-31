
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/sollano/forestmangr.svg?branch=master)](https://travis-ci.org/sollano/forestmangr)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/forestmangr)](https://cran.r-project.org/package=forestmangr)
[![Coverage
Status](https://codecov.io/gh/sollano/forestmangr/branch/master/graph/badge.svg)](https://codecov.io/gh/sollano/forestmangr?branch=master)
[![](https://cranlogs.r-pkg.org/badges/grand-total/forestmangr)](https://cran.rstudio.com/web/packages/forestmangr/index.html)
[![](https://cranlogs.r-pkg.org/badges/forestmangr)](https://cran.rstudio.com/web/packages/forestmangr/index.html)

# forestmangr

Set of functions for processing forest inventory data with methods such
as simple random sampling, stratified random sampling and systematic
sampling. There are also functions for yield and growth predictions and
model fitting, linear and non linear grouped data fitting, and
statistical tests.

## Installation

To install the stable CRAN version, use:

``` r
install.packages("forestmangr")
```

Or you can install forestmangr from github, for the latest dev version
with:

``` r
# install.packages("devtools")
devtools::install_github("sollano/forestmangr")
```

## Example

``` r
library(forestmangr)
library(dplyr)
data("exfm16")
head(exfm16)
#> # A tibble: 6 x 7
#>   strata  plot   age    DH     N     V     B
#>    <int> <int> <dbl> <dbl> <int> <dbl> <dbl>
#> 1      1     1  26.4  12.4  1020  19.7   5.7
#> 2      1     1  38.4  17.2  1020  60.8   9.8
#> 3      1     1  51.6  19.1  1020 103.   13.9
#> 4      1     1  63.6  21.8  1020 136.   15.3
#> 5      1     2  26.4  15     900  27.3   6  
#> 6      1     2  38.4  20.3   900  80    10.5
```

Now, we can fit a model for S estimatation. With `nls_table`, we can fit
a non-linear model, extract it’s coefficients, and merge it with the
original data in one line. Here we’ll use Chapman & Richards model:

``` r
age_i <- 64
exfm16_fit <- exfm16 %>%
  nls_table(DH ~ b0 * (1-exp(-b1* age))^b2, mod_start = c( b0=23, b1=0.03, b2 = 1.3), output="merge") %>% 
  mutate(site = DH *( ( (1- exp( -b1/age ))^b2 ) / (( 1 - exp(-b1/age_i))^b2 ))) %>% 
  select(-b0,-b1,-b2)
head(exfm16_fit)
#>   strata plot  age   DH    N     V    B     site
#> 1      1    1 26.4 12.4 1020  19.7  5.7 22.48027
#> 2      1    1 38.4 17.2 1020  60.8  9.8 24.24290
#> 3      1    1 51.6 19.1 1020 103.4 13.9 22.07375
#> 4      1    1 63.6 21.8 1020 136.5 15.3 21.89203
#> 5      1    2 26.4 15.0  900  27.3  6.0 27.19388
#> 6      1    2 38.4 20.3  900  80.0 10.5 28.61226
```

Now, to fit Clutter’s model, we can use the `fit_clutter` function,
indicating the DH, B, V, S and Plot variable
names:

``` r
coefs_clutter <- fit_clutter(exfm16_fit, "age", "DH", "B", "V", "site", "plot")
coefs_clutter
#>         b0        b1        b2       b3       a0         a1
#> 1 1.398861 -28.84038 0.0251075 1.241779 1.883471 0.05012873
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
#> # A tibble: 10 x 3
#>    TOTAL_AREA PLOT_AREA   VWB
#>         <dbl>     <int> <int>
#>  1       46.8      3000    41
#>  2       46.8      3000    33
#>  3       46.8      3000    24
#>  4       46.8      3000    31
#>  5       46.8      3000    10
#>  6       46.8      3000    32
#>  7       46.8      3000    62
#>  8       46.8      3000    16
#>  9       46.8      3000    66
#> 10       46.8      3000    25
```

First we should try a pilot inventory, to see if the number of plots
sampled is enough for reaching the desired error:

``` r
sprs(exfm3, "VWB", "PLOT_AREA", "TOTAL_AREA", error = 20, pop = "fin")
#>                                        Variables    Values
#> 1              Total number of sampled plots (n)   10.0000
#> 2                    Number of maximum plots (N)  156.0000
#> 3                      Variance Quoeficient (VC)   53.2670
#> 4                                      t-student    2.2622
#> 5                         recalculated t-student    2.0452
#> 6  Number of samples regarding the admited error   25.0000
#> 7                                       Mean (Y)   34.0000
#> 8                Standard error of the mean (Sy)    5.5405
#> 9                                 Absolute Error   12.5335
#> 10                            Relative Error (%)   36.8634
#> 11                  Estimated Total Value (Yhat) 5304.0000
#> 12                                   Total Error 1955.2326
#> 13             Inferior Confidence Interval (m3)   21.4665
#> 14             Superior Confidence Interval (m3)   46.5335
#> 15          Inferior Confidence Interval (m3/ha)   71.5549
#> 16          Superior Confidence Interval (m3/ha)  155.1118
#> 17       inferior Total Confidence Interval (m3) 3348.7674
#> 18       Superior Total Confidence Interval (m3) 7259.2326
```

We can see that we have 10 plots, but 15 more are needed if we want a
minimum of 20% error. The exfm4 data has new samples, that we now can
use to run a definitive inventory:

``` r
sprs(exfm4, "VWB", "PLOT_AREA", "TOTAL_AREA", error = 20, pop = "fin")
#>                                        Variables    Values
#> 1              Total number of sampled plots (n)   25.0000
#> 2                    Number of maximum plots (N)  156.0000
#> 3                      Variance Quoeficient (VC)   45.4600
#> 4                                      t-student    2.0639
#> 5                         recalculated t-student    2.0930
#> 6  Number of samples regarding the admited error   20.0000
#> 7                                       Mean (Y)   33.1200
#> 8                Standard error of the mean (Sy)    2.7595
#> 9                                 Absolute Error    5.6952
#> 10                            Relative Error (%)   17.1957
#> 11                  Estimated Total Value (Yhat) 5166.7200
#> 12                                   Total Error  888.4555
#> 13             Inferior Confidence Interval (m3)   27.4248
#> 14             Superior Confidence Interval (m3)   38.8152
#> 15          Inferior Confidence Interval (m3/ha)   91.4159
#> 16          Superior Confidence Interval (m3/ha)  129.3841
#> 17       inferior Total Confidence Interval (m3) 4278.2645
#> 18       Superior Total Confidence Interval (m3) 6055.1755
```

The exfm2 data has a strata variable. Say we wanted to run a SRS
inventory for every stand. We can do this with the .groups argument:

``` r
head(exfm2,10)
#> # A tibble: 10 x 4
#>    STRATA STRATA_AREA PLOT_AREA   VWB
#>     <int>       <dbl>     <int> <dbl>
#>  1      1        14.4      1000  7.9 
#>  2      1        14.4      1000  3.8 
#>  3      1        14.4      1000  4.4 
#>  4      1        14.4      1000  6.25
#>  5      1        14.4      1000  5.55
#>  6      1        14.4      1000  8.1 
#>  7      1        14.4      1000  6.1 
#>  8      1        14.4      1000  6.6 
#>  9      1        14.4      1000  7.4 
#> 10      1        14.4      1000  5.35
sprs(exfm2, "VWB", "PLOT_AREA", "STRATA_AREA", "STRATA", error = 20, pop = "fin")
#>                                        Variables    Values
#> 1                                            Age    2.1579
#> 2              Total number of sampled plots (n)   57.0000
#> 3                    Number of maximum plots (N)  150.2105
#> 4                      Variance Quoeficient (VC)   32.5358
#> 5                                      t-student    2.0032
#> 6                         recalculated t-student    2.2622
#> 7  Number of samples regarding the admited error   13.0000
#> 8                                       Mean (Y)   11.2439
#> 9                Standard error of the mean (Sy)    0.3817
#> 10                                Absolute Error    0.7646
#> 11                            Relative Error (%)    6.8005
#> 12                  Estimated Total Value (Yhat) 1688.9461
#> 13                                   Total Error  114.8566
#> 14             Inferior Confidence Interval (m3)   10.4792
#> 15             Superior Confidence Interval (m3)   12.0085
#> 16          Inferior Confidence Interval (m3/ha)  104.7922
#> 17          Superior Confidence Interval (m3/ha)  120.0850
#> 18       inferior Total Confidence Interval (m3) 1574.0895
#> 19       Superior Total Confidence Interval (m3) 1803.8026
```

We can also run a stratified random sampling inventory with this
data:

``` r
strs(exfm2, "VWB", "PLOT_AREA", "STRATA_AREA", "STRATA", error = 20, pop = "fin")
#> $Table1
#>                                             Variables  STRATA 1  STRATA 2
#> 1                                           Plot Area 1000.0000 1000.0000
#> 2            Number of sampled plots per stratum (nj)   14.0000   20.0000
#> 3                   Total number of sampled plots (n)   57.0000   57.0000
#> 4            Number of maximum plots per stratum (Nj)  144.0000  164.0000
#> 5                         Number of maximum plots (N)  450.0000  450.0000
#> 6                                     Nj/N Ratio (Pj)    0.3200    0.3644
#> 7                                   Stratum sum (Eyj)   84.5000  240.3000
#> 8                        Stratum quadratic sum (Eyj2)  538.3950 2955.9100
#> 9                         Mean of Yi per stratum (Yj)    6.0357   12.0150
#> 10                                              PjSj2    0.6985    1.3179
#> 11                                               PjSj    0.4728    0.6930
#> 12                                               PjYj    1.9314    4.3788
#> 13                                             EPjSj2    3.6949    3.6949
#> 14                                              EPjSj    1.8936    1.8936
#> 15                                Stratified mean (Y)   10.6471   10.6471
#> 16                          Variance Quoeficient (VC)   17.7851   17.7851
#> 17                                          t-student    2.0032    2.0032
#> 18                             recalculated t-student    3.1824    3.1824
#> 19      Number of samples regarding the admited error    8.0000    8.0000
#> 20 Optimal number of samples per stratum (nj optimal)    2.0000    3.0000
#> 21              Optimal number of samples (n optimal)    9.0000    9.0000
#> 22               Total value of Y per stratum (Yhatj)  869.1429 1970.4600
#>     STRATA 3
#> 1  1000.0000
#> 2    23.0000
#> 3    57.0000
#> 4   142.0000
#> 5   450.0000
#> 6     0.3156
#> 7   316.1000
#> 8  4461.3350
#> 9    13.7435
#> 10    1.6785
#> 11    0.7278
#> 12    4.3368
#> 13    3.6949
#> 14    1.8936
#> 15   10.6471
#> 16   17.7851
#> 17    2.0032
#> 18    3.1824
#> 19    8.0000
#> 20    4.0000
#> 21    9.0000
#> 22 1951.5739
#> 
#> $Table2
#>                                  Variables     value
#> 1                                t-student    2.0032
#> 2          Standard error of the mean (Sy)    0.2339
#> 3                      Stratified Mean (Y)   10.6471
#> 4                           Absolute Error    0.4685
#> 5                       Relative Error (%)    4.4003
#> 6             Estimated Total Value (Yhat) 4791.1768
#> 7                              Total Error  210.8250
#> 8        Inferior Confidence Interval (m3)   10.1786
#> 9        Superior Confidence Interval (m3)   11.1156
#> 10    Inferior Confidence Interval (m3/ha)  101.7856
#> 11    Superior Confidence Interval (m3/ha)  111.1556
#> 12 inferior Total Confidence Interval (m3) 4580.3518
#> 13 Superior Total Confidence Interval (m3) 5002.0018
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
