Premier League Stats
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/aboland/premieRleague.svg?branch=master)](https://travis-ci.org/aboland/premieRleague)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/aboland/premieRleague?branch=master&svg=true)](https://ci.appveyor.com/project/aboland/premieRleague)
[![codecov](https://codecov.io/gh/aboland/premieRleague/branch/master/graph/badge.svg)](https://codecov.io/gh/aboland/premieRleague)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This repo contains an R library and an R Shiny app. Both of which can be
used to look at Premier League game stats.

### Data

The match data is sourced from
[football-data.co.uk](http://www.football-data.co.uk).

## R Library

The library can be install in R using the
[`devtools`](https://devtools.r-lib.org/) library.

``` r
install.packages("devtools")
devtools::install_github("aboland/premieRleague")
```

## Shiny App

The Shiny app takes the match data and visually displays statistics. The
application is hosted at [pl.aboland.ie](http://pl.aboland.ie/).

Alternatively the app can be run locally using the above R library.

``` r
premieRleague::run_ShinyApp()
```
