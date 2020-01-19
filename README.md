Premier League Stats
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/aboland/PremierLeagueStats.svg?branch=master)](https://travis-ci.org/aboland/PremierLeagueStats)
[![codecov](https://codecov.io/gh/aboland/PremierLeagueStats/branch/master/graph/badge.svg)](https://codecov.io/gh/aboland/PremierLeagueStats)

This repo contains an R library and an R Shiny app. Both of which can be
used to look at Premier League game stats.

### Data

The match data is taken from
[football-data.co.uk](http://www.football-data.co.uk).

## R Library

The library can be install in R using the `devtools` library.

    install.packages("devtools")
    devtools::install_github("aboland/PremierLeagueStats/PremieRLeague")

## Shiny App

The Shiny app takes the match data and visually displays statistics. The
application is hosted at [ff.aboland.ie](http://ff.aboland.ie/).

Alternatively the app can be run locally using the above R library.

    PremieRLeague::run_ShinyApp()
