#' Start and end dates of seasons
#'
#' A dataset containing the start and end dates of all premier league seasons from 2000 until 2018.
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{years}{Character for name of years}
#'   \item{start_date}{Dates of the first match in each season}
#'   \item{end_date}{Dates of the last match in each season}
#'   }
#'
"year_dates"

#' Team names and team colours
#'
#' A dataset containing the teams active in the premier league
#' for the seasons from 2000 until 2018, and their team colours
#'
#' @format A data frame with 41 rows and 5 variables:
#' \describe{
#'   \item{teams}{Character for name of years}
#'   \item{col1}{Teams first colour}
#'   \item{col2}{Teams second colour}
#'   \item{col1_rgb}{RGB version of first colour}
#'   \item{col2_rgb}{RGB version of second colour}
#'   }
#'
"team_cols"

#' Match data for 2000/2001 to 2017/2018
#'
#' A dataset containing information on all matches from
#' the seasons from 2000 until 2018, and their team colours
#'
#' @format A data frame with 6840 rows and 103 variables:
#' \describe{
#'   \item{Div}{Division, this is unnecessary for this data}
#'   \item{Date}{Date of match}
#'   \item{HomeTeam}{Home Team}
#'   \item{AwayTeam}{Away Team}
#'   }
#'
"pl_data"
