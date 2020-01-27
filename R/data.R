#' Premier League seasons start and end dates
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

#' Premier League team names and team colours
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

#' Premier League match data for seasons 2000/2001 to 2018/2019
#'
#' A dataset containing information on all matches in the Premier League for
#' the seasons from 2000 until 2019.
#'
#' @format A data frame with 7220 rows and 30 variables:
#' \describe{
#'   \item{Div}{Division, this is unnecessary for this data}
#'   \item{Date}{Match Date (dd/mm/yy)}
#'   \item{HomeTeam}{Home Team}
#'   \item{AwayTeam}{Away Team}
#'   \item{FTHG}{Full Time Home Team Goals}
#'   \item{FTAG}{Full Time Away Team Goals}
#'   \item{FTR}{Full Time Result (H=Home Win, D=Draw, A=Away Win)}
#'   \item{HTHG}{Half Time Home Team Goals}
#'   \item{HTAG}{Half Time Away Team Goals}
#'   \item{HTR}{Half Time Result (H=Home Win, D=Draw, A=Away Win)}
#'   \item{Attendance}{Crowd Attendance}
#'   \item{Referee}{Match Referee}
#'   \item{HS}{Home Team Shots}
#'   \item{AS}{Away Team Shots}
#'   \item{HST}{Home Team Shots on Target}
#'   \item{AST}{Away Team Shots on Target}
#'   \item{HHW}{Home Team Hit Woodwork}
#'   \item{AHW}{Away Team Hit Woodwork}
#'   \item{HC}{Home Team Corners}
#'   \item{AC}{Away Team Corners}
#'   \item{HF}{Home Team Fouls Committed}
#'   \item{AF}{Away Team Fouls Committed}
#'   \item{HO}{Home Team Offsides}
#'   \item{AO}{Away Team Offsides}
#'   \item{HY}{Home Team Yellow Cards}
#'   \item{AY}{Away Team Yellow Cards}
#'   \item{HR}{Home Team Red Cards}
#'   \item{AR}{Away Team Red Cards}
#'   \item{HBP}{Home Team Bookings Points (10 = yellow, 25 = red)}
#'   \item{ABP}{Away Team Bookings Points (10 = yellow, 25 = red)}
#'   }
#'
"pl_data"


