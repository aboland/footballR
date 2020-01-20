#' Function to calculate stats for Premier League teams
#'
#' This function outputs stats for given dates
#' @param team_data Data Frame in the form output by read_PL_data
#' @param date_from Start date
#' @param date_to End date
#' @param x_stat The statistic output for the x axis. See details.
#' @param y_stat The statistic output for the y axis. See details.
#' @param teams The teams to output.
#' @param x_stat_by The aggregation for the x stat. See details.
#' @param y_stat_by The aggregation for the y stat. See details.
#' @param custom_boundary Boolean
#' @import dplyr
#' @export

PL_plot_data <-
  function(team_data, date_from, date_to, x_stat, y_stat, teams, x_stat_by=NA, y_stat_by=NA, custom_boundary=FALSE){

  start_date <- date_from
  end_date <- date_to

  selected_range <- which(team_data$Date >= start_date & team_data$Date <= end_date)
  clab2 <- paste("between",
                 format(as.Date(start_date),"%d %b '%y"),"and",
                 format(as.Date(end_date),"%d %b '%y"))

  plot_data_full <- team_data[selected_range,]

  active_teams <- sort(unique(plot_data_full$HomeTeam))


  cx <- switch(x_stat,
               "goals" = c("FTHG","FTAG","Goals scored"),
               "goals_conc" = c("FTAG","FTHG","Goals conceded"),
               "starget" = c("HST","AST","Shots on target"),
               "shots" = c("HS","AS","Shots"),
               "corners" = c("HC","AC","Corners"),
               "fouls" = c("HF","AF","Fouls"),
               "ycard" = c("HY","AY", "Yellows"),
               "rcard" = c("HR","AR", "Reds"),
               "halfgoals" = c("HTHG","HTAG", "Halftime Goalds"))

  cy <- switch(y_stat,
               "goals" = c("FTHG","FTAG","Goals scored"),
               "goals_conc" = c("FTAG","FTHG","Goals conceded"),
               "starget" = c("HST","AST","Shots on target"),
               "shots" = c("HS","AS","Shots"),
               "corners" = c("HC","AC","Corners"),
               "fouls" = c("HF","AF","Fouls"),
               "ycard" = c("HY","AY","Yellows"),
               "rcard" = c("HR","AR","Red"),
               "halfgoals" = c("HTHG","HTAG","Halftime Goalds"))

  plot_data_cx <- plot_data_cy <- NULL

  home_temp <-
    plot_data_full %>%
    group_by(.data$HomeTeam) %>%
    summarise(stat = sum(!!sym(cx[1]))) %>%
    rename(Team = .data$HomeTeam) %>%
    filter(.data$Team %in% active_teams)

  away_temp <-
    plot_data_full %>%
    group_by(.data$AwayTeam) %>%
    summarise(stat = sum(!!sym(cx[2]))) %>%
    rename(Team = .data$AwayTeam ) %>%
    filter(.data$Team %in% active_teams)

  plot_data_cx <-
    bind_rows(home_temp,away_temp) %>%
    group_by(.data$Team) %>%
    summarise_all(sum) %>%
    arrange(.data$Team)

  plot_data_cx <- plot_data_cx$stat


  home_temp_y <-
    plot_data_full %>%
    group_by(.data$HomeTeam) %>%
    summarise(stat = sum(!!sym(cy[1]))) %>%
    rename(Team = .data$HomeTeam) %>%
    filter(.data$Team %in% active_teams)

  away_temp_y <-
    plot_data_full %>%
    group_by(.data$AwayTeam) %>%
    summarise(stat = sum(!!sym(cy[2]))) %>%
    rename(Team = .data$AwayTeam) %>%
    filter(.data$Team %in% active_teams)

  plot_data_cy <-
    bind_rows(home_temp_y,away_temp_y) %>%
    group_by(.data$Team) %>%
    summarise_all(sum) %>%
    arrange(.data$Team)

  plot_data_cy <- plot_data_cy$stat

  ###############################################################
  # Second statistic choice.................
  ################################################################

  per_cx <- switch(x_stat_by,
                   "no_div" = c("nodiv", "nodiv", ""),
                   "p_game" = c("ngames", "ngames", "per game"),
                   "p_goal" = c("FTHG", "FTAG", "per goal"),
                   "p_goal_conc" = c("FTAG", "FTHG", "per goal"),
                   "p_home" = c("home", "home", "at home"),
                   "p_away" = c("away", "away", "away"),
                   "p_shot" = c("HS", "AS", "per shot"),
                   "p_shot_t" = c("HST", "AST", "per shot on target"),
                   "p_shot_f" = c("AS", "HS", "per shot faced"),
                   "p_corner" = c("HC", "AC", "per corner"),
                   "p_corner_f" = c("AC", "HC", "per corner faced"),
                   "p_foul" = c("HF", "AF","per foul"))



  plot_data_cx2 <- plot_data_cy2 <- NULL

  if (!is.null(per_cx)) {

    if (per_cx[1] != "home" && per_cx[1] != "away" &&
       per_cx[2] != "home" && per_cx[2] != "away" &&
       per_cx[1] != "nodiv" && per_cx[2] != "nodiv" &&
       per_cx[1] != "ngames" && per_cx[2] != "ngames") {
      for (k in 1:length(active_teams)) {
        plot_data_cx2[k] <- sum(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), per_cx[1]],
                                plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), per_cx[2]])
      }
    }else if (per_cx[1] == "nodiv") {
      plot_data_cx2 <- 1
    }else if (per_cx[1] == "home") {
      for (k in 1:length(active_teams)) {
        plot_data_cx[k] <- sum(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), cx[1]])
      }
      plot_data_cx2 <- 1
    }else if (per_cx[1] == "away") {
      for (k in 1:length(active_teams)) {
        plot_data_cx[k] <- sum(plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), cx[2]])
      }
      plot_data_cx2 <- 1
    }else if (per_cx[1] == "ngames") {
      for (k in 1:length(active_teams)) {
        plot_data_cx2[k] <- length(c(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), cx[1]],
                                     plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), cx[2]]))
      }
    }

    plot_data_cx <- round(plot_data_cx/plot_data_cx2, digits = 4)

  }

  per_cy <- switch(y_stat_by,
                   "no_div" = c("nodiv", "nodiv", ""),
                   "p_game" = c("ngames", "ngames", "per game"),
                   "p_goal" = c("FTHG", "FTAG", "per goal"),
                   "p_goal_conc" = c("FTAG", "FTHG", "per goal"),
                   "p_home" = c("home", "home", "at home"),
                   "p_away" = c("away", "away", "away"),
                   "p_shot" = c("HS", "AS", "per shot"),
                   "p_shot_t" = c("HST", "AST", "per shot on target"),
                   "p_shot_f" = c("AS", "HS", "per shot faced"),
                   "p_corner" = c("HC", "AC", "per corner"),
                   "p_corner_f" = c("AC", "HC", "per corner faced"),
                   "p_foul" = c("HF", "AF","per foul"))

  if (!is.null(per_cy)) {

    if (per_cy[1] != "home" && per_cy[1] != "away" &&
       per_cy[2] != "home" && per_cy[2] != "away" &&
       per_cy[1] != "nodiv" && per_cy[2] != "nodiv" &&
       per_cy[1] != "ngames" && per_cy[2] != "ngames") {
      for (k in 1:length(active_teams)) {
        plot_data_cy2[k] <- sum(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), per_cy[1]],
                                plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), per_cy[2]])
      }
    }else if (per_cy[1] == "nodiv") {
      plot_data_cy2 <- 1
    }else if (per_cy[1] == "home") {
      for (k in 1:length(active_teams)) {
        plot_data_cy[k] <- sum(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), cy[1]])
      }
      plot_data_cy2 <- 1
    }else if (per_cy[1] == "away") {
      for (k in 1:length(active_teams))
        plot_data_cy[k] <- sum(plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), cy[2]])
      plot_data_cy2 <- 1
    }else if (per_cy[1] == "ngames") {
      for (k in 1:length(active_teams)) {
        plot_data_cy2[k] <- length(c(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), cy[1]],
                                     plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), cy[2]]))
      }
    }

    plot_data_cy <- round(plot_data_cy/plot_data_cy2, digits = 4)

  }

  #################################################################


  if (custom_boundary == TRUE) {
    mymax <- max(c(plot_data_cx,plot_data_cy))
    mymin <- min(c(plot_data_cx,plot_data_cy))
    my_xlim = c(mymin, mymax + (abs(range(plot_data_cx)[1] - range(plot_data_cx)[2])/10))
    my_ylim = c(mymin, mymax)
  }else{
    my_xlim = c(min(plot_data_cx), max(plot_data_cx) +  (abs(range(plot_data_cx)[1] - range(plot_data_cx)[2])/10))
    my_ylim = range(plot_data_cy)
  }

  output_data <- list(x = plot_data_cx,
                      y = plot_data_cy,
                      teams = active_teams,
                      xlim = my_xlim,
                      ylim = my_ylim,
                      xlabels = cx[3],
                      ylabels = cy[3],
                      xlabels_per = per_cx[3],
                      ylabels_per = per_cy[3],
                      main_label = clab2,
                      cols = premieRleague::team_cols[active_teams,])

}
