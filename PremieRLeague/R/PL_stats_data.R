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
#' @import dplyr
#' @export

PL_stats_data <-
  function(team_data, date_from, date_to, x_stat, y_stat, teams, x_stat_by=NA, y_stat_by=NA){

    start_date <- date_from
    end_date <- date_to

    selected_range <- which(team_data$Date >= start_date & team_data$Date <= end_date)
    date_string <- paste("between",
                   format(as.Date(start_date),"%d %b '%y"),"and",
                   format(as.Date(end_date),"%d %b '%y"))

    plot_data_full <- team_data[selected_range,]

    active_teams <- sort(unique(plot_data_full$HomeTeam))


    stat1 <- switch(x_stat,
                 "goals" = c("FTHG","FTAG","Goals scored"),
                 "goals_conc" = c("FTAG","FTHG","Goals conceded"),
                 "starget" = c("HST","AST","Shots on target"),
                 "shots" = c("HS","AS","Shots"),
                 "corners" = c("HC","AC","Corners"),
                 "fouls" = c("HF","AF","Fouls"),
                 "ycard" = c("HY","AY", "Yellows"),
                 "rcard" = c("HR","AR", "Reds"),
                 "halfgoals" = c("HTHG","HTAG", "Halftime Goalds"))

    stat2 <- switch(y_stat,
                 "goals" = c("FTHG","FTAG","Goals scored"),
                 "goals_conc" = c("FTAG","FTHG","Goals conceded"),
                 "starget" = c("HST","AST","Shots on target"),
                 "shots" = c("HS","AS","Shots"),
                 "corners" = c("HC","AC","Corners"),
                 "fouls" = c("HF","AF","Fouls"),
                 "ycard" = c("HY","AY","Yellows"),
                 "rcard" = c("HR","AR","Red"),
                 "halfgoals" = c("HTHG","HTAG","Halftime Goalds"))

    stat1_data <- stat2_data <- NULL

    home_temp <- plot_data_full %>% group_by(HomeTeam) %>%
      summarise(stat = sum(get(stat1[1]))) %>%
      rename(Team = HomeTeam) %>% filter(Team %in% active_teams)
    away_temp <- plot_data_full %>% group_by(AwayTeam) %>%
      summarise(stat = sum(get(stat1[2]))) %>%
      rename(Team = AwayTeam )%>% filter(Team %in% active_teams)
    stat1_data <- bind_rows(home_temp,away_temp) %>%
      group_by(Team) %>%
      summarise_all(sum) %>%
      arrange(Team)
    stat1_data <- stat1_data$stat

    home_temp_y <- plot_data_full %>% group_by(HomeTeam) %>%
      summarise(stat = sum(get(stat2[1]))) %>%
      rename(Team = HomeTeam) %>% filter(Team %in% active_teams)
    away_temp_y <- plot_data_full %>% group_by(AwayTeam) %>%
      summarise(stat = sum(get(stat2[2]))) %>%
      rename(Team = AwayTeam)%>% filter(Team %in% active_teams)
    stat2_data <- bind_rows(home_temp_y,away_temp_y) %>%
      group_by(Team) %>%
      summarise_all(sum) %>%
      arrange(Team)
    stat2_data <- stat2_data$stat

    ###############################################################
    # Second statistic choice.................
    ################################################################

    stat1_by <- switch(x_stat_by,
                     "no_div" = c("nodiv", "nodiv", ""),
                     "p_game" = c("ngames", "ngames", "per game"),
                     "p_goal" = c("FTHG", "FTAG", "per goal"),
                     "p_goal_conc" = c("FTAG", "FTHG", "per goal"),
                     "p_home" = c("home", "home", "at home"),
                     "p_away"=c("away", "away", "away"),
                     "p_shot" = c("HS", "AS", "per shot"),
                     "p_shot_t" = c("HST", "AST", "per shot on target"),
                     "p_shot_f" = c("AS", "HS", "per shot faced"),
                     "p_corner" = c("HC", "AC", "per corner"),
                     "p_corner_f" = c("AC", "HC", "per corner faced"),
                     "p_foul" = c("HF", "AF","per foul"))



    # home_temp_by <- plot_data_full %>% group_by(HomeTeam) %>%
    #   summarise(stat = sum(rlang::UQ(rlang::sym(stat1[1])))) %>%
    #   rename(Team = HomeTeam) %>% filter(Team %in% active_teams)
    # away_temp_by <- plot_data_full %>% group_by(AwayTeam) %>%
    #   summarise(stat = sum(rlang::UQ(rlang::sym(stat1[2])))) %>%
    #   rename(Team = AwayTeam )%>% filter(Team %in% active_teams)
    # stat1_data_by <- bind_rows(home_temp,away_temp) %>%
    #   group_by(Team) %>%
    #   summarise_all(sum) %>%
    #   arrange(Team)
    # stat1_data_by <- stat1_data_by$stat

    stat1_by_data <- stat2_by_data <- NULL

    if(!is.null(stat1_by)){

      if(stat1_by[1]!="home" && stat1_by[1]!="away" &&
         stat1_by[2]!="home" && stat1_by[2]!="away" &&
         stat1_by[1]!="nodiv" && stat1_by[2]!="nodiv" &&
         stat1_by[1]!="ngames" && stat1_by[2]!="ngames"){
        for(k in 1:length(active_teams)){
          stat1_by_data[k] <- sum(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), stat1_by[1]],
                                  plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), stat1_by[2]])
        }
      }else if(stat1_by[1] == "nodiv"){
        stat1_by_data <- 1
      }else if(stat1_by[1]=="home"){
        for(k in 1:length(active_teams)){
          stat1_data[k] <- sum(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), stat1[1]])
        }
        stat1_by_data <- 1
      }else if(stat1_by[1]=="away"){
        for(k in 1:length(active_teams)){
          stat1_data[k] <- sum(plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), stat1[2]])
        }
        stat1_by_data <- 1
      }else if(stat1_by[1]=="ngames"){
        for(k in 1:length(active_teams)){
          stat1_by_data[k] <- length(c(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), stat1[1]],
                                       plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), stat1[2]]))
        }
      }

      stat1_data <- round(stat1_data/stat1_by_data, digits=4)

    }

    stat2_by <- switch(y_stat_by,
                     "no_div" = c("nodiv", "nodiv", ""),
                     "p_game" = c("ngames", "ngames", "per game"),
                     "p_goal" = c("FTHG", "FTAG", "per goal"),
                     "p_goal_conc" = c("FTAG", "FTHG", "per goal"),
                     "p_home" = c("home", "home", "at home"),
                     "p_away"=c("away", "away", "away"),
                     "p_shot" = c("HS", "AS", "per shot"),
                     "p_shot_t" = c("HST", "AST", "per shot on target"),
                     "p_shot_f" = c("AS", "HS", "per shot faced"),
                     "p_corner" = c("HC", "AC", "per corner"),
                     "p_corner_f" = c("AC", "HC", "per corner faced"),
                     "p_foul" = c("HF", "AF","per foul"))

    if(!is.null(stat2_by)){

      if(stat2_by[1]!="home" && stat2_by[1]!="away" &&
         stat2_by[2]!="home" && stat2_by[2]!="away" &&
         stat2_by[1]!="nodiv" && stat2_by[2]!="nodiv" &&
         stat2_by[1]!="ngames" && stat2_by[2]!="ngames"){
        for(k in 1:length(active_teams)){
          stat2_by_data[k] <- sum(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), stat2_by[1]],
                                  plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), stat2_by[2]])
        }
      }else if(stat2_by[1] == "nodiv"){
        stat2_by_data <- 1
      }else if(stat2_by[1]=="home"){
        for(k in 1:length(active_teams)){
          stat2_data[k] <- sum(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), stat2[1]])
        }
        stat2_by_data <- 1
      }else if(stat2_by[1]=="away"){
        for(k in 1:length(active_teams))
          stat2_data[k] <- sum(plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), stat2[2]])
        stat2_by_data <- 1
      }else if(stat2_by[1]=="ngames"){
        for(k in 1:length(active_teams)){
          stat2_by_data[k] <- length(c(plot_data_full[which(plot_data_full$HomeTeam == active_teams[k]), stat2[1]],
                                       plot_data_full[which(plot_data_full$AwayTeam == active_teams[k]), stat2[2]]))
        }
      }

      stat2_data <- round(stat2_data/stat2_by_data, digits=4)

    }

    #################################################################

    output_data <- list(stat1 = stat1_data,
                        stat2 = stat2_data,
                        teams = active_teams,
                        stat1_name = stat1[3],
                        stat2_name = stat2[3],
                        stat1_by = stat1_by[3],
                        stat2_by = stat2_by[3],
                        main_label = date_string)

  }
