# Fantasy Football server script

library(shiny)
library(plyr)
library(XML)
library(ggvis)


#load("current_web_data.RData")
load("current_web_data_tidy.RData")

#gameweek=18

shinyServer(function(input, output) {

  
  # ------------------ Fixture and results tables -------------------------------------------
  
  # output$gameweek_choice<-renderUI({
  #   selectInput("gw_choice", 
  #               label = h3("Gameweek"),
  #               choices = as.list(1:38),
  #               selected = 18)
  # })
  # 
  # output$fix_res <- renderTable({
  #   browser()
  #   if(is.null(input$gw_choice)){
  #     gw <- data.frame("", "", "", "")  # data.frame(0, 0, 0, 0)
  #     dimnames(gw)[[2]] <- c("Date", "Home", " ", "Away")
  #     return(gw)
  #   }
  #   gw <- readHTMLTable(paste0("http://fantasy.premierleague.com/fixtures/",input$gw_choice,"/"))$ismFixtureTable
  #   gw <- gw[!is.na(gw[,2]),]
  #   gw[,4]<-apply(gw[,3:5],1,function(x)paste(x[1],x[2],x[3]))
  #   dimnames(gw)[[2]] <- c("Date", "Home", " ", " ", " ", "Away") 
  #   gw[,c(1,2,4,6)]
  # },include.rownames=F)
  
  
  
  
  
  
  
  
  
  
  
  #  ------------------------ Historical Data!!!!!
  
  
  withProgress(message = "Loading team data", value = 0, {
  
    incProgress(0.1, detail = "Loading historical data")
    load(file="FullHist.RData")
  
  
  current_season <-read.csv(paste0("http://www.football-data.co.uk/mmz4281/1617/E0.csv"))
  
  incProgress(0.3, detail = "Downloading this season")
  
  current_season$Date <- as.Date(current_season$Date,"%d/%m/%y")
  vars<-c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "HTHG", "HTAG", "HTR", "Referee",
          "HS", "AS", "HST", "AST", "HC", "AC", "HF", "AF", "HY", "AY", "HR", "AR")
  current_season <- current_season[,vars]
  incProgress(0.5, detail = "Cleaning this seasons data")
  current_season$Div<-as.character(current_season$Div)
  current_season$HomeTeam<-as.character(current_season$HomeTeam)
  current_season$AwayTeam<-as.character(current_season$AwayTeam)
  current_season$HomeTeam<-as.factor(current_season$HomeTeam)
  current_season$AwayTeam<-as.factor(current_season$AwayTeam)
  current_season$FTR<-as.character(current_season$FTR)
  current_season$HTR<-as.character(current_season$HTR)
  current_season$Referee<-as.character(current_season$Referee)
  incProgress(0.7, detail = "Cleaning data")
  
  fulld <- rbind(histPL,current_season)
  current_teams <- levels(current_season$HomeTeam)
  
  incProgress(0.8, detail = "Cleaning data")
  # possible_games <- reactive({
  #   gw2 <- readHTMLTable(paste0("http://fantasy.premierleague.com/fixtures/",input$gw_choice,"/"))$ismFixtureTable
  #   
  #   gw2 <- readHTMLTable(paste0("https://fantasy.premierleague.com/a/fixtures/18"))
  #   
  #   gw2 <- gw2[!is.na(gw2[,2]),]
  #   gw2[,4]<-apply(gw2[,3:5],1,function(x)paste(x[1],x[2],x[3]))
  #   dimnames(gw2)[[2]] <- c("Date", "Home", " ", " ", " ", "Away") 
  #   output <- list()
  #   for(i in 1:nrow(gw2))
  #     output[[i]] <- paste(as.character(gw2[i,2]),"vs",as.character(gw2[i,6]))
  #   output
  # })
  incProgress(1, detail = "Cleaning data")
  
  })  # end progress bar

  # output$game_hist_choice<-renderUI({
  #   selectInput("game_hist", 
  #               label = h3("Historical Data"),# h3("Historical Data"),
  #               choices = possible_games(),
  #               selected = NULL)
  # })
  
  
  output$historical_result <- renderTable({
    if(is.null(input$game_hist)){
      ht <- "Tottenham"
      at <- "Stoke"
    }else{
      #browser()
      split_t <- strsplit(input$game_hist, split=" vs ")
      ht <- split_t[[1]][1]
      at <- split_t[[1]][2]
      if(ht == "Spurs")
        ht <- "Tottenham"
      if(ht == "Man Utd")
        ht <- "Man United"
      if(at == "Spurs")
        at <- "Tottenham"
      if(at == "Man Utd")
        at <- "Man United"
      # Spurs to Tottenham
      # Man Utd Man United
    }
    
    if(sum(fulld$HomeTeam==ht & fulld$AwayTeam==at)!=0){
      if(input$return_leg==FALSE){
        current_data <- fulld[which(fulld$HomeTeam==ht & fulld$AwayTeam==at),]
      }else{
        current_data <- fulld[which((fulld$HomeTeam==ht & fulld$AwayTeam==at)|(fulld$HomeTeam==at & fulld$AwayTeam==ht)),]
      }
      
      current_data[,"Date"] <-  format(current_data[,"Date"], "%d %b %y")
      #current_data[,"Date"] <- as.character(current_data[,"Date"])
      
      df_out <- data.frame(current_data[,c("Date","HomeTeam","FTHG","FTAG","AwayTeam")])
      for(i in 1:nrow(df_out))
        df_out[i,3] <- paste(df_out[i,3],"-",df_out[i,4])
      dimnames(df_out)[[2]] <- c("Date", "Home", " ", " ", "Away") 
      df_out <- df_out[,c(1,2,3,5)]
    }else{
      df_out <- data.frame(Message="No historical data!")
      return(df_out)
    }
    df_out[rev(rownames(df_out)),]
  },include.rownames=F)
  
  
  
  
  # -------------- Stat plots!!
  
  team_colours <- c("firebrick", "maroon4", "red2", "royalblue4", "red3", "mediumblue", "blue3", "red",
                    "lightskyblue", "red", "black", "green", "red", "red", "red", "blue", "navy", "goldenrod2", "steelblue4", "maroon4")
  team_colours2 <- c("firebrick", "lightskyblue", "black", "royalblue4", "blue", "mediumblue", "blue3", "red",
                    "lightskyblue", "black", "white", "yellow", "white", "white", "white", "blue", "white", "black", "white", "lightskyblue")
  
  team_colours_rgb <- rgb(t(col2rgb(c("firebrick", "maroon4", "red2", "royalblue4", "red3", "mediumblue", "blue3", "red",
                    "lightskyblue", "red", "black", "green", "red", "red", "red", "blue", "navy", "goldenrod2", "steelblue4", "maroon4"))), maxColorValue = 255)
  team_colours2_rgb <- rgb(t(col2rgb(c("firebrick", "lightskyblue", "black", "royalblue4", "blue", "mediumblue", "blue3", "red",
                     "lightskyblue", "black", "white", "yellow", "white", "white", "white", "blue", "white", "black", "white", "lightskyblue"))), maxColorValue = 255)
  
  
  teams_selected <- current_teams
  plot_data <- plot_data2 <- pd2_jit <- NULL
  

  
  # ---------- Head to Head
  
  hh_teams_selected <- current_teams
  
  output$hh_teamA <- renderUI({
    selectInput("hh_tA_in", 
                label = h4("Teams"),
                choices = hh_teams_selected,
                selected = "Tottenham")
  })
  output$hh_teamB <- renderUI({
    selectInput("hh_tB_in", 
                label = NULL,#h4("Team B"),
                choices = hh_teams_selected,
                selected = "Stoke")
  })
  
  
  
  
  
  output$plot_hh <- renderPlot({
    
    if(input$hh_stat_choice == "goals" || is.null(input$hh_stat_choice)){
      hh1 <- "FTHG"
      hh2 <- "FTAG"
      hhlab <- "Goals scored"
    }else if(input$hh_stat_choice == "goals_conc"){
      hh2 <- "FTHG"
      hh1 <- "FTAG"
      hhlab <- "Goals conceded"
    }else if(input$hh_stat_choice== "starget"){
      hh1 <- "HST"
      hh2 <- "AST"
      hhlab <- "Shots on target"
    }else if(input$hh_stat_choice == "shots"){
      hh1 <- "HS"
      hh2 <- "AS"
      hhlab <- "Shots"
    }else if(input$hh_stat_choice== "corners"){
      hh1 <- "HC"
      hh2 <- "AC"
      hhlab <- "Corners"
    }else if(input$hh_stat_choice== "fouls"){
      hh1 <- "HF"
      hh2 <- "AF"
      hhlab <- "Fouls"
    }else if(input$hh_stat_choice== "ycard"){
      hh1 <- "HY"
      hh2 <- "AY"
      hhlab <- "Yellows"
    }else if(input$hh_stat_choice== "rcard"){
      hh1 <- "HR"
      hh2 <- "AR"
      hhlab <- "Reds"
    }else if(input$hh_stat_choice== "halfgoals"){
      hh1 <- "HTHG"
      hh2 <- "HTAG"
      hhlab <- "Halftime goals"
    }
    
    
    
    if(input$hh_season_range[1] == "2015-08-08" && input$hh_season_range[2] == Sys.Date()){
      hh_plot_data_teams <- current_season
      hh_lab2 <- "this season"
      hh_teams_selected <<- current_teams
    }else if(input$hh_season_range[1] == input$hh_season_range[2]){
      hh_plot_data_teams <- current_season
      hh_lab2 <- "this season"
      hh_teams_selected <<- current_teams
    }else{
      sel_range <- which(fulld$Date >= input$hh_season_range[1] & fulld$Date <= input$hh_season_range[2])
      hh_plot_data_teams <- fulld[sel_range,]
      hh_lab2 <- paste("between",format(input$hh_season_range[1],"%d %b %y"),"and",format(input$hh_season_range[2],"%d %b %y"))
      hh_teams_selected <<- current_teams
      # Uncomment if you want to add in older teams...
      # teams_selected <- levels(as.factor(as.character(plot_data_teams$HomeTeam)))
    }
    
    if(!is.null(input$hh_tA_in)&&!is.null(input$hh_tB_in)){
      #browser()
      hh_team_dataA <- hh_plot_data_teams[which(hh_plot_data_teams$HomeTeam==input$hh_tA_in|hh_plot_data_teams$AwayTeam==input$hh_tA_in),]
      hh_team_dataA1 <- hh_plot_data_teams[which(hh_plot_data_teams$HomeTeam==input$hh_tA_in),c(hh1,"FTR","Date","HomeTeam","AwayTeam")]
      hh_team_dataA2 <- hh_plot_data_teams[which(hh_plot_data_teams$AwayTeam==input$hh_tA_in),c(hh2,"FTR","Date","HomeTeam","AwayTeam")]
      dimnames(hh_team_dataA1)[[2]] <- dimnames(hh_team_dataA2)[[2]] <- c("V1","FTR","Date","HomeTeam","AwayTeam")
      hh_team_dataA <- rbind(hh_team_dataA1,hh_team_dataA2)
      hh_team_dataA <- hh_team_dataA[order(hh_team_dataA$Date),]
      hh_team_dataB <- hh_plot_data_teams[which(hh_plot_data_teams$HomeTeam==input$hh_tB_in|hh_plot_data_teams$AwayTeam==input$hh_tB_in),]
      hh_team_dataB1 <- hh_plot_data_teams[which(hh_plot_data_teams$HomeTeam==input$hh_tB_in),c(hh1,"FTR","Date","HomeTeam","AwayTeam")]
      hh_team_dataB2 <- hh_plot_data_teams[which(hh_plot_data_teams$AwayTeam==input$hh_tB_in),c(hh2,"FTR","Date","HomeTeam","AwayTeam")]
      dimnames(hh_team_dataB1)[[2]] <- dimnames(hh_team_dataB2)[[2]] <- c("V1","FTR","Date","HomeTeam","AwayTeam")
      hh_team_dataB <- rbind(hh_team_dataB1, hh_team_dataB2)
      hh_team_dataB <- hh_team_dataB[order(hh_team_dataB$Date),]
      
      if(input$cumul_sum==TRUE){
        hh_team_dataA[,1] <- cumsum(hh_team_dataA[,1])
        hh_team_dataB[,1] <- cumsum(hh_team_dataB[,1])
      }
      
      hh_ylim <- c(min(hh_team_dataA[,1],
                       hh_team_dataB[,1]),
                max(hh_team_dataA[,1],
                    hh_team_dataB[,1]))
      hh_xlim <- c(min(hh_team_dataA[,"Date"],
                       hh_team_dataB[,"Date"]),
                max(hh_team_dataA[,"Date"],
                    hh_team_dataB[,"Date"]))
      
      col_A <- NULL
      for(c in 1:nrow(hh_team_dataA)){
        if(hh_team_dataA[c,"FTR"]=="H" && hh_team_dataA[c,"HomeTeam"]==input$hh_tA_in)
          col_A <- c(col_A, "Green")
        if(hh_team_dataA[c,"FTR"]=="A" && hh_team_dataA[c,"AwayTeam"]==input$hh_tA_in)
          col_A <- c(col_A, "Green")
        if(hh_team_dataA[c,"FTR"]=="A" && hh_team_dataA[c,"HomeTeam"]==input$hh_tA_in)
          col_A <- c(col_A, "Red")
        if(hh_team_dataA[c,"FTR"]=="H" && hh_team_dataA[c,"AwayTeam"]==input$hh_tA_in)
          col_A <- c(col_A, "Red")
        if(hh_team_dataA[c,"FTR"]=="D")
          col_A <- c(col_A, "Black")
        #col_A <- c(col_A, "Blue")
      }
      
      col_B <- NULL
      for(c in 1:nrow(hh_team_dataB)){
        if(hh_team_dataB[c,"FTR"]=="H" && hh_team_dataB[c,"HomeTeam"]==input$hh_tB_in)
          col_B <- c(col_B, "Green")
        if(hh_team_dataB[c,"FTR"]=="A" && hh_team_dataB[c,"AwayTeam"]==input$hh_tB_in)
          col_B <- c(col_B, "Green")
        if(hh_team_dataB[c,"FTR"]=="A" && hh_team_dataB[c,"HomeTeam"]==input$hh_tB_in)
          col_B <- c(col_B, "Red")
        if(hh_team_dataB[c,"FTR"]=="H" && hh_team_dataB[c,"AwayTeam"]==input$hh_tB_in)
          col_B <- c(col_B, "Red")
        if(hh_team_dataB[c,"FTR"]=="D")
          col_B <- c(col_B, "Black")
      }
      
      
      
    plot(hh_team_dataA[,"Date"],
         hh_team_dataA[,1],type = "b",
         xlim = hh_xlim, ylim = hh_ylim, col = "Black",
         xlab = "Date", ylab = hhlab, pch = 19, lty=2, xaxt="n")
    
    tdiff <- abs(as.numeric(difftime(hh_xlim[1], hh_xlim[2], units="days")))
    axis.Date(1, at=seq(hh_xlim[1], hh_xlim[2], by=tdiff/7) ,format="%d %b %y")
    
    lines(hh_team_dataA[,"Date"],
         hh_team_dataA[,1],type = "p",
         xlim = hh_xlim, ylim = hh_ylim, col = col_A,
         xlab = "Date", ylab = "Stat", pch = 19)
    
    if(input$hh_tA_in!=input$hh_tB_in){
      team_b_jit <- jitter(hh_team_dataB[,1],
                         factor=0.3)
      lines(hh_team_dataB[,"Date"],
          team_b_jit, type = "b",
          col = "Black", lty = 3)
      lines(hh_team_dataB[,"Date"],
          team_b_jit, type = "p",
         col = col_B, pch=19)
      legend("topleft",c("Win","Draw","Loss",input$hh_tA_in,input$hh_tB_in),col=c("Green","Black","Red","Black","Black"),pch=c(19,19,19,NA,NA),lty=c(NA,NA,NA,2,3))
    }else{
      lines(hh_team_dataB[,"Date"],
            hh_team_dataB[,1], type = "b",
            col = "Black", lty = 2)
      lines(hh_team_dataB[,"Date"],
            hh_team_dataB[,1], type = "p",
            col = col_B, pch=19)
      legend("topleft",c("Win","Draw","Loss",input$hh_tA_in),col=c("Green","Black","Red","Black"),pch=c(19,19,19,NA),lty=c(NA,NA,NA,2))
      
    }
    }else{
      plot(hh_plot_data_teams[which(hh_plot_data_teams$HomeTeam=="Tottenham"|hh_plot_data_teams$AwayTeam=="Tottenham"),"Date"],
           hh_plot_data_teams[which(hh_plot_data_teams$HomeTeam=="Tottenham"|hh_plot_data_teams$AwayTeam=="Tottenham"),"FTHG"], type = "b",
           ylab="Goals", xlab = "Date")
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ------------ Cutom plot, this gon be big!!!
  
  
  output$plot_stats_custom <- renderPlot({
    #switch(input$stat_choice_x,
    #       "goals" = c("FTHG","FTAG"),
    #       "goals_conc" = c("FTHG","FTAG"))
    
    
    if((input$season_range_c[1] <= "2015-08-08" &&  input$season_range_c[1] >= "2015-06-08") 
       && input$season_range_c[2] >= (Sys.Date()-1)){
      plot_data_teams <- current_season
      clab2 <- "this season"
      teams_selected2 <<- current_teams
    }else if(input$season_range_c[1] == input$season_range_c[2]){
      plot_data_teams <- current_season
      clab2 <- "this season"
      teams_selected2 <<- current_teams
    }else{
      sel_range <- which(fulld$Date >= input$season_range_c[1] & fulld$Date <= input$season_range_c[2])
      plot_data_teams <- fulld[sel_range,]
      clab2 <- paste("between",format(input$season_range_c[1],"%d %b '%y"),"and",format(input$season_range_c[2],"%d %b '%y"))
      teams_selected2 <<- current_teams
      # Uncomment if you want to add in older teams...
      # teams_selected <- levels(as.factor(as.character(plot_data_teams$HomeTeam)))
    }
    
    if(input$stat_choice_x == "goals" || is.null(input$stat_choice_x)){
      cx1 <- "FTHG"
      cx2 <- "FTAG"
      cxlab <- "Goals scored"
    }else if(input$stat_choice_x == "goals_conc"){
      cx2 <- "FTHG"
      cx1 <- "FTAG"
      cxlab <- "Goals conceded"
    }else if(input$stat_choice_x == "shots"){
      cx1 <- "HS"
      cx2 <- "AS"
      cxlab <- "Shots"
    }else if(input$stat_choice_x == "starget"){
      cx1 <- "HST"
      cx2 <- "AST"
      cxlab <- "Shots on target"
    }else if(input$stat_choice_x == "corners"){
      cx1 <- "HC"
      cx2 <- "AC"
      cxlab <- "Corners"
    }else if(input$stat_choice_x == "fouls"){
      cx1 <- "HF"
      cx2 <- "AF"
      cxlab <- "Fouls"
    }else if(input$stat_choice_x == "ycard"){
      cx1 <- "HY"
      cx2 <- "AY"
      cxlab <- "Yellows"
    }else if(input$stat_choice_x == "rcard"){
      cx1 <- "HR"
      cx2 <- "AR"
      cxlab <- "Reds"
    }
    
    if(input$stat_choice_y == "goals" || is.null(input$stat_choice_y)){
      cy1 <- "FTHG"
      cy2 <- "FTAG"
      cylab <- "Goals scored"
    }else if(input$stat_choice_y == "goals_conc"){
      cy2 <- "FTHG"
      cy1 <- "FTAG"
      cylab <- "Goals conceded"
    }else if(input$stat_choice_y == "shots"){
      cy1 <- "HS"
      cy2 <- "AS"
      cylab <- "Shots"
    }else if(input$stat_choice_y == "starget"){
      cy1 <- "HST"
      cy2 <- "AST"
      cylab <- "Shots on target"
    }else if(input$stat_choice_y == "corners"){
      cy1 <- "HC"
      cy2 <- "AC"
      cylab <- "Corners"
    }else if(input$stat_choice_y == "fouls"){
      cy1 <- "HF"
      cy2 <- "AF"
      cylab <- "Fouls"
    }else if(input$stat_choice_y == "ycard"){
      cy1 <- "HY"
      cy2 <- "AY"
      cylab <- "Yellows"
    }else if(input$stat_choice_y == "rcard"){
      cy1 <- "HR"
      cy2 <- "AR"
      cylab <- "Reds"
    }
    
    plot_data_cx <- plot_data_cy <- NULL
    for(k in 1:length(teams_selected2)){
      plot_data_cx[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cx1],
                             plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cx2])
      
      plot_data_cy[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cy1],
                             plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cy2])
    }
    
    
    # Second statistic choice.................
    
    
    if(input$stat_choice_x_per == "no_div" || is.null(input$stat_choice_x_per)){
      per_cx1 <- "nodiv"
      per_cx2 <- "nodiv"
      per_cxlab <- ""
    }else if(input$stat_choice_x_per == "p_game"){
      per_cx1 <- "ngames"
      per_cx2 <- "ngmaes"
      per_cxlab <- "per game"
    }else if(input$stat_choice_x_per == "p_goal"){
      per_cx1 <- "FTHG"
      per_cx2 <- "FTAG"
      per_cxlab <- "per goal"
    }else if(input$stat_choice_x_per == "p_goal_conc"){
      per_cx2 <- "FTHG"
      per_cx1 <- "FTAG"
      per_cxlab <- "per goal"
    }else if(input$stat_choice_x_per == "p_home"){
      per_cx1 <- "home"
      per_cx2 <- "home"
      per_cxlab <- "at home"
    }else if(input$stat_choice_x_per == "p_away"){
      per_cx1 <- "away"
      per_cx2 <- "away"
      per_cxlab <- "away"
    }else if(input$stat_choice_x_per == "p_shot"){
      per_cx1 <- "HS"
      per_cx2 <- "AS"
      per_cxlab <- "per shot"
    }else if(input$stat_choice_x_per == "p_shot_t"){
      per_cx1 <- "HST"
      per_cx2 <- "AST"
      per_cxlab <- "per shot on target"
    }else if(input$stat_choice_x_per == "p_shot_f"){
      per_cx2 <- "HS"
      per_cx1 <- "AS"
      per_cxlab <- "per shot faced"
    }else if(input$stat_choice_x_per == "p_corner"){
      per_cx1 <- "HC"
      per_cx2 <- "AC"
      per_cxlab <- "per corner"
    }else if(input$stat_choice_x_per == "p_corner_f"){
      per_cx2 <- "HC"
      per_cx1 <- "AC"
      per_cxlab <- "per corner faced"
    }else if(input$stat_choice_x_per == "p_foul"){
      per_cx1 <- "HF"
      per_cx2 <- "AF"
      per_cxlab <- "per foul"
    }
    
    
    if(input$stat_choice_y_per == "no_div" || is.null(input$stat_choice_y_per)){
      per_cy1 <- "nodiv"
      per_cy2 <- "nodiv"
      per_cylab <- ""
    }else if(input$stat_choice_y_per == "p_game"){
      per_cy1 <- "ngames"
      per_cy2 <- "ngmaes"
      per_cylab <- "per game"
    }else if(input$stat_choice_y_per == "p_goal"){
      per_cy1 <- "FTHG"
      per_cy2 <- "FTAG"
      per_cylab <- "per goal"
    }else if(input$stat_choice_y_per == "p_goal_conc"){
      per_cy2 <- "FTHG"
      per_cy1 <- "FTAG"
      per_cylab <- "per goal"
    }else if(input$stat_choice_y_per == "p_home"){
      per_cy1 <- "home"
      per_cy2 <- "home"
      per_cylab <- "at home"
    }else if(input$stat_choice_y_per == "p_away"){
      per_cy1 <- "away"
      per_cy2 <- "away"
      per_cylab <- "away"
    }else if(input$stat_choice_y_per == "p_shot"){
      per_cy1 <- "HS"
      per_cy2 <- "AS"
      per_cylab <- "per shot"
    }else if(input$stat_choice_y_per == "p_shot_t"){
      per_cy1 <- "HST"
      per_cy2 <- "AST"
      per_cylab <- "per shot on target"
    }else if(input$stat_choice_y_per == "p_shot_f"){
      per_cy2 <- "HS"
      per_cy1 <- "AS"
      per_cylab <- "per shot faced"
    }else if(input$stat_choice_y_per == "p_corner"){
      per_cy1 <- "HC"
      per_cy2 <- "AC"
      per_cylab <- "per corner"
    }else if(input$stat_choice_y_per == "p_corner_f"){
      per_cy2 <- "HC"
      per_cy1 <- "AC"
      per_cylab <- "per corner faced"
    }else if(input$stat_choice_y_per == "p_foul"){
      per_cy1 <- "HF"
      per_cy2 <- "AF"
      per_cylab <- "per foul"
    }
    
    
    plot_data_cx2 <- plot_data_cy2 <- NULL
    if(per_cx1!="home" && per_cx1!="away" && 
       per_cx2!="home" && per_cx2!="away" && 
       per_cx1!="nodiv" && per_cx2!="nodiv" && 
       per_cx1!="ngames" && per_cx2!="ngames"){
      for(k in 1:length(teams_selected2)){
        plot_data_cx2[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), per_cx1],
                                plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), per_cx2])
      }
    }else if(per_cx1 == "nodiv"){
      plot_data_cx2 <- 1
    }else if(per_cx1=="home"){
      for(k in 1:length(teams_selected2)){
        plot_data_cx[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cx1])
      }
      plot_data_cx2 <- 1
    }else if(per_cx1=="away"){
      for(k in 1:length(teams_selected2)){
        plot_data_cx[k] <- sum(plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cx2])
      }
      plot_data_cx2 <- 1
    }else if(per_cx1=="ngames"){
      for(k in 1:length(teams_selected2)){
        plot_data_cx2[k] <- length(c(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cx1],
                                   plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cx2]))
      }
    }
    
    
    if(per_cy1!="home" && per_cy1!="away" && 
       per_cy2!="home" && per_cy2!="away" && 
       per_cy1!="nodiv" && per_cy2!="nodiv" && 
       per_cy1!="ngames" && per_cy2!="ngames"){
      for(k in 1:length(teams_selected2)){
        plot_data_cy2[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), per_cy1],
                                plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), per_cy2])
        }
      }else if(per_cy1 == "nodiv"){
        plot_data_cy2 <- 1
      }else if(per_cy1=="home"){
        for(k in 1:length(teams_selected2)){
          plot_data_cy[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cy1])
        }
        plot_data_cy2 <- 1
      }else if(per_cy1=="away"){
        for(k in 1:length(teams_selected2))
          plot_data_cy[k] <- sum(plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cy2])
        plot_data_cy2 <- 1
      }else if(per_cy1=="ngames"){
        for(k in 1:length(teams_selected2)){
          plot_data_cy2[k] <- length(c(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cy1],
                                  plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cy2]))
        }
      }
    
    
    #labels for click data
    click_xlab <<- paste0(cxlab," ",per_cxlab)
    click_ylab <<- paste0(cylab," ",per_cylab)
    
    plot_data_cx <- round(plot_data_cx/plot_data_cx2, digits=4)
    click_xdata <<- plot_data_cx
    plot_data_cy <- round(plot_data_cy/plot_data_cy2, digits=4)
    click_ydata <<- plot_data_cy
    

    pd2_jit_cy <<- plot_data_cy
    pd2_jit_cy[duplicated(cbind(plot_data_cy,plot_data_cx))] <<- jitter(plot_data_cy[duplicated(cbind(plot_data_cy,plot_data_cx))], factor = 1.5)
    
    if(input$custom_boundaries == TRUE){
      mymax <- max(c(plot_data_cx,pd2_jit_cy))
      mymin <- min(c(plot_data_cx,pd2_jit_cy))
      my_xlim = c(mymin, mymax + (abs(range(plot_data_cx)[1] - range(plot_data_cx)[2])/10))
      my_ylim = c(mymin, mymax)
    }else{
      my_xlim = c(min(plot_data_cx),max(plot_data_cx) +  (abs(range(plot_data_cx)[1] - range(plot_data_cx)[2])/10))
      my_ylim = range(pd2_jit_cy)
    }
    plot(plot_data_cx, pd2_jit_cy, xlab = paste(cxlab,per_cxlab) , ylab = paste(cylab,per_cylab), 
         main = paste(cylab,per_cylab,"vs.",cxlab,per_cxlab,clab2),
         pch = 19, col = team_colours,cex=1.4, 
         xlim = my_xlim,
         ylim = my_ylim
    )
    
    points(plot_data_cx, pd2_jit_cy,
         pch = 4, col = team_colours2, lwd=1.2
    )
    
    text(plot_data_cx, pd2_jit_cy, current_teams, pos=4)
    
  })

  observe({
    if(!is.null(input$custom_plot_click$x)&&!is.null(input$custom_plot_click$y)&&!is.null(click_xdata)){
      selected <- c(input$custom_plot_click$x,input$custom_plot_click$y)
      closest_team <- which.min(
        sqrt(apply((cbind(click_xdata, pd2_jit_cy)- matrix(selected,nrow=length(click_xdata),byrow=T,ncol=2))^2,1,sum))
      )
      output$info_cus <- renderText({
        paste0(teams_selected[closest_team],":\ ",click_xlab,": ", click_xdata[closest_team],",\ ",click_ylab,": ", click_ydata[closest_team])
      })
    }else{
      output$info_cus <- renderText({
        paste0("Click on graph to see exact values.")
      })
    }
  })
  
  
  
  
  
  mygg_data <- reactive({
    
    if((input$season_range_c[1] <= "2015-08-08" &&  input$season_range_c[1] >= "2015-06-08") 
       && input$season_range_c[2] >= (Sys.Date()-1)){
      plot_data_teams <- current_season
      clab2 <- "this season"
      teams_selected2 <<- current_teams
    }else if(input$season_range_c[1] == input$season_range_c[2]){
      plot_data_teams <- current_season
      clab2 <- "this season"
      teams_selected2 <<- current_teams
    }else{
      sel_range <- which(fulld$Date >= input$season_range_c[1] & fulld$Date <= input$season_range_c[2])
      plot_data_teams <- fulld[sel_range,]
      clab2 <- paste("between",format(input$season_range_c[1],"%d %b %y"),"and",format(input$season_range_c[2],"%d %b %y"))
      teams_selected2 <<- current_teams
      # Uncomment if you want to add in older teams...
      # teams_selected <- levels(as.factor(as.character(plot_data_teams$HomeTeam)))
    }
    
    if(input$stat_choice_x == "goals" || is.null(input$stat_choice_x)){
      cx1 <- "FTHG"
      cx2 <- "FTAG"
      cxlab <- "Goals scored"
    }else if(input$stat_choice_x == "goals_conc"){
      cx2 <- "FTHG"
      cx1 <- "FTAG"
      cxlab <- "Goals conceded"
    }else if(input$stat_choice_x == "shots"){
      cx1 <- "HS"
      cx2 <- "AS"
      cxlab <- "Shots"
    }else if(input$stat_choice_x == "starget"){
      cx1 <- "HST"
      cx2 <- "AST"
      cxlab <- "Shots on target"
    }else if(input$stat_choice_x == "corners"){
      cx1 <- "HC"
      cx2 <- "AC"
      cxlab <- "Corners"
    }else if(input$stat_choice_x == "fouls"){
      cx1 <- "HF"
      cx2 <- "AF"
      cxlab <- "Fouls"
    }else if(input$stat_choice_x == "ycard"){
      cx1 <- "HY"
      cx2 <- "AY"
      cxlab <- "Yellows"
    }else if(input$stat_choice_x == "rcard"){
      cx1 <- "HR"
      cx2 <- "AR"
      cxlab <- "Reds"
    }
    
    if(input$stat_choice_y == "goals" || is.null(input$stat_choice_y)){
      cy1 <- "FTHG"
      cy2 <- "FTAG"
      cylab <- "Goals scored"
    }else if(input$stat_choice_y == "goals_conc"){
      cy2 <- "FTHG"
      cy1 <- "FTAG"
      cylab <- "Goals conceded"
    }else if(input$stat_choice_y == "shots"){
      cy1 <- "HS"
      cy2 <- "AS"
      cylab <- "Shots"
    }else if(input$stat_choice_y == "starget"){
      cy1 <- "HST"
      cy2 <- "AST"
      cylab <- "Shots on target"
    }else if(input$stat_choice_y == "corners"){
      cy1 <- "HC"
      cy2 <- "AC"
      cylab <- "Corners"
    }else if(input$stat_choice_y == "fouls"){
      cy1 <- "HF"
      cy2 <- "AF"
      cylab <- "Fouls"
    }else if(input$stat_choice_y == "ycard"){
      cy1 <- "HY"
      cy2 <- "AY"
      cylab <- "Yellows"
    }else if(input$stat_choice_y == "rcard"){
      cy1 <- "HR"
      cy2 <- "AR"
      cylab <- "Reds"
    }
    
    plot_data_cx <- plot_data_cy <- NULL
    for(k in 1:length(teams_selected2)){
      plot_data_cx[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cx1],
                             plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cx2])
      
      plot_data_cy[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cy1],
                             plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cy2])
    }
    
    
    # Second statistic choice.................
    
    
    if(input$stat_choice_x_per == "no_div" || is.null(input$stat_choice_x_per)){
      per_cx1 <- "nodiv"
      per_cx2 <- "nodiv"
      per_cxlab <- ""
    }else if(input$stat_choice_x_per == "p_game"){
      per_cx1 <- "ngames"
      per_cx2 <- "ngmaes"
      per_cxlab <- "per game"
    }else if(input$stat_choice_x_per == "p_goal"){
      per_cx1 <- "FTHG"
      per_cx2 <- "FTAG"
      per_cxlab <- "per goal"
    }else if(input$stat_choice_x_per == "p_goal_conc"){
      per_cx2 <- "FTHG"
      per_cx1 <- "FTAG"
      per_cxlab <- "per goal"
    }else if(input$stat_choice_x_per == "p_home"){
      per_cx1 <- "home"
      per_cx2 <- "home"
      per_cxlab <- "at home"
    }else if(input$stat_choice_x_per == "p_away"){
      per_cx1 <- "away"
      per_cx2 <- "away"
      per_cxlab <- "away"
    }else if(input$stat_choice_x_per == "p_shot"){
      per_cx1 <- "HS"
      per_cx2 <- "AS"
      per_cxlab <- "per shot"
    }else if(input$stat_choice_x_per == "p_shot_t"){
      per_cx1 <- "HST"
      per_cx2 <- "AST"
      per_cxlab <- "per shot on target"
    }else if(input$stat_choice_x_per == "p_shot_f"){
      per_cx2 <- "HS"
      per_cx1 <- "AS"
      per_cxlab <- "per shot faced"
    }else if(input$stat_choice_x_per == "p_corner"){
      per_cx1 <- "HC"
      per_cx2 <- "AC"
      per_cxlab <- "per corner"
    }else if(input$stat_choice_x_per == "p_corner_f"){
      per_cx2 <- "HC"
      per_cx1 <- "AC"
      per_cxlab <- "per corner faced"
    }else if(input$stat_choice_x_per == "p_foul"){
      per_cx1 <- "HF"
      per_cx2 <- "AF"
      per_cxlab <- "per foul"
    }
    
    
    if(input$stat_choice_y_per == "no_div" || is.null(input$stat_choice_y_per)){
      per_cy1 <- "nodiv"
      per_cy2 <- "nodiv"
      per_cylab <- ""
    }else if(input$stat_choice_y_per == "p_game"){
      per_cy1 <- "ngames"
      per_cy2 <- "ngmaes"
      per_cylab <- "per game"
    }else if(input$stat_choice_y_per == "p_goal"){
      per_cy1 <- "FTHG"
      per_cy2 <- "FTAG"
      per_cylab <- "per goal"
    }else if(input$stat_choice_y_per == "p_goal_conc"){
      per_cy2 <- "FTHG"
      per_cy1 <- "FTAG"
      per_cylab <- "per goal"
    }else if(input$stat_choice_y_per == "p_home"){
      per_cy1 <- "home"
      per_cy2 <- "home"
      per_cylab <- "at home"
    }else if(input$stat_choice_y_per == "p_away"){
      per_cy1 <- "away"
      per_cy2 <- "away"
      per_cylab <- "away"
    }else if(input$stat_choice_y_per == "p_shot"){
      per_cy1 <- "HS"
      per_cy2 <- "AS"
      per_cylab <- "per shot"
    }else if(input$stat_choice_y_per == "p_shot_t"){
      per_cy1 <- "HST"
      per_cy2 <- "AST"
      per_cylab <- "per shot on target"
    }else if(input$stat_choice_y_per == "p_shot_f"){
      per_cy2 <- "HS"
      per_cy1 <- "AS"
      per_cylab <- "per shot faced"
    }else if(input$stat_choice_y_per == "p_corner"){
      per_cy1 <- "HC"
      per_cy2 <- "AC"
      per_cylab <- "per corner"
    }else if(input$stat_choice_y_per == "p_corner_f"){
      per_cy2 <- "HC"
      per_cy1 <- "AC"
      per_cylab <- "per corner faced"
    }else if(input$stat_choice_y_per == "p_foul"){
      per_cy1 <- "HF"
      per_cy2 <- "AF"
      per_cylab <- "per foul"
    }
    
    
    plot_data_cx2 <- plot_data_cy2 <- NULL
    if(per_cx1!="home" && per_cx1!="away" && 
       per_cx2!="home" && per_cx2!="away" && 
       per_cx1!="nodiv" && per_cx2!="nodiv" && 
       per_cx1!="ngames" && per_cx2!="ngames"){
      for(k in 1:length(teams_selected2)){
        plot_data_cx2[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), per_cx1],
                                plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), per_cx2])
      }
    }else if(per_cx1 == "nodiv"){
      plot_data_cx2 <- 1
    }else if(per_cx1=="home"){
      for(k in 1:length(teams_selected2)){
        plot_data_cx[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cx1])
      }
      plot_data_cx2 <- 1
    }else if(per_cx1=="away"){
      for(k in 1:length(teams_selected2)){
        plot_data_cx[k] <- sum(plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cx2])
      }
      plot_data_cx2 <- 1
    }else if(per_cx1=="ngames"){
      for(k in 1:length(teams_selected2)){
        plot_data_cx2[k] <- length(c(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cx1],
                                     plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cx2]))
      }
    }
    
    
    if(per_cy1!="home" && per_cy1!="away" && 
       per_cy2!="home" && per_cy2!="away" && 
       per_cy1!="nodiv" && per_cy2!="nodiv" && 
       per_cy1!="ngames" && per_cy2!="ngames"){
      for(k in 1:length(teams_selected2)){
        plot_data_cy2[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), per_cy1],
                                plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), per_cy2])
      }
    }else if(per_cy1 == "nodiv"){
      plot_data_cy2 <- 1
    }else if(per_cy1=="home"){
      for(k in 1:length(teams_selected2)){
        plot_data_cy[k] <- sum(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cy1])
      }
      plot_data_cy2 <- 1
    }else if(per_cy1=="away"){
      for(k in 1:length(teams_selected2))
        plot_data_cy[k] <- sum(plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cy2])
      plot_data_cy2 <- 1
    }else if(per_cy1=="ngames"){
      for(k in 1:length(teams_selected2)){
        plot_data_cy2[k] <- length(c(plot_data_teams[which(plot_data_teams$HomeTeam == teams_selected2[k]), cy1],
                                     plot_data_teams[which(plot_data_teams$AwayTeam == teams_selected2[k]), cy2]))
      }
    }
    
    
    #labels for click data
    click_xlab <- paste0(cxlab," ",per_cxlab)
    click_ylab <- paste0(cylab," ",per_cylab)
    
    plot_data_cx <- round(plot_data_cx/plot_data_cx2, digits=4)
    #click_xdata <<- plot_data_cx
    plot_data_cy <- round(plot_data_cy/plot_data_cy2, digits=4)
    #click_ydata <<- plot_data_cy
    
    
    pd2_jit_cy <- plot_data_cy
    pd2_jit_cy[duplicated(cbind(plot_data_cy,plot_data_cx))] <- jitter(plot_data_cy[duplicated(cbind(plot_data_cy,plot_data_cx))], factor = 1.5)
    
    if(input$custom_boundaries == TRUE){
      mymax <- max(c(plot_data_cx,pd2_jit_cy))
      mymin <- min(c(plot_data_cx,pd2_jit_cy))
      my_xlim = c(mymin, mymax + (abs(range(plot_data_cx)[1] - range(plot_data_cx)[2])/10))
      my_ylim = c(mymin, mymax)
    }else{
      my_xlim = c(min(plot_data_cx),max(plot_data_cx) +  (abs(range(plot_data_cx)[1] - range(plot_data_cx)[2])/10))
      my_ylim = range(pd2_jit_cy)
    }
    
    return(data.frame(x_data = click_xdata, 
                      y_data = pd2_jit_cy,
                      teams = current_teams,
                      my_cols1 = team_colours,
                      my_cols2 = team_colours2_rgb,
                      my_xlab = paste(cxlab,per_cxlab),
                      my_ylab = paste(cylab,per_cylab)))
    
    #main = paste(cylab,per_cylab,"vs.",cxlab,per_cxlab,clab2),
    
  })
  
  my_tooltip <- function(x){
    return(paste0(x$teams,": ",x$x_data,", ", x$y_data))
  }
  
  
  
  
  
  
 # my_ggvis <- reactive({
 #   #browser()
 #  ggvis(mygg_data(), ~x_data, ~y_data) %>%
 #    layer_points(size := 50, size.hover := 200,
 #                 fillOpacity := 0.5, fillOpacity.hover := 0.9,
 #                 fill :=  ~my_cols2,
 #                 stroke := ~my_cols1,
 #                 key := ~teams) %>%
 #     add_axis("x", title = as.character(mygg_data()$my_xlab[1])) %>%
 #     add_axis("y", title = as.character(mygg_data()$my_ylab[1])) %>%
 #     #add_legend(~my_cols1) %>%
 #     #hide_legend(c("fill","stroke")) %>%
 #    add_tooltip(my_tooltip, "hover")
 # 
 # })
 # 
 # my_ggvis %>% bind_shiny("myggplot")
  
  
  
  
  
  # ----------------- Fantasy players table --------------------------------------
  
  
  
  # Sidebar options
  
  output$field1<-renderUI({
    # Reactive input displaying possible fields
    selectInput("field_choice_1", 
                label = h5("Field 1"),
                choices = as.list(available_fields),
                selected = "% selected by")
  })
  output$field2<-renderUI({
    # Reactive input displaying possible fields
    selectInput("field_choice_2", 
                label = h5("Field 2"),
                choices = as.list(available_fields),
                selected = "Total points")
  })
  output$field3<-renderUI({
    # Reactive input displaying possible fields
    selectInput("field_choice_3", 
                label = h5("Field 3"),
                choices = as.list(available_fields),
                selected = "Next fixture")
  })
  output$field4<-renderUI({
    # Reactive input displaying possible fields
    selectInput("field_choice_4", 
                label = h5("Field 4"),
                choices = as.list(available_fields),
                selected = "Status")
  })
  
  sort_choice <- reactive({
    if(is.null(input$player_data_sort))
      return("Total points")
    input$player_data_sort
  })
  
  output$sort_field<-renderUI({
    # Very poor if construct!!! Could be improved
    if(is.null(sort_choice())||is.null(input$field_choice_1)||is.null(input$field_choice_2)||is.null(input$field_choice_3)||is.null(input$field_choice_4)){
      selectInput("player_data_sort", 
                  label = h4("Sort by"),
                  choices = list("id", "Cost", "Total points"),
                  selected = "Total points")
    }else{
      # Reactive input displaying possible fields
      selectInput("player_data_sort", 
                  label = h4("Sort by"),
                  choices = list("id", "Cost",
                               input$field_choice_1, 
                               input$field_choice_2, 
                               input$field_choice_3, 
                               input$field_choice_4),
                  selected = sort_choice())
    }
  })
  
  
  # Data selections
  
  output$team_choice<-renderUI({
    # Reactive input displaying possible fields
    selectInput("team_ch", 
                label = h5("Team"),
                choices = as.list(c("All",levels(player_data$Team))),
                selected = "All")
  })
  output$position_choice<-renderUI({
    # Reactive input displaying possible fields
    selectInput("pos_ch", 
                label = h5("Position"),
                choices = as.list(c("All",levels(player_data$Position))),
                selected = "All")
  })
  output$cost_choice<-renderUI({
    # Reactive input displaying possible fields
    sliderInput("cost_ch", label = h5("Cost"), min = 0, max = max(player_data$Cost), value = c(0,max(player_data$Cost)))
  })
  
  output$data_display <- renderTable({
    
    if(is.null(input$team_ch) && is.null(input$pos_ch))
      return(player_data[,c("Name", "Team", "Position" ,"Cost")])
    
    rows_display <- rows_display_tm <- rows_display_pos <- 1:nrow(player_data)
    if(!input$team_ch=="All")
      rows_display_tm <- which(player_data$Team==input$team_ch)
    if(!input$pos_ch=="All")
      rows_display_pos <- which(player_data$Position==input$pos_ch)
    
    rows_display_cost <- which(player_data$Cost > input$cost_ch[1] & player_data$Cost < input$cost_ch[2])
    
    rows_display<-rows_display[which(rows_display %in% rows_display_tm)]
    rows_display<-rows_display[which(rows_display %in% rows_display_pos)]
    rows_display<-rows_display[which(rows_display %in% rows_display_cost)]
    
    #browser()
    output_table <- player_data[rows_display,
                                c("Name", "Team", "Position" ,"Cost",
                                  input$field_choice_1, 
                                  input$field_choice_2, 
                                  input$field_choice_3, 
                                  input$field_choice_4)]
    
    if(!is.null(input$player_data_sort)&&(input$player_data_sort=="id"||input$player_data_sort=="Cost"||input$player_data_sort==input$field_choice_1||input$player_data_sort==input$field_choice_2||input$player_data_sort==input$field_choice_3||input$player_data_sort==input$field_choice_4))
      if(input$player_data_sort!="id")
        return(output_table[order(output_table[,input$player_data_sort],decreasing = T),])
    return(output_table)
    
  })

  
  })






  
  
  
  
  
  
  
