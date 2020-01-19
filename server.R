# Fantasy Football server script

library(shiny)
#library(plyr)
library(XML)
# library(ggvis)
library(jsonlite)
library(RCurl)
library(dplyr)
library(plotly)

# library(devtools)
# devtools::install_github("aboland/PremierLeagueStats/PremieRLeague")
library(PremieRLeague)


shinyServer(function(input, output, session) {


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
    # load(file = "FullHist_17.RData")
    # histPL <- read.csv("PL_Hist_00-19.csv", stringsAsFactors = F)
    histPL <- pl_data

    incProgress(0.3, detail = "Downloading this season")
    current_season <- PL_read_data("2019-06-01", Sys.Date())


    missing_columns <- names(histPL)[which(!names(histPL) %in% names(current_season))]

    if (length(missing_columns) > 0)
      for (j in 1:length(missing_columns))
        current_season[missing_columns[j]] <- NA
    current_season <- current_season[,names(histPL)]

    fulld <- rbind(histPL, current_season)

    updateDateRangeInput(session, inputId = "season_range_c", end = max(fulld$Date), max = max(fulld$Date))
    #fulld_test <- bind_rows(histPL, current_season)

    current_teams <- unique(current_season$HomeTeam)

    incProgress(1, detail = "Cleaning data")

  })  # end progress bar

  # output$game_hist_choice<-renderUI({
  #   selectInput("game_hist",
  #               label = h3("Historical Data"),# h3("Historical Data"),
  #               choices = possible_games(),
  #               selected = NULL)
  # })


  output$historical_result <- renderTable({
    if (is.null(input$game_hist)) {
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

    if(sum(fulld$HomeTeam == ht & fulld$AwayTeam == at) != 0){
      if(input$return_leg == FALSE){
        current_data <- fulld[which(fulld$HomeTeam == ht & fulld$AwayTeam == at),]
      }else{
        current_data <- fulld[which((fulld$HomeTeam == ht & fulld$AwayTeam == at)|(fulld$HomeTeam==at & fulld$AwayTeam==ht)),]
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

    hh1 <- "FTHG"
    hh2 <- "FTAG"
    hhlab <- "Goals scored"

    hh1 <- switch(input$hh_stat_choice,
                  "goals" = "FTHG",
                  "goals_conc" = "FTAG",
                  "starget" = "HST",
                  "shots" = "HS",
                  "corners" = "HC",
                  "fouls" = "HF",
                  "ycard" = "HY",
                  "rcard" = "HR",
                  "halfgoals" = "HTHG")

    hh2 <- switch(input$hh_stat_choice,
                  "goals" = "FTAG",
                  "goals_conc" = "FTHG",
                  "starget" = "AST",
                  "shots" = "AS",
                  "corners" = "AC",
                  "fouls" = "AF",
                  "ycard" = "AY",
                  "rcard" = "AR",
                  "halfgoals" = "HTAG")

    hhlab <- switch(input$hh_stat_choice,
                    "goals" = "Goals scored",
                    "goals_conc" = "Goals conceded",
                    "starget" = "Shots on target",
                    "shots" = "Shots",
                    "corners" = "Corners",
                    "fouls" = "Fouls",
                    "ycard" = "Yellows",
                    "rcard" = "Reds",
                    "halfgoals" = "Halftime goals")


    if (input$hh_season_range[1] == "2015-08-08" && input$hh_season_range[2] == Sys.Date()) {
      hh_plot_data_teams <- current_season
      hh_lab2 <- "this season"
      hh_teams_selected <<- current_teams
    }else if (input$hh_season_range[1] == input$hh_season_range[2]) {
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

    if (!is.null(input$hh_tA_in)&&!is.null(input$hh_tB_in)) {
      #browser()
      hh_team_dataA <- hh_plot_data_teams[which(hh_plot_data_teams$HomeTeam == input$hh_tA_in|hh_plot_data_teams$AwayTeam==input$hh_tA_in),]
      hh_team_dataA1 <- hh_plot_data_teams[which(hh_plot_data_teams$HomeTeam == input$hh_tA_in),c(hh1,"FTR","Date","HomeTeam","AwayTeam")]
      hh_team_dataA2 <- hh_plot_data_teams[which(hh_plot_data_teams$AwayTeam == input$hh_tA_in),c(hh2,"FTR","Date","HomeTeam","AwayTeam")]
      dimnames(hh_team_dataA1)[[2]] <- dimnames(hh_team_dataA2)[[2]] <- c("V1","FTR","Date","HomeTeam","AwayTeam")
      hh_team_dataA <- rbind(hh_team_dataA1,hh_team_dataA2)
      hh_team_dataA <- hh_team_dataA[order(hh_team_dataA$Date),]
      hh_team_dataB <- hh_plot_data_teams[which(hh_plot_data_teams$HomeTeam == input$hh_tB_in|hh_plot_data_teams$AwayTeam==input$hh_tB_in),]
      hh_team_dataB1 <- hh_plot_data_teams[which(hh_plot_data_teams$HomeTeam == input$hh_tB_in),c(hh1,"FTR","Date","HomeTeam","AwayTeam")]
      hh_team_dataB2 <- hh_plot_data_teams[which(hh_plot_data_teams$AwayTeam == input$hh_tB_in),c(hh2,"FTR","Date","HomeTeam","AwayTeam")]
      dimnames(hh_team_dataB1)[[2]] <- dimnames(hh_team_dataB2)[[2]] <- c("V1","FTR","Date","HomeTeam","AwayTeam")
      hh_team_dataB <- rbind(hh_team_dataB1, hh_team_dataB2)
      hh_team_dataB <- hh_team_dataB[order(hh_team_dataB$Date),]

      if (input$cumul_sum == TRUE) {
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







  # ------------ Custom plot, this gon be big!!!

  plot_data2 <- reactive({
    PL_plot_data(team_data = fulld,
                 date_from = input$season_range_c[1],
                 date_to = input$season_range_c[2],
                 x_stat = input$stat_choice_x,
                 y_stat = input$stat_choice_y,
                 teams = NA,
                 x_stat_by = input$stat_choice_x_per,
                 y_stat_by = input$stat_choice_y_per,
                 custom_boundary = FALSE)

  })



  output$plot_stats_custom2 <- renderPlotly({

    this_plot_data <- plot_data2()
    temp_plot_data <- data.frame(x = this_plot_data$x,
                                 y = this_plot_data$y,
                                 teams = this_plot_data$teams)

    main_title <- paste(this_plot_data$ylabels,this_plot_data$ylabels_per, "vs.",
                        this_plot_data$xlabels,this_plot_data$xlabels_per,
                        this_plot_data$ylabels_per,this_plot_data$main_label)

    plot_ly(data = temp_plot_data, x = ~x, y = ~y, #color = teams, colors=team_colours,
            text = ~teams,
            hooverinfo = "text",
            type = 'scatter', mode = 'markers',
            marker = list(color = team_cols[this_plot_data$teams,c("col1_rgb")],
                        line = list(color=team_cols[this_plot_data$teams,c("col2_rgb")], width = 2))
    )  %>%  # text = ~paste0("(",this_plot_data$xlabels,", ",this_plot_data$ylabels,")"))
      layout(title = main_title,
             xaxis = list(
               range = c(this_plot_data$xlim[1] - (0.05*abs(this_plot_data$xlim[1] - this_plot_data$xlim[2])),
                         this_plot_data$xlim[2] + (0.05*abs(this_plot_data$xlim[1] - this_plot_data$xlim[2]))),
               title = paste(this_plot_data$xlabels,this_plot_data$xlabels_per)
             ),
             yaxis = list(
               range = c(this_plot_data$ylim[1] - (0.05*abs(this_plot_data$ylim[1] - this_plot_data$ylim[2])),
                         this_plot_data$ylim[2] + (0.05*abs(this_plot_data$ylim[1] - this_plot_data$ylim[2]))),
               title = paste(this_plot_data$ylabels,this_plot_data$ylabels_per)
             )
      ) %>%
      layout(plot_bgcolor = 'rgba(0,0,0,0)') %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)') %>%
      #add_text(textposition = "top right")
      add_annotations(showarrow = F, xanchor = 'left', yanchor = "top") %>%
      config(displayModeBar = F)


  })




  # ----------------- Fantasy players table --------------------------------------

  output$dt_field_choices <- renderUI({
    isolate({
      choices <- available_fields
      if(!is.null(input$dt_fields)){
        selected <- input$dt_fields
      }else{
        selected = c("Name","Photo","Team","Cost")
      }
      selectizeInput("dt_fields", label="Choose Columns", choices= choices, selected=selected, multiple=T, options=list(plugins=list('remove_button', 'drag_drop')))
    })
  })


  output$dt_data_display <- DT::renderDataTable(DT::datatable({
    if(!is.null(input$dt_fields)&&length(input$dt_fields)>1){
      #browser()
      player_data[,input$dt_fields]
    }
  },rownames = FALSE, escape=1, filter='top',options=list(dom='lrtip')))













  # Sidebar options

  output$field1<-renderUI({
    # Reactive input displaying possible fields
    selectInput("field_choice_1",
                label = h5("Field 1"),
                choices = as.list(available_fields),
                selected = "Photo")
  })
  output$field2<-renderUI({
    # Reactive input displaying possible fields
    selectInput("field_choice_2",
                label = h5("Field 2"),
                choices = as.list(available_fields),
                selected = "Total Points")
  })
  output$field3<-renderUI({
    # Reactive input displaying possible fields
    selectInput("field_choice_3",
                label = h5("Field 3"),
                choices = as.list(available_fields),
                selected = "Value Form")
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
      return(player_data[,c("Name", "Team", "Status" ,"Cost")])

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
                                c("Name","Photo", "Team" ,"Cost",
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



