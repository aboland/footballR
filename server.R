# Fantasy Football server script

library(shiny)
library(plyr)
library(XML)

#library(shinyapps)
#shinyapps::deployApp('/Users/aidanboland/Google Drive/Fantasy Football/FF15-16')

#load("current_web_data.RData")
load("current_web_data_tidy.RData")

managers <- c("Aidan", "Wes", "Sean", "Garry", "Tristan", "Craig")
ids = c(1693603, 1710052, 1748757, 1904476, 304705, 2176015)
league_id <- 401525
monthly_weeks <- data.frame(Month = c("August","September", "October", "November", "December", "January", "February", "March", "April"),
                            Gameweeks = c("1 2 3 4", "5 6 7", "8 9 10 11", "12 13 14", "15 16 17 18 19", "21 22 23",
                                          "24 25 26 27", "28 29 30 31", "32 33 34 35 36"))

shinyServer(function(input, output) {
  
  gameweek <- nrow(readHTMLTable("http://fantasy.premierleague.com/entry/1693603/history/", stringsAsFactors=F)[[1]])
  
  #files_avail <- list.files("/Users/aidanboland/Google Drive/Fantasy Football/Data/")  # availiable data
  #load("/Users/aidanboland/Google Drive/Fantasy Football/Data/15_7_4_full.RData")
  
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
           return("id")
    input$player_data_sort
  })
  
  output$sort_field<-renderUI({
    # Reactive input displaying possible fields
    selectInput("player_data_sort", 
                label = h3("Sort by"),
                choices = list("id", "Cost",
                                  isolate(input$field_choice_1), 
                                  input$field_choice_2, 
                                  input$field_choice_3, 
                                  input$field_choice_4),
                selected = sort_choice())
  })
  
  # Side panel shite talk ------------------------
  output$n_managers<- renderPrint(cat(paste0("We currently have ",nrow(page_tables[[1]]))," managers."))
  output$pricing<- renderPrint(cat(paste0("Putting in 15 each gives a pot of ",15*nrow(page_tables[[2]])),"."))
  #output$n_players<- renderPrint(cat(nrow(page_tables[[2]])))
  
  # Main Panel -------------------------------------------------------------
  
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
    
    if(!is.null(input$player_data_sort))
      if(input$player_data_sort!="id")
        return(output_table[order(output_table[,input$player_data_sort],decreasing = T),])
    return(output_table)
          
  })
  
  # ------------------ Our table
  
  page_tables<-readHTMLTable(paste0("http://fantasy.premierleague.com/my-leagues/",league_id,"/standings/"))
  output$personal_table <- renderTable({
    #page_tables[[1]]  # Table 1 will give standing!
    page_tables[[2]]  # Table 2 gives people in the league
  },include.rownames=F)
  output$personal_table2 <- renderTable({
    page_tables[[1]][,-1]  # Table 1 will give standing!
  }, include.rownames=F)
  
  # ------------------ Gameweek tables
  
#   gw_now<-readHTMLTable("http://fantasy.premierleague.com/fixtures/")[[1]]
#   for(i in 1:6)
#     gw_now[,i] <- as.character(gw_now[,i])
#   gw_now[,4]<-apply(gw_now[,3:5],1,function(x)paste(x[1],x[2],x[3]))
#   dimnames(gw_now)[[2]] <- c("Date", "Home", " ", " ", " ", "Away") 
#   output$currentGameweek <- renderTable({
#     gw_now[,c(1,2,4,6)]
#   },include.rownames=F)
#   
#   
  output$gameweek_choice<-renderUI({
    selectInput("gw_choice", 
                label = h3("Gameweek"),
                choices = as.list(1:38),
                selected = gameweek)
    })
  
  output$fix_res <- renderTable({
    if(is.null(input$gw_choice)){
      gw <- data.frame("", "", "", "")  # data.frame(0, 0, 0, 0)
      dimnames(gw)[[2]] <- c("Date", "Home", " ", "Away")
      return(gw)
    }
    gw <- readHTMLTable(paste0("http://fantasy.premierleague.com/fixtures/",input$gw_choice,"/"))$ismFixtureTable
    gw <- gw[!is.na(gw[,2]),]
    gw[,4]<-apply(gw[,3:5],1,function(x)paste(x[1],x[2],x[3]))
    dimnames(gw)[[2]] <- c("Date", "Home", " ", " ", " ", "Away") 
    gw[,c(1,2,4,6)]
    },include.rownames=F)
  
    
  output$MonthGW <- renderTable({
    data.frame(Month = c("August","September", "October", "November", "December", "January", "February", "March", "April"),
             Gameweeks = c("1, 2, 3, 4", "5, 6, 7", "8, 9, 10, 11", "12, 13, 14", "15, 16, 17, 18, 19", "21, 22, 23",
                           "24, 25, 26 27", "28, 29, 30, 31", "32, 33, 34, 35, 36"))
  },include.rownames=F)
  
  
  # --- Scrape individual player points/history
  withProgress(message = 'Retrieving latest data', value = 0, {
  manager_data_history <- list()
  manager_team_history <- list()
  for(i in 1:length(managers)){
    incProgress(0, detail = paste(managers[i]))
    manager_data_history[[i]] <- readHTMLTable(paste0("http://fantasy.premierleague.com/entry/",ids[i],"/history/"), stringsAsFactors=F)
    manager_team_history[[i]] <- readHTMLList(paste0("http://fantasy.premierleague.com/entry/",ids[i],"/event-history/",gameweek,"/"), stringsAsFactors=F)
    incProgress(1/length(managers), detail = paste(managers[i]))
    }
  })
#   # Get current team choice
#   withProgress(message = 'Retrieving latest data', value = 0, {
#     incProgress(1/10, detail = paste("Aidan"))
#     Aidan_data <- readHTMLTable("http://fantasy.premierleague.com/entry/1693603/history/", stringsAsFactors=F)
#     Aidan_team <- readHTMLList(paste0("http://fantasy.premierleague.com/entry/1693603/event-history/",gameweek,"/"), stringsAsFactors=F)
#     incProgress(2/10, detail = paste("Wes"))
#     Wes_data <- readHTMLTable("http://fantasy.premierleague.com/entry/1710052/history/", stringsAsFactors=F)
#     Wes_team <- readHTMLList(paste0("http://fantasy.premierleague.com/entry/1710052/event-history/",gameweek,"/"), stringsAsFactors=F)
#     incProgress(2/10, detail = paste("Sean"))
#     Flynn_data <- readHTMLTable("http://fantasy.premierleague.com/entry/1748757/history/", stringsAsFactors=F)
#     Flynn_team <- readHTMLList(paste0("http://fantasy.premierleague.com/entry/1748757/event-history/",gameweek,"/"), stringsAsFactors=F)
#     incProgress(2/10, detail = paste("Garry"))
#     Gazza_data <- readHTMLTable("http://fantasy.premierleague.com/entry/1904476/history/", stringsAsFactors=F)
#     Gazza_team <- readHTMLList(paste0("http://fantasy.premierleague.com/entry/1904476/event-history/",gameweek,"/"), stringsAsFactors=F)
#     incProgress(1/10, detail = paste("Tristan"))
#     Tristan_data <- readHTMLTable("http://fantasy.premierleague.com/entry/304705/history/", stringsAsFactors=F)
#     Tristan_team <- readHTMLList(paste0("http://fantasy.premierleague.com/entry/304705/event-history/",gameweek,"/"), stringsAsFactors=F)
#     incProgress(1/10, detail = paste("Craig"))
#     Craig_data <- readHTMLTable("http://fantasy.premierleague.com/entry/2176015/history/", stringsAsFactors=F)
#     Craig_team <- readHTMLList(paste0("http://fantasy.premierleague.com/entry/2176015/event-history/",gameweek,"/"), stringsAsFactors=F)
#     incProgress(1/10, detail = paste("Combining"))
#     manager_team_history <- list(Aidan_team, Wes_team, Flynn_team, Gazza_team, Tristan_team, Craig_team)
#     manager_data_history <- list(Aidan_data, Wes_data, Flynn_data, Gazza_data, Tristan_data, Craig_data)
#   })
  
  
  
  
  # ---  Choice for managers table gameweek
  output$table_gameweek_choice<-renderUI({
    selectInput("table_gw", 
                label = h3("Week"),
                choices = as.list(1:gameweek),
                selected = gameweek)
  })
  
  # Create data frame of points
  own_league_table <- data.frame(Manager = managers, Total = rep(0, length(managers)), Gameweek = rep(0, length(managers)), Bench = rep(0,length(managers)), Transfers = rep(0,length(managers)))
  
  output$manager_current_stand <- renderTable({
    if(is.null(input$table_gw))
      return(own_league_table)
    for(i in 1:6)
      own_league_table[i,2:5] <- manager_data_history[[i]][[1]][input$table_gw, c("OP","GP","PB","TM")]
    own_league_table[order(as.numeric(own_league_table[,3]), decreasing = T),]
  }, include.rownames=F)
  
  # -----------------------------------------  Monthly Code
  # ---  Monthly total!! Choice for managers table
  
  monthly_numeric <- lapply(sapply(as.character(monthly_weeks[[2]]),strsplit,split=" "),as.numeric)
  
  months_current <- as.character(monthly_weeks[[1]][sapply(monthly_numeric,function(x)sum(which(x==gameweek)))!=0])
  
  output$table_monthly_choice<-renderUI({
    selectInput("table_month", 
                label = h3("Month"),
                choices = as.list(as.character(monthly_weeks[[1]])),
                selected = months_current)
  })
  
  #month_ch <- sapply(monthly_numeric,function(x)sum(which(x==1)))
  
  # Create data frame of points
  own_league_table_monthly <- data.frame(Manager = managers, Total = rep(0, length(managers)), Bench = rep(0,length(managers)), Transfers = rep(0,length(managers)))
  
  output$manager_current_stand_monthly <- renderTable({
    if(is.null(input$table_month))
      return(own_league_table_monthly)
    
    weeks <- lapply(sapply(as.character(monthly_weeks[[2]][which(monthly_weeks[[1]]==input$table_month)]),strsplit,split=" "),as.numeric)[[1]]
   # browser()
    if(weeks[length(weeks)] > gameweek){
      if(weeks[1] <= gameweek){
        weeks <- weeks[1]:gameweek
      }else
        return(own_league_table_monthly)
    }
    
    for(j in weeks)
    for(i in 1:6)
      own_league_table_monthly[i,2:4] <- own_league_table_monthly[i,2:4] + as.numeric(manager_data_history[[i]][[1]][j, c("GP","PB","TM")])
    
    own_league_table_monthly[order(as.numeric(own_league_table_monthly[,2]), decreasing = T),]
    
  }, include.rownames=F, digits=0)
  
  
  # -----Create plot of results
  
  output$plot_data_type<-renderUI({
    selectInput("data_dis", 
                label = h5("Data"),
                choices = as.list(c("Overall","Gameweek","Bench")),
                selected = "Overall")
  })
  
  output$plot_gw_range<-renderUI({
    sliderInput("graph_range", 
                label = h5("Gameweeks"),
                min = 1, max = gameweek,
                value = c(1,gameweek), step=1)
  })
  
  data_plot_choice <- reactive({
    if(is.null(input$data_dis))
      return("OP")
    if(input$data_dis == "Overall")
      return("OP")
    if(input$data_dis == "Gameweek")
      return("GP")
    if(input$data_dis == "Bench")
      return("PB")
  })
  
  output$points_plot <- renderPlot({
    
    my_ylim <- range(as.numeric(manager_data_history[[1]][[1]][,data_plot_choice()]))
    for(i in 2:6){
      my_ylim[1] <- ifelse(min(as.numeric(manager_data_history[[i]][[1]][,data_plot_choice()])) < my_ylim[1],
                        min(as.numeric(manager_data_history[[i]][[1]][,data_plot_choice()])),
                        my_ylim[1])
      my_ylim[2] <- ifelse(max(as.numeric(manager_data_history[[i]][[1]][,data_plot_choice()])) > my_ylim[2],
                        max(as.numeric(manager_data_history[[i]][[1]][,data_plot_choice()])),
                        my_ylim[2])
    }
    plot(manager_data_history[[1]][[1]][,data_plot_choice()], type="n", 
         ylim= my_ylim, ylab = "Points", xlab="Gameweek", xaxt="n",
         main=input$data_dis)
    axis(1,at=1:nrow(manager_data_history[[1]][[1]]))
    for(i in 1:6)
      lines(manager_data_history[[i]][[1]][,data_plot_choice()], type="b", col = i)
    legend("topleft", managers, col=1:length(managers), lty=1)
  })
    
  
  
  
  # ---- Display current teams
  
  output$manager_choice1<-renderUI({
    # Reactive input displaying possible managers
    selectInput("manager_ch1", 
                label = h4("Manager 1"),
                choices = as.list(managers),
                selected = sample(managers,1))
  })
  
  output$manager_choice2<-renderUI({
    # Reactive input displaying possible managers
    selectInput("manager_ch2", 
                label = h4("Manager 2"),
                choices = as.list(managers),
                selected = sample(managers,1))
  })
  
  output$manager_choice3<-renderUI({
    # Reactive input displaying possible managers
    selectInput("manager_ch3", 
                label = h4("Manager 3"),
                choices = as.list(managers),
                selected = sample(managers,1))
  })
  
  output$manager_team1 <- renderTable({
    managers_selected <- data.frame(Names=rep(" ",15),Points=rep(" ",15),stringsAsFactors = F)
    
    if(is.null(input$manager_ch1))
      return(managers_selected)
    if(sum(managers==input$manager_ch1)==0)
      return(data.frame("No history available"=c(" ")))
    
    p_ch <- which(managers==input$manager_ch1)
    if(input$manager_ch1=="All")
      p_ch <- 1
    if(input$manager_ch1=="All")
      p_ch <- 1
    
    for(i in 65:79){
      managers_selected[i-64,1] <- names(manager_team_history[[p_ch]][[i]])
      managers_selected[i-64,2] <- gsub(" \n\n ", "",as.character(manager_team_history[[p_ch]][[i]]))
    }
    managers_selected
  },include.rownames=F)
  
  output$manager_team2 <- renderTable({
    managers_selected <- data.frame(Names=rep(" ",15),Points=rep(" ",15),stringsAsFactors = F)
    
    if(is.null(input$manager_ch2))
      return(managers_selected)
    if(sum(managers==input$manager_ch2)==0)
      return(data.frame("No history available"=c(" ")))
    
    p_ch <- which(managers==input$manager_ch2)
    if(input$manager_ch2=="All")
      p_ch <- 1
    if(input$manager_ch2=="All")
      p_ch <- 1
    
    for(i in 65:79){
      managers_selected[i-64,1] <- names(manager_team_history[[p_ch]][[i]])
      managers_selected[i-64,2] <- gsub(" \n\n ", "",as.character(manager_team_history[[p_ch]][[i]]))
    }
    managers_selected
  },include.rownames=F)
  
  output$manager_team3 <- renderTable({
    managers_selected <- data.frame(Names=rep(" ",15),Points=rep(" ",15),stringsAsFactors = F)
    
    if(is.null(input$manager_ch3))
      return(managers_selected)
    if(sum(managers==input$manager_ch3)==0)
      return(data.frame("No history available"=c(" ")))
    
    p_ch <- which(managers==input$manager_ch3)
    if(input$manager_ch3=="All")
      p_ch <- 1
    if(input$manager_ch3=="All")
      p_ch <- 1
    
    for(i in 65:79){
      managers_selected[i-64,1] <- names(manager_team_history[[p_ch]][[i]])
      managers_selected[i-64,2] <- gsub(" \n\n ", "",as.character(manager_team_history[[p_ch]][[i]]))
    }
    managers_selected
  },include.rownames=F)
    
  })



