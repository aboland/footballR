# Fantasy Football server script

library(shiny)
library(plyr)
library(XML)

#library(shinyapps)
#shinyapps::deployApp('/Users/aidanboland/Google Drive/Fantasy Football/FF15-16')

#load("current_web_data.RData")
load("current_web_data_tidy.RData")

managers_id <- data.frame(names = c("Aidan", "Wes", "Sean", "Garry", "Tristan", "Craig", "Keith"),
                           ids = c(1693603, 1710052, 1748757, 1904476, 304705, 2176015, 509881))
league_id <- 401525
page_tables<-readHTMLTable(paste0("http://fantasy.premierleague.com/my-leagues/",league_id,"/standings/"))
managers <- as.character(page_tables[[1]]$Manager[order(as.character(page_tables[[1]]$Manager))])
team_names <- as.character(page_tables[[1]]$Team[order(as.character(page_tables[[1]]$Manager))])

ids <- noid <- NULL
for(i in 1:length(managers)){
  if(sum(strsplit(managers[i],split=" ")[[1]][1] == managers_id$names)!=0){  # Check for a valid id number
    ids <- c(ids, managers_id$ids[which(strsplit(managers[i],split=" ")[[1]][1] == managers_id$names)])
  }else{
    noid <- c(noid,i)  # Mark manager without ID
  }
}
if(length(noid)>0)
  managers <- managers[-noid]  # Remove managers without an id

monthly_weeks <- data.frame(Month = c("August","September", "October", "November", "December", "January", "February", "March", "April"),
                            Gameweeks = c("1 2 3 4", "5 6 7", "8 9 10 11", "12 13 14", "15 16 17 18 19", "21 22 23",
                                          "24 25 26 27", "28 29 30 31", "32 33 34 35 36"))

shinyServer(function(input, output) {
  
  gameweek <- nrow(readHTMLTable("http://fantasy.premierleague.com/entry/1693603/history/", stringsAsFactors=F)[[1]])

  # Side panel shite talk ------------------------
  output$n_managers<- renderPrint(cat(paste0("We currently have ",nrow(page_tables[[1]]))," managers."))
  output$pricing<- renderPrint(cat(paste0("Putting in 15 each gives a pot of ",15*nrow(page_tables[[2]])),"."))
  #output$n_players<- renderPrint(cat(nrow(page_tables[[2]])))
  
  # Main Panel -------------------------------------------------------------
  
  
  # ------------------ Our table
  
  output$personal_table <- renderTable({
    #page_tables[[1]]  # Table 1 will give standing!
    page_tables[[2]]  # Table 2 gives people in the league
  },include.rownames=F)
  output$personal_table2 <- renderTable({
    page_tables[[1]][,-1]  # Table 1 will give standing!
  }, include.rownames=F)
  
  
  
    
  output$MonthGW <- renderTable({
    #output_table <- monthly_weeks
    data.frame(Month = c("August","September", "October", "November", "December", "January", "February", "March", "April"),
             Gameweeks = c("1, 2, 3, 4", "5, 6, 7", "8, 9, 10, 11", "12, 13, 14", "15, 16, 17, 18, 19", "21, 22, 23",
                           "24, 25, 26 27", "28, 29, 30, 31", "32, 33, 34, 35, 36"),
             Winner = c("Tristan","","","","","","","",""))
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
  
  
  # ---------------------------------------------------
  # ---  Choice for managers table gameweek
  
  #monthly_numeric <- lapply(sapply(as.character(monthly_weeks[[2]]),strsplit,split=" "),as.numeric)
  #months_current <- as.character(monthly_weeks[[1]][sapply(monthly_numeric,function(x)sum(which(x==gameweek)))!=0])
  
  monthly_numeric <- lapply(sapply(as.character(monthly_weeks$Gameweeks),strsplit,split=" "),as.numeric)
  months_current <- as.character(monthly_weeks$Month[sapply(monthly_numeric,function(x)sum(which(x==gameweek)))!=0])
  
  output$table_monthly_choice<-renderUI({
    selectInput("table_month", 
                label = h4("Month"),
                choices = as.list(c("All",as.character(monthly_weeks[[1]]))),
                selected = months_current)
  })
  
  output$table_gameweek_choice<-renderUI({
    if(is.null(input$table_month)||input$table_month=="All"){
      weeks_avail = as.list(c("All",1:gameweek))
    }else{
      monthly_weeks_table <- lapply(sapply(as.character(monthly_weeks[[2]][which(monthly_weeks[[1]]==input$table_month)]),strsplit,split=" "),as.numeric)[[1]]
      weeks_avail = as.list(c("All", monthly_weeks_table))
    }
    
    selectInput("table_gw", 
                label = h4("Week"),
                choices = weeks_avail,
                selected = weeks_avail[[1]])
  })
  
  
  
  # -----------------------------------------  Monthly Code
  # ---  Monthly total!! Choice for managers table
  
  
  output$manager_current_stand_monthly <- renderTable({
    # Create data frame of points
    own_league_table_monthly <- data.frame(Team = team_names, Manager = managers, Total = rep(0, length(managers)), Bench = rep(0,length(managers)), Transfers = rep(0,length(managers)), 
                                           TransferCost = rep(0,length(managers)), TeamValue = rep(0,length(managers)))
    
    if(is.null(input$table_month))
      return(own_league_table_monthly)
    
    if(input$table_month == "All"){
      if(is.null(input$table_gw) || input$table_gw == "All"){
        weeks <- 1:gameweek
      }else{
        weeks <- input$table_gw
      }
      
        for(i in 1:length(managers)){
          own_league_table_monthly[i,"TeamValue"] <- manager_data_history[[i]][[1]][weeks[length(weeks)], c("TV")]
          for(j in weeks){
            own_league_table_monthly[i,c("Total", "Bench", "Transfers", "TransferCost")] <- own_league_table_monthly[i,c("Total", "Bench", "Transfers", "TransferCost")] + as.numeric(manager_data_history[[i]][[1]][j, c("GP","PB","TM","TC")])
            own_league_table_monthly[i,"Total"] <- own_league_table_monthly[i,"Total"] - as.numeric(manager_data_history[[i]][[1]][j, c("TC")])
            
          }
        }
          return(own_league_table_monthly[order(as.numeric(own_league_table_monthly[,"Total"]), decreasing = T),])
    }
    
    
    if(input$table_gw == "All"){
      weeks <- lapply(sapply(as.character(monthly_weeks[[2]][which(monthly_weeks[[1]]==input$table_month)]),strsplit,split=" "),as.numeric)[[1]]
    }else{
      weeks <- input$table_gw
    }
     lapply(sapply(as.character(monthly_weeks[[2]][which(monthly_weeks[[1]]==input$table_month)]),strsplit,split=" "),as.numeric)[[1]]
    if(weeks[length(weeks)] > gameweek){
      if(weeks[1] <= gameweek){
        weeks <- weeks[1]:gameweek
      }else
        return(own_league_table_monthly)
    }

    for(i in 1:length(managers)){
      own_league_table_monthly[i,"TeamValue"] <- manager_data_history[[i]][[1]][weeks[length(weeks)], c("TV")]
      for(j in weeks){
        own_league_table_monthly[i,c("Total", "Bench", "Transfers", "TransferCost")] <- own_league_table_monthly[i,c("Total", "Bench", "Transfers", "TransferCost")] + as.numeric(manager_data_history[[i]][[1]][j, c("GP","PB","TM","TC")])
        own_league_table_monthly[i,"Total"] <- own_league_table_monthly[i,"Total"] - as.numeric(manager_data_history[[i]][[1]][j, c("TC")])
      }
    }
    return(own_league_table_monthly[order(as.numeric(own_league_table_monthly[,"Total"]), decreasing = T),])
    
  }, include.rownames=F, digits=0)
  
  # --------------------------------------------------------------------
  # -----Create plot of results
  
  output$plot_data_type<-renderUI({
    selectInput("data_dis", 
                label = h5("Data"),
                choices = as.list(c("Overall", "Gameweek", "Bench", "Transfers")),# ,"Team Value")),
                selected = "Overall")
  })
  
  
  output$graph_monthly_choice<-renderUI({
    selectInput("plot_month", 
                label = h5("Month"),
                choices = as.list(c("All",as.character(monthly_weeks[[1]]))),
                selected = "All")
  })
  
  output$plot_gw_range<-renderUI({
    if(is.null(input$plot_month)||input$plot_month=="All"){
      weeks_avail = 1:gameweek
    }else{
      if(input$plot_month=="All"){
        weeks_avail = 1:gameweek
      }else{
      monthly_weeks_plot <- lapply(sapply(as.character(monthly_weeks[[2]][which(monthly_weeks[[1]]==input$plot_month)]),strsplit,split=" "),as.numeric)[[1]]
      weeks_avail = monthly_weeks_plot
      }
    }
      if(weeks_avail[1] > gameweek){
        week_min <- 1
      }else{
        week_min <- weeks_avail[1]
      }
      if(weeks_avail[length(weeks_avail)] > gameweek){
        week_max <- gameweek
      }else{
        week_max <- weeks_avail[length(weeks_avail)]
      }
    if(week_min==week_max){
      sliderInput("graph_range", 
                  label = h5("Gameweeks"),
                  min = 1, max = week_max,
                  value = c(week_min,week_max), step=1)
    }else{
      sliderInput("graph_range", 
                  label = h5("Gameweeks"),
                  min = week_min, max = week_max,
                  value = c(week_min,week_max), step=1)
    }
  })
  
#   output$plot_gw_range<-renderUI({
#     sliderInput("graph_range", 
#                 label = h5("Gameweeks"),
#                 min = 1, max = gameweek,
#                 value = c(1,gameweek), step=1)
#   })
  
  data_plot_choice <- reactive({
    if(is.null(input$data_dis))
      return("OP")
    if(input$data_dis == "Overall")
      return("OP")
    if(input$data_dis == "Gameweek")
      return("GP")
    if(input$data_dis == "Bench")
      return("PB")
    if(input$data_dis == "Transfers")
      return("TM")
    ##if(input$data_dis == "Team Value")
    ##  return("TV")
  })
  
  output$points_plot <- renderPlot({
    gw_min <- 1
    gw_max <- gameweek
      
    if(!is.null(input$graph_range[1]))
      gw_min<-input$graph_range[1]
    if(!is.null(input$graph_range[2]))
      gw_max<-input$graph_range[2]

    
    if(!is.null(current_stand_plot())){
    my_ylim <- range(as.numeric(current_stand_plot()[[1]][[1]][,data_plot_choice()]))
    for(i in 2:length(managers)){
      my_ylim[1] <- ifelse(min(as.numeric(current_stand_plot()[[i]][[1]][gw_min:gw_max,data_plot_choice()])) < my_ylim[1],
                           min(as.numeric(current_stand_plot()[[i]][[1]][gw_min:gw_max,data_plot_choice()])),
                           my_ylim[1])
      my_ylim[2] <- ifelse(max(as.numeric(current_stand_plot()[[i]][[1]][gw_min:gw_max,data_plot_choice()])) > my_ylim[2],
                           max(as.numeric(current_stand_plot()[[i]][[1]][gw_min:gw_max,data_plot_choice()])),
                           my_ylim[2])
    }
    plot(gw_min:gw_max, current_stand_plot()[[1]][[1]][gw_min:gw_max, data_plot_choice()], type="n", 
         ylim= my_ylim, ylab = "Points", xlab="Gameweek", xaxt="n",
         main=input$data_dis)
    #axis(1,at=1:nrow(current_stand_plot()[[1]][[1]]),labels=gw_min:gw_max)
    axis(1,at=gw_min:gw_max,labels=gw_min:gw_max)
    for(i in 1:length(managers))
      lines(gw_min:gw_max, current_stand_plot()[[i]][[1]][gw_min:gw_max,data_plot_choice()], type="b", col = i)
    legend("topleft", managers, col=1:length(managers), lty=1)
    }else{
      plot(c(0,0), type="n", 
           ylim= c(1,100),xlim=c(1,gameweek), ylab = "Points", xlab="Gameweek", xaxt="n",
           main=input$data_dis)
    }
  })
    
  
  
  # Table for plot!!!!
  current_stand_plot <- reactive({
    
    manager_data_history2 <- manager_data_history
    ##if(data_plot_choice() == "TV")
    ##  for(i in 1:length(managers))
    ##    manager_data_history2[[i]][[1]][,"TV"] <- as.numeric(gsub("(removed pound symbol)|m","",manager_data_history2[[i]][[1]][,"TV"]))
      
    if(is.null(input$plot_month)||input$plot_month=="All"){
      return(manager_data_history2)
    }else{
      if(input$plot_month=="All"){
        return(manager_data_history2)
      }else if(as.numeric(strsplit(as.character(monthly_weeks[[2]][which(monthly_weeks[[1]]==input$plot_month)]),split=" ")[[1]][1]) > gameweek){
        return(manager_data_history2)
      }else{
        prev_month <- as.numeric(strsplit(as.character(monthly_weeks[[2]][which(monthly_weeks[[1]]==input$plot_month)]),split=" ")[[1]][1]) - 1
        if(prev_month == 0){
          return(manager_data_history2)
        }else{
          #browser()
        for(i in 1:length(managers))
          manager_data_history2[[i]][[1]][,"OP"] <- as.numeric(manager_data_history2[[i]][[1]][,"OP"]) - as.numeric(manager_data_history2[[i]][[1]][prev_month,"OP"])
        return(manager_data_history2)
        }
      }
    }
  })
  
  
  
  
  
  # ---- Display and compare current teams ---------------------------------------------
  
  
  
  managers_sample<- sample(managers,4)
  
  output$manager_choice1<-renderUI({
    # Reactive input displaying possible managers
    selectInput("manager_ch1", 
                label = h4("Manager 1"),
                choices = as.list(managers),
                selected = managers_sample[1])
  })
  
  output$manager_choice2<-renderUI({
    # Reactive input displaying possible managers
    selectInput("manager_ch2", 
                label = h4("Manager 2"),
                choices = as.list(managers),
                selected = managers_sample[2])
  })
  
  output$manager_choice3<-renderUI({
    # Reactive input displaying possible managers
    selectInput("manager_ch3", 
                label = h4("Manager 3"),
                choices = as.list(managers),
                selected = managers_sample[3])
  })
  
  output$manager_choice4<-renderUI({
    # Reactive input displaying possible managers
    selectInput("manager_ch4", 
                label = h4("Manager 4"),
                choices = as.list(managers),
                selected = managers_sample[4])
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
    
    temp_id <- NULL
    browser()
    for(i in 65:79){
      managers_selected[i-64,1] <- names(manager_team_history[[p_ch]][[i]])
      temp_id[i-64] <- which(player_data[,"Name"]==gsub("^\\s+|\\s+$","",names(manager_team_history[[p_ch]][[i]])))
      #managers_selected[i-64,2] <- gsub(" \n\n ", "",as.character(manager_team_history[[p_ch]][[i]]))
      managers_selected[i-64,2] <- player_data[temp_id[i-64],"GW points"]
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
    
  output$manager_team4 <- renderTable({
    managers_selected <- data.frame(Names=rep(" ",15),Points=rep(" ",15),stringsAsFactors = F)
    
    if(is.null(input$manager_ch4))
      return(managers_selected)
    if(sum(managers==input$manager_ch4)==0)
      return(data.frame("No history available"=c(" ")))
    
    p_ch <- which(managers==input$manager_ch4)
    if(input$manager_ch4=="All")
      p_ch <- 1
    if(input$manager_ch4=="All")
      p_ch <- 1
    
    for(i in 65:79){
      managers_selected[i-64,1] <- names(manager_team_history[[p_ch]][[i]])
      managers_selected[i-64,2] <- gsub(" \n\n ", "",as.character(manager_team_history[[p_ch]][[i]]))
    }
    managers_selected
  },include.rownames=F)
  
  
  
  
  # ------------------ Fixture and results tables -------------------------------------------
  
  
  
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
      return("id")
    input$player_data_sort
  })
  
  output$sort_field<-renderUI({
    # Reactive input displaying possible fields
    selectInput("player_data_sort", 
                label = h4("Sort by"),
                choices = list("id", "Cost",
                               isolate(input$field_choice_1), 
                               input$field_choice_2, 
                               input$field_choice_3, 
                               input$field_choice_4),
                selected = sort_choice())
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
    
    if(!is.null(input$player_data_sort))
      if(input$player_data_sort!="id")
        return(output_table[order(output_table[,input$player_data_sort],decreasing = T),])
    return(output_table)
    
  })
  
  })



