library(shiny)
library(ggvis)


# Define UI for application to display data
shinyUI(fluidPage(#theme="bootstrap.css",
  
  # Application title
  titlePanel("Premier League 16/17"),
  
  
  # Sidebar ---------------------------------------------------------------------------------------
      navlistPanel(widths =c(2, 10),
                   
                   tabPanel("Fixtures/Results",
                            tabsetPanel(
                              # tabPanel("Gameweek",
                              #          fluidRow(
                              #            column(width = 6, offset = 0 ,
                              #                   #uiOutput("gameweek_choice"),
                              #                   selectInput("gw_choice", "Gameweek",choices = as.list(1:38),selected = 18),
                              #                   tableOutput("fix_res")
                              #            ),
                              #            column(6,
                              #                   uiOutput("game_hist_choice"),
                              #                   checkboxInput("return_leg", label = "Return Fixture", value = FALSE),
                              #                   
                              #                   tableOutput("historical_result")
                              #                   #h3("League Table"),
                              #                   #tableOutput("personal_table")
                              #            )
                              #          )
                              # ),
                              
                              tabPanel("Statistics",
                                       fluidRow(
                                         column(3,
                                                selectInput("stat_choice_y", label = h4("Stat 1"), 
                                                            choices = list("Goals scored" = "goals",
                                                                           "Goals conceded" = "goals_conc",
                                                                           "Shots on target" = "starget",
                                                                           "Shots" = "shots",
                                                                           "Corners" = "corners",
                                                                           "Fouls" = "fouls"), selected = "goals"),
                                                selectInput("stat_choice_y_per", label = NULL,# h4("Choose x by"), 
                                                            choices = list("By" = "no_div",
                                                                           "Per game" = "p_game",
                                                                           "Per goal" = "p_goal",
                                                                           "Per goal conceded" = "p_goal_conc",
                                                                           "At home" = "p_home",
                                                                           "Away" = "p_away",
                                                                           "Per shot" = "p_shot",
                                                                           "Per shot on target" = "p_shot_t",
                                                                           "Per shot faced" = "p_shot_f",
                                                                           "Per corner" = "p_corner",
                                                                           "Per corner faced" = "p_corner_f",
                                                                           "Per foul" = "p_foul"), selected = "no_div")),
                                         column(3,
                                                selectInput("stat_choice_x", label = h4("Stat 2"), 
                                                            choices = list("Goals scored" = "goals",
                                                                           "Goals conceded" = "goals_conc",
                                                                           "Shots on target" = "starget",
                                                                           "Shots" = "shots",
                                                                           "Corners" = "corners",
                                                                           "Fouls" = "fouls"), selected = "goals_conc"),
                                                selectInput("stat_choice_x_per", label = NULL,#h4("Choose y by"), 
                                                            choices = list("By" = "no_div",
                                                                           "Per game" = "p_game",
                                                                           "Per goal" = "p_goal",
                                                                           "Per goal conceded" = "p_goal_conc",
                                                                           "At home" = "p_home",
                                                                           "Away" = "p_away",
                                                                           "Per shot" = "p_shot",
                                                                           "Per shot on target" = "p_shot_t",
                                                                           "Per shot faced" = "p_shot_f",
                                                                           "Per corner" = "p_corner",
                                                                           "Per corner faced" = "p_corner_f",
                                                                           "Per foul" = "p_foul"), selected = "no_div")),
                                         column(4,
                                                #h4("Time range"),
                                                #checkboxInput("this_season", label = "All seasons", value = FALSE),
                                                #sliderInput("season_range", label= h4("Season"),
                                                #            min = 2000, max = 2016, value = c(2015, 2016),step=1, sep=""),
                                                dateRangeInput("season_range_c", label= h4("Date range"),
                                                               format = "dd-mm-yyyy", 
                                                               start = "2016-08-13", 
                                                               #end = "2016-03-08",
                                                               end = Sys.Date()-1, 
                                                               min="2000-08-09"),
                                                checkboxInput("custom_boundaries","Fixed aspect", value = FALSE)
                                         )),
                                       plotOutput("plot_stats_custom", click = "custom_plot_click"),# , height="auto", width = "100%"),
                                       #plotOutput("plot_stats"),
                                       textOutput("info_cus")#,
                                       #ggvisOutput("myggplot")
                              ),
                              tabPanel("Head to head",
                                       fluidRow(
                                         column(2,
                                                selectInput("hh_stat_choice", label = h4("Statistic"), 
                                                            choices = list("Goals scored" = "goals",
                                                                           "Goals conceded" = "goals_conc",
                                                                           "Shots on target" = "starget",
                                                                           "Shots" = "shots",
                                                                           #"Goals per shot on target" = "gperst",
                                                                           #"Goals per shot" = "gpers",
                                                                           "Corners" = "corners",
                                                                           "Fouls" = "fouls",
                                                                           "Yellow cards" = "ycard",
                                                                           "Red cards" = "rcard",
                                                                           "Goals by halftime" = "halfgoals"), selected = "goals")),
                                         column(3,
                                                uiOutput("hh_teamA"),
                                                uiOutput("hh_teamB")
                                         ),
                                         #column(3,
                                         #       uiOutput("hh_teamB")
                                         #),
                                         column(4,
                                                #h4("Time range"),
                                                #checkboxInput("this_season", label = "All seasons", value = FALSE),
                                                #sliderInput("season_range", label= h4("Season"),
                                                #            min = 2000, max = 2016, value = c(2015, 2016),step=1, sep=""),
                                                dateRangeInput("hh_season_range", label= h4("Date range"),
                                                               format = "dd-mm-yyyy", start = "2016-08-13", end = Sys.Date()-1, min="2000-08-09"),
                                                checkboxInput("cumul_sum","Cumulative sum", value = TRUE)
                                         )
                                         ),
                                       plotOutput("plot_hh")
                                       
                              )
                            
                   )),
        
                   tabPanel("Player Data",
                            uiOutput("dt_field_choices"),
                            DT::dataTableOutput("dt_data_display")
                            
                   )#,
        # tabPanel("Player Data_old",
        #          sidebarLayout(
        #            sidebarPanel(h3("Player Data Controls"),
        #                   h4("Choose fields to display"),
        #                   uiOutput("field1"),
        #                   uiOutput("field2"),
        #                   uiOutput("field3"),
        #                   uiOutput("field4"),
        #                   uiOutput("sort_field")),
        #            mainPanel(
        #          fluidRow(
        #            column(3, uiOutput("team_choice")),
        #            column(3, uiOutput("position_choice")),
        #            column(5, uiOutput("cost_choice"))),
        #          tableOutput("data_display"))
        #          )
        # )
          
#         tabPanel("Odds tracker (beta)",
#          tabsetPanel(
#            tabPanel("Man City vs Everton",
#                     plotOutput("odds_plot_home"),
#                     plotOutput("odds_plot_away"),
#                     plotOutput("odds_plot_draw")
#            )
#          )
#         )
      )
))
