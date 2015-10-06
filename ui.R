library(shiny)


# Define UI for application to display data
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Premier League 15/16"),
  
  
  # Sidebar ---------------------------------------------------------------------------------------
      navlistPanel(widths =c(2, 10),
                   
                   tabPanel("Fixtures/Results",
                            tabsetPanel(
                              tabPanel("Gameweek",
                                       fluidRow(
                                         column(width = 6, offset = 0 ,
                                                uiOutput("gameweek_choice"),
                                                tableOutput("fix_res")
                                         ),
                                         column(6,
                                                #uiOutput("gameweek_hist_choice"),
                                                #h3("Historical Data"),
                                                #fluidRow(
                                                  #column(8,
                                                         uiOutput("game_hist_choice"),
                                                         checkboxInput("return_leg", label = "Return Fixture", value = FALSE),
                                                  #),
                                                  #column(4,
                                                  #       checkboxInput("return_leg", label = "Return Leg", value = FALSE)
                                                  #)
                                                #),
                                                tableOutput("historical_result")
                                                #h3("League Table"),
                                                #tableOutput("personal_table")
                                         )
                                       )
                              ),
                              tabPanel("Statistics",
                                       fluidRow(
                                         column(4,
                                                selectInput("stat_choice", label = h4("Choose stat"), 
                                                            choices = list("Goals" = "goals", 
                                                                           "Shots on target" = "starget",
                                                                           "Shots" = "shots",
                                                                           "Goals per shot on target" = "gperst",
                                                                           "Goals per shot" = "gpers",
                                                                           "Corners" = "corners",
                                                                           "Fouls" = "fouls",
                                                                           "Halftime Goals" = "halfgoals"), selected = "goals")),
                                         column(5,
                                                #h4("Time range"),
                                                #checkboxInput("this_season", label = "All seasons", value = FALSE),
                                                #sliderInput("season_range", label= h4("Season"),
                                                #            min = 2000, max = 2016, value = c(2015, 2016),step=1, sep=""),
                                                dateRangeInput("season_range2", label= h4("Date range"),
                                                      format = "dd-mm-yyyy", start = "2015-08-08", end = Sys.Date(), min="2000-08-09")
                                         ),
                                         column(3,
                                                radioButtons("sum_tot", label = h4("Summary variable"),
                                                             choices = list("Average" = "avg", 
                                                                            "Total" = "tot"),selected = "avg")
                                         )),
                                       plotOutput("plot_stats", click = "stat_plot_click"),
                                       textOutput("info")
                              ),
                              tabPanel("Head to head",
                                       fluidRow(
                                         column(2,
                                                selectInput("hh_stat_choice", label = h4("Statistic"), 
                                                            choices = list("Goals" = "goals", 
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
                                                uiOutput("hh_teamA")
                                         ),
                                         column(3,
                                                uiOutput("hh_teamB")
                                         ),
                                         column(4,
                                                #h4("Time range"),
                                                #checkboxInput("this_season", label = "All seasons", value = FALSE),
                                                #sliderInput("season_range", label= h4("Season"),
                                                #            min = 2000, max = 2016, value = c(2015, 2016),step=1, sep=""),
                                                dateRangeInput("hh_season_range", label= h4("Date range"),
                                                               format = "dd-mm-yyyy", start = "2015-08-08", end = Sys.Date(), min="2000-08-09")
                                         )
                                         ),
                                       plotOutput("plot_hh")
                                       
                              )
                            )
                   ),
                   
        tabPanel("Fantasy League",
                 tabsetPanel(
                   tabPanel("Standings",
                            fluidRow(
                              column(width = 4,
                                     h3("Monthly Gameweeks"),
                                     p("The following gameweeks will be used to decide the monthly prizes"),
                                     tableOutput("MonthGW")),
                              column(8,
                                     h3("Fantasy Table"),
                                     fluidRow(
                                       column(3,
                                              uiOutput("table_monthly_choice")),
                                       column(3,
                                              uiOutput("table_gameweek_choice"))
                                     ),
                                     tableOutput("manager_current_stand_monthly"))
                            )),
                 tabPanel("Summary",
                          #p("This takes the data straight from the web, once I get it set up properly it should run the monthly leagues automatically."),
                          
                          sidebarLayout(
                            sidebarPanel(
                                   h4("League code: 1693603-401525"),
                                   p("The current plan is a pay-in of 15 each. The person with most points within each month will get 10 and the overall
                                     winner will get the left over money. 6 players will give enough money for the monthly prizes. Some gameweeks
                                     occuring at the beginning/end of a month will span two months. The month a gameweek starts in will be the
                                     month that the gameweek counts in. A list of the months and repective gameweeks is displayed on the right."),
                                   p(textOutput("n_managers"))
                            ),
                            mainPanel(
                              h3("Fantasy Table (Official)"),
                              tableOutput("personal_table2")
                          )
                 )),
                 tabPanel("Graphs",
                          fluidRow(
                            column(width = 2, offset = 1,
                                   uiOutput("plot_data_type")),
                            column(4,
                                   uiOutput("plot_gw_range")),
                            column(2,
                                 uiOutput("graph_monthly_choice")),
                            column(2,
                                   h5("Reactive Boundaries"),
                                   checkboxInput("reactive_lim","Yes", value = FALSE)
                                   #checkboxInput("animate","Animate", value = FALSE)
                                   
                                   )
                          ),
                          plotOutput("points_plot")
                 ),
                 tabPanel("Compare Teams",
                          fluidRow(
                            column(width = 2, offset = 1,
                                    uiOutput("manager_choice1"),
                                    tableOutput("manager_team1")
                            ),
                            column(2,
                                  uiOutput("manager_choice2"),
                                  tableOutput("manager_team2")
                                  ),
                            column(2,
                                   uiOutput("manager_choice3"),
                                   tableOutput("manager_team3")
                            ),
                            column(2,
                                   uiOutput("manager_choice4"),
                                   tableOutput("manager_team4")
                            )
                          )
                 )
        )),
        
        
        tabPanel("Player Data",
                 sidebarLayout(
                   sidebarPanel(h3("Player Data Controls"),
                          h4("Choose fields to display"),
                          uiOutput("field1"),
                          uiOutput("field2"),
                          uiOutput("field3"),
                          uiOutput("field4"),
                          uiOutput("sort_field")),
                   mainPanel(
                 fluidRow(
                   column(3, uiOutput("team_choice")),
                   column(3, uiOutput("position_choice")),
                   column(5, uiOutput("cost_choice"))),
                 tableOutput("data_display"))
                 )
        )
      
    
)))
