library(shiny)


# Define UI for application to display data
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Fantasy Football 15/16"),
  
  
  # Sidebar ---------------------------------------------------------------------------------------
      navlistPanel(widths =c(2, 10),
        tabPanel("League",
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
                                   checkboxInput("reactive_lim", label = "Yes", value = FALSE))
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
        tabPanel("Fixtures/Results",
                 fluidRow(
                   column(width = 6, offset =1 ,
                          uiOutput("gameweek_choice"),
                          tableOutput("fix_res")
                   ),
                   column(4,
                          h3("League Table"),
                          tableOutput("personal_table")
                   )
                   )
                 
        ),
        
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
