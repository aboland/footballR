library(shiny)


# Define UI for application to display data
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Fantasy Football 15/16"),
  
  
  # Sidebar ---------------------------------------------------------------------------------------
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(
        tabPanel("Summary",
                 h4("League code: 1693603-401525"),
                 p("The current plan is a pay-in of 15 each. The person with most points within each month will get 10 and the overall
                   winner will get the left over money. 6 players will give enough money for the monthly prizes. Some gameweeks
                   occuring at the beginning/end of a month will span two months. The month a gameweek starts in will be the
                   month that the gameweek counts in. A list of the months and repective gameweeks is displayed on the right."),
          p(textOutput("n_managers"))),
        tabPanel("Player Data Controls",
                 h3("Choose fields to display"),
                 uiOutput("field1"),
                 uiOutput("field2"),
                 uiOutput("field3"),
                 uiOutput("field4"),
                 uiOutput("sort_field"))
        )),
  
    # Main panel -------------------------------------------------------------------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel("League",
                 tabsetPanel(
                 tabPanel("Overall",
                          #p("This takes the data straight from the web, once I get it set up properly it should run the monthly leagues automatically."),
                          fluidRow(
                            column(6,
                              h3("Fantasy Table (Official)"),
                              tableOutput("personal_table2")
                          ),
                          #tableOutput("personal_table")),
                            column(6,
                              h3("Current Standings"),
                              uiOutput("table_gameweek_choice"),
                              tableOutput("manager_current_stand"))
                          )
                 ),
                 tabPanel("Monthly",
                          fluidRow(
                            column(5,
                            h3("Monthly Gameweeks"),
                            p("The following gameweeks will be used to decide the monthly prizes"),
                          tableOutput("MonthGW")))),
                 tabPanel("Graphs",
                          fluidRow(
                            column(4,
                                   uiOutput("plot_data_type")),
                            column(4,
                                   p("..."))
                            ),
                          plotOutput("points_plot")
                 ),
                 tabPanel("Compare Teams",
                          fluidRow(
                            column(6,
                                   h3("Team 1"),
                                    uiOutput("manager_choice1"),
                                    tableOutput("manager_team1")
                            ),
                            column(6,
                                  h3("Team 2"),
                                  uiOutput("manager_choice2"),
                                  tableOutput("manager_team2")
                                  )
                          )
                 )
                          #tableOutput("player_history")))
                 )
        ),
        tabPanel("Fixtures/Results",
                 uiOutput("gameweek_choice"),
                 tableOutput("fix_res")),
        
        tabPanel("Player Data",
                 fluidRow(
                   column(3, uiOutput("team_choice")),
                   column(3, uiOutput("position_choice")),
                   column(3, uiOutput("cost_choice"))),
                 tableOutput("data_display")
                 )
        )
      )
    )
))
