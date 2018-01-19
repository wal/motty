source('R/data_helper.R', local = TRUE)

ui <- fluidPage(
  titlePanel(title=div(img(src="images/premier_league/EPL2016.png"), "EPL 2017/2018 - Score Prediction")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "home_team",
                  label = "Home Team:",
                  choices = teams,
                  selected = head(teams, 1)),
      
      selectInput(inputId = "away_team",
                  label = "Away Team:",
                  choices = teams,
                  selected = tail(teams, 1))
    ),
    
    mainPanel(
      tags$table(width="100%",
        tags$tr(
          tags$td(
            uiOutput("home_team_logo")
          ), 
          tags$td(
            tableOutput("match_outcome_probabilities")
          ),
          tags$td(
            uiOutput("away_team_logo")
          )
        )
      ),

      hr(),
      plotOutput("matchResultPlot", height = 400),
      hr(),
      DT::dataTableOutput("matchResultTable"),
      tableOutput("homeSummaryTable"),
      tableOutput("awaySummaryTable"),
      plotOutput("homeGoalsPlot", height = 200),
      plotOutput("awayGoalsPlot", height = 200)
    )
  )
)
