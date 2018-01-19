rm(list = ls())

library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)
library(DT)

premier_league_2017_2018_url <- "http://www.football-data.co.uk/mmz4281/1718/E0.csv"

data <- read.csv(premier_league_2017_2018_url, stringsAsFactors = FALSE)

average_goals <- mean(data$FTHG + data$FTAG)

home_data <- data %>% 
  group_by(HomeTeam) %>%
  summarise(HomeMatchesPlayed = n(),
            HomeGoalsScored = sum(FTHG),
            HomeGoalsScoredAverage = HomeGoalsScored / n(),
            HomeGoalsConceeded = sum(FTAG),
            HomeGoalsConceededAverage = HomeGoalsConceeded / n()) %>%
  rename(Team = HomeTeam) %>%
  arrange(Team)

away_data <- data %>% 
  group_by(AwayTeam) %>%
  summarise(AwayMatchesPlayed = n(),
            AwayGoalsScored = sum(FTAG),
            AwayGoalsScoredAverage = AwayGoalsScored / n(),
            AwayGoalsConceeded = sum(FTHG),
            AwayGoalsConceededAverage = AwayGoalsConceeded / n()) %>%
  rename(Team = AwayTeam) %>%
  arrange(Team)

season_goals_data <- bind_cols(home_data, away_data) %>% 
  select(Team, 
         HomeMatchesPlayed,
         HomeGoalsScored, HomeGoalsScoredAverage, 
         HomeGoalsConceeded, HomeGoalsConceededAverage, 
         AwayMatchesPlayed,
         AwayGoalsScored, AwayGoalsScoredAverage,
         AwayGoalsConceeded, AwayGoalsConceededAverage)

average_league_home_goals = mean(data$FTHG)
average_league_away_goals = mean(data$FTAG)

season_attack_defence_strengths <- season_goals_data %>%
  mutate(HomeAttackStrength = HomeGoalsScoredAverage / average_league_home_goals,
         HomeDefenceStrength = HomeGoalsConceededAverage/ average_league_away_goals,
         AwayAttackStrength = AwayGoalsScoredAverage / average_league_away_goals,
         AwayDefenceStrength = AwayGoalsConceededAverage / average_league_home_goals)

teams = unique(season_attack_defence_strengths$Team)

home_goal_probability_distribution <- function(home_team, away_team) {
  home_team_data = season_attack_defence_strengths %>% filter(Team == home_team)
  away_team_data = season_attack_defence_strengths %>% filter(Team == away_team)
  
  expected_home_goals = home_team_data$HomeAttackStrength * 
    away_team_data$AwayDefenceStrength * 
    average_league_home_goals
  
  return(dpois(0:5,expected_home_goals))
}

away_goal_probability_distribution <- function(home_team, away_team) {
  home_team_data = season_attack_defence_strengths %>% filter(Team == home_team)
  away_team_data = season_attack_defence_strengths %>% filter(Team == away_team)
  
  expected_away_goals = away_team_data$AwayAttackStrength *
    home_team_data$HomeDefenceStrength *
    average_league_away_goals
  
  return(dpois(0:5,expected_away_goals))
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("EPL 2017/2018"),
  
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
      textOutput("matchDescription"),
      tableOutput("matchProjection"),
      plotOutput("matchResultPlot", height = 400),
      DT::dataTableOutput("matchResultTable"),
      tableOutput("homeSummaryTable"),
      tableOutput("awaySummaryTable"),
      plotOutput("homeGoalsPlot", height = 200),
      plotOutput("awayGoalsPlot", height = 200)
    )
  )
)

server <- function(input, output) {
  
  output$matchDescription <- renderText(paste(input$home_team, " v ", input$away_team))

  output$homeSummaryTable <- renderTable({ 
    home_team <- input$home_team
    home_team_home_stats <- season_attack_defence_strengths %>% filter(Team == home_team)
    
    data.table(home_team = c("Home Matches Played",
                             "Home Goals Scored",
                             "Home Goals Scored Average",
                             "Home Attack Strength", 
                             "Home Goals Conceeded",
                             "Home Goals Conceeded Average",
                             "Home Defence Strength"), 
               "Value" = c(home_team_home_stats$HomeMatchesPlayed,
                           home_team_home_stats$HomeGoalsScored, 
                           home_team_home_stats$HomeGoalsScoredAverage, 
                           home_team_home_stats$HomeAttackStrength, 
                           home_team_home_stats$HomeGoalsConceeded,
                           home_team_home_stats$HomeGoalsConceededAverage,
                           home_team_home_stats$HomeDefenceStrength
               )
    )
  })
  
  
  output$awaySummaryTable <- renderTable({ 
    away_team <- input$away_team
    away_team_away_stats <- season_attack_defence_strengths %>% filter(Team == away_team) 
    
    data.table(away_team = 
                 c("Away Matches Played",
                 "Away Goals Scored",
                 "Away Goals Scored Average",
                 "Away Attack Strength", 
                 "Away Goals Conceeded",
                 "Away Goals Conceeded Average",
                 "Away Defence Strength"), 
               "Value" = 
                 c(away_team_away_stats$AwayMatchesPlayed,
                 away_team_away_stats$AwayGoalsScored, 
                 away_team_away_stats$AwayGoalsScoredAverage, 
                 away_team_away_stats$AwayAttackStrength, 
                 away_team_away_stats$AwayGoalsConceeded,
                 away_team_away_stats$AwayGoalsConceededAverage,
                 away_team_away_stats$AwayDefenceStrength
                 )
               )
  })
  
  output$homeGoalsPlot <- renderPlot({
    home_team <- input$home_team
    away_team <- input$away_team
    
    home_goal_probabilities <- home_goal_probability_distribution(home_team, away_team)
    
    ggplot(data.frame(home_goal_probabilities), 
           aes(seq_along(home_goal_probabilities) -1, 
               home_goal_probabilities)) +
      geom_bar(stat = "identity") +
      labs(x=home_team, y="Probability", title=paste(home_team, " (Home) Goals - Probability")) +
      theme_minimal() +
      scale_x_continuous(labels=c(0:5), breaks=c(0:5)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$awayGoalsPlot <- renderPlot({
    
    home_team <- input$home_team
    away_team <- input$away_team
    
    away_goal_probabilities <- away_goal_probability_distribution(home_team, away_team)
    
    ggplot(data.frame(away_goal_probabilities), 
           aes(seq_along(away_goal_probabilities) -1, 
               away_goal_probabilities)) +
      geom_bar(stat = "identity") +
      labs(x=away_team, y="Probability", title=paste(away_team, " (Away) Goals - Probability")) +
      theme_minimal()  +
      scale_x_continuous(labels=c(0:5), breaks=c(0:5)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$matchResultPlot <- renderPlot({
    
    home_team <- input$home_team
    away_team <- input$away_team
    
    home_goal_probabilities <- home_goal_probability_distribution(home_team, away_team)
    away_goal_probabilities <- away_goal_probability_distribution(home_team, away_team)
    
    probability_matrix <- outer(home_goal_probabilities, away_goal_probabilities)
    rownames(probability_matrix) <- seq(0,5)
    colnames(probability_matrix) <- seq(0,5)
    
    longData <- melt(probability_matrix)
    longData$value <- round(longData$value, 3)
    longData$Var1 <- as.factor(longData$Var1)
    longData$Var2 <- as.factor(longData$Var2)
    longData %>% arrange(desc(value))

    ggplot(longData, aes(x = Var1, y = Var2)) + 
      geom_tile(aes(fill=value)) + 
      geom_text(aes(label = value)) +
      scale_fill_gradient(low="grey90", high="red") +
      labs(x=paste(home_team, " (Home) Goals"), y=paste(away_team, " (Away) Goals"), title="Match Score Probability") +
      theme_minimal() +
      scale_x_discrete(labels=c(0:5), breaks=c(0:5)) +
      scale_x_discrete(labels=c(0:5), breaks=c(0:5)) +
      guides(fill=FALSE) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$matchResultTable <- DT::renderDataTable({
    home_team <- input$home_team
    away_team <- input$away_team
    
    home_goal_probabilities <- home_goal_probability_distribution(home_team, away_team)
    away_goal_probabilities <- away_goal_probability_distribution(home_team, away_team)
    
    probability_matrix <- outer(home_goal_probabilities, away_goal_probabilities)
    rownames(probability_matrix) <- seq(0,5)
    colnames(probability_matrix) <- seq(0,5)
    
    probability_matrix_long <- melt(probability_matrix)
    
    probability_matrix_long$value <- round(probability_matrix_long$value, 3)
    probability_matrix_long$Var1 <- as.factor(probability_matrix_long$Var1)
    probability_matrix_long$Var2 <- as.factor(probability_matrix_long$Var2)
    
    names(probability_matrix_long) <- c(home_team, away_team, "Probability")
    
    probability_matrix_long <- probability_matrix_long %>% arrange(desc(Probability))
    
    DT::datatable(head(probability_matrix_long, 5), options = list(dom = 't'), rownames= FALSE)
  })
  
  output$matchProjection <- renderTable({
    
    home_team <- input$home_team
    away_team <- input$away_team
    
    home_goal_probabilities <- home_goal_probability_distribution(home_team, away_team)
    away_goal_probabilities <- away_goal_probability_distribution(home_team, away_team)
    
    probability_matrix <- outer(home_goal_probabilities, away_goal_probabilities)
    rownames(probability_matrix) <- seq(0,5)
    colnames(probability_matrix) <- seq(0,5)
    
    longData <- melt(probability_matrix)

    home_win_probability <- longData %>% 
      filter(Var1 > Var2) %>%
      summarise(Probability = sum(value)) %>%
      .$Probability
    
    draw_probability <- longData %>% 
      filter(Var1 == Var2) %>%
      summarise(Probability = sum(value)) %>%
      .$Probability
    
    away_win_probability <- longData %>% 
      filter(Var1 < Var2) %>%
      summarise(Probability = sum(value)) %>%
      .$Probability
    
    data.table("Home Win" = c(home_win_probability), 
               Draw = c(draw_probability), 
               "Away Win" = c(away_win_probability))
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)