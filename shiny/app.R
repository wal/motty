rm(list = ls())

library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)

premier_league_2017_2018_url <- "http://www.football-data.co.uk/mmz4281/1718/E0.csv"

data <- read.csv(premier_league_2017_2018_url, stringsAsFactors = FALSE)

average_goals <- mean(data$FTHG + data$FTAG)

home_data <- data %>% 
  group_by(HomeTeam) %>%
  summarise(HomeGoalsScored = sum(FTHG),
            HomeGoalsScoredAverage = HomeGoalsScored / n(),
            HomeGoalsConceeded = sum(FTAG),
            HomeGoalsConceededAverage = HomeGoalsConceeded / n()) %>%
  rename(Team = HomeTeam) %>%
  arrange(Team)

away_data <- data %>% 
  group_by(AwayTeam) %>%
  summarise(AwayGoalsScored = sum(FTAG),
            AwayGoalsScoredAverage = AwayGoalsScored / n(),
            AwayGoalsConceeded = sum(FTHG),
            AwayGoalsConceededAverage = AwayGoalsConceeded / n()) %>%
  rename(Team = AwayTeam) %>%
  arrange(Team)

season_goals_data <- bind_cols(home_data, away_data) %>% 
  select(Team, 
         HomeGoalsScored, HomeGoalsScoredAverage, 
         HomeGoalsConceeded, HomeGoalsConceededAverage, 
         AwayGoalsScored, AwayGoalsScoredAverage,
         AwayGoalsConceeded, AwayGoalsConceededAverage)

average_league_home_goals = mean(data$FTHG)
average_league_away_goals = mean(data$FTAG)

season_attack_defence_strengths <- season_goals_data %>%
  mutate(HomeAttackStrength = HomeGoalsScoredAverage / average_league_home_goals,
         HomeDefenceStrength = HomeGoalsConceededAverage/ average_league_away_goals,
         AwayAttackStrength = AwayGoalsScoredAverage / average_league_away_goals,
         AwayDefenceStrength = AwayGoalsConceededAverage / average_league_home_goals) %>%
  select(Team, HomeAttackStrength, HomeDefenceStrength, AwayAttackStrength, AwayDefenceStrength)

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
  titlePanel("EPL 2017/2018 - Results Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "home_team",
                  label = "Home Team:",
                  choices = teams),
      
      selectInput(inputId = "away_team",
                  label = "Away Team:",
                  choices = teams)
    ),
    
    mainPanel(
      plotOutput(outputId = "matchResultPlot", height = 400),
      plotOutput(outputId = "homeGoalsPlot", height = 200),
      plotOutput(outputId = "awayGoalsPlot", height = 200)
    )
  )
)

server <- function(input, output) {
  
  output$homeGoalsPlot <- renderPlot({
    
    home_team <- input$home_team
    away_team <- input$away_team
    
    home_goal_probabilities <- home_goal_probability_distribution(home_team, away_team)
    
    ggplot(data.frame(home_goal_probabilities), 
           aes(seq_along(home_goal_probabilities) -1, 
               home_goal_probabilities)) +
      geom_bar(stat = "identity") +
      labs(x="Goals", 
           y="Probability", 
           title="Home Goals - Probability") +
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
      labs(x="Goals", 
           y="Probability", 
           title = "Away Goals - Probability") +
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
      labs(x="Home Goals", 
           y="Away Goals", 
           title="Match Score Probability") +
      theme_minimal() +
      scale_x_discrete(labels=c(0:5), breaks=c(0:5)) +
      scale_x_discrete(labels=c(0:5), breaks=c(0:5)) +
      guides(fill=FALSE) +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)