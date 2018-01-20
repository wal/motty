rm(list = ls())

library(tidyverse)
library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)
library(DT)

source('R/data_helper.R', local = TRUE)

team_logos_map = list("Arsenal" = "602.png",
                      "Bournemouth" = "600.png",
                      "Brighton" = "618.png",
                      "Burnley" = "622.png",       
                      "Chelsea" = "630.png",
                      "Crystal Palace" = "642.png",
                      "Everton" = "650.png",
                      "Huddersfield" = "664.png",
                      "Leicester" = "673.png",
                      "Liverpool" = "676.png",
                      "Man City" = "679.png",
                      "Man United" = "680.png",
                      "Newcastle" = "688.png",
                      "Southampton" = "713.png",
                      "Stoke" = "721.png",
                      "Swansea" = "714.png",       
                      "Tottenham" = "718.png",
                      "Watford" = "732.png",
                      "West Brom" = "734.png",
                      "West Ham" = "735.png"     
)

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

logo_url_for_team <- function(team_name) {
  paste("images/premier_league/",team_logos_map[team_name], sep="")
}

server <- function(input, output) {
  
  home_team <- reactive(input$home_team)
  away_team <- reactive(input$away_team)
  
  home_goal_probabilities <- reactive(home_goal_probability_distribution(home_team(), away_team()))
  away_goal_probabilities <- reactive(away_goal_probability_distribution(home_team(), away_team()))
  
  output$home_team_logo <- renderUI({
    tags$img(src = logo_url_for_team(home_team()), width=120)
  })
  
  output$away_team_logo <- renderUI({
    tags$img(src = logo_url_for_team(away_team()), width=120)
  })
  
  output$match_outcome_probabilities <- renderTable({

    probability_matrix <- outer(home_goal_probabilities(), away_goal_probabilities())
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
    
    data.table(
      "Home Win" = paste(round(home_win_probability * 100, 2), " %"),
      "Draw" = paste(round(draw_probability * 100, 2), " %"),
      "Away Win" =paste(round(away_win_probability * 100, 2), " %")
    )
  })
  
  output$homeSummaryTable <- renderTable({ 
    home_team_home_stats <- season_attack_defence_strengths %>% filter(Team == home_team())
    
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
    away_team_away_stats <- season_attack_defence_strengths %>% filter(Team == away_team()) 
    
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

    ggplot(data.frame(home_goal_probabilities()), 
           aes(seq_along(home_goal_probabilities()) -1, 
               home_goal_probabilities())) +
      geom_bar(stat = "identity") +
      labs(x=home_team(), y="Probability", title=paste(home_team(), " (Home) Goals - Probability")) +
      theme_minimal() +
      scale_x_continuous(labels=c(0:5), breaks=c(0:5)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$awayGoalsPlot <- renderPlot({
    
    ggplot(data.frame(away_goal_probabilities()), 
           aes(seq_along(away_goal_probabilities()) -1, 
               away_goal_probabilities())) +
      geom_bar(stat = "identity") +
      labs(x=away_team(), y="Probability", title=paste(away_team(), " (Away) Goals - Probability")) +
      theme_minimal()  +
      scale_x_continuous(labels=c(0:5), breaks=c(0:5)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$matchResultPlot <- renderPlot({
    
    probability_matrix <- outer(home_goal_probabilities(), away_goal_probabilities())
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
      labs(x=paste(home_team(), " (Home) Goals"), y=paste(away_team(), " (Away) Goals"), title="Match Score Probability") +
      theme_minimal() +
      scale_x_discrete(labels=c(0:5), breaks=c(0:5)) +
      scale_x_discrete(labels=c(0:5), breaks=c(0:5)) +
      guides(fill=FALSE) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$matchResultTable <- DT::renderDataTable({

    probability_matrix <- outer(home_goal_probabilities(), away_goal_probabilities())
    rownames(probability_matrix) <- seq(0,5)
    colnames(probability_matrix) <- seq(0,5)
    
    probability_matrix_long <- melt(probability_matrix)
    
    probability_matrix_long$value <- round(probability_matrix_long$value, 3)
    probability_matrix_long$Var1 <- as.factor(probability_matrix_long$Var1)
    probability_matrix_long$Var2 <- as.factor(probability_matrix_long$Var2)
    
    names(probability_matrix_long) <- c(home_team(), away_team(), "Probability")
    
    probability_matrix_long <- probability_matrix_long %>% arrange(desc(Probability))
    
    DT::datatable(head(probability_matrix_long, 5), options = list(dom = 't'), rownames= FALSE)
  })
}
