
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
