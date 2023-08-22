
library(tidyverse)

nfl <- read_csv('data/games.csv', show_col_types = FALSE) |>
  select(game_id, season, game_type, week, home_team, away_team, home_score, away_score, result, total, overtime, home_moneyline, away_moneyline, spread_line, away_spread_odds, home_spread_odds) |>
  update_team_names(c(home_team, away_team)) |>
  mutate(
    winning_team = case_when(
      home_score > away_score ~ home_team,
      away_score > home_score ~ away_team,
      home_score == away_score ~ 'Draw'),
    spread_fav = case_when(
      spread_line > 0 ~ home_team,
      spread_line < 0 ~ away_team,
      spread_line == 0 ~ 'Push'),
    outcome = case_when(
      home_score > away_score ~ 1,
      away_score > home_score ~ 0,
      home_score ==  away_score~ .5)
  ) 

nfl |>
  reframe(
    home_win = sum(home_team == winning_team, na.rm = TRUE) / nrow(nfl)
  )

nfl |>
  reframe(
    spread_win = sum(spread_fav == winning_team, na.rm = TRUE) / nrow(nfl)
  )


home_win <- nfl |>
  group_by(season) |>
  reframe(
    home_win = sum(home_team == winning_team, na.rm = TRUE) / n()
  ) 

spread_win <- nfl |>
  group_by(season) |>
  reframe(
    spread_win = sum(spread_fav == winning_team, na.rm = TRUE) / n()
  )

home_win |> left_join(spread_win, by = 'season') |> View()

K <- 32
initial_ratings <- make_init_ratings(nfl)


nfl <- nfl |>
  filter(season == 2022) |>
  rename(team_a = home_team, team_b = away_team)

weekly_ratings <- data.frame(week = integer(), team = character(), elo = numeric(), game_id = character())

# Loop through each row in the nfl data frame
for (i in 1:nrow(nfl)) {
  team_a <- nfl$team_a[i]
  team_b <- nfl$team_b[i]
  elo_a <- initial_ratings$elo[initial_ratings$team == team_a]
  elo_b <- initial_ratings$elo[initial_ratings$team == team_b]
  outcome_a <- nfl$outcome[i]
  outcome_b <- 1 - outcome_a 
  expected_a <- calculate_expected_outcome(elo_a, elo_b)
  expected_b <- 1 - expected_a
  new_elo_a <- elo_a + K * (outcome_a - expected_a)
  new_elo_b <- elo_b + K * (outcome_b - expected_b)
  
  # Store the ratings for this week in the weekly_ratings data frame
  weekly_ratings <- rbind(weekly_ratings, data.frame(game_id = nfl$game_id[i], week = nfl$week[i], team = team_a, elo = new_elo_a))
  weekly_ratings <- rbind(weekly_ratings, data.frame(game_id = nfl$game_id[i], week = nfl$week[i], team = team_b, elo = new_elo_b))
  
  # Update the ratings in the initial_ratings data frame
  initial_ratings$elo[initial_ratings$team == team_a] <- new_elo_a
  initial_ratings$elo[initial_ratings$team == team_b] <- new_elo_b
}

#initial_ratings <- data.frame(
#  team = distinct(nfl, team_a),
#  elo = 1500
#)

nfl1 <- weekly_ratings |>
  mutate(team_id = ave(team, game_id, FUN = seq_along)) |>
  pivot_wider(
    id_cols = game_id,
    names_from = team_id,
    values_from = c(team, elo),
    names_sep = "_"
  ) |>
  left_join(nfl |> select(game_id, week, winning_team), by = 'game_id') |>
  mutate(predicted_team = case_when(
    elo_1 > elo_2 ~ team_1,
    elo_2 > elo_1 ~ team_2,
    elo_1 == elo_2 ~ 'Draw'
  ))

nfl1 |>
  group_by(week) |>
  reframe(
    elo_win = sum(winning_team == predicted_team, na.rm = TRUE) / n()
  )
