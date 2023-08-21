
update_team_names <- function(data, ...) {
  columns_to_update <- enquos(...)
  
  updated_data <- data |>
    mutate_at(columns_to_update, ~ case_when(
      . == 'OAK' ~ 'LV',
      . == 'SD' ~ 'LAC',
      . == 'STL' ~ 'LA',
      TRUE ~ .
    ))
  
  return(updated_data)
}

calculate_expected_outcome <- function(elo_a, elo_b, scaling_factor = 400) {
  return (1 / (1 + 10^((elo_b - elo_a) / scaling_factor)))
}

make_init_ratings <- function(data) {
  initial_ratings <- data.frame(
    team = distinct(data, team_a),
    elo = 1500
  )
  return(initial_ratings)
}


calculate_elo <- function(data, init_ratings, K = 32){
  for (i in 1:nrow(data)) {
    if ("team_a" %in% colnames(data)) {
      team_a <- data$team_a[i]
      team_b <- data$team_b[i]
    } else if ("home_team" %in% colnames(data)) {
      team_a <- data$home_team[i]
      team_b <- data$away_team[i]
    } else {
      stop("Data must contain either 'team_a' and 'team_b' or 'home_team' and 'away_team' columns.")
    }
    
    elo_a <- init_ratings$elo[init_ratings$team == team_a]
    elo_b <- init_ratings$elo[init_ratings$team == team_b]
    outcome_a <- data$outcome[i]
    outcome_b <- 1 - outcome_a 
    expected_a <- calculate_expected_outcome(elo_a, elo_b)
    expected_b <- 1 - expected_a
    new_elo_a <- elo_a + K * (outcome_a - expected_a)
    new_elo_b <- elo_b + K * (outcome_b - expected_b)
    init_ratings$elo[init_ratings$team == team_a] <- new_elo_a
    init_ratings$elo[init_ratings$team == team_b] <- new_elo_b
  }
  return(init_ratings)
}

calculate_expected_outcome2 <- function(elo_a, elo_b, point_diff) {
  return (1 / (1 + 10^((elo_b - elo_a + log(point_diff + 1)) / 400)))
}
