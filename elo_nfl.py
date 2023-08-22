
import numpy as np
import pandas as pd

names_to_replace = {
    'OAK': 'LV',
    'SD': 'LAC',
    'STL': 'LA'
}

nfl = (
    pd.read_csv('data/games.csv')
    [['game_id', 'season', 'game_type', 'week', 'home_team', 'away_team', 'home_score', 'away_score', 'result', 'total', 'overtime', 'home_moneyline', 'away_moneyline', 'spread_line', 'away_spread_odds', 'home_spread_odds']]
    .query('season == 2022')
    .reset_index(drop = True)
    .assign(
        home_team = lambda x: x['home_team'].map(names_to_replace).fillna(x['home_team']),
        away_team = lambda x: x['away_team'].map(names_to_replace).fillna(x['away_team']),
        winning_team = lambda x: np.select(
            [x['home_score'] > x['away_score'],
            x['away_score'] > x['home_score']],
            [x['home_team'], x['away_team']],
            default = 'Draw'),
        spread_fav = lambda x: np.select(
            [x['spread_line'] > 0,
            x['spread_line'] < 0],
            [x['home_team'], x['away_team']],
            default = 'Push'),
        outcome = lambda x: np.select(
            [x['home_score'] > x['away_score'],
            x['away_score'] > x['home_score']],
            [1, 0],
            default = .5),
            )
)

K = 32

def calculate_expected_outcome(team_a, team_b, scaling_factor = 400):
    return 1 / (1 + 10 ** ((team_b - team_a) / scaling_factor))

def make_init_ratings(data):
    team_column = 'home_team' if 'home_team' in data.columns else 'team_a'
    teams = data[team_column].unique()
    ratings = pd.DataFrame({'team': teams, 'rating': 1500})
    return ratings

initial_ratings = make_init_ratings(nfl)
nfl = nfl.rename(columns = {'home_team': 'team_a', 'away_team': 'team_b'})

weekly_ratings = pd.DataFrame(columns = ['week', 'team', 'elo'])

for index, row in nfl.iterrows():
    team_a = row['team_a']
    team_b = row['team_b']
    elo_a = initial_ratings.loc[initial_ratings['team'] == team_a, 'rating'].values[0]
    elo_b = initial_ratings.loc[initial_ratings['team'] == team_b, 'rating'].values[0]
    outcome_a = row['outcome']
    outcome_b = 1 - outcome_a
    expected_a = calculate_expected_outcome(elo_a, elo_b)
    expected_b = 1 - expected_a
    new_elo_a = elo_a + K * (outcome_a - expected_a)
    new_elo_b = elo_b + K * (outcome_b - expected_b)
    
    # Create DataFrames for the current week's ratings
    week_ratings = pd.DataFrame({'week': [row['week'], row['week']],
                                  'team': [team_a, team_b],
                                  'elo': [new_elo_a, new_elo_b]})
    
    # Concatenate the week's ratings to the weekly_ratings DataFrame
    weekly_ratings = pd.concat([weekly_ratings, week_ratings], ignore_index=True)

    # Update the ratings in the initial_ratings DataFrame
    initial_ratings.loc[initial_ratings['team'] == team_a, 'rating'] = new_elo_a
    initial_ratings.loc[initial_ratings['team'] == team_b, 'rating'] = new_elo_b