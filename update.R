# update games then teams then comps

update_games <- function(game_ids, games) {
  games0 <- game_numbers_to_info(game_ids)
  games %>% rows_upsert(games0) 
}

update_teams <- function(teams, games) {
  team_ids <- games_to_teams(games, teams)
  teams0 <- numbers_to_team(team_ids)
  teams %>% rows_upsert(teams0)
}

update_comps <- function(comps, games) {
  comp_ids <- games_to_comps(games, comps)
  comps0 <- numbers_to_comp(comp_ids)
  comps %>% rows_upsert(comps0)
}