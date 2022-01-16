# update games then teams then comps

update_games <- function(game_ids, games) {
  games0 <- game_numbers_to_info(game_ids)
  games0 %>% filter(!error) %>% select(-error) -> games0
  games %>% rows_upsert(games0) 
}

update_teams <- function(teams, games) {
  team_ids <- games_to_teams(games, teams)
  print(glue::glue("There are {length(team_ids)} teams to add."))
  if (length(team_ids) > 0) {
    teams0 <- numbers_to_team(team_ids)
    teams %>% rows_upsert(teams0)
  } else {
    teams
  }
}

update_comps <- function(comps, games) {
  comp_ids <- games_to_comps(games, comps)
  print(glue::glue("There are {length(comp_ids)} comps to add."))
  if (length(comp_ids) > 0) {
    comps0 <- numbers_to_comp(comp_ids)
    comps %>% rows_upsert(comps0)
  } else {
    print("comps didn't change")
    comps
  }
}