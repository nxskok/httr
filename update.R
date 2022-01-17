# update games then teams then comps

update_games <- function(game_ids, games) {
  print(glue::glue("There are {length(game_ids)} games to add."))
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

update_everything <- function(game_ids) {
  games <- read_rds("games.rds")
  teams <- read_rds("teams.rds")
  comps <- read_rds("comps.rds")
  games <- update_games(game_ids, games)
  teams <- update_teams(teams, games)
  comps <- update_comps(comps, games) # this can fail if no comp ids to add (check inside)
  write_rds(games, "games.rds")
  write_rds(teams, "teams.rds")
  write_rds(comps, "comps.rds")
}

display_games <- function() {
  games <- read_rds("games.rds")
  teams <- read_rds("teams.rds")
  comps <- read_rds("comps.rds")
  games %>% 
    left_join(teams, by = c("t1"="id")) %>% 
    left_join(teams, by = c("t2"="id")) %>% 
    left_join(comps, by = c("comp" = "comp_id")) %>% 
    select(game, home_team = name.x, away_team = name.y, score, status, comp, # comp is competition number
           where, comp_name, comp_part, season, timestamp, retrieved) %>% 
    arrange(desc(status), desc(timestamp))
}

