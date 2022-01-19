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

league_of <- function(game_id) {
  games <- read_rds("games.rds")
  games %>% filter(game == game_id) %>% pull(comp) 
}

league_limits <- function(game_id) {
  games <- read_rds("games.rds")
  games %>% arrange(game) %>% 
    mutate(row = row_number()) %>% 
    select(row, game, comp) -> d
  # find comp of this game
  d %>% filter(game == game_id) %>% pull(comp) -> this_comp
  print(glue::glue("this comp is {this_comp}"))
  d %>% drop_na(comp) %>% 
    mutate(lag_comp = lag(comp, default = -1)) %>% 
    mutate(difft = (comp != lag_comp)) %>% 
    mutate(y = cumsum(difft)) -> dd 
  # return(dd)
  # then select the rows with the same y value as our game
  dd %>% filter(game == game_id) %>% pull(y) -> our_y
  dd %>% filter(y == our_y) %>% pull(row) %>% range() -> limits
  lo <- limits[1]-1
  hi <- limits[2]+1
  d %>% slice(lo, hi) %>% pull(game) -> game_limits
  game_limits
}

get_league <- function(this_game) {
  n <- 10
  while(TRUE) {
    lim <- league_limits(this_game)
    print(glue::glue("Limits: {lim}"))
    print(glue::glue("Length {lim[2] - lim[1]}"))
    if (lim[2] - lim[1] > 2000) stop("too many games to get")
    print(glue::glue("Time now is {Sys.time()}"))
    game_ids <- random_fixtures(lim[1], lim[2], n)
    n_game_ids <- length(game_ids)
    print(glue::glue("There are {n_game_ids} games to get."))
    if (n_game_ids==0) break
    update_everything(game_ids)
    n <- n*2
  }
}

get_team_games <- function(data) {
  # data is the games for a league
  data %>% pivot_longer(t1:t2) %>% count(value) %>% 
    summarize(team_count = length(n), m = mean(n), s = sd(n)) %>% 
    mutate(cycles = m/(team_count - 1))
}

league_games <- function() {
  games <- read_rds("games.rds")
  # teams <- read_rds("teams.rds")
  comps <- read_rds("comps.rds")
  games %>% nest_by(comp) %>% 
    rowwise() %>% 
    mutate(stats = list(get_team_games(data))) %>% 
    unnest_wider(stats) %>%
    select(-data) %>% 
    left_join(comps, by = c("comp" = "comp_id")) %>% 
    select(-class) %>% 
    arrange(desc(m)) %>% 
    View("comps")
}