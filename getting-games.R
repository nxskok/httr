results_to_get <- function(n = 20) {
  time_crit <- Sys.time() - hours(2)
  games <- read_rds("games.rds")
  games %>% filter(status == "") %>%
    filter(timestamp <= time_crit) %>% 
    slice_sample(n = n) %>% 
    pull(game)
}

fixtures_to_get <- function(ids) {
  games <- read_rds("games.rds")
  tibble(game = ids) %>% anti_join(games) %>% 
    pull(game)
}

random_fixtures <- function(lo, hi, n) {
  n <- min(n, hi-lo+1)
  ids <- sample(lo:hi, n)
  fixtures_to_get(ids)
}