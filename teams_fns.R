number_to_country <- function(number) {
  url_base <- "https://uk.soccerway.com/teams/estonia/jalgpalliklubi-fc-flora/"
  url <- str_c(url_base, number, "/")
  content <- url_to_content(url)
  if (content$error) return("error")
  content$body %>% html_nodes(xpath = '//*[@id="page_team_1_block_team_info_7"]/div/div[2]') -> dl 
  dl %>% html_nodes("dt") %>% html_text() -> heading
  dl %>% html_nodes("dd") %>% html_text() -> entry
  tibble(heading, entry) %>% filter(heading == "Country") %>% pull("entry")
}

number_to_team <- function(number) {
  country <- number_to_country(number)
  url_base <- "https://uk.soccerway.com/teams/estonia/jalgpalliklubi-fc-flora/"
  url <- str_c(url_base, number, "/matches/")
  content <- url_to_content(url)
  if (content$error) return(list())
  content$body %>% html_nodes("a") -> as
  as %>% html_attr("href") -> hrefs
  as %>% html_attr("title") -> titles
  as %>% html_text() -> texts
  bind_cols(title = titles, href = hrefs) %>% drop_na(title) %>%
    filter(str_detect(href, str_c("/", number, "/$"))) %>% 
    pluck("title", 1) -> team_name
  list(name = team_name, country = country, date = Sys.time())
}

numbers_to_team <- function(x) {
  enframe(x, name = NULL, value = "id") %>% 
    rowwise() %>% 
    mutate(info = list(number_to_team(id))) %>% 
    unnest_wider(info)
}

games_to_teams <- function(g, tt) {
  union(g$t1, g$t2) %>% setdiff(tt$id) -> v
  v[!is.na(v)]
}


