url_to_content <- function(url) {
  # now with httr2
  print(url)
  request(url) %>% req_retry(max_tries = 10, is_transient = ~ resp_status(.x) %in% c(429, 500, 503)) %>% 
    req_throttle(rate = 1/1) -> req
  req %>% req_perform() -> resp
  resp %>% resp_body_html()
}

content_to_date <- function(content, tz = "America/Toronto") {
  content %>% 
    html_nodes("#page_match_1_block_match_info_5 > div > div > div.details > a:nth-child(1)") %>% 
    html_nodes("span") %>%
    html_attr("data-value") %>% 
    as.numeric() %>% 
    as_datetime(tz = tz)  
}

content_to_team <- function(content, team_no) {
  direction <- ifelse(team_no == 1, "left", "right")
  selector1 <- "#page_match_1_block_match_info_5 > div > div > div.container."
  selector2 <- " > a.team-title"
  selector <- str_c(selector1, direction, selector2)
  content %>% 
    html_nodes(selector) %>% 
    html_attr("href") %>% 
    str_extract("/[0-9]+/$") %>% 
    parse_number()
}

content_to_comp <- function(content) {
  content %>% html_nodes("#page_match_1_block_match_info_5 > div > div > div.details > a:nth-child(3)") %>% 
    html_attr("href") %>% 
    str_extract("/r[0-9]+/$") %>% 
    parse_number()
}

content_to_statuses <- function(content) {
  content %>% 
    html_nodes("#page_match_1_block_match_info_5 > div > div > div.container.middle > h3") %>% html_text() %>%
    str_split("\n") -> statuses
  if (length(statuses) > 0) {
    statuses %>% .[[1]] 
  } else {
    c("", "", "", "0 - 0", "0 - 0") # dummy status
  }
}

statuses_to_status <- function(statuses) {
  statuses[3] %>% str_trim()
}

statuses_to_score <- function(statuses, status) {
  if (str_detect(status, "FT")) {
    statuses[5] %>% str_trim()
  } else {
    statuses[4] %>% str_trim()
  }
}

game_number_to_url <- function(game_number) {
  str_c("https://uk.soccerway.com/matches/2021/07/28/europe/uefa-champions-league/galatasaray-sk/psv-nv/",
        game_number, "/")
}

game_number_to_info <- function(game_number) {
  my_url <- game_number_to_url(game_number)
  content <- url_to_content(my_url)
  date <- content_to_date(content)
  t1 <- content_to_team(content, 1)
  t2 <- content_to_team(content, 2)
  comp <- content_to_comp(content)
  statuses <- content_to_statuses(content)
  status <- statuses_to_status(statuses)
  score <- statuses_to_score(statuses, status)
  list(timestamp = date, t1 = t1, t2 = t2, comp = comp, 
       status = status, score = score, retrieved = Sys.time())
}

game_numbers_to_info <- function(game_numbers) {
  enframe(game_numbers, name = NULL, value = "game") %>% 
    rowwise() %>% 
    mutate(info = list(game_number_to_info(game))) %>% 
    unnest_wider(info)
}