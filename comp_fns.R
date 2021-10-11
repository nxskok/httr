number_to_comp <- function(number) {
  url_base <- "https://uk.soccerway.com/national/chile/primera-division/2021/regular-season/r"
  url <- str_c(url_base, number, "/")
  content <- url_to_content(url)
  content %>% html_nodes("#subheading > h1") %>% 
    html_text() -> comp_name
  content %>% html_nodes("#submenu > ul > li.current > a") %>% 
    html_attr("href") %>% str_split("/") %>% pluck(1) -> v
  list(class = v[2], where = v[3], comp_name = comp_name, season = v[5], comp_part = v[6])
}

numbers_to_comp <- function(comp_id) {
  enframe(comp_id, name = NULL, value = "comp_id") %>% 
    rowwise() %>% 
    mutate(comp = list(number_to_comp(comp_id))) %>% 
    unnest_wider(comp)
}

games_to_comps <- function(games, comps) {
  games$comp %>% setdiff(comps$comp_id) -> v
  v[!is.na(v)]
}

