library(leaflet)
library(yaml)

# Load Config
config <- yaml::read_yaml("config.yml")

source("./scraping.R")

url <- "https://suumo.jp/jj/bukken/ichiran/JJ010FJ001/?ar=060&bs=021&ta=27&jspIdFlg=patternShikugun&sc=27203&sc=27205&sc=27211&kb=1&kt=9999999&tb=0&tt=9999999&hb=0&ht=9999999&ekTjCd=&ekTjNm=&tj=0&cnb=0&cn=9999999&srch_navi=1"

read_html(url) %>% 
  html_elements(".property_unit") %>%
  purrr::map_dfr(extract_property_unit) %>% 
  tidyr::unnest(info) %>% postprocess_chuko() %T>% View %>%
  mutate(point = purrr::map_df(所在地, function(d,...) add_latlon_from_yahoo(d, limit_f, warn=F))) %>%
  tidyr::unnest(point) %T>% View %>%
  leaflet() %>% addTiles() %>%
  addMarkers(~lon, ~lat, label = ~販売価格, popup = ~title)
