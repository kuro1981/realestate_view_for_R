library(googleway)
library(yaml)

# Load Config
config <- yaml::read_yaml("config.yml")

set_key(config$google$map_apikey)

# input suumo search result url https://suumo.jp/
url <- "https://suumo.jp/jj/bukken/ichiran/JJ010FJ001/?ar=060&bs=021&ta=27&jspIdFlg=patternShikugun&sc=27203&sc=27205&sc=27211&kb=1&kt=9999999&tb=0&tt=9999999&hb=0&ht=9999999&ekTjCd=&ekTjNm=&tj=0&cnb=0&cn=9999999&srch_navi=1"

source("./scraping.R")

read_html(url) %>% 
  html_elements(".property_unit") %>%
  purrr::map_dfr(extract_property_unit) %>% 
  tidyr::unnest(info) %>% postprocess_chuko() %>%
  mutate(point = purrr::map_df(所在地, function(d,...) add_latlon_from_yahoo(d, limit_f))) %>%
  tidyr::unnest(point) %T>% View %>%
  google_map(key = config$google$map_apikey) %>%
  add_markers(lat = "lat", lon = "lon", label="販売価格", info_window = "title")
