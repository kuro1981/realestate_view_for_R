library(rvest)
library(zipangu)
library(dplyr)
library(magrittr)
library(polite)



# SUUMO関数
gen_suumo_url <- function(
    ar,bs,ta,
    jspIdFlg='patternShikugen',
    kb=9999999,kt=9999999,
    mb=9999999,mt=9999999,
    sc=list(), # 地域名
    md=NaN,ekTjCd=NaN,
    tj=0,cnb=0,
    cn=9999999,
    srch_navi=1,...) {
  BaseURL = "https://suumo.jp/jj/bukken/ichiran/JJ010FJ001/?"
  paste(sep = "&")
}


extract_property_unit <- function(node, ...) {
  title <-
    node %>% html_element(".property_unit-title a") %>% html_text()
  ref <- node %>% html_element(".property_unit-title a") %>% html_attr("href")
  info <- node %>% html_element(".property_unit-body .dottable") %>%
    html_list_table() %>% 
    tidyr::pivot_wider(names_from = X1, values_from = X2)
  list(
    title=title,
    ref=ref,
    info = info
    
  )
}

postprocess_chuko <- function(info) {
  # 中古物件用の後処理関数
  info %>% 
    mutate(price = 販売価格 %>% stringr::str_remove("円") %>% kansuji2arabic_str(),
           area = 土地面積 %>% stringr::str_remove("m2.*|㎡.*") %>% as.double() ,
           house_area = 建物面積 %>%  stringr::str_remove("m2.*|㎡.*") %>% as.double() ,
           room_count = 間取り %>% stringr::str_remove("L[DK]*.*|DK.*|K.*|LLDDKK.*") %>% as.integer()
           
           
    )
}



html_list_table <- function (x, header = NA, trim = TRUE,
                           dec = ".", na.strings = "NA", convert = TRUE)
{
  # dl dt ddタグ処理用関数
  # TODO リストで返すように処理を変更する。
  # refer from rvest::html_table
  ns <- xml2::xml_ns(x)
  rows <- xml2::xml_find_all(x, ".//dl", ns = ns)
  cells <- lapply(rows, xml2::xml_find_all, ".//dt|.//dd", 
                  ns = ns)
  if (length(cells) == 0) {
    return(tibble::tibble())
  }
  out <- rvest:::table_fill(cells, trim = trim)
  if (is.na(header)) {
    header <- all(html_name(cells[[1]]) == "th")
  }
  if (header) {
    col_names <- out[1, , drop = FALSE]
    out <- out[-1, , drop = FALSE]
  }
  else {
    col_names <- paste0("X", seq_len(ncol(out)))
  }
  colnames(out) <- col_names
  df <- tibble::as_tibble(out, .name_repair = "minimal")
  if (isTRUE(convert)) {
    df[] <- lapply(df, function(x) {
      utils::type.convert(x, as.is = TRUE, dec = dec, 
                          na.strings = na.strings)
    })
  }
  df
}

gen_yahoo_geocoder_url <- function (urlArgs) 
{
  # yahoo geocoder url生成
  BASEURL <- "https://map.yahooapis.jp/geocode/V1/geoCoder"
  return(utils::URLencode(paste0(BASEURL, "?", paste0(paste0(names(urlArgs)), 
                                                      "=", paste0(urlArgs), collapse = "&")),  reserved = F))
}

downloadDataYahoo <- function (map_url, simplify, curl_proxy = NULL, warn=TRUE) 
{
  # refer from gooleway:::downloadData
  out <- NULL
  if (length(map_url) > 1) 
    stop("invalid map_url")
  if (curl::has_internet() == FALSE) 
    stop("Can not retrieve results. No valid internet connection (tested using curl::has_internet() )")
  if (!is.null(curl_proxy)) {
    con <- curl_proxy(map_url)
  }
  else {
    con <- curl::curl(map_url)
  }
  tryCatch({
    out <- readLines(con, warn = warn)
  }, error = function(cond) {
    stop("There was an error downloading results. Please manually check the following URL is valid by entering it into a browswer. If valid, please file a bug report citing this URL (note: your API key has been removed, so you will need to add that back in) \n\n", 
         gsub("key=.*", "", map_url), "key=", sep = "")
  }, finally = {
    close(con)
  })
  if (simplify == TRUE) {
    out <- jsonlite::fromJSON(out)
  }
  return(out)
}

limit_f <- ratelimitr::limit_rate(downloadDataYahoo, ratelimitr::rate(10,1))

add_latlon_from_yahoo <- function(address, fun=NULL, simplify = TRUE, ...) {
  
  if(is.null(fun)) {
    fun = downloadDataYahoo
  } 
  geocoder_args <- list(appid=config$yahoo_appid, 
                        query = address,
                        al=3,
                        ar="ge",
                        output="json")

  ret <-
    gen_yahoo_geocoder_url(geocoder_args) %>% 
    fun(simplify = T, ...)
  point <- ret$Feature$Geometry$Coordinates %>% str_split(",",n=2, simplify = T)
  list(lat = as.double(point[1,2]), lon = as.double(point[1,1]))
}




