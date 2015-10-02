#' function POSTing HTTP requests to data server
#' @param date date for which to get data
#' @param lang language of the mensa plan -- either "de" or "en"
#' @importFrom httr POST

get_mensaplan <- function(lang=c("de","en"), date=format(Sys.Date(), "%Y-%m-%d")){
  url <- "https://www.max-manager.de/daten-extern/seezeit/html/inc/ajax-php_konnektor.inc.php"
  post_results <-
    httr::POST(
      url,
      body = list(
        func = "make_spl",
        loc  = "mensa_giessberg",
        lang = lang[1],
        date = date
      )
    )
  return(post_results)
}


#' function parsing data retrieved
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom stringr str_replace_all
#' @param post_results results from get_mesaplan()

parse_mensaplan <- function(post_results){
  status       <- post_results$status_code
  date_request <- post_results$date
  content      <- content(post_results, as="text", encoding="UTF-8")
  date_dish    <- post_results$request$fields$date

  tmp       <- html_text(html_nodes(read_html(content, encoding="UTF-8"), xpath = "//tr/td[1]"))
  if( length(tmp)>0 ){
    types     <- str_replace_all(iconv(even(tmp), "UTF-8", "latin1"), "\n", "")
  }else{
    types <- NA
  }

  tmp       <- html_text(html_nodes(read_html(content, encoding = "UTF-8"), xpath = "//tr/td[2]"))
  if( length(tmp)>0 ){
    dish      <- str_replace_all(iconv(even(tmp), "UTF-8", "latin1"), "\n", "")
    additives <- str_replace_all(iconv(odd(tmp), "UTF-8", "latin1"), "\n", "")
  }else{
    dish <- NA
    additives <- NA
  }

  res <- data.frame(date_request, date_dish, types, dish, additives, status)
  return(res)
}

