#' function POSTing HTTP requests to data server
#' @param date date for which to get data
#' @param lang language of the mensa plan -- either "de" or "en"
#' @param loc location identifier

mp_scrape <-
  function(
    lang = c("de","en"),
    date = format(Sys.Date(), "%Y-%m-%d"),
    loc  = "mensa_giessberg"
  )
  {
    url <- "https://www.max-manager.de/daten-extern/seezeit/html/inc/ajax-php_konnektor.inc.php"
    post_results <-
      httr::POST(
        url,
        body = list(
          func = "make_spl",
          loc  = loc,
          lang = lang[1],
          date = date
        )
      )
    return(post_results)
  }


#' function parsing data retrieved
#' @param post_results results from get_mesaplan()

mp_parse <- function(post_results){
  # infor derives directly from http request
  http_status    <- post_results$status_code
  date_request   <- post_results$date
  content        <- httr::content(post_results, as="text", encoding="UTF-8")
  content_length <- nchar(content)
  content        <- ifelse(content_length==0, "<body></body>", content)
  date_dish      <- post_results$request$fields$date
  language       <- post_results$request$fields$lang
  location       <- post_results$request$fields$loc
  # dish types
  tmp       <- rvest::html_text(rvest::html_nodes(xml2::read_html(content, encoding="UTF-8"), xpath = "//tr/td[1]"))
  if( length(tmp)>0 ){
    types     <- stringr::str_replace_all(iconv(even(tmp), "UTF-8", "latin1"), "\n", "")
  }else{
    types <- NA
  }
  # dishes and additives
  tmp       <-
    rvest::html_text(
      rvest::html_nodes(
        xml2::read_html(content, encoding = "UTF-8"),
        xpath = "//tr/td[2]"
      )
    )
  if( length(tmp)>0 ){
    dish      <- stringr::str_replace_all(iconv(even(tmp), "UTF-8", "latin1"), "\n", "")
    additives <- stringr::str_replace_all(iconv( odd(tmp), "UTF-8", "latin1"), "\n", "")
  }else{
    dish      <- NA
    additives <- NA
  }
  # results
  res <-
    data.frame(
      date_request,
      date_dish,
      language,
      types,
      dish,
      additives,
      location,
      http_status,
      content_length
  )
  # return
  return(res)
}

#' wrapper function for get_mensaplan() and parse_mensaplan
#' @param date date for which to get data
#' @param lang language of the mensa plan -- either "de" or "en"
#' @param loc location identifier
mensaplan <- function(
  lang = c("de","en"),
  date = format(Sys.Date(), "%Y-%m-%d"),
  loc  = "mensa_giessberg"
){
  mp_parse(mp_scrape(lang, date, loc))
}
