#' function POSTing HTTP requests to data server
#' @param date date for which to get data
#' @param lang language of the mensa plan -- either "de" or "en"
#' @param loc location identifier

mp_http_post <-
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

#' function for managing data retrieval
#' downloads data (if necessary) and stores requests in database
#' @param lang paramenter for HTTP POST determining the language of data
#' @param date paramenter for HTTP POST determining the date of the dish
#' @param loc  paramenter for HTTP POST determining the location for which to
#'    retrieve data
#' @param force by default the function will not download data for which it got
#'    an valid (HTTP 200) response already - if set to true it will do it
#'    anyways
mp_data_retrieval <- function(
  lang  = c("de","en"),
  date  = format(Sys.Date(), "%Y-%m-%d"),
  loc   = "mensa_giessberg",
  force = FALSE
){
  lang <- lang[1]
  # create table in DB
  db_ensure_table_exists("", "requests")
  # check for old requests
  db <- db_connect()
  res <- db_get_request_data(date = date)
  if( dim(res)[1]==0 | force ) {
    res  <- mp_http_post(lang, date, loc)
    # extract content
    cont <- httr::content(res, encoding="UTF-8", type="text")
    # prepare data frame for db
    df <-
      data.frame(
        req_date  = res$date,
        status    = res$status_code,
        res$request$fields,
        httr_content_as_list(res$times, "t_"),
        httr_content_as_list(res$cookies, "cookies_"),
        length    = length(res$content),
        content   = cont,
        stringsAsFactors = FALSE
      )
    # write data to db
    RSQLite::dbWriteTable(db, "requests", df, append = TRUE )
  }else{
    df <- res
    df$req_date <- Sys.time()
    df$status   <- 0
    df[,7:21]   <- ""
    RSQLite::dbWriteTable(db, "requests", df, append = TRUE )
  }
  RSQLite::dbDisconnect(db)
  return(TRUE)
}


db_get_request_data <- function(date=Sys.Date(), status=200, loc="mensa_giessberg", lang="de"){
  db <- db_connect()
  sql_innize <- function(x){paste0("(", paste0("'",x ,"'", collapse = ", "), ")")}
  sql <- paste0(
    "SELECT * FROM requests WHERE ",
    "     \n status IN ",  sql_innize(status),
    "  AND\n date   IN ", sql_innize(date),
    "  AND\n loc    IN ", sql_innize(loc),
    "  AND\n lang   IN ",  sql_innize(lang[1])
  )
  res <- RSQLite::dbGetQuery(db, sql)
  db_disconnect(db)
  return(res)
}

db_get_dish_data <- function(date=Sys.Date(), loc="mensa_giessberg", lang="de"){
  db <- db_connect()
  sql_innize <- function(x){paste0("(", paste0("'",x ,"'", collapse = ", "), ")")}
  sql <- paste0(
    "SELECT * FROM dishes WHERE ",
    "     \n loc IN ",  sql_innize(loc),
    "  AND\n lang   IN ", sql_innize(lang),
    "  AND\n date    IN ", sql_innize(date)
  )
  res <- RSQLite::dbGetQuery(db, sql)
  db_disconnect(db)
  return(res)
}

#' function translating additive numbers into description
#' @param x vector of additives
get_additives <- function(x){
  sort(x)
  paste(storage$additives[storage$additives[,1] %in% x ,2], collapse=", ")
}


#' function parsing data retrieved
#' @param post_results results from get_mesaplan()
request_to_dish <- function(res){
  # infor derives directly from http request
  date   <- res$date
  lang   <- res$lang
  loc    <- res$loc
  html   <- xml2::read_html(res$content, encoding = "UTF-8")
  # dish types
  tmp       <- rvest::html_text(rvest::html_nodes(html, xpath = "//tr/td[1]"))
  if( length(tmp)>0 ){
    type     <- stringr::str_replace_all(iconv(even(tmp), "UTF-8", "latin1"), "\n", "")
  }else{
    type <- NA
  }
  # dishes and additives
  tmp       <-
    rvest::html_text(
      rvest::html_nodes(
        html,
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
  res <- data.frame( loc, lang, date, type, dish, additives)
  add <- unlist(lapply(stringr::str_extract_all(res$dish, "\\d+"), get_additives))
  res$dish <-
    stringr::str_trim(
      stringr::str_replace_all(
        stringr::str_replace_all(
          stringr::str_replace_all(res$dish, "\\(.*?\\)",""),
          " ,", ","
        ), "[ \t\n]+", " "
      )
    )
  res$additives <-  paste( res$additives, add, sep=", " )
  sql <-
    paste0(
      "INSERT OR REPLACE INTO dishes\n  (loc, lang, date, type, dish, additives) \n",
      "  VALUES (\n",
      paste0(
      "    '", res$loc,  "', ",
        "  '", res$lang, "', ",
        "  '", res$date, "', ",
        "  '", res$type, "', ",
        "  '", stringr::str_replace_all(res$dish, "'", "''"), "', ",
        " '", stringr::str_replace_all(res$additives, "'", "''"), "'  "
        ),
      "\n  )"
    )
  db <- db_connect()
  db_ensure_table_exists(table="dishes")
  for( i in seq_along(sql) ){
    RSQLite::dbGetQuery(db, sql[i])
  }
  db_disconnect(db)
  # return
  return(TRUE)
}

#' wrapper function for get_mensaplan() and parse_mensaplan
#' @param date date for which to get data
#' @param lang language of the mensa plan -- either "de" or "en"
#' @param loc location identifier
mensaplan <- function(
  date = format(Sys.Date(), "%Y-%m-%d"),
  lang = "de",
  loc  = "mensa_giessberg"
){
  mp_data_retrieval()
  dat <- db_get_request_data(lang=lang, date=date, loc=loc)
  for( i in seq_along(dat[,1]) ){
    request_to_dish(dat[i, ])
  }
  res <- db_get_dish_data(lang = lang, date = date, loc = loc)
  class(res) <- c("mensaplan", "data.frame")
  res
}

#' customized print function for mensaplan data.frames
#' @inheritParams print.data.frame
print.mensaplan <- function (
  x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE
)
{
  n <- length(row.names(x))
  if (length(x) == 0L) {
    cat(sprintf(ngettext(n, "data frame with 0 columns and %d row",
                         "data frame with 0 columns and %d rows", domain = "R-base"),
                n), "\n", sep = "")
  }
  else if (n == 0L) {
    print.default(names(x), quote = FALSE)
    cat(gettext("<0 rows> (or 0-length row.names)\n"))
  }
  else {
    m <- as.matrix(format.data.frame(x, digits = digits,
                                     na.encode = FALSE))
    if (!isTRUE(row.names))
      dimnames(m)[[1L]] <- if (identical(row.names, FALSE))
        rep.int("", n)
    else row.names
    for (i in seq_along(m[1,])) {
      if ( max(nchar(m[,i])) > 20 ) {
        m[,i] <- paste0(substring(m[,i], 1, 16), " ...")
      }
    }
    print(m, ..., quote = quote, right = right)
  }
  invisible(x)
}











